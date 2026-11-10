# Shared, session-independent calculations. One observation is a transect-meter pair.
suppressPackageStartupMessages({library(dplyr); library(tidyr); library(ggplot2)})
reference_options <- c(
  'Official baseline' = 'official',
  'Median of official baseline, Con1 and Con2' = 'median012',
  'Mean of Con1 and Con2' = 'mean12',
  'Mean of Con1, Con2 and Con3' = 'mean123'
)
construction_end <- as.Date('2026-07-12')
survey_label <- function(survey, date_start, date_end) {
  paste0(survey, ' | ', date_start,
    ifelse(date_start == date_end, '', paste0(' to ', date_end)))
}
reference_spec <- function(key) {
  switch(key, official = list(surveys = 0L, fun = mean),
         median012 = list(surveys = 0:2, fun = median),
         mean12 = list(surveys = 1:2, fun = mean),
         mean123 = list(surveys = 1:3, fun = mean),
         stop('Unknown baseline option.'))
}
prepare_observations <- function(raw) {
  depths <- grep('^sed_depth_cm_', names(raw), value = TRUE)
  if(length(depths) != 3) stop('Expected three sediment depth replicate columns.')
  raw <- raw |> mutate(date = as.Date(date), survey = stringr::str_squish(survey),
    monitoring_phase = case_when(
      grepl('^Baseline$', survey, ignore.case = TRUE) ~ 'pre_construction',
      grepl('^Con\\s*[1-9][0-9]*$', survey, ignore.case = TRUE) ~ 'during_construction',
      grepl('^(Int|Interim)\\s*[1-9][0-9]*$', survey, ignore.case = TRUE) ~ 'post_construction'),
    survey_round = ifelse(monitoring_phase == 'pre_construction', 0L,
      as.integer(stringr::str_extract(survey, '\\d+'))))
  if(anyNA(raw$monitoring_phase)) stop('Unknown survey labels in source data.')
  # survey_n is a playback order, never a construction label for Interim surveys.
  last_con <- max(c(0L, raw$survey_round[raw$monitoring_phase == 'during_construction']))
  raw <- raw |> mutate(
    survey = case_when(monitoring_phase == 'pre_construction' ~ 'Baseline',
      monitoring_phase == 'during_construction' ~ paste0('Con', survey_round),
      TRUE ~ paste0('Int', survey_round)),
    survey_n = ifelse(monitoring_phase == 'post_construction', last_con + survey_round, survey_round),
    transect = gsub('\\s+', '_', trimws(transect)),
    transect_type = trimws(transect_type), meter = as.integer(meter),
    n_replicates = rowSums(!is.na(pick(all_of(depths)))),
    meter_mean_cm = rowMeans(pick(all_of(depths)), na.rm = TRUE))
  raw$meter_mean_cm[!is.finite(raw$meter_mean_cm)] <- NA_real_
  # Cumulative workbooks repeat history. Collapse only identical observations,
  # retaining the first source; conflicting measurements must be resolved explicitly.
  raw <- raw |> distinct(across(-source_workbook), .keep_all = TRUE)
  if(anyNA(raw$date) || anyNA(raw$meter) || anyNA(raw$transect) ||
     any(raw$meter < 0) || anyDuplicated(raw[c('transect','meter','survey')]))
    stop('Invalid or duplicate transect/meter/survey identifiers in source data.')
  if(any(raw$date[raw$monitoring_phase == 'during_construction'] > construction_end) ||
     any(raw$date[raw$monitoring_phase == 'post_construction'] <= construction_end))
    stop('Survey phase conflicts with the July 12, 2026 construction end date.')
  raw |> arrange(survey_n, transect, meter)
}
read_monitoring <- function(root) {
  folder <- file.path(root, 'data/sedimentation_monitoring')
  files <- list.files(folder, '\\.xlsx$', recursive = TRUE, full.names = TRUE)
  files <- files[!grepl('^~\\$', basename(files))]
  number <- vapply(stringr::str_extract_all(basename(files), '(?i)(?<=con)\\d+'),
                   function(x) if(length(x)) max(as.integer(x)) else NA_integer_, integer(1))
  construction <- which(!is.na(number))
  if(!length(construction)) stop('No cumulative construction workbook found.')
  latest <- files[construction[order(-number[construction], basename(files[construction]), files[construction])][1]]
  interim <- sort(files[grepl('(?i)(interim|int)\\s*[0-9]+', basename(files), perl = TRUE)])
  sources <- unique(c(latest, interim))
  raw <- bind_rows(lapply(sources, function(path) {
    readxl::read_excel(path, sheet = 'SedimentDepth_RawData') |> janitor::clean_names() |>
      select(date, survey, transect_type, transect, meter, starts_with('sed_depth_cm_')) |>
      mutate(source_workbook = basename(path))
  })) |> prepare_observations()
  inventory <- raw |> group_by(transect) |> summarise(transect_type = first(transect_type),
    has_construction = any(monitoring_phase == 'during_construction'),
    has_interim = any(monitoring_phase == 'post_construction'),
    has_monitoring = has_construction | has_interim, .groups = 'drop')
  gps <- readxl::read_excel(file.path(folder, 'PE_SB_Station_Transect_GPS.xlsx'), sheet = 'Hardbottom') |>
    janitor::clean_names() |> transmute(
      transect = gsub('\\s+', '_', trimws(stringr::str_remove(station_transect_id, '\\s+\\d+m$'))),
      meter = as.integer(stringr::str_extract(station_transect_id, '\\d+(?=m$)')),
      lat = as.numeric(lat_dd), lon = as.numeric(lon_dd)) |>
    filter(!is.na(meter), is.finite(lat), is.finite(lon)) |> arrange(transect, meter) |>
    inner_join(inventory, by = 'transect')
  gps <- gps |> mutate(prediction = case_when(transect %in% c('ST22','ST31') ~ '1.0 cm predicted',
      transect %in% c('ST28','ST5') ~ '1.5 mm predicted', TRUE ~ '0 cm predicted'))
  list(raw = raw, gps = gps, inventory = inventory, source = paste(basename(sources), collapse = '; '),
       construction_end = construction_end,
       prepared = as.character(Sys.Date()))
}
# Clip the map to the analysis segment, interpolating an endpoint when the GPS
# workbook provides only 0 m and 70 m anchors (BT23 and BT24).
map_segments <- function(gps, max_meter = 50) {
  gps |> group_by(transect) |> group_modify(function(d, key) {
    meters <- sort(unique(c(d$meter[d$meter >= 0 & d$meter <= max_meter], max_meter)))
    d[rep(1, length(meters)), ] |> mutate(meter = meters,
      lat = approx(d$meter, d$lat, xout = meters)$y,
      lon = approx(d$meter, d$lon, xout = meters)$y)
  }) |> ungroup()
}
calculate_analysis <- function(raw, selected, reference = 'official') {
  spec <- reference_spec(reference)
  # Support small analytical fixtures as well as the prepared, phase-aware bundle.
  if(!'monitoring_phase' %in% names(raw)) raw <- raw |> mutate(
    monitoring_phase = ifelse(survey_n == 0, 'pre_construction', 'during_construction'))
  if(!'source_workbook' %in% names(raw)) raw$source_workbook <- NA_character_
  raw <- raw |> filter(transect %in% selected, meter >= 0, meter <= 50)
  refs <- raw |> filter(monitoring_phase != 'post_construction', survey_n %in% spec$surveys) |> group_by(transect, meter) |>
    summarise(reference_cm = if(n_distinct(survey_n) == length(spec$surveys) && all(is.finite(meter_mean_cm)))
      spec$fun(meter_mean_cm) else NA_real_, .groups = 'drop')
  raw <- raw |> filter(monitoring_phase != 'pre_construction',
    monitoring_phase == 'post_construction' | survey_n > max(spec$surveys))
  candidates <- raw |>
    left_join(refs, by = c('transect','meter')) |> mutate(delta_cm = meter_mean_cm - reference_cm)
  # Fix the meter cohort for the entire analysis, including all selected surveys.
  valid <- candidates |> group_by(transect, meter) |>
    summarise(valid = all(is.finite(delta_cm)), .groups = 'drop') |> filter(valid)
  candidates <- candidates |> semi_join(valid, by = c('transect','meter'))
  coverage <- candidates |> distinct(transect, survey_n) |> count(survey_n, name = 'n_transects')
  shared <- coverage |> filter(n_transects == length(selected)) |> pull(survey_n)
  deltas <- candidates |> filter(survey_n %in% shared)
  # Require each retained meter to be observed in every shared survey.
  cohort <- deltas |> count(transect, meter) |> filter(n == length(shared))
  deltas <- deltas |> semi_join(cohort, by = c('transect','meter')) |> arrange(survey_n, transect, meter)
  omitted <- raw |> filter(!survey_n %in% shared) |> group_by(survey_n, survey) |>
    summarise(date_start = first(sort(date)), date_end = last(sort(date)), .groups = 'drop') |>
    arrange(survey_n) |> mutate(label = survey_label(survey, date_start, date_end))
  empty <- list(deltas = deltas, omitted = omitted$survey_n, omitted_surveys = omitted,
                reference = reference, selected = selected, references = refs)
  if(!nrow(deltas) || n_distinct(deltas$transect) != length(selected)) return(empty)
  surveys <- deltas |> group_by(survey_n, survey, monitoring_phase) |> summarise(date_start = min(date), date_end = max(date),
      n_positions = n(), mean_delta_cm = mean(delta_cm),
      source_workbook = paste(sort(unique(source_workbook)), collapse = '; '), .groups = 'drop') |> arrange(survey_n) |>
    mutate(label = survey_label(survey, date_start, date_end))
  xmax <- max(1, ceiling(2 * max(abs(deltas$delta_cm))) / 2)
  thresholds <- sort(unique(c(seq(0, xmax, by = .05), 5)))
  profiles <- bind_rows(lapply(surveys$survey_n, function(s) {
    x <- deltas$delta_cm[deltas$survey_n == s]
    a <- vapply(thresholds, function(t) mean(if(t == 0) x > 0 else x >= t), numeric(1))
    l <- vapply(thresholds, function(t) mean(if(t == 0) x < 0 else x <= -t), numeric(1))
    tibble(survey_n = s, threshold_cm = thresholds, accumulation = a, loss = l, net = a-l)
  })) |> group_by(threshold_cm) |> arrange(survey_n, .by_group = TRUE) |>
    mutate(maximum_net = cummax(net)) |> ungroup() |> left_join(surveys, by = 'survey_n') |>
    mutate(label = factor(label, levels = surveys$label))
  bands <- profiles |> filter(round(threshold_cm, 2) %in% c(.15,.5,1)) |>
    mutate(key = paste0('t', round(threshold_cm * 100))) |>
    select(survey_n, key, net) |> pivot_wider(names_from = key, values_from = net) |>
    transmute(survey_n, `1 cm+` = t100, `5 mm to <1 cm` = t50-t100, `1.5 mm to <5 mm` = t15-t50) |>
    pivot_longer(-survey_n, names_to = 'band', values_to = 'contribution') |>
    left_join(surveys, by = 'survey_n') |>
    mutate(survey = factor(survey, levels = surveys$survey), label = factor(label, levels = surveys$label),
           band = factor(band, levels = c('1 cm+','5 mm to <1 cm','1.5 mm to <5 mm')),
           direction = ifelse(contribution >= 0, 'Accumulation', 'Loss'))
  c(empty, list(surveys = surveys, profiles = profiles, bands = bands, xmax = xmax))
}
plot_style <- function() theme_minimal(base_size = 12) + theme(
  panel.grid.minor = element_blank(), legend.position = 'bottom',
  plot.title = element_text(face = 'bold', color = '#153e4a'),
  plot.title.position = 'plot', legend.title = element_blank())
plot_tails <- function(a, survey_n = NULL, summary = FALSE) {
  p <- a$profiles |> filter(threshold_cm <= a$xmax)
  if(!is.null(survey_n)) p <- filter(p, .data$survey_n == !!survey_n)
  g <- ggplot(p, aes(threshold_cm)) + geom_hline(yintercept = 0, color = 'grey55')
  if(!summary) {
    tails <- p |> select(survey_n, label, threshold_cm, accumulation, loss) |>
      mutate(loss = -loss) |> pivot_longer(c(accumulation,loss), names_to = 'tail', values_to = 'fraction') |>
      mutate(tail = ifelse(tail == 'accumulation', 'Accumulation', 'Equivalent loss')) |>
      group_by(survey_n, tail) |> arrange(threshold_cm, .by_group = TRUE) |>
      mutate(end = lead(threshold_cm, default = a$xmax)) |> ungroup()
    g <- g + geom_rect(data = tails, aes(xmin = threshold_cm, xmax = end,
      ymin = pmin(fraction, 0), ymax = pmax(fraction, 0), fill = tail), inherit.aes = FALSE, alpha = .28) +
      geom_step(data = tails, aes(y = fraction, color = tail, group = tail), linewidth = .7)
  }
  g <- g + geom_step(aes(y = net, color = 'Net balance'), linewidth = .85)
  if(!is.null(survey_n)) g <- g + geom_step(aes(y = maximum_net, color = 'Maximum net to date'), linetype = 'longdash', linewidth = .9)
  means <- distinct(p, survey_n, label, mean_delta_cm)
  g <- g + geom_vline(xintercept = c(.15,.5,1), linetype = 'dotted', color = 'grey55') +
    geom_vline(data = means, aes(xintercept = abs(mean_delta_cm)), linetype = 'longdash',
               color = ifelse(means$mean_delta_cm >= 0, '#7f0000', '#084594')) +
    scale_color_manual(values = c('Accumulation' = '#b2182b','Equivalent loss' = '#2166ac',
      'Net balance' = '#222222','Maximum net to date' = '#1b7837')) +
    scale_fill_manual(values = c('Accumulation' = '#d6604d','Equivalent loss' = '#4393c3'), guide = 'none') +
    guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
    scale_x_continuous(limits = c(0,a$xmax), expand = expansion(mult = c(0,.01))) +
    scale_y_continuous(labels = scales::label_percent(), limits = c(-1,1)) +
    labs(x = 'Absolute sediment-depth change threshold (cm)', y = if(summary) 'Net prevalence' else 'Prevalence',
      title = if(is.null(survey_n)) 'Every survey' else sub(' | ', '\n', as.character(p$label[1]), fixed = TRUE),
      subtitle = 'Dotted: 1.5 mm, 5 mm, 1 cm\nLong dash: absolute mean change') + plot_style()
  if(is.null(survey_n)) g <- g + facet_wrap(~label, ncol = 3) + theme(strip.text = element_text(size = 9))
  g
}
plot_heatmap <- function(a) {
  d <- a$deltas |> left_join(select(a$surveys, survey_n, label), by = 'survey_n') |>
    mutate(label = factor(label, levels = rev(a$surveys$label)))
  lim <- max(.1, abs(quantile(d$delta_cm, c(.02,.98))))
  ggplot(d, aes(meter, label, fill = delta_cm)) + geom_tile() +
    geom_point(data = filter(d, delta_cm >= 1), shape = 21, fill = NA, size = 1, stroke = .3) +
    facet_wrap(~transect, ncol = 2) +
    scale_fill_gradient2(low = '#2166ac', mid = 'white', high = '#b2182b', limits = c(-lim,lim), oob = scales::squish) +
    labs(x = 'Meter position', y = NULL, fill = 'Change (cm)', title = 'Where sediment changed',
      subtitle = 'Outlined cells: ≥1 cm accumulation. Color scale clipped at the 2nd / 98th percentiles.') + plot_style() +
    theme(axis.text.y = element_text(size = 8), panel.grid = element_blank())
}
plot_bands <- function(a) {
  top <- ggplot(a$bands, aes(label, contribution, fill = direction, alpha = band, group = band)) +
    geom_hline(yintercept = 0, color = 'grey55') + geom_col(position = position_stack(reverse = TRUE), width = .75) +
    scale_y_continuous(labels = scales::label_percent(), limits = c(-1,1)) +
    scale_fill_manual(values = c(Accumulation = '#b2182b', Loss = '#2166ac')) +
    scale_alpha_manual(values = c('1 cm+' = 1, '5 mm to <1 cm' = .65, '1.5 mm to <5 mm' = .35)) +
    labs(x = NULL, y = 'Net prevalence contribution', title = 'Net change by sediment-depth band') + plot_style() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  bottom <- ggplot(a$surveys, aes(factor(label, levels = a$surveys$label), mean_delta_cm, group = 1)) +
    geom_hline(yintercept = 0, color = 'grey55') + geom_line(color = '#287271') + geom_point(color = '#153e4a') +
    scale_x_discrete(labels = function(x) sub(' | ', '\n', x, fixed = TRUE)) +
    labs(x = 'Survey and sampling date(s)', y = 'Mean change (cm)') + plot_style() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 9))
  patchwork::wrap_plots(top, bottom, heights = c(2,1))
}
