source('shiny/sedimentation/R/analysis.R')
library(testthat)
bundle <- readRDS('shiny/sedimentation/data/monitoring.rds')
test_that('Interim survey identity, chronology, dates and conflicts are validated', {
  raw <- tibble(date = as.Date(c('2025-09-16','2025-11-06','2026-07-08','2026-07-17','2026-07-24')),
    survey = c('Baseline','Con1','Con37','Int1','Interim2'), transect = 'X',
    transect_type = 'Permanent', meter = 0, sed_depth_cm_1 = 1,
    sed_depth_cm_2 = 2, sed_depth_cm_3 = 3, source_workbook = 'example.xlsx')
  d <- prepare_observations(raw)
  expect_equal(d$survey, c('Baseline','Con1','Con37','Int1','Int2'))
  expect_equal(d$survey_n, c(0L,1L,37L,38L,39L))
  expect_equal(d$survey_round, c(0L,1L,37L,1L,2L))
  expect_equal(tail(d$monitoring_phase,2), rep('post_construction',2))
  repeated <- bind_rows(raw, mutate(raw, source_workbook = 'cumulative.xlsx'))
  expect_equal(prepare_observations(repeated), d)
  conflicting <- bind_rows(raw, mutate(raw[4,], sed_depth_cm_1 = 9, source_workbook = 'revision.xlsx'))
  expect_error(prepare_observations(conflicting), 'duplicate')
  invalid <- raw; invalid$survey[4] <- 'Con38'
  expect_error(prepare_observations(invalid), 'construction end date')
  invalid <- raw; invalid$date[4] <- construction_end
  expect_error(prepare_observations(invalid), 'construction end date')
  invalid <- raw; invalid$survey[4] <- 'Unclassified1'
  expect_error(prepare_observations(invalid), 'Unknown survey')
  # Int1 must never serve as Con1, even without any construction observations.
  interim_only <- prepare_observations(raw[c(1,4),])
  expect_equal(calculate_analysis(interim_only, 'X')$surveys$survey, 'Int1')
  expect_null(calculate_analysis(interim_only, 'X', 'mean12')$profiles)
})
test_that('refresh discovers Interim workbooks and preserves construction history', {
  refreshed <- read_monitoring('.')
  expect_equal(refreshed$raw, bundle$raw)
  expect_equal(nrow(bundle$raw), 18750L + 1359L)
  expect_equal(sum(bundle$inventory$has_construction), 13L)
  expect_equal(sum(bundle$inventory$has_interim), 19L)
  expect_equal(sum(bundle$inventory$has_monitoring), 19L)
  expect_false(anyDuplicated(bundle$raw[c('transect','meter','survey')]) > 0)
  expect_equal(sum(bundle$raw$survey == 'Int1'), 1359L)
  expect_setequal(unique(bundle$raw$date[bundle$raw$survey == 'Int1']),
    as.Date(c('2026-07-17','2026-07-22','2026-07-23','2026-07-24')))
  expect_equal(unique(bundle$raw$source_workbook[bundle$raw$survey == 'Int1']),
    'PESandBypass_SedimentTransects_Interim1_Data (1).xlsx')
  # Construction-only refreshes still work when no Interim file is present.
  root <- tempfile('sediment-refresh-'); folder <- file.path(root, 'data/sedimentation_monitoring')
  dir.create(folder, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE))
  originals <- c('data/sedimentation_monitoring/2_During-Construction/Data/Weekly Monitoring/2025-2026_PESandBypass_TransectData_Con37.xlsx',
    'data/sedimentation_monitoring/PE_SB_Station_Transect_GPS.xlsx')
  expect_true(all(file.copy(originals, folder)))
  old <- read_monitoring(root)
  expect_equal(old$raw, filter(bundle$raw, monitoring_phase != 'post_construction'))
  expect_equal(sum(old$inventory$has_monitoring), 13L)
})
test_that('Int1 continues the same time series with survey names and actual dates', {
  ids <- bundle$inventory$transect
  for(selected in c(as.list(ids), list(ids))) {
    a <- calculate_analysis(bundle$raw, selected)
    latest <- filter(a$surveys, survey == 'Int1')
    d <- filter(a$deltas, survey == 'Int1')
    expect_equal(tail(a$surveys$survey, 1), 'Int1')
    expect_equal(latest$n_positions, 51L * length(selected))
    expect_equal(latest$mean_delta_cm, mean(d$meter_mean_cm - d$reference_cm))
    p <- filter(a$profiles, threshold_cm == 1)
    expect_equal(p$net[p$survey == 'Int1'], mean(d$delta_cm >= 1) - mean(d$delta_cm <= -1))
    expect_equal(p$maximum_net, cummax(p$net))
    expect_equal(sum(a$bands$contribution[a$bands$survey == 'Int1']), mean(d$delta_cm >= .15) - mean(d$delta_cm <= -.15))
    expect_equal(a$surveys$label, survey_label(a$surveys$survey, a$surveys$date_start, a$surveys$date_end))
  }
  combined <- calculate_analysis(bundle$raw, ids)
  expect_equal(combined$surveys$survey, 'Int1')
  expect_equal(combined$surveys$date_start, as.Date('2026-07-17'))
  expect_equal(combined$surveys$date_end, as.Date('2026-07-24'))
  expect_equal(combined$surveys$label, 'Int1 | 2026-07-17 to 2026-07-24')
  expect_false('Int1' %in% combined$omitted_surveys$survey)
  for(key in unname(reference_options)) {
    a <- calculate_analysis(bundle$raw, 'ST22', key)
    expect_equal(tail(a$surveys$survey, 3), c('Con35','Con36','Int1'))
    expect_equal(tail(a$surveys$label, 3), c('Con35 | 2026-06-22', 'Con36 | 2026-06-29', 'Int1 | 2026-07-17'))
    history <- calculate_analysis(filter(bundle$raw, survey != 'Int1'), 'ST22', key)
    expect_equal(filter(a$deltas, survey != 'Int1'), history$deltas)
    actual <- a$profiles |> filter(survey != 'Int1') |>
      semi_join(history$profiles, by = c('survey_n','threshold_cm')) |> mutate(label = as.character(label))
    expect_equal(actual, mutate(history$profiles, label = as.character(label)))
  }
  added <- bundle$inventory |> filter(has_interim, !has_construction) |> pull(transect)
  expect_setequal(added, c('ACER_NS','ACER_WE','BT23','BT23_S','BT24','BT24_N'))
  expect_null(calculate_analysis(bundle$raw, 'ACER_NS', 'median012')$profiles)
})
test_that('all monitored map segments include their 50 m endpoint', {
  gps <- map_segments(bundle$gps)
  coverage <- gps |> group_by(transect) |> summarise(start = min(meter), end = max(meter))
  expect_equal(nrow(coverage), 19L)
  expect_true(all(coverage$start == 0 & coverage$end == 50))
  expect_true(all(is.finite(gps$lat) & is.finite(gps$lon)))
  for(id in c('BT23','BT24')) {
    anchors <- filter(bundle$gps, transect == id)
    endpoint <- filter(gps, transect == id, meter == 50)
    expect_equal(endpoint$lat, anchors$lat[1] + 50/70 * diff(anchors$lat))
    expect_equal(endpoint$lon, anchors$lon[1] + 50/70 * diff(anchors$lon))
  }
})
test_that('references use the requested statistics and exclude reference rounds', {
  raw <- tibble(transect = 'X', meter = 0L, survey_n = 0:4,
    survey = c('Baseline',paste0('Con',1:4)), date = as.Date('2025-01-01') + 0:4,
    meter_mean_cm = c(0,2,10,4,8))
  expected <- c(official = 0, median012 = 2, mean12 = 6, mean123 = 16/3)
  for(key in names(expected)) {
    a <- calculate_analysis(raw,'X',key)
    expect_equal(a$references$reference_cm, unname(expected[key]))
    expect_true(all(a$deltas$survey_n > max(reference_spec(key)$surveys)))
    expect_equal(a$deltas$delta_cm, a$deltas$meter_mean_cm - unname(expected[key]))
  }
})
test_that('pooling preserves transect identity and weights positions equally', {
  raw <- expand_grid(transect = c('A','B'), meter = 0:1, survey_n = 0:3) |>
    mutate(date = as.Date('2025-01-01') + survey_n, survey = ifelse(survey_n == 0, 'Baseline',paste0('Con',survey_n)),
           meter_mean_cm = ifelse(survey_n == 0, ifelse(transect == 'A',10,20), ifelse(transect == 'A',11,19)))
  # B has one sampled meter, A has two. Pooled mean is 1/3, not zero.
  raw <- raw |> filter(!(transect == 'B' & meter == 1))
  a <- calculate_analysis(raw,c('A','B'))
  expect_equal(a$surveys$mean_delta_cm, rep(1/3,3))
  p <- filter(a$profiles, threshold_cm == 1)
  expect_equal(p$accumulation, rep(2/3,3)); expect_equal(p$loss, rep(1/3,3))
  raw <- raw |> filter(!(transect == 'B' & survey_n == 2))
  a <- calculate_analysis(raw,c('A','B'))
  expect_equal(a$surveys$survey_n,c(1L,3L)); expect_equal(a$omitted,2L)
})
test_that('zero changes, signed maxima, and incomplete references are handled', {
  raw <- expand_grid(transect = 'A', meter = 0:1, survey_n = 0:4) |>
    mutate(date = as.Date('2025-01-01') + survey_n, survey = paste0('Con',survey_n),
      meter_mean_cm = ifelse(survey_n == 0,2, ifelse(meter == 0,2,1)))
  a <- calculate_analysis(raw,'A')
  p <- filter(a$profiles, threshold_cm == 0)
  expect_equal(p$accumulation, rep(0,4)); expect_equal(p$loss, rep(.5,4))
  expect_equal(p$maximum_net, rep(-.5,4))
  raw$meter_mean_cm[raw$meter == 1 & raw$survey_n == 1] <- NA_real_
  a <- calculate_analysis(raw,'A','mean12')
  expect_equal(unique(a$deltas$meter),0L)
})
test_that('real data reproduce direct empirical calculations for all references', {
  ids <- bundle$inventory$transect[bundle$inventory$has_construction]
  expect_equal(nrow(bundle$inventory),19L); expect_equal(length(ids),13L)
  expect_setequal(bundle$inventory$transect,unique(bundle$gps$transect))
  for(key in unname(reference_options)) {
    for(selected in c(as.list(ids), list(ids))) {
      a <- calculate_analysis(bundle$raw,selected,key)
      expect_true(nrow(a$deltas) > 0)
      expect_length(unique(a$surveys$n_positions),1)
      for(s in a$surveys$survey_n) {
        x <- a$deltas$delta_cm[a$deltas$survey_n == s]
        p <- filter(a$profiles, survey_n == s, round(threshold_cm,2) == 1)
        expect_equal(p$net,mean(x >= 1) - mean(x <= -1))
        bands <- filter(a$bands, survey_n == s)
        expect_equal(sum(bands$contribution),mean(x >= .15) - mean(x <= -.15))
      }
      expect_true(all(a$profiles$maximum_net >= a$profiles$net))
    }
  }
  for(id in c('ST1','ST38')) expect_equal(max(calculate_analysis(bundle$raw,id)$deltas$meter),50L)
})
test_that('plots render and server responds to changed selections and baselines', {
  old <- getwd()
  setwd('shiny/sedimentation')
  on.exit(setwd(old))
  source('app.R', local = TRUE)
  excluded <- c('ACER_NS','ACER_WE','BT23','BT23_S','BT24','BT24_N')
  expect_length(active_transects, 13L)
  expect_false(any(excluded %in% active_transects))
  shiny::testServer(server, {
    session$setInputs(transects = 'ST22', reference = 'official', frame = 1)
    expect_equal(analysis()$selected,'ST22')
    expect_true(length(output$animation) > 0)
    session$setInputs(transects = c('ST22','ST28'), reference = 'median012', frame = 999)
    expect_equal(min(analysis()$surveys$survey_n),3L)
    expect_equal(frame(), nrow(analysis()$surveys) + 1L)
    expect_true(length(output$bands) > 0)
    expect_true(length(output$animation) > 0)
    expect_true(length(output$heatmap) > 0)
    session$setInputs(transects = character())
    expect_error(analysis())
    for(id in excluded) {
      session$setInputs(transects = id, reference = 'official')
      expect_length(selected(), 0L)
      expect_error(analysis(), 'Select at least one transect')
    }
    expect_setequal(unique(gps$transect), active_transects)
    # Even stale selections containing all source transects must use only the 13.
    session$setInputs(transects = monitoring$inventory$transect, reference = 'official')
    expect_equal(length(selected()),13L)
    expect_true(all(c('Con1','Con36','Int1') %in% analysis()$surveys$survey))
    expect_equal(tail(analysis()$surveys$survey,1),'Int1')
    expect_equal(unique(analysis()$surveys$n_positions),663L)
    expect_match(output$metrics$html, 'Int1')
    expect_match(output$metrics$html, '2026-07-17 to 2026-07-23')
    expect_true(length(output$animation) > 0)
    expect_true(length(output$bands) > 0)
    expect_true(length(output$heatmap) > 0)
    meter_export <- read.csv(output$download)
    expect_setequal(unique(meter_export$transect),active_transects)
    expect_false(any(meter_export$transect %in% excluded))
    interim_export <- filter(meter_export, survey == 'Int1')
    expect_equal(nrow(interim_export),663L)
    expect_equal(unique(interim_export$monitoring_phase),'post_construction')
    expect_equal(unique(interim_export$source_workbook),
      'PESandBypass_SedimentTransects_Interim1_Data (1).xlsx')
    summary_export <- read.csv(output$summary_download)
    expect_true(all(c('Con1','Con36','Int1') %in% summary_export$survey))
    expect_equal(unique(summary_export$label[summary_export$survey == 'Int1']),
      'Int1 | 2026-07-17 to 2026-07-23')
    expect_equal(unique(summary_export$selected_transects),paste(active_transects, collapse = ';'))
    session$setInputs(reference = 'mean12')
    expect_equal(min(analysis()$surveys$survey_n),3L)
    expect_true(all(c('Con3','Con36','Int1') %in% analysis()$surveys$survey))
    session$setInputs(transects = 'not-a-transect')
    expect_error(analysis(), 'Select at least one transect')
  })
})
