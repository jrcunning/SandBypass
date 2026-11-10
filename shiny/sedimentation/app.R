library(shiny)
library(bslib)
library(leaflet)
source('R/analysis.R', local = TRUE)
monitoring <- readRDS('data/monitoring.rds')
active_transects <- sort(monitoring$inventory$transect[monitoring$inventory$has_construction])
choices <- setNames(active_transects, active_transects)
ui <- page_sidebar(
  title = 'Sand Bypass | Sediment explorer',
  fillable = FALSE,
  theme = bs_theme(version = 5, primary = '#146b70', bg = '#f4f7f8', fg = '#193b45', base_font = 'system-ui'),
  sidebar = sidebar(width = 310,
    tags$p(class = 'eyebrow', 'SEDIMENT MONITORING'),
    tags$h3('Explore the transects'),
    tags$p('Select a transect on the map or combine several to explore their shared monitoring record.'),
    selectizeInput('transects', 'Selected transects', choices, selected = 'ST22', multiple = TRUE),
    div(class = 'button-row', actionButton('all', 'All monitored'), actionButton('clear', 'Clear')),
    selectInput('reference', 'Baseline reference', reference_options, selected = 'official'),
    tags$hr(), uiOutput('selection_info'),
    downloadButton('download', 'Download meter data'),
    downloadButton('summary_download', 'Download survey summary'),
    downloadButton('coverage_download', 'Download survey coverage'),
    tags$hr(), tags$small('Source: ', monitoring$source),
    tags$small(paste('Data prepared:', monitoring$prepared))
  ),
  tags$head(tags$style(HTML('
    .leaflet-container {isolation:isolate;z-index:0}
    .eyebrow {font-size:11px;letter-spacing:2px;font-weight:700;color:#14777a}
    h3 {font-weight:700}.button-row{display:flex;gap:8px;margin-bottom:18px}
    .button-row .btn{padding:8px 10px;white-space:nowrap;font-size:13px}
    .card{border:1px solid #dbe5e7;box-shadow:0 3px 14px #193b4508}
    .notice{padding:12px 16px;border-left:3px solid #16858a;background:#eaf4f3;margin:12px 0}
    .metrics{display:flex;align-items:center;gap:20px;padding:12px 4px;overflow-x:auto}
    .metric-survey{flex:0 0 105px}.metric-survey strong{display:block;font-size:22px}
    .metric-survey span{display:block;font-size:12px;color:#547078}
    .metric-table{flex:1;border-collapse:collapse;min-width:460px;font-variant-numeric:tabular-nums}
    .metric-table th,.metric-table td{padding:5px 8px;text-align:right;white-space:nowrap}
    .metric-table thead th{font-size:12px;color:#547078;font-weight:600;text-align:center}
    .metric-table thead{border-bottom:1px solid #dbe5e7}
    .metric-table tbody th{text-align:left;font-size:12px;color:#547078;font-weight:500}
    .metric-table td{font-size:20px;font-weight:700}
    .metric-table .metric-net{border-left:1px solid #dbe5e7}
    .shiny-plot-output{background:white}
    .bslib-sidebar-layout>.main{gap:18px} .tab-content{padding-top:14px}
  '))),
  card(card_header('Monitoring footprint'),
    tags$p('Click a transect line to add or remove it. Selected lines are thicker. Hover over a line to see its name and predicted sedimentation.'),
    tags$p('Solid colored lines show the common 0–50 m analysis segment. Gray dashed lines show the 50–150 m extensions of ST1 and ST38, which were not monitored at every survey. See Survey coverage for actual measurements; gray does not mean zero sedimentation.'),
    leafletOutput('map', height = 360)),
  uiOutput('coverage_note'),
  card(navset_tab(
    nav_panel('Change through time', tags$p('Play through the shared surveys, or drag to a survey. The green curve tracks the largest signed net balance reached at each threshold.'),
      uiOutput('frame_control'), uiOutput('metrics'), plotOutput('animation', height = 480),
      tags$details(tags$summary('How to read these curves'),
        tags$p('Red is the fraction accumulating at least the threshold depth. Blue is the fraction losing at least that depth, plotted below zero. Black is accumulation minus loss. At zero, unchanged positions are excluded from both tails. Green is a threshold-specific running maximum, which may be negative.'),
        tags$p('The dark red or blue vertical dash marks the absolute mean change (red = positive, blue = negative). Percentages are fractions of sampled meter positions, not estimates of impacted seabed area.'))),
    nav_panel('Change by depth band', plotOutput('bands', height = 750)),
    nav_panel('Change along transects',
      tags$p('Sediment change is shown for the common 0–50 m analysis segment. Use Survey coverage to see where measurements exist over the full 150 m length of ST1 and ST38.'),
      uiOutput('heatmap_container')),
    nav_panel('Survey coverage',
      tags$p('This view shows measurement coverage, independently of the baseline reference. Teal means a sediment-depth measurement exists, including measured zeros. Gray means no measurement; orange means a record exists but its depth is missing.'),
      tags$p('ST1 and ST38 extend to 150 m. Only their common 0–50 m segments enter the time-series statistics. Measurements beyond 50 m are shown here, but are not added to the statistical denominator. Dashed boundaries mark the end of the common segment.'),
      uiOutput('coverage_container')),
    nav_panel('About the analysis',
      tags$h4('Meter-matched change'),
      tags$p('The available sediment-depth replicates are averaged at each transect-meter-survey position. The chosen reference is computed at that same position, with equal weight for each reference survey. Change equals survey mean minus reference depth, in centimeters. Positive values indicate accumulation.'),
      tags$p('Official baseline results begin at Con1. The median of Baseline/Con1/Con2 and the mean of Con1/Con2 begin at Con3. The mean of Con1/Con2/Con3 begins at Con4. Construction-based references are sensitivity analyses; they include construction observations.'),
      tags$h4('Combining transects and maintaining coverage'),
      tags$p('The app pools individual transect-meter changes, rather than averaging transect curves. Each retained meter position has equal weight. Only surveys available for every selected transect are included. A fixed set of positions observed across all included surveys is used. Positions with incomplete reference surveys or non-finite changes are excluded and reported.'),
      tags$p('Analyses use meters 0–50 to match the notebook’s consistently monitored segments, including ST1 and ST38. The map shows the full 150 m lengths of ST1 and ST38, with their 50–150 m extensions drawn as gray dashed lines. The explorer includes the 13 transects sampled during construction, with Int1 continuing their monitoring record.'),
      tags$h4('Unequal survey coverage'),
      tags$p('ST1 and ST38 were surveyed to 150 m at Baseline and Int1. In the loaded construction rounds, their measurements stop at 50 m. The monitoring protocol allows longer construction surveys following sediment impacts greater than 1 cm above baseline. Such intermittent or triggered coverage is not pooled into the consistently sampled 0–50 m time series.'),
      tags$p('The Survey coverage view includes reference surveys and all recorded rounds, even when a round is omitted from a pooled change analysis. It distinguishes absent measurements from recorded depths, including zeros. Where a selected transect has no observations for an entire round, the date range comes from the other project transects. No missing measurements are filled with zero, and missing coverage does not establish an absence of sedimentation.'),
      tags$h4('Survey names and dates'),
      tags$p('All surveys form one time series, including Int1 after Con37. Each survey is labeled with its supplied name and the actual sampling date or date range for the selected transects. Historical maxima continue across the full series, including Int1.'),
      tags$p('ACER_NS, ACER_WE, BT23, BT23_S, BT24, and BT24_N have only Baseline and Int1 observations. They cannot support construction-based reference comparisons and are excluded from the map, selector, analyses, and downloads. Their records remain in the source data.'),
      tags$h4('Empirical tails and depth bands'),
      tags$p('At each 0.05 cm threshold, accumulation is P(change ≥ threshold), loss is P(change ≤ −threshold), and net balance is accumulation minus loss. At zero, strict inequalities exclude unchanged observations. The animation retains the maximum signed net balance at each threshold through the current survey.'),
      tags$p('The disjoint depth-band contributions are net(1 cm), net(0.5 cm) − net(1 cm), and net(0.15 cm) − net(0.5 cm). Their signed sum equals net prevalence at 1.5 mm. They describe magnitude and direction; they do not establish the cause of sediment change.'),
      tags$p('The notebook uses a mean of Baseline, Con1 and Con2. This app implements the requested median for that option. Survey numbers can skip when the cumulative workbook has no observations for those rounds.'))
  ))
)
server <- function(input, output, session) {
  selected <- reactive(sort(intersect(input$transects, active_transects)))
  observeEvent(input$all, updateSelectizeInput(session, 'transects', selected = active_transects))
  observeEvent(input$clear, updateSelectizeInput(session, 'transects', selected = character()))
  toggle <- function(id) {
    if(!is.null(id)) id <- sub('::extension$', '', id)
    if(is.null(id) || !id %in% active_transects) return()
    s <- selected()
    updateSelectizeInput(session, 'transects', selected = if(id %in% s) setdiff(s,id) else c(s,id))
  }
  observeEvent(input$map_shape_click, toggle(input$map_shape_click$id))
  gps <- monitoring$gps |> filter(transect %in% active_transects) |> map_segments()
  extensions <- monitoring$gps |> filter(transect %in% active_transects, transect %in% c('ST1','ST38')) |>
    map_segments(min_meter = 50, max_meter = 150)
  map_extent <- bind_rows(gps, extensions)
  palette <- c('1.0 cm predicted' = '#c1121f', '1.5 mm predicted' = '#f28e2b', '0 cm predicted' = '#287271')
  draw_transects <- function(m, selection) {
    for(id in unique(extensions$transect)) {
      d <- filter(extensions, transect == id)
      m <- addPolylines(m, data = d, lng = ~lon, lat = ~lat, layerId = paste0(id, '::extension'),
        group = 'transects', color = '#80868b', dashArray = '8,6',
        weight = if(id %in% selection) 7 else 4, opacity = .9,
        label = paste(id, '• 50–150 m extension • Variable survey coverage; see Survey coverage • Excluded from time-series statistics'),
        options = pathOptions(bubblingMouseEvents = FALSE))
    }
    for(id in unique(gps$transect)) {
      d <- filter(gps, transect == id)
      m <- addPolylines(m, data = d, lng = ~lon, lat = ~lat, layerId = id,
        group = 'transects', color = unname(palette[d$prediction[1]]),
        weight = if(id %in% selection) 10 else 5, opacity = .95,
        label = paste(id, '•', d$prediction[1], if(id %in% selection) '• selected' else ''),
        options = pathOptions(bubblingMouseEvents = FALSE))
    }
    m
  }
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 10)) |>
      addTiles() |>
      fitBounds(min(map_extent$lon), min(map_extent$lat), max(map_extent$lon), max(map_extent$lat)) |>
      addScaleBar(position = 'bottomleft') |>
      draw_transects(isolate(selected())) |>
      addLegend('bottomright', colors = unname(palette), labels = names(palette), title = 'Modeled sedimentation') |>
      addLegend('bottomleft', colors = '#80868b', labels = '50–150 m: variable survey coverage', title = 'Gray dashed extensions')
  })
  observe({
    # Replace lines by layer ID so selection changes preserve the map viewport.
    draw_transects(leafletProxy('map', session), selected())
  })
  output$selection_info <- renderUI({
    tags$p(paste(length(selected()), 'transect(s) selected. Analysis segment: 0–50 m.'))
  })
  analysis <- reactive({
    validate(need(length(selected()) > 0, 'Select at least one transect to explore the monitoring data.'))
    a <- calculate_analysis(monitoring$raw, selected(), input$reference)
    validate(need(!is.null(a$profiles), 'No complete, shared observations are available for this selection and baseline.'))
    a
  }) |> bindCache(selected(), input$reference)
  coverage <- reactive({
    validate(need(length(selected()) > 0, 'Select at least one transect to explore survey coverage.'))
    survey_coverage(filter(monitoring$raw, transect %in% active_transects), selected())
  }) |> bindCache(selected())
  output$coverage_note <- renderUI({
    a <- analysis()
    n <- nrow(distinct(a$deltas, transect, meter))
    expected <- monitoring$raw |> filter(transect %in% selected(), meter <= 50,
      monitoring_phase != 'pre_construction',
      monitoring_phase == 'post_construction' | survey_n > max(reference_spec(input$reference)$surveys)) |>
      distinct(transect, meter) |> nrow()
    div(class = 'notice', paste('Reference:', names(reference_options)[match(input$reference, reference_options)],
      '•', n, 'fixed meter positions •', nrow(a$surveys),
      if(nrow(a$surveys) == 1) 'shared survey.' else 'shared surveys.'),
      if(any(selected() %in% c('ST1','ST38'))) tags$p('ST1/ST38 statistics cover only 0–50 m of each 150 m transect. Their outer 100 m are not assumed to have zero sedimentation; see Survey coverage for the full measurement record.'),
      if(length(a$omitted)) tags$p(paste('Omitted rounds without coverage for every selected transect:', paste(a$omitted_surveys$label, collapse = '; '))),
      if(expected > n) tags$p(paste(expected - n, 'meter positions excluded because reference or shared-survey coverage is incomplete.')))
  })
  output$frame_control <- renderUI({
    a <- analysis()
    sliderInput('frame', 'Survey playback', min = 1,
      max = nrow(a$surveys) + 1, value = 1, step = 1, ticks = FALSE,
      animate = animationOptions(interval = 800, loop = FALSE))
  })
  frame <- reactive({
    a <- analysis()
    i <- if(is.null(input$frame)) 1L else as.integer(input$frame)
    max(1L, min(i, nrow(a$surveys) + 1L))
  })
  output$animation <- renderPlot({
    a <- analysis(); i <- frame()
    plot_tails(a, a$surveys$survey_n[min(i,nrow(a$surveys))], summary = i > nrow(a$surveys))
  }, res = 110)
  output$metrics <- renderUI({
    a <- analysis(); i <- frame()
    s <- a$surveys[min(i,nrow(a$surveys)),]
    final_frame <- i > nrow(a$surveys)
    maximum_mean <- max(a$surveys$mean_delta_cm[a$surveys$survey_n <= s$survey_n])
    p <- a$profiles |> filter(survey_n == s$survey_n, round(threshold_cm,2) %in% c(.15,.5,1,5)) |>
      arrange(threshold_cm)
    metric_row <- function(label, mean_value, net_values) {
      tags$tr(tags$th(scope = 'row', label), tags$td(sprintf('%+.2f', mean_value)),
        lapply(seq_along(net_values), function(j) tags$td(
          class = if(j == 1) 'metric-net' else NULL,
          scales::percent(net_values[j], accuracy = .1))))
    }
    date_label <- if(s$date_start == s$date_end) as.character(s$date_start) else
      paste(s$date_start, 'to', s$date_end)
    tagList(
      div(class = 'metrics',
        div(class = 'metric-survey',
          tags$span(if(final_frame) 'Latest survey' else 'Survey'),
          tags$strong(s$survey),
          tags$span(date_label)),
        tags$table(class = 'metric-table', `aria-label` = 'Current and historical maximum sediment change from the selected baseline',
          tags$thead(
            tags$tr(tags$th(rowspan = 2, scope = 'col', ''),
              tags$th(rowspan = 2, scope = 'col', 'Mean change', tags$br(), 'from baseline (cm)'),
              tags$th(colspan = 4, scope = 'colgroup', class = 'metric-net', 'Net accumulation from baseline')),
            tags$tr(tags$th(scope = 'col', class = 'metric-net', '≥1.5 mm'),
              tags$th(scope = 'col', '≥5 mm'), tags$th(scope = 'col', '≥1 cm'),
              tags$th(scope = 'col', '≥5 cm'))),
          tags$tbody(
            metric_row('Current', s$mean_delta_cm, p$net),
            metric_row('Historical max', maximum_mean, p$maximum_net)))),
      tags$small('Percentages describe sampled positions: the fraction accumulating at least the indicated depth minus the fraction losing at least that depth, relative to the selected baseline. Historical maxima are the highest signed values through the displayed survey, relative to the selected baseline; they can be negative. Maximum net accumulation follows the green curve and is not the total area ever affected.')
    )
  })
  output$heatmap_container <- renderUI({
    a <- analysis(); plotOutput('heatmap', height = max(520, ceiling(length(selected())/2) * (150 + 16*nrow(a$surveys))))
  })
  output$heatmap <- renderPlot(plot_heatmap(analysis()), res = 110)
  output$bands <- renderPlot(plot_bands(analysis()), res = 110)
  output$coverage_container <- renderUI({
    d <- coverage()
    plotOutput('coverage_plot', height = max(520, ceiling(length(selected())/2) * (170 + 16*n_distinct(d$survey_n))))
  })
  output$coverage_plot <- renderPlot(plot_coverage(coverage()), res = 110)
  output$download <- downloadHandler(
    filename = function() paste0('sediment-', input$reference, '.csv'),
    content = function(file) {
      d <- analysis()$deltas |> mutate(reference_method = input$reference)
      write.csv(d, file, row.names = FALSE, na = '')
    })
  output$summary_download <- downloadHandler(
    filename = function() paste0('sediment-surveys-', input$reference, '.csv'),
    content = function(file) {
      p <- analysis()$profiles |> mutate(reference_method = input$reference,
        selected_transects = paste(selected(), collapse = ';'))
      write.csv(p, file, row.names = FALSE, na = '')
    })
  output$coverage_download <- downloadHandler(
    filename = function() 'sediment-survey-coverage.csv',
    content = function(file) write.csv(coverage(), file, row.names = FALSE, na = ''))
}
shinyApp(ui, server)
