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
    tags$hr(), tags$small('Source: ', monitoring$source),
    tags$small(paste('Data prepared:', monitoring$prepared))
  ),
  tags$head(tags$style(HTML('
    .leaflet-container {isolation:isolate;z-index:0}
    .eyebrow {font-size:11px;letter-spacing:2px;font-weight:700;color:#14777a}
    h3 {font-weight:700}.button-row{display:flex;gap:8px;margin-bottom:18px}
    .card{border:1px solid #dbe5e7;box-shadow:0 3px 14px #193b4508}
    .notice{padding:12px 16px;border-left:3px solid #16858a;background:#eaf4f3;margin:12px 0}
    .metrics{display:grid;grid-template-columns:repeat(auto-fit,minmax(min(100%,210px),1fr));gap:16px;padding:12px 4px}
    .metric-group{min-width:0;padding:0 12px;border-left:2px solid #dbe5e7}
    .metric-heading{font-size:13px;font-weight:600;margin:0 0 8px;color:#193b45}
    .metric-pair{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:12px}
    .metric strong{display:block;font-size:23px;white-space:nowrap;font-variant-numeric:tabular-nums}
    .metric span{display:block;font-size:12px;color:#547078}.shiny-plot-output{background:white}
    .bslib-sidebar-layout>.main{gap:18px} .tab-content{padding-top:14px}
  '))),
  card(card_header('Monitoring footprint'),
    tags$p('Click a transect line to add or remove it. Selected lines are thicker. Hover over a line to see its name and predicted sedimentation.'),
    leafletOutput('map', height = 360)),
  uiOutput('coverage_note'),
  card(navset_tab(
    nav_panel('Change through time', tags$p('Play through the shared surveys, or drag to a survey. The green curve tracks the largest signed net balance reached at each threshold.'),
      uiOutput('frame_control'), uiOutput('metrics'), plotOutput('animation', height = 480),
      tags$details(tags$summary('How to read these curves'),
        tags$p('Red is the fraction accumulating at least the threshold depth. Blue is the fraction losing at least that depth, plotted below zero. Black is accumulation minus loss. At zero, unchanged positions are excluded from both tails. Green is a threshold-specific running maximum, which may be negative.'),
        tags$p('The dark red or blue vertical dash marks the absolute mean change (red = positive, blue = negative). Percentages are fractions of sampled meter positions, not estimates of impacted seabed area.'))),
    nav_panel('Change along transects', uiOutput('heatmap_container')),
    nav_panel('Compare surveys', uiOutput('curves_container')),
    nav_panel('Change by depth band', plotOutput('bands', height = 650)),
    nav_panel('About the analysis',
      tags$h4('Meter-matched change'),
      tags$p('The available sediment-depth replicates are averaged at each transect-meter-survey position. The chosen reference is computed at that same position, with equal weight for each reference survey. Change equals survey mean minus reference depth, in centimeters. Positive values indicate accumulation.'),
      tags$p('Official baseline results begin at Con1. The median of Baseline/Con1/Con2 and the mean of Con1/Con2 begin at Con3. The mean of Con1/Con2/Con3 begins at Con4. Construction-based references are sensitivity analyses; they include construction observations.'),
      tags$h4('Combining transects and maintaining coverage'),
      tags$p('The app pools individual transect-meter changes, rather than averaging transect curves. Each retained meter position has equal weight. Only surveys available for every selected transect are included. A fixed set of positions observed across all included surveys is used. Positions with incomplete reference surveys or non-finite changes are excluded and reported.'),
      tags$p('Analyses use meters 0–50 to match the notebook’s consistently monitored segments, including ST1 and ST38. The map shows these same 0–50 m segments using their surveyed GPS anchors. Only transects with construction monitoring are shown; baseline-only observations remain in the source data.'),
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
    if(is.null(id) || !id %in% active_transects) return()
    s <- selected()
    updateSelectizeInput(session, 'transects', selected = if(id %in% s) setdiff(s,id) else c(s,id))
  }
  observeEvent(input$map_shape_click, toggle(input$map_shape_click$id))
  gps <- monitoring$gps |> filter(transect %in% active_transects, meter >= 0, meter <= 50)
  palette <- c('1.0 cm predicted' = '#c1121f', '1.5 mm predicted' = '#f28e2b', '0 cm predicted' = '#287271')
  draw_transects <- function(m, selection) {
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
      fitBounds(min(gps$lon), min(gps$lat), max(gps$lon), max(gps$lat)) |>
      addScaleBar(position = 'bottomleft') |>
      draw_transects(isolate(selected())) |>
      addLegend('bottomright', colors = unname(palette), labels = names(palette), title = 'Modeled sedimentation')
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
  output$coverage_note <- renderUI({
    a <- analysis()
    n <- nrow(distinct(a$deltas, transect, meter))
    expected <- monitoring$raw |> filter(transect %in% selected(), meter <= 50, survey_n > max(reference_spec(input$reference)$surveys)) |>
      distinct(transect, meter) |> nrow()
    div(class = 'notice', paste('Reference:', names(reference_options)[match(input$reference, reference_options)],
      '•', n, 'fixed meter positions •', nrow(a$surveys), 'shared surveys.'),
      if(length(a$omitted)) tags$p(paste('Omitted rounds without coverage for every selected transect:', paste0('Con', a$omitted, collapse = ', '))),
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
    p <- a$profiles |> filter(survey_n == s$survey_n, round(threshold_cm,2) == 1)
    metric <- function(value, label) div(class = 'metric', tags$strong(value), tags$span(label))
    date_label <- if(s$date_start == s$date_end) as.character(s$date_start) else
      paste(s$date_start, 'to', s$date_end)
    tagList(
      div(class = 'metrics',
        div(class = 'metric-group',
          tags$h4(class = 'metric-heading', if(final_frame) 'Latest survey' else 'Survey'),
          metric(s$survey, date_label)),
        div(class = 'metric-group',
          tags$h4(class = 'metric-heading', 'Mean change from baseline'),
          div(class = 'metric-pair',
            metric(sprintf('%+.2f cm',s$mean_delta_cm), 'Current'),
            metric(sprintf('%+.2f cm',maximum_mean), 'Historical max'))),
        div(class = 'metric-group',
          tags$h4(class = 'metric-heading', 'Net accumulation ≥1 cm'),
          div(class = 'metric-pair',
            metric(scales::percent(p$net, accuracy = .1), 'Current'),
            metric(scales::percent(p$maximum_net, accuracy = .1), 'Historical max')))),
      if(final_frame) tags$small(style = 'display:block;margin-bottom:8px',
        'Historical maxima cover the full monitoring period.'),
      tags$small('Percentages describe sampled positions: the fraction accumulating ≥1 cm minus the fraction losing ≥1 cm, relative to the selected baseline. Historical maxima are the highest signed values through the displayed survey, relative to the selected baseline; they can be negative. Maximum net accumulation follows the green curve and is not the total area ever affected.')
    )
  })
  output$heatmap_container <- renderUI({
    a <- analysis(); plotOutput('heatmap', height = max(520, ceiling(length(selected())/2) * (150 + 16*nrow(a$surveys))))
  })
  output$heatmap <- renderPlot(plot_heatmap(analysis()), res = 110)
  output$curves_container <- renderUI({
    a <- analysis(); plotOutput('curves', height = max(450, 220 * ceiling(nrow(a$surveys)/3)))
  })
  output$curves <- renderPlot(plot_tails(analysis()), res = 100)
  output$bands <- renderPlot(plot_bands(analysis()), res = 110)
  output$download <- downloadHandler(
    filename = function() paste0('sediment-', input$reference, '.csv'),
    content = function(file) {
      d <- analysis()$deltas |> mutate(reference_method = input$reference, source_workbook = monitoring$source)
      write.csv(d, file, row.names = FALSE, na = '')
    })
  output$summary_download <- downloadHandler(
    filename = function() paste0('sediment-surveys-', input$reference, '.csv'),
    content = function(file) {
      p <- analysis()$profiles |> mutate(reference_method = input$reference,
        selected_transects = paste(selected(), collapse = ';'), source_workbook = monitoring$source)
      write.csv(p, file, row.names = FALSE, na = '')
    })
}
shinyApp(ui, server)
