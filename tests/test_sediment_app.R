source('shiny/sedimentation/R/analysis.R')
library(testthat)
bundle <- readRDS('shiny/sedimentation/data/monitoring.rds')
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
    expect_true(length(output$curves) > 0)
    session$setInputs(transects = character())
    expect_error(analysis())
    session$setInputs(transects = 'ACER_NS')
    expect_length(selected(), 0L)
    expect_error(analysis(), 'Select at least one transect')
  })
})
