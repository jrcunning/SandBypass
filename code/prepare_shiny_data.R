# Run from the repository root after syncing new monitoring workbooks.
source('shiny/sedimentation/R/analysis.R')
bundle <- read_monitoring('.')
saveRDS(bundle, 'shiny/sedimentation/data/monitoring.rds', compress = 'xz')
cat('Prepared', bundle$source, 'with', nrow(bundle$inventory), 'mapped transects.\n')
