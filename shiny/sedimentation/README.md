# Sand Bypass sediment explorer

An interactive Shiny version of `code/sedimentation_localized_analysis.Rmd`.
Map the 13 construction-monitored transects, select one or pool several, choose a meter-specific reference,
and explore the animation, heatmap, empirical tails, net depth bands, and data exports.
The bundled Con37 snapshot contains 13 construction-monitored transects and six
baseline-only transects retained in the source data but excluded from the map and selector.
Colored lines support hover labels and click selection; selected lines are thicker.
The original notebook is unchanged.

## Run locally

From the repository root, in R:

```r
install.packages(c("shiny", "bslib", "leaflet", "dplyr", "tidyr", "ggplot2",
                   "scales", "patchwork", "readxl", "janitor", "stringr"))
shiny::runApp("shiny/sedimentation")
```

Requires R >= 4.1, dplyr >= 1.1, and bslib >= 0.6. The app ships a compact
prepared RDS dataset, so running or deploying does not require the original Excel
folder. Map tiles require internet access; analytical data are bundled locally.
The current development preview uses Leaflet installed in
`/private/tmp/sandbypass-r-library`; a normal installation above is needed for
future sessions on this machine.

## Refresh after new monitoring data arrive

From the repository root:

```r
source("code/prepare_shiny_data.R")
```

This reads the cumulative construction workbook with the highest Con number in its
filename and the GPS workbook. Ties are resolved by filename rather than filesystem
modification time so Git sync does not change the chosen file. It validates unique
transect-meter-survey records, averages available depth replicates, and writes
`shiny/sedimentation/data/monitoring.rds`. Restart/redeploy the app after refreshing.
The source workbook and preparation date appear in the sidebar and source workbook
is included in downloads.

## Analysis decisions

- All references are calculated separately for each transect-meter pair.
- Official baseline: results from Con1 onward.
- Median of Baseline, Con1, Con2: results from Con3 onward. This intentionally
  differs from the notebook's arithmetic mean of those three surveys.
- Mean of Con1, Con2: results from Con3 onward.
- Mean of Con1, Con2, Con3: results from Con4 onward.
- Every required reference survey must have a finite meter mean. Missing references
  are not replaced with a partial mean or median.
- Analysis uses 0–50 m, including ST1/ST38. The map shows the same 0–50 m segments using surveyed GPS anchors.
- Combined views concatenate meter-level deltas. Each position gets equal weight.
  Only shared survey rounds are retained, with a fixed meter cohort. Dates are
  presented as ranges where selected transects were sampled on different dates.
- Missing whole survey rounds and excluded meter positions are reported. Baseline-only transects are excluded from the map and selection controls.
- Empirical curves use a 0.05 cm grid and the notebook's strict comparisons at zero.
  Green is the running maximum of signed net balance, not the maximum absolute value.
  Axes show the full ±100% prevalence range to avoid clipping.
- Playback includes a final latest-versus-maximum summary. Baseline/selection changes
  rebuild the slider at its first frame. Expensive analyses are cached by selection
  and baseline, and playback only draws the selected frame.
- The heatmap keeps transects in separate panels so coincident meter numbers never
  imply spatial adjacency. The pooled curves and bands use all retained positions.

## Tests

```sh
Rscript tests/test_sediment_app.R
```

Requires `testthat`. Tests cover all four references across each of the 13 monitored
transects and their pooled selection, independent empirical calculations, unequal
pooling weights, missing surveys/references, zero-change tails, signed memory,
band reconstruction, plot rendering, reactive changes, and empty selections.

## Share with stakeholders

Deploy the **entire `shiny/sedimentation` directory** to an R-capable Shiny host
(such as shinyapps.io, Posit Connect, or Shiny Server). After configuring your hosting
account locally, a shinyapps.io deployment can be made from the repository root:

```r
rsconnect::deployApp(appDir = "shiny/sedimentation",
                     appName = "sand-bypass-sediment-explorer")
```

No hosting account or credentials are embedded. This repository change does not
publish the data or create a public deployment. The app requires a running R server;
the generated notebook HTML and GitHub Pages cannot run it.

References: [Shiny slider playback](https://shiny.posit.co/r/reference/shiny/latest/sliderinput.html)
and [Leaflet Shiny events](https://rstudio.github.io/leaflet/articles/shiny.html).
