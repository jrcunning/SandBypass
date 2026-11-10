# Sand Bypass sediment explorer

An interactive Shiny version of `code/sedimentation_localized_analysis.Rmd`.
Map the 13 construction-monitored transects, select one or pool several, choose a meter-specific reference,
and explore the animation, heatmap, empirical tails, net depth bands, and data exports.
The bundled snapshot contains Baseline through Con37 and the first post-construction
Interim survey, **Int1**, sampled July 17 and July 22–24, 2026. Dredging concluded
July 12, 2026. Int1 adds 1,359 meter records across all 19 transects, including six
adjacent transects previously sampled only at Baseline. The explorer omits those six
because they have no construction observations for alternative reference comparisons.
Their records remain in the source data. The app analyzes meters 0–50 of the 13
construction-monitored transects (663 positions), including their Int1 observations.
Longer segments remain in the source data.
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

## Refresh after new monitoring data arrive

From the repository root:

```r
source("code/prepare_shiny_data.R")
```

This reads the cumulative construction workbook with the highest Con number in its
filename, all Interim workbooks, and the GPS workbook. Interim filenames must contain
`Interim` or `Int` followed by a round number (such as `Interim1`), anywhere under
`data/sedimentation_monitoring`, including its top level. Excel lock files are ignored.
Construction ties are resolved by filename and path rather than filesystem modification
time so Git sync does not change the chosen file. Repeated, identical history in
cumulative files is included once. Conflicting observations for the same
transect-meter-survey stop preparation for explicit resolution.

Survey labels `Baseline`, `ConN`, `IntN`, and `InterimN` determine phase and identity.
Interim labels are standardized to `IntN` and ordered after construction; Int1 is
never interpreted as Con1 or relabeled Con38. `survey_round` is the round within its
phase; `survey_n` is an internal playback order, not a construction survey number.
Dates are checked against the July 12 construction end date. Preparation validates
unique transect-meter-survey records, averages available depth replicates, and writes
`shiny/sedimentation/data/monitoring.rds`. Restart/redeploy the app after refreshing.
The source workbooks and preparation date appear in the sidebar. Each exported meter
record and survey summary retains its observation source workbook and survey name.
Sampling dates are retained per meter and as date ranges in survey summaries.

## Analysis decisions

- All references are calculated separately for each transect-meter pair.
- Official baseline: results from Con1 onward.
- Median of Baseline, Con1, Con2: results from Con3 onward. This intentionally
  differs from the notebook's arithmetic mean of those three surveys.
- Mean of Con1, Con2: results from Con3 onward.
- Mean of Con1, Con2, Con3: results from Con4 onward.
- Every required reference survey must have a finite meter mean. Missing references
  are not replaced with a partial mean or median.
- All surveys form one continuous time series. Int1 uses the same calculations,
  references, and running maxima as the Con rounds. Time labels show each survey's
  supplied name and actual sampling date or date range for the selected transects
  in the animation, heatmap, and depth-band chart. Phase remains source metadata
  and does not divide the analysis or create a survey-period filter.
- The six adjacent transects (ACER_NS, ACER_WE, BT23, BT23_S, BT24, and BT24_N)
  have only Baseline and Int1 observations. They cannot support construction-based
  reference comparisons, so they are omitted from the map, selector, analyses,
  and app downloads while retained in the source bundle. This also avoids reducing
  pooled views to Int1 when those six are selected alongside the original 13.
  All monitored selects the 13 construction-monitored transects.
- Analysis uses 0–50 m, including ST1/ST38. The map shows the same segments using
  surveyed GPS anchors.
- Combined views concatenate meter-level deltas. Each position gets equal weight.
  Only shared survey rounds are retained, with a fixed meter cohort. Dates are
  presented as ranges where selected transects were sampled on different dates.
- Missing whole survey rounds and excluded meter positions are reported by their
  survey names and dates. Only transects with construction monitoring are selectable.
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

Requires `testthat`. Tests cover all four references across each of the 13 construction
transects and their pooled selection, retention and validation of all 19 source
transects, exclusion of the six Baseline/Int1-only transects from the app and downloads,
cumulative workbook discovery and deduplication, phase/date validation,
construction-only refresh compatibility, map endpoint interpolation, continuous
running maxima through Int1, survey/date labels, reference changes, CSV downloads,
independent empirical calculations, unequal
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
