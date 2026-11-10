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
Solid colored lines show the common 0–50 m segments. ST1 and ST38 also show their
50–150 m extensions as gray dashed lines, indicating variable survey coverage,
not zero sedimentation. Both sections support hover labels and click selection;
selected lines are thicker. The Survey coverage tab shows actual measurement
availability over each selected transect's full length.
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
  surveyed GPS anchors, plus the 50–150 m extensions of ST1/ST38 as gray dashed
  lines. Those extensions are outside the common time-series cohort.
- ST1 and ST38 have 0–150 m measurements at Baseline and Int1, but only 0–50 m
  in the loaded construction rounds. Additional construction coverage may be
  triggered by sediment impacts greater than 1 cm above baseline. Such intermittent
  observations do not expand the denominator for the routine time series, and no
  sedimentation values are inferred for unmeasured positions.
- Survey coverage is independent of the reference selection. It includes Baseline,
  the construction reference rounds, and rounds omitted from a pooled change
  analysis. Teal cells have finite depth measurements, including genuine zeros;
  gray cells have no measurement record; orange cells have a record but no finite
  depth. The dashed boundary marks the last position in the 0–50 m cohort.
  Dates reflect the selected transects where available; entirely missing rounds
  use the date range recorded for other construction-monitored project transects.
  The coverage download includes every expected meter/survey position, with blank
  measured depths and observation dates where records are absent. This is separate
  from the meter-change and survey-summary downloads, which remain limited to
  the fixed 0–50 m analysis cohort.
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
full-length coverage, measured zeros versus missing records, missing depths,
intermittent extended surveys, unchanged time-series denominators,
independent empirical calculations, unequal
pooling weights, missing surveys/references, zero-change tails, signed memory,
band reconstruction, plot rendering, reactive changes, and empty selections.

## Publish updates to Posit Connect Cloud

The existing deployment is the
[Sand Bypass Sediment Explorer](https://connect.posit.cloud/jrcunning/content/01a0fdd1-3ec4-ee6e-2d2c-02b9b037c13b)
in the **jrcunning** account. Pushing changes to GitHub does not update the hosted
app; publish from R after the local changes are ready.

Run the commands below in the **R console**, with the SandBypass repository as the
working directory (for example, open the project in RStudio).

### One-time setup

Install the publishing package in your regular R library, using a current release
of R. This avoids depending on the temporary `/private/tmp` library used for the
initial update:

```r
install.packages("rsconnect")
```

Connect your Posit account:

```r
rsconnect::connectCloudUser()
```

Follow the browser prompts, verify the displayed code, and select **jrcunning** if
asked to choose an account. Wait for authentication to finish in R. This login is
saved locally; repeat it on a new computer or if publishing asks you to sign in
again. Credentials should not be added to the repository.

### Each time you publish an update

1. If monitoring workbooks changed, refresh the bundled data first:

   ```r
   source("code/prepare_shiny_data.R")
   ```

   This step is unnecessary for code, display, or documentation changes alone.

2. Deploy the entire app directory to the existing content ID:

   ```r
   rsconnect::deployApp(
     appDir = "shiny/sedimentation",
     appName = "sand-bypass-sediment-explorer",
     appId = "01a0fdd1-3ec4-ee6e-2d2c-02b9b037c13b",
     account = "jrcunning",
     server = "connect.posit.cloud"
   )
   ```

   Keep these identifiers unchanged to update the existing app. The upload uses
   the local app files, including `data/monitoring.rds`; the original Excel folder
   is not needed on the server.

3. Wait for R to report a successful deployment, then refresh the
   [hosted app](https://connect.posit.cloud/jrcunning/content/01a0fdd1-3ec4-ee6e-2d2c-02b9b037c13b)
   and check the changed behavior. For the coverage display, select **ST1** and
   **ST38** and open **Survey coverage**: Baseline and Int1 cover the full 150 m,
   while construction rounds show the unmeasured outer 100 m in gray.

The saved deployment details are in
`rsconnect/connect.posit.cloud/jrcunning/sand-bypass-sediment-explorer.dcf`.
The app requires a running R server; the generated notebook HTML and GitHub Pages
cannot run it.

References: [Shiny slider playback](https://shiny.posit.co/r/reference/shiny/latest/sliderinput.html)
and [Leaflet Shiny events](https://rstudio.github.io/leaflet/articles/shiny.html).
