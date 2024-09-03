
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Simple Macroeconomic Forecast Distributions for the G7 Economies

<!-- badges: start -->
<!-- badges: end -->

This repository contains the documentation, results, and code of a
project where we produce simple macroeconomic forecast distributions for
the G7 economies. It also contains code to continue to produce these
forecasts in real-time. See the documentation below for further details.

## Short summary of the project

We generate easy-to-understand forecast intervals for output growth and
inflation in the G7 economies, using the point forecasts published in
the International Monetary Fund World Economic Outlook (IMF WEO). Our
forecast intervals are generated at the 50% and 80% levels in a simple
and transparent manner, using empirical quantiles of the past history of
point forecast errors in the IMF WEO. For more details, please refer to
the working paper below.

## Citation

The working paper can be found on arXiv. Please cite this using the
following:

> Becker, Krüger, and Schienle. 2024 “Simple Macroeconomic Forecast
> Distributions for the G7 Economies” arXiv.
> <https://doi.org/10.48550/arXiv.2408.08304>.

    @unpublished{Beckeretal_2024,
      title={Simple Macroeconomic Forecast Distributions for the G7 Economies},
      author={Becker, Friederike and Kr{\"u}ger, Fabian and Schienle, Melanie},
      note={Preprint, arXiv:2408.08304},
      year={2024}
    }

## Further links

This repository mainly contains files for the retrospective analysis in
the working paper, see above. Please visit the following repository to
find the forecasts as they are published in real-time, as well as
further links to a dashboard, a preregistration protocol, etc.

> <https://github.com/KITmetricslab/MacroPI>

## Contents of this repository

More details on each folder can be found in its own respective section
further below.

| Folder                                      | Purpose                                                                                                                                    |
|---------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------|
| [`benchmarks`](benchmarks/)                 | Code and forecast data of benchmark forecasts.                                                                                             |
| [`data`](data/)                             | Processed WEO and benchmark forecast data.                                                                                                 |
| [`miscellanous`](miscellanous/)             | Files that are not relevant for the final analysis (exploratory scripts, presentation files, etc). Kept for completeness and transparency. |
| [`oecd_data`](oecd_data/)                   | OECD truth data used for fitting benchmarks and for robustness checks.                                                                     |
| [`quantile_forecasts`](quantile_forecasts/) | The generated forecast intervals, recorded via their upper and lower endpoints.                                                            |
| [`R`](R/)                                   | R functions used in the analysis and for evaluation.                                                                                       |
| [`scores`](scores/)                         | Scores of forecasts.                                                                                                                       |
| [`scripts`](scripts/)                       | Executive scripts to produce all results.                                                                                                  |
| [`specs`](specs/)                           | Specifications and global options for the analysis.                                                                                        |

## General notes on naming data files

Note that data files in this repository are generally identified with
some “core-name”, e.g. quantile_forecasts, and then with prefixes and/or
suffixes that further describe deviations from the default setting.

- Default setting: training data (target years 2001-2012); absolute
  error method; IMF WEO truth data; complete data before discarding
  instances for scoring
- “ho” suffix: holdout data (target years 2013-2023)
- “directional” suffix: directional error method
- “oecd” prefix: OECD truth data (used for robustness check)
- “to_score” prefix: excludes some instances for scoring data. All
  scores in scores/ folder are calculated based on these data versions.

### R folder

- `R`: All functions that the analysis is based on. Scripts that process
  data based on these functions are in `scripts`

  - `plot.R`: plotting files

  - `score.R`: functions to score quantile forecasts (coverage,
    interval_score, crps by sample)

  - `uqfc.R`: functions to generate quantile forecasts from past errors

  - `uqimf-package.R`: some stuff, as this is technically an R package

  - `utils.R`: miscellaneous small functions

  - `utils-manuscript.R`: miscellaneous small functions for manuscript

### benchmarks folder

- `point_benchmarks_processed.csv`: point benchmark forecasts (AR and
  BVAR model) as generated by `scripts/process-benchmarks`, see below

- `quantile_benchmarks_processed.csv`: quantile benchmark forecasts
  (BVAR model) as generated by `scripts/process-benchmarks`, see below

### data folder

- `weodat.csv`: tidy IMF WEO data, for G7 countries only
- `point_forecasts.csv`: all point forecasts (weo and benchmarks),
  generated by `scripts/process-benchmarks`, see below

### oecd_data folder

- `oecd_data`: contains annual and quarterly truth data for inflation
  and GDP growth, as published by the OECD. Also contains R markdown
  scripts that check (and verify) compatibility of annual OECD truth
  values with annual IMF truth values

### quantile_forecasts folder

- `quantile-forecasts`: contains forecasts as produced by
  `scripts/make-forecasts`, see below. Note that the forecast intervals
  are identified via their upper and lower endpoints. That is, for the
  50% interval, the 0.25 and 0.75 predictive quantiles, and for the 80%
  interval, the 0.1 and 0.9 predictive quantiles. Also note the naming
  data notes outlined above.

### scores folder

- `scores`: Contains scores of forecasts in `quantile-forecasts` and
  BVAR benchmarks, at various levels of resolutions. Contains its own
  Readme file to explain what scores the different files contain. Also
  note the naming data notes outlined above.

### scripts

- `scripts`: Contains scripts that process data based on the functions
  in `R`
  - `master.R`: produces all results based on the following sub-scripts
    (listed in the order as they appear in master.R, so not
    alphabetical)

  - `download_data.R`: download IMF data

  - `process-weo.R`: short script to filter and process WEO data

  - `process-benchmarks.R` process benchmarks to bring into compatible
    format with `weodat.csv`. Generates file that contains point
    forecasts from all sources in long format
    (`data/point_forecasts.csv`)

  - `encode-missing-predictions.R` explicitly encode missing data
    (although we actually have no missing data, but just for
    completenss)

  - `make-forecasts.R`: generate forecast intervals

  - `exclude-from-scoring.R`: exclude any instances that are listed in
    `specs/specs.R` from scoring

  - `score-forecasts.R`: score quantile forecasts, those generated in
    `make-forecasts.R` and the benchmark BVAR quantile forecasts.
- `specs`: contains file to specify global settings (window length,
  etc.)

## References

International Monetary Fund. 2023. *World Economic Outlook: A Rocky
Recovery.* Washington, DC. April.
