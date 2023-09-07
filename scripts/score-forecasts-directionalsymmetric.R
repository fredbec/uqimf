library(here)
library(data.table)
library(ggplot2)
library(MetBrewer)
devtools::load_all()

.d <- `[`

max_year <- 2012
min_year <- 1990
tv_release <- 1
window_length <- 9

cis <- c(0.5, 0.8)

qufcs_symm <- data.table::fread(here("quantile_forecasts", "qufcs_directionalsymmetric.csv"))



weodat_qu_symm <- qufcs_symm |>
  .d(,.(country, target, horizon, target_year, true_value, prediction, quantile, method, error_method, source)) |>
  setnames("source", "model") |>
  .d(quantile %in% c(0.1, 0.25, 0.75, 0.9))


scores_symm <- scoreempQu(weodat_qu_symm, cvg_rg = c(50,80),
                     by = c("model", "error_method", "method", "country", "target", "horizon"))

scores_avgcountry_symm <- scoreempQu(weodat_qu_symm, cvg_rg = c(50,80),
                                by = c("model", "error_method", "method", "target", "horizon"))

scores_cvgshort_symm <- scoreempQu(weodat_qu_symm, cvg_rg = c(50,80),
                              by = c("model", "error_method", "method", "target"))


data.table::fwrite(scores_symm, here("quantile_forecasts", "ci_scores_directionalsymmetric.csv"))
data.table::fwrite(scores_cvgshort_symm, here("quantile_forecasts", "cvg_pooled_directionalsymmetric.csv"))
data.table::fwrite(scores_avgcountry_symm, here("quantile_forecasts", "ci_scores_avgcnt_directionalsymmetric.csv"))
