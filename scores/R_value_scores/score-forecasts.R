library(here)
library(data.table)
devtools::load_all()

source(here("specs", "specs.R"))

.d <- `[`

max_year <- specs$score_max_year
min_year <- specs$min_year
score_min_year <- specs$score_min_year
tv_release <- specs$tv_release
window_length <- specs$window_length

cis <- specs$ci_levels_eval
qus <- specs$qu_levels
cis100 <- specs$ci_levels_eval_su #for passing to scoring function

#for scoring crps values
#as these are scored directly on the point forecasts and not on the extracted
#quantile forecasts
fcdat <- data.table::fread(here("data", "point_forecasts.csv"))|>
  .d(target_year <= max_year)


####################################read in quantile forecasts########################
qufcs <- data.table::fread(here("quantile_forecasts", "quantile_forecasts.csv")) |>
  .d(target_year <= max_year)
combs <- data.table::fread(here("quantile_forecasts", "setting_combinations.csv"))
bvar_qus <- data.table::fread(here("benchmarks", "quantile_benchmarks_processed.csv")) |>
  setnames(paste0("tv_", tv_release), "true_value")|>
  .d(target_year <= max_year)
qufcs_pava <- data.table::fread(here("quantile_forecasts", "quantile_forecasts_pava.csv")) |>
  .d(target_year <= max_year)


#################score CI's####################################################
weodat_qu_sameyearset <- qufcs |>
  data.table::copy() |>
  .d(target_year>=score_min_year) |>
  .d(,.(country, target, horizon, target_year, true_value, prediction, quantile, method, error_method, source)) |>
  setnames("source", "model") |>
  .d(quantile %in% qus)

weodat_qu_allyears <- qufcs |>
  .d(,.(country, target, horizon, target_year, true_value, prediction, quantile, method, error_method, source)) |>
  setnames("source", "model") |>
  .d(quantile %in% qus)


scores <- scoreempQu(weodat_qu_sameyearset, cvg_rg = cis100,
                     by = c("model", "error_method", "method", "country", "target", "horizon"))

scores_allyears <- scoreempQu(weodat_qu_allyears, cvg_rg = cis100,
                     by = c("model", "error_method", "method", "country", "target", "horizon"))

scores_avgcountry <- scoreempQu(weodat_qu_sameyearset, cvg_rg = cis100,
                     by = c("model", "error_method", "method", "target", "horizon"))

scores_cvgshort <- scoreempQu(weodat_qu_sameyearset, cvg_rg = cis100,
                     by = c("model", "error_method", "method", "target"))

#################score CI's for PAVA####################################################
weodat_qu_sameyearset_pava <- qufcs_pava |>
  data.table::copy() |>
  .d(target_year>=score_min_year) |>
  .d(,.(country, target, horizon, target_year, true_value, prediction, quantile, method, error_method, source)) |>
  setnames("source", "model") |>
  .d(quantile %in% qus)


scores_pava <- scoreempQu(weodat_qu_sameyearset_pava, cvg_rg = cis100,
                     by = c("model", "error_method", "method", "country", "target", "horizon"))

scores_avgcountry_pava <- scoreempQu(weodat_qu_sameyearset_pava, cvg_rg = cis100,
                                by = c("model", "error_method", "method", "target", "horizon"))

scores_cvgshort_pava <- scoreempQu(weodat_qu_sameyearset_pava, cvg_rg = cis100,
                              by = c("model", "error_method", "method", "target"))



######################################Saving#######################################################
data.table::fwrite(scores, here("scores", "R_value_scores", paste0("ci_scores_R", window_length, ".csv")))
data.table::fwrite(scores_allyears, here("scores", "R_value_scores", paste0("ci_scores_allyears", window_length, ".csv")))
data.table::fwrite(scores_cvgshort, here("scores", "R_value_scores", paste0("cvg_pooled", window_length, ".csv")))
data.table::fwrite(scores_avgcountry, here("scores", "R_value_scores", paste0("ci_scores_avgcnt", window_length, ".csv")))
data.table::fwrite(scores_pava, here("scores", "R_value_scores", paste0("ci_scores_pava", window_length, ".csv")))
data.table::fwrite(scores_cvgshort_pava, here("scores", "R_value_scores", paste0("cvg_pooled_pava", window_length, ".csv")))
data.table::fwrite(scores_avgcountry_pava, here("scores", "R_value_scores", paste0("ci_scores_avgcnt_pava", window_length, ".csv")))

