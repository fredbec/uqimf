library(here)
library(data.table)
devtools::load_all()

source(here("specs", "specs.R"))

.d <- `[`

tv_release <- specs$tv_release
#window_length <- specs$window_length

cis <- specs$ci_levels_eval
qus <- specs$qu_levels
cis100 <- specs$ci_levels_eval_su #for passing to scoring function


####################################read in quantile forecasts########################
qufcs <- data.table::fread(here("scores", "R_value_scores", "quantile_forecasts",
                                paste0("quantile_forecasts.csv"))) |>
  .d(target_year >= min_score_yr_forall)
qufcs_directional <- data.table::fread(here("scores", "R_value_scores", "quantile_forecasts",
                                            paste0("quantile_forecasts_directional.csv")))|>
  .d(target_year >= min_score_yr_forall)

#################score CI's####################################################
qufcs <- qufcs |>
  .d(,.(country, target, horizon, target_year, true_value, prediction, quantile, method, error_method, source)) |>
  setnames("source", "model") |>
  .d(quantile %in% qus)
qufcs_directional <- qufcs_directional |>
  .d(,.(country, target, horizon, target_year, true_value, prediction, quantile, method, error_method, source)) |>
  setnames("source", "model") |>
  .d(quantile %in% qus)

scores <- scoreempQu(qufcs, cvg_rg = cis100,
                     by = c("model", "error_method", "method", "country", "target", "horizon"))
scores_directional <- scoreempQu(qufcs_directional, cvg_rg = cis100,
                                 by = c("model", "error_method", "method", "country", "target", "horizon"))

scores_avgcountry <- scoreempQu(qufcs, cvg_rg = cis100,
                                by = c("model", "error_method", "method", "target", "horizon"))
scores_avgcountry_directional <- scoreempQu(qufcs_directional, cvg_rg = cis100,
                                            by = c("model", "error_method", "method", "target", "horizon"))

scores_cvgshort <- scoreempQu(qufcs, cvg_rg = cis100,
                              by = c("model", "error_method", "method", "target"))
scores_cvgshort_directional <- scoreempQu(qufcs_directional, cvg_rg = cis100,
                                          by = c("model", "error_method", "method", "target"))





######################################Saving#######################################################
data.table::fwrite(scores, here("scores", "R_value_scores", paste0("ci_scores_R", window_length, ".csv")))
data.table::fwrite(scores_directional, here("scores", "R_value_scores", paste0("ci_scores_directional_R", window_length, ".csv")))

data.table::fwrite(scores_avgcountry, here("scores", "R_value_scores", paste0("ci_scores_avgcnt_R", window_length, ".csv")))
data.table::fwrite(scores_avgcountry_directional, here("scores", "R_value_scores", paste0("ci_scores_avgcnt_directional_R", window_length, ".csv")))

data.table::fwrite(scores_cvgshort, here("scores", "R_value_scores", paste0("cvg_pooled_R", window_length, ".csv")))
data.table::fwrite(scores_cvgshort_directional, here("scores", "R_value_scores", paste0("cvg_pooled_directional_R", window_length, ".csv")))


