library(here)
library(data.table)
devtools::load_all()

source(here("specs", "specs.R"))

.d <- `[`

tv_release <- specs$tv_release
window_length <- specs$window_length

cis <- specs$ci_levels_eval
qus <- specs$qu_levels
cis100 <- specs$ci_levels_eval_su #for passing to scoring function

#for scoring point forecasts
fcdat <- data.table::fread(here("data", "toscore_point_forecasts.csv"))|>
  .d(target_year <= max_year)


####################################read in quantile forecasts########################
qufcs <- data.table::fread(here("quantile_forecasts",
                                paste0("toscore", prefix, "_quantile_forecasts.csv")))
qufcs_directional <- data.table::fread(here("quantile_forecasts",
                                            paste0("toscore", prefix, "_quantile_forecasts_directional.csv")))
qufcs_ho <- data.table::fread(here("quantile_forecasts",
                                   paste0("toscore", prefix, "_quantile_forecasts_ho.csv")))

combs <- data.table::fread(here("quantile_forecasts", "setting_combinations.csv"))

#################score CI's####################################################
qufcs <- qufcs |>
  .d(,.(country, target, horizon, target_year, true_value, prediction, quantile, method, error_method, source)) |>
  setnames("source", "model") |>
  .d(quantile %in% qus)
qufcs_directional <- qufcs_directional |>
  .d(,.(country, target, horizon, target_year, true_value, prediction, quantile, method, error_method, source)) |>
  setnames("source", "model") |>
  .d(quantile %in% qus)
qufcs_ho <- qufcs_ho |>
  .d(,.(country, target, horizon, target_year, true_value, prediction, quantile, method, error_method, source)) |>
  setnames("source", "model") |>
  .d(quantile %in% qus)


scores <- scoreempQu(qufcs, cvg_rg = cis100,
                     by = c("model", "error_method", "method", "country", "target", "horizon"))
scores_directional <- scoreempQu(qufcs_directional, cvg_rg = cis100,
                                 by = c("model", "error_method", "method", "country", "target", "horizon"))
scores_ho <- scoreempQu(qufcs_ho, cvg_rg = cis100,
                        by = c("model", "error_method", "method", "country", "target", "horizon"))

scores_avgcountry <- scoreempQu(qufcs, cvg_rg = cis100,
                     by = c("model", "error_method", "method", "target", "horizon"))
scores_avgcountry_directional <- scoreempQu(qufcs_directional, cvg_rg = cis100,
                                            by = c("model", "error_method", "method", "target", "horizon"))
scores_avgcountry_ho <- scoreempQu(qufcs_ho, cvg_rg = cis100,
                                   by = c("model", "error_method", "method", "target", "horizon"))

scores_cvgshort <- scoreempQu(qufcs, cvg_rg = cis100,
                              by = c("model", "error_method", "method", "target"))
scores_cvgshort_directional <- scoreempQu(qufcs_directional, cvg_rg = cis100,
                                          by = c("model", "error_method", "method", "target"))
scores_cvgshort_ho <- scoreempQu(qufcs_ho, cvg_rg = cis100,
                                 by = c("model", "error_method", "method", "target"))


################################Score Point Predictions############################
pp_scores <- fcdat |>
  data.table::copy() |>
  .d(, ae := abs(get(paste0("tv_", tv_release)) - prediction)) |>
  .d(, sque := (get(paste0("tv_", tv_release)) - prediction)^2) |>
  .d(, meanae := mean(ae), .(source, target, country, horizon)) |>
  .d(, meansque := mean(sque), .(source, target, country, horizon)) |>
  .d(, .(source, target, country, horizon, meanae, meansque)) |>
  setnames(c("meanae", "meansque"), c("ae", "sque")) |>
  unique()


####################################Score BVAR Quantile Forecasts################################

bvar_qus <- data.table::fread(here("benchmarks",
                                   paste0("toscore", prefix, "_bvar_direct_quantile_forecasts.csv"))) |>
  setnames(paste0("tv_", tv_release), "true_value")
bvar_qus_ho <- data.table::fread(here("benchmarks",
                                      paste0("toscore", prefix, "_bvar_direct_quantile_forecasts_ho.csv"))) |>
  setnames(paste0("tv_", tv_release), "true_value")

bvar_qus <- bvar_qus |>
  data.table::copy() |>
  .d(,.(country, target, horizon, target_year, true_value, prediction, quantile, source)) |>
  setnames("source", "model") |>
  .d(quantile %in% qus)
bvar_qus_ho <- bvar_qus_ho |>
  data.table::copy() |>
  .d(,.(country, target, horizon, target_year, true_value, prediction, quantile, source)) |>
  setnames("source", "model") |>
  .d(quantile %in% qus)


bvar_scores <- scoreempQu(bvar_qus, cvg_rg = cis100,
                          by = c("model", "country", "target", "horizon"))
bvar_scores_ho <- scoreempQu(bvar_qus_ho, cvg_rg = cis100,
                             by = c("model", "country", "target", "horizon"))

bvar_scores_avgcountry <- scoreempQu(bvar_qus, cvg_rg = cis100,
                                by = c("model", "target", "horizon"))
bvar_scores_avgcountry_ho <- scoreempQu(bvar_qus_ho, cvg_rg = cis100,
                                        by = c("model", "target", "horizon"))

bvar_scores_cvgshort <- scoreempQu(bvar_qus, cvg_rg = cis100,
                              by = c("model", "target"))
bvar_scores_cvgshort_ho <- scoreempQu(bvar_qus_ho, cvg_rg = cis100,
                                   by = c("model", "target"))





######################################Saving#######################################################
data.table::fwrite(scores, here("scores", prefix, "ci_scores.csv"))
data.table::fwrite(scores_directional, here("scores", prefix, "ci_scores_directional.csv"))
data.table::fwrite(scores_ho, here("scores", prefix, "ci_scores_ho.csv"))

data.table::fwrite(scores_avgcountry, here("scores",prefix, "ci_scores_avgcnt.csv"))
data.table::fwrite(scores_avgcountry_directional, here("scores",prefix, "ci_scores_avgcnt_directional.csv"))
data.table::fwrite(scores_avgcountry_ho, here("scores", prefix, "ci_scores_avgcnt_ho.csv"))

data.table::fwrite(scores_cvgshort, here("scores", prefix,"cvg_pooled.csv"))
data.table::fwrite(scores_cvgshort_directional, here("scores", prefix, "cvg_pooled_directional.csv"))
data.table::fwrite(scores_cvgshort_ho, here("scores", prefix, "cvg_pooled_ho.csv"))

data.table::fwrite(pp_scores, here("scores", prefix, "pointfc_scores.csv"))

data.table::fwrite(bvar_scores, here("scores", prefix, "bvar_ci_scores.csv"))
data.table::fwrite(bvar_scores_ho, here("scores", prefix, "bvar_ci_scores_ho.csv"))

data.table::fwrite(bvar_scores_cvgshort, here("scores", prefix, "bvar_cvg_pooled.csv"))
data.table::fwrite(bvar_scores_cvgshort_ho, here("scores", prefix, "bvar_cvg_pooled_ho.csv"))

data.table::fwrite(bvar_scores_avgcountry, here("scores", prefix, "bvar_ci_scores_avgcnt.csv"))
data.table::fwrite(bvar_scores_avgcountry_ho, here("scores", prefix, "bvar_ci_scores_avgcnt_ho.csv"))

