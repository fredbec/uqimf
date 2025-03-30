library(here)
library(data.table)
devtools::load_all()

source(here("specs", "specs.R"))

.d <- `[`

window_length <- specs$window_length
tgt_yrs <- 2020:2022

global_file_prefix <- ""
prefix <- ""
tv_release <- 1

if(specs$ciset == "extended"){
  cis <- specs$ci_levels_eval_extended
} else {
  cis <- specs$ci_levels_eval
}

if(specs$ciset == "extended"){
  qus <- specs$qu_levels_extended
} else {
  qus <- specs$qu_levels
}

if(specs$ciset == "extended"){
  cis100 <- specs$ci_levels_eval_su_extended
} else {
  cis100 <- specs$ci_levels_eval_su
}


####################################read in quantile forecasts########################

qufcs_ho <- data.table::fread(here("quantile_forecasts",
                                   paste0(global_file_prefix, "toscore", prefix, "_quantile_forecasts_ho.csv")))


#################score CI's####################################################
qufcs_ho <- qufcs_ho |>
  .d(,.(country, target, horizon, target_year, true_value, prediction, quantile, method, error_method, source)) |>
  setnames("source", "model")


scores_ho <- scoreempQu(qufcs_ho, cvg_rg = cis100,
                        by = c("model", "error_method", "method", "country", "target", "horizon"))
scores_avgcountry_ho <- scoreempQu(qufcs_ho, cvg_rg = cis100,
                                   by = c("model", "error_method", "method", "target", "horizon"))
scores_cvgshort_ho <- scoreempQu(qufcs_ho, cvg_rg = cis100,
                                 by = c("model", "error_method", "method", "target"))


bvar_qus_ho <- data.table::fread(here("benchmarks",
                                      paste0(global_file_prefix, "toscore", prefix, "_bvar_direct_quantile_forecasts_ho.csv"))) |>
  setnames(paste0("tv_", tv_release), "true_value")

bvar_qus_ho <- bvar_qus_ho |>
  data.table::copy() |>
  .d(,.(country, target, horizon, target_year, true_value, prediction, quantile, source)) |>
  setnames("source", "model") #|>
#.d(quantile %in% qus)


bvar_scores_ho <- scoreempQu(bvar_qus_ho, cvg_rg = cis100,
                             by = c("model", "country", "target", "horizon"))
bvar_scores_avgcountry_ho <- scoreempQu(bvar_qus_ho, cvg_rg = cis100,
                                        by = c("model", "target", "horizon"))
bvar_scores_cvgshort_ho <- scoreempQu(bvar_qus_ho, cvg_rg = cis100,
                                      by = c("model", "target"))


######################################Saving#######################################################
data.table::fwrite(scores_ho, here("revision_plotstables", "cov_19scores", prefix, paste0(global_file_prefix, "ci_scores_ho.csv")))
data.table::fwrite(scores_avgcountry_ho, here("revision_plotstables", "cov_19scores", prefix, paste0(global_file_prefix, "ci_scores_avgcnt_ho.csv")))
data.table::fwrite(scores_cvgshort_ho, here("revision_plotstables", "cov_19scores", prefix, paste0(global_file_prefix, "cvg_pooled_ho.csv")))
data.table::fwrite(bvar_scores_ho, here("revision_plotstables", "cov_19scores", prefix, paste0(global_file_prefix, "bvar_ci_scores_ho.csv")))
data.table::fwrite(bvar_scores_cvgshort_ho, here("revision_plotstables", "cov_19scores", prefix, paste0(global_file_prefix, "bvar_cvg_pooled_ho.csv")))
data.table::fwrite(bvar_scores_avgcountry_ho, here("revision_plotstables", "cov_19scores", prefix, paste0(global_file_prefix, "bvar_ci_scores_avgcnt_ho.csv")))

