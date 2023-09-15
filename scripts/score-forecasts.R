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
qus <- ci_to_quantiles(cis, "directional") ###################remove hardcoding

#for scoring crps values
#as these are scored directly on the point forecasts and not on the extracted
#quantile forecasts
fcdat <- data.table::fread(here("point_forecasts.csv"))|>
  .d(target_year <= max_year)


####################################read in quantile forecasts########################
qufcs <- data.table::fread(here("quantile_forecasts", "quantile_forecasts.csv")) |>
  .d(target_year <= max_year)
combs <- data.table::fread(here("quantile_forecasts", "setting_combinations.csv"))
bvar_qus <- data.table::fread(here("benchmarks", "quantile_benchmarks_processed.csv")) |>
  setnames("tv_1", "true_value")|>
  .d(target_year <= max_year)


#################score CI's####################################################
weodat_qu_sameyearset <- qufcs |>
  data.table::copy() |>
  .d(target_year>=score_min_year) |>
  .d(,.(country, target, horizon, target_year, true_value, prediction, quantile, method, error_method, source)) |>
  setnames("source", "model") |>
  .d(quantile %in% c(0.1, 0.25, 0.75, 0.9))

weodat_qu_allyears <- qufcs |>
  .d(,.(country, target, horizon, target_year, true_value, prediction, quantile, method, error_method, source)) |>
  setnames("source", "model") |>
  .d(quantile %in% c(0.1, 0.25, 0.75, 0.9))


scores <- scoreempQu(weodat_qu_sameyearset, cvg_rg = c(50,80),
                     by = c("model", "error_method", "method", "country", "target", "horizon"))

scores_allyears <- scoreempQu(weodat_qu_allyears, cvg_rg = c(50,80),
                     by = c("model", "error_method", "method", "country", "target", "horizon"))

scores_avgcountry <- scoreempQu(weodat_qu_sameyearset, cvg_rg = c(50,80),
                     by = c("model", "error_method", "method", "target", "horizon"))

scores_cvgshort <- scoreempQu(weodat_qu_sameyearset, cvg_rg = c(50,80),
                     by = c("model", "error_method", "method", "target"))



################################Score Point Predictions############################
pp_scores <- fcdat |>
  .d(target_year>=score_min_year) |>
  data.table::copy() |>
  .d(, ae := abs(get(paste0("tv_", tv_release)) - prediction)) |>
  .d(, sque := (get(paste0("tv_", tv_release)) - prediction)^2) |>
  .d(, meanae := mean(ae), .(source, target, country, horizon)) |>
  .d(, meansque := mean(sque), .(source, target, country, horizon)) |>
  .d(, .(source, target, country, horizon, meanae, meansque)) |>
  setnames(c("meanae", "meansque"), c("ae", "sque")) |>
  unique()



#################################Score by CRPS Sample##############################
weodat_samples <- fcdat |>
  .d(horizon < 2) |>
  .d(,.(country, source, target, horizon, target_year, tv_1, prediction))


combs_methods <- data.table::CJ(error_method = c("directional", "absolute"),
                                method = c("rolling window", "expanding window", "leave-one-out"),
                                source = c("IMF", "bvar", "ar"))


all_crps <- lapply(1:nrow(combs_methods), function(idx){

  comb_set <- combs_methods[idx,]

  if(comb_set[, "method"] == "leave-one-out"){

    start_year <- min_year
  } else {

    start_year <- min_year + window_length
  }

  sub_samples <- weodat_samples |>
    .d(source == comb_set[, "source"])

  score_by_crps(sub_samples, target_years = start_year:max_year, tv_release = tv_release,
                error_method = comb_set[, "error_method"], method = comb_set[, "method"],
                window_length = window_length) |>
    .d(, method := comb_set[, "method"]) |>
    .d(, error_method := comb_set[, "error_method"]) |>
    .d(, source := comb_set[, "source"])

  }
) |>
  rbindlist() |>
  .d(, .(score = mean(score)), by = c("target", "horizon", "method", "error_method", "source"))




####################################Score BVAR Quantile Forecasts###########################
bvar_qu_sameyearset <- bvar_qus |>
  data.table::copy() |>
  .d(target_year>=score_min_year) |>
  .d(,.(country, target, horizon, target_year, true_value, prediction, quantile, source)) |>
  setnames("source", "model") |>
  .d(quantile %in% c(0.1, 0.25, 0.75, 0.9))

bvar_qu_allyears <- bvar_qus |>
  .d(!is.na(horizon)) |>
  .d(,.(country, target, horizon, target_year, true_value, prediction, quantile,  source)) |>
  setnames("source", "model") |>
  .d(quantile %in% c(0.1, 0.25, 0.75, 0.9))


bvar_scores <- scoreempQu(bvar_qu_sameyearset, cvg_rg = c(50,80),
                     by = c("model", "country", "target", "horizon"))

bvar_scores_allyears <- scoreempQu(bvar_qu_allyears, cvg_rg = c(50,80),
                              by = c("model", "country", "target", "horizon"))

bvar_scores_avgcountry <- scoreempQu(bvar_qu_sameyearset, cvg_rg = c(50,80),
                                by = c("model", "target", "horizon"))

bvar_scores_cvgshort <- scoreempQu(bvar_qu_sameyearset, cvg_rg = c(50,80),
                              by = c("model", "target"))





######################################Saving#######################################################
data.table::fwrite(scores, here("scores", "ci_scores.csv"))
data.table::fwrite(scores_allyears, here("scores", "ci_scores_allyears.csv"))
data.table::fwrite(scores_cvgshort, here("scores", "cvg_pooled.csv"))
data.table::fwrite(scores_avgcountry, here("scores", "ci_scores_avgcnt.csv"))
data.table::fwrite(all_crps, here("scores", "sample_scores.csv"))
data.table::fwrite(pp_scores, here("scores", "pointfc_scores.csv"))
data.table::fwrite(bvar_scores, here("scores", "bvar_ci_scores.csv"))
data.table::fwrite(bvar_scores_allyears, here("scores", "bvar_ci_scores_allyears.csv"))
data.table::fwrite(bvar_scores_cvgshort, here("scores", "bvar_cvg_pooled.csv"))
data.table::fwrite(bvar_scores_avgcountry, here("scores", "bvar_ci_scores_avgcnt.csv"))

