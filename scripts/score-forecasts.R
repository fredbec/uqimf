library(here)
library(data.table)
devtools::load_all()

source(here("specs", "specs.R"))

.d <- `[`

window_length <- specs$window_length

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

#for scoring point forecasts
fcdat <- data.table::fread(here("data", paste0(global_file_prefix, "toscore_point_forecasts.csv"))) |>
  .d(target_year <= max_year)


####################################read in quantile forecasts########################
qufcs <- data.table::fread(here("quantile_forecasts",
                                paste0(global_file_prefix, "toscore", prefix, "_quantile_forecasts.csv")))

if(cset == "base"){
  qufcs_directional <- data.table::fread(here("quantile_forecasts",
                                            paste0(global_file_prefix, "toscore", prefix, "_quantile_forecasts_directional.csv")))
}
qufcs_ho <- data.table::fread(here("quantile_forecasts",
                                   paste0(global_file_prefix, "toscore", prefix, "_quantile_forecasts_ho.csv")))


#################score CI's####################################################
qufcs <- qufcs |>
  .d(,.(country, target, horizon, target_year, true_value, prediction, quantile, method, error_method, source)) |>
  setnames("source", "model") #|>
  #.d(round(quantile,8) %in% qus)
if(cset == "base"){
  qufcs_directional <- qufcs_directional |>
    .d(,.(country, target, horizon, target_year, true_value, prediction, quantile, method, error_method, source)) |>
    setnames("source", "model")# |>
    #.d(quantile %in% qus)
}
qufcs_ho <- qufcs_ho |>
  .d(,.(country, target, horizon, target_year, true_value, prediction, quantile, method, error_method, source)) |>
  setnames("source", "model")# |>
  #.d(quantile %in% qus)


scores <- scoreempQu(qufcs, cvg_rg = cis100,
                     by = c("model", "error_method", "method", "country", "target", "horizon"))
scores_ho <- scoreempQu(qufcs_ho, cvg_rg = cis100,
                        by = c("model", "error_method", "method", "country", "target", "horizon"))

scores_avgcountry <- scoreempQu(qufcs, cvg_rg = cis100,
                     by = c("model", "error_method", "method", "target", "horizon"))
scores_avgcountry_ho <- scoreempQu(qufcs_ho, cvg_rg = cis100,
                                   by = c("model", "error_method", "method", "target", "horizon"))

scores_cvgshort <- scoreempQu(qufcs, cvg_rg = cis100,
                              by = c("model", "error_method", "method", "target"))
scores_cvgshort_ho <- scoreempQu(qufcs_ho, cvg_rg = cis100,
                                 by = c("model", "error_method", "method", "target"))

scores_ho_byyr <- scoreempQu(qufcs_ho, cvg_rg = cis100,
                             by = c("target_year", "target", "horizon", "method", "error_method", "model"))

if(cset == "base"){
  scores_directional <- scoreempQu(qufcs_directional, cvg_rg = cis100,
                                   by = c("model", "error_method", "method", "country", "target", "horizon"))
  scores_avgcountry_directional <- scoreempQu(qufcs_directional, cvg_rg = cis100,
                                              by = c("model", "error_method", "method", "target", "horizon"))
  scores_cvgshort_directional <- scoreempQu(qufcs_directional, cvg_rg = cis100,
                                            by = c("model", "error_method", "method", "target"))
}
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
#if(FALSE){ #placeholder until we have bvar data for extended countries
if(cset == "base"){
  bvar_qus <- data.table::fread(here("benchmarks",
                                     paste0(global_file_prefix, "toscore", prefix, "_bvar_direct_quantile_forecasts.csv"))) |>
    setnames(paste0("tv_", tv_release), "true_value")
  bvar_qus <- bvar_qus |>
    data.table::copy() |>
    .d(,.(country, target, horizon, target_year, true_value, prediction, quantile, source)) |>
    setnames("source", "model")
}


bvar_qus_ho <- data.table::fread(here("benchmarks",
                                      paste0(global_file_prefix, "toscore", prefix, "_bvar_direct_quantile_forecasts_ho.csv"))) |>
  setnames(paste0("tv_", tv_release), "true_value")

bvar_qus_ho <- bvar_qus_ho |>
  data.table::copy() |>
  .d(,.(country, target, horizon, target_year, true_value, prediction, quantile, source)) |>
  setnames("source", "model")

if(prefix != "_bvarspecs" & cset == "base"){ #only score in bvarspecs setting
  bvar_qus <- bvar_qus |>
    .d(model != "bvar_ciss")

  bvar_qus_ho <- bvar_qus_ho |>
    .d(model != "bvar_ciss")
}

if(specs$ciset == "base"){
  bvar_qus_ho <- bvar_qus_ho |>
    .d(quantile %in% qus)

  if(length(unique(bvar_qus_ho$quantile))!=length(qus)){
    warning("quantiles were filtered out in bvar_qus_ho")
  }

  if(cset == "base"){
    bvar_qus <- bvar_qus |>
      .d(quantile %in% qus)

    if(length(unique(bvar_qus$quantile))!=length(qus)){
      warning("quantiles were filtered out in bvar_qus")
    }
  }
}



if(cset == "base"){
  bvar_scores <- scoreempQu(bvar_qus, cvg_rg = cis100,
                            by = c("model", "country", "target", "horizon"))
  bvar_scores_avgcountry <- scoreempQu(bvar_qus, cvg_rg = cis100,
                                       by = c("model", "target", "horizon"))
  bvar_scores_cvgshort <- scoreempQu(bvar_qus, cvg_rg = cis100,
                                     by = c("model", "target"))
}

bvar_scores_ho <- scoreempQu(bvar_qus_ho, cvg_rg = cis100,
                             by = c("model", "country", "target", "horizon"))
bvar_scores_avgcountry_ho <- scoreempQu(bvar_qus_ho, cvg_rg = cis100,
                                        by = c("model", "target", "horizon"))
bvar_scores_cvgshort_ho <- scoreempQu(bvar_qus_ho, cvg_rg = cis100,
                                   by = c("model", "target"))

bvar_scores_ho_byyr <- scoreempQu(bvar_qus_ho, cvg_rg = cis100,
                             by = c("target_year", "target", "horizon", "model"))


#if(FALSE){
############### CRPS by sample ###############################
if(prefix != "_bvarspecs"){ #crps not needed for bvarspecs setting
  if(cset == "base"){
    combs_methods <- data.table::CJ(error_method = c("absolute"),
                                    method = c("rolling window"),
                                    source = c("IMF", "bvar", "ar", "ar_bic", "bvar", "bvar_const", "mean_ensemble",
                                               "ar-direct", "ar_bic-direct", "bvar_const-direct", "bvar_qu-direct"))

    all_crps <- lapply(1:nrow(combs_methods), function(idx){

      comb_set <- combs_methods[idx,]

      start_year <- 2001

      sub_samples <- qufcs |>
        rbind(bvar_qus |>
                copy() |>
                .d(, model := paste0(model, "-direct")) |>
                .d(,method := NA) |>
                .d(,error_method := NA)) |>
        .d(model == comb_set[, "source"])

      score_by_crps_quants(sub_samples, target_years = start_year:2012, tv_release = tv_release,
                           error_method = comb_set[, "error_method"], method = comb_set[, "method"],
                           window_length = window_length) |>
        .d(, method := comb_set[, "method"]) |>
        .d(, error_method := comb_set[, "error_method"]) |>
        .d(, source := comb_set[, "source"])
    }
    ) |>
      rbindlist() |>
      .d(, .(score = mean(score)), by = c("target", "horizon", "method", "error_method", "source"))




    ############### CRPS by sample directional ##############################
    combs_methods <- data.table::CJ(error_method = c("directional"),
                                    method = c("rolling window"),
                                    source = c("IMF", "bvar", "ar", "ar_bic", "bvar", "bvar_const", "mean_ensemble"))

    all_crps_directional <- lapply(1:nrow(combs_methods), function(idx){

      comb_set <- combs_methods[idx,]

      start_year <- 2001

      sub_samples <- qufcs_directional |>
        .d(model == comb_set[, "source"])

      score_by_crps_quants(sub_samples, target_years = start_year:2012, tv_release = tv_release,
                           error_method = comb_set[, "error_method"], method = comb_set[, "method"],
                           window_length = window_length) |>
        .d(, method := comb_set[, "method"]) |>
        .d(, error_method := comb_set[, "error_method"]) |>
        .d(, source := comb_set[, "source"])
    }
    ) |>
      rbindlist() |>
      .d(, .(score = mean(score)), by = c("target", "horizon", "method", "error_method", "source"))
    ############### CRPS by sample holdout ##############################
    combs_methods <- data.table::CJ(error_method = c("absolute"),
                                    method = c("rolling window"),
                                    source = c("IMF", "bvar", "ar", "ar_bic", "bvar", "bvar_const", "bvar_mix", "mean_ensemble",
                                               "ar-direct", "ar_bic-direct", "bvar_const-direct", "bvar_qu-direct",
                                               "ar_annual-direct", "bvar_mix-direct", "arx_annual-direct"))

    all_crps_ho <- lapply(1:nrow(combs_methods), function(idx){

      comb_set <- combs_methods[idx,]
      start_year <- 2013

      sub_samples <- qufcs_ho  |>
        rbind(bvar_qus_ho |>
                copy() |>
                .d(, model := paste0(model, "-direct")) |>
                .d(,method := NA) |>
                .d(,error_method := NA)) |>
        .d(model == comb_set[, "source"])

      score_by_crps_quants(sub_samples, target_years = start_year:2023, tv_release = tv_release,
                           error_method = comb_set[, "error_method"], method = comb_set[, "method"],
                           window_length = window_length) |>
        .d(, method := comb_set[, "method"]) |>
        .d(, error_method := comb_set[, "error_method"]) |>
        .d(, source := comb_set[, "source"])
    }
    ) |>
      rbindlist()

    all_crps_ho_byyr <- all_crps_ho |>
      data.table::copy()  |>
      .d(, .(score = mean(score)), by = c("target_year", "target", "horizon", "method", "error_method", "source"))

    all_crps_ho <- all_crps_ho |>
      data.table::copy() |>
      .d(, .(score = mean(score)), by = c("target", "horizon", "method", "error_method", "source"))

  } else {
    combs_methods <- data.table::CJ(error_method = c("absolute"),
                                    method = c("rolling window"),
                                    source = c("IMF", "ar_annual-direct", "arx_annual-direct"))

    all_crps_ho <- lapply(1:nrow(combs_methods), function(idx){

      comb_set <- combs_methods[idx,]
      start_year <- 2013

      sub_samples <- qufcs_ho  |>
        rbind(bvar_qus_ho |>
                copy() |>
                .d(, model := paste0(model, "-direct")) |>
                .d(,method := NA) |>
                .d(,error_method := NA)) |>
        .d(model == comb_set[, "source"])

      score_by_crps_quants(sub_samples, target_years = start_year:2023, tv_release = tv_release,
                           error_method = comb_set[, "error_method"], method = comb_set[, "method"],
                           window_length = window_length) |>
        .d(, method := comb_set[, "method"]) |>
        .d(, error_method := comb_set[, "error_method"]) |>
        .d(, source := comb_set[, "source"])
    }
    ) |>
      rbindlist() |>
      .d(, .(score = mean(score)), by = c("target", "horizon", "method", "error_method", "source"))


  }
}

######################################Saving#######################################################
data.table::fwrite(scores, here("scores", prefix, paste0(global_file_prefix, "ci_scores.csv")))
data.table::fwrite(scores_ho, here("scores", prefix, paste0(global_file_prefix, "ci_scores_ho.csv")))

data.table::fwrite(scores_avgcountry, here("scores",prefix, paste0(global_file_prefix, "ci_scores_avgcnt.csv")))
data.table::fwrite(scores_avgcountry_ho, here("scores", prefix, paste0(global_file_prefix, "ci_scores_avgcnt_ho.csv")))

data.table::fwrite(scores_cvgshort, here("scores", prefix,paste0(global_file_prefix, "cvg_pooled.csv")))
data.table::fwrite(scores_cvgshort_ho, here("scores", prefix, paste0(global_file_prefix, "cvg_pooled_ho.csv")))

data.table::fwrite(pp_scores, here("scores", prefix, paste0(global_file_prefix, "pointfc_scores.csv")))

if(prefix != "_bvarspecs"){
  data.table::fwrite(all_crps_ho, here("scores", prefix, paste0(global_file_prefix, "crps_values_ho.csv")))
  if(cset == "base"){
    data.table::fwrite(all_crps_ho_byyr, here("scores", prefix, paste0(global_file_prefix, "crps_values_ho_byyr.csv")))
  }
}

if(cset == "base"){
  data.table::fwrite(scores_directional, here("scores", prefix, paste0(global_file_prefix, "ci_scores_directional.csv")))
  data.table::fwrite(scores_avgcountry_directional, here("scores", prefix, paste0(global_file_prefix, "ci_scores_avgcnt_directional.csv")))
  data.table::fwrite(scores_cvgshort_directional, here("scores", prefix, paste0(global_file_prefix, "cvg_pooled_directional.csv")))
  if(prefix != "_bvarspecs"){
    data.table::fwrite(all_crps, here("scores", prefix, paste0(global_file_prefix, "crps_values.csv")))
    data.table::fwrite(all_crps_directional, here("scores", prefix, paste0(global_file_prefix, "crps_values_directional.csv")))
  }
  data.table::fwrite(bvar_scores, here("scores", prefix, paste0(global_file_prefix, "bvar_ci_scores.csv")))
  data.table::fwrite(bvar_scores_cvgshort, here("scores", prefix, paste0(global_file_prefix, "bvar_cvg_pooled.csv")))
  data.table::fwrite(bvar_scores_avgcountry, here("scores", prefix, paste0(global_file_prefix, "bvar_ci_scores_avgcnt.csv")))
}

data.table::fwrite(bvar_scores_ho, here("scores", prefix, paste0(global_file_prefix, "bvar_ci_scores_ho.csv")))
data.table::fwrite(bvar_scores_cvgshort_ho, here("scores", prefix, paste0(global_file_prefix, "bvar_cvg_pooled_ho.csv")))
data.table::fwrite(bvar_scores_avgcountry_ho, here("scores", prefix, paste0(global_file_prefix, "bvar_ci_scores_avgcnt_ho.csv")))

data.table::fwrite(bvar_scores_ho_byyr, here("scores", prefix, paste0(global_file_prefix, "bvar_ci_scores_byyr_ho.csv")))
data.table::fwrite(scores_ho_byyr, here("scores", prefix, paste0(global_file_prefix, "ci_scores_byyr_ho.csv")))


