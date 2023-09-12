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

#########This Block is repeated from make-forecasts and only for scoring via crps_sample
#extract truth and horizon values from WEO forecasts
hordat <- data.table::fread(
  here("WEOforecasts_tidy.csv")
) |>
  .d(, .(target_year, forecast_year, forecast_season, horizon)) |>
  unique() |>
  setnames("forecast_season", "season")

truth <- data.table::fread(
  here("WEOforecasts_tidy.csv")
) |>
  .d(g7 == 1) |>
  .d(, .(ISOAlpha_3Code, target, target_year, tv_0.5, tv_1, tv_1.5, tv_2)) |>
  .d(!is.na(get(paste0("tv_", tv_release)))) |>
  unique() |>
  setnames("ISOAlpha_3Code", "country")


#read in forecast data and prepare for feeding to empFC function
#rename some stuff, join with truth and horizon data, reduce columns and
#filter for year set
fcdat <- data.table::fread(
  here("benchmarks", "forecast_long.csv")
) |>
  .d(, V1 := NULL) |>
  .d(, target := ifelse(var == "cpi", "pcpi_pch", "ngdp_rpch")) |>
  .d(, var := NULL)

fcdat <- hordat[fcdat, on = c("target_year", "forecast_year", "season")] |>
  .d(method != "Truth") |>
  split(by = c("method")) |>
  lapply(function(dt)
    dt |>
      .d(, method := NULL) |>
      .d(!is.na(horizon)) |>
      setnames("value", "prediction")) |>
  lapply(function(dt)
    truth[dt, on = c("target", "target_year", "country")]) |>
  rbindlist(idcol = "source") |>
  .d(order(source, target, country, forecast_year, horizon)) |>
  .d(, .(source, target, country, forecast_year, horizon, target_year,
         prediction, get(paste0("tv_", tv_release))
  )
  ) |>
  setnames("V8", paste0("tv_", tv_release)) |>
  .d(target_year <= max_year)



####################################read in quantile forecasts########################
qufcs <- data.table::fread(here("quantile_forecasts", "quantile_forecasts.csv"))
combs <- data.table::fread(here("quantile_forecasts", "setting_combinations.csv"))
bvar_qufcs <- data.table::fread(here("benchmarks", "forecast_quantiles.csv"))


#################score CI's####################################################
weodat_qu_sameyearset <- qufcs |>
  data.table::copy() |>
  .d(target_year>=1999) |>
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
  .d(target_year>=1999) |>
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
idx_bvar <- which(bvar_qufcs$quantile_level == 0.05)[1] #where bvar starts

#remove redundant rows and rename target
bvar_qus <- hordat[bvar_qufcs[idx_bvar:nrow(bvar_qufcs)], on = c("target_year", "forecast_year", "season")]  |>
  .d(, target := ifelse(var == "cpi", "pcpi_pch", "ngdp_rpch")) |>
  .d(, var := NULL)  |>
  .d(, source := "bvar") |>
  .d(, method := NULL) |>
  setnames("value", "prediction") |>
  .d(target_year >= min_year & target_year <= max_year) |>
  setnames("quantile_level", "quantile")

#join with truth data
bvar_qus <- truth[bvar_qus, on = c("target", "target_year", "country") ]  |>
  .d(order(source, target, country, forecast_year, horizon)) |>
  .d(, .(source, target, country, forecast_year, horizon, target_year,
         prediction, get(paste0("tv_", tv_release)), quantile)) |>
  setnames("V8", paste0("tv_", tv_release)) |>
  setnames(paste0("tv_", tv_release), "true_value") |>
  .d(target_year <= max_year)


bvar_qu_sameyearset <- bvar_qus |>
  data.table::copy() |>
  .d(target_year>=1999) |>
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
data.table::fwrite(scores, here("quantile_forecasts", "ci_scores.csv"))
data.table::fwrite(scores_allyears, here("quantile_forecasts", "ci_scores_allyears.csv"))
data.table::fwrite(scores_cvgshort, here("quantile_forecasts", "cvg_pooled.csv"))
data.table::fwrite(scores_avgcountry, here("quantile_forecasts", "ci_scores_avgcnt.csv"))
data.table::fwrite(all_crps, here("quantile_forecasts", "sample_scores.csv"))
data.table::fwrite(pp_scores, here("quantile_forecasts", "pointfc_scores.csv"))
data.table::fwrite(bvar_scores, here("quantile_forecasts", "bvar_ci_scores.csv"))
data.table::fwrite(bvar_scores_allyears, here("quantile_forecasts", "bvar_ci_scores_allyears.csv"))
data.table::fwrite(bvar_scores_cvgshort, here("quantile_forecasts", "bvar_cvg_pooled.csv"))
data.table::fwrite(bvar_scores_avgcountry, here("quantile_forecasts", "bvar_ci_scores_avgcnt.csv"))

