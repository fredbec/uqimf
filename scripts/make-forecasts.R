library(here)
library(data.table)
devtools::load_all()

.d <- `[`
max_year <- 2012
min_year <- 1990
tv_release <- 1
window_length <- 9

error_method <- c("directional", "absolute")
method <- c("rolling window", "expanding window", "leave-one-out")
target <- c("pcpi_pch", "ngdp_rpch")
source <- c("IMF", "bvar", "ar")

cis <- seq(0.1, 0.9, by = 0.1)

#make all combinations of settings
combs <- data.table::CJ(source, target, error_method, method)
data.table::fwrite(combs, here("quantile_forecasts", "setting_combinations.csv"))


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


#make quantile forecasts for all combinations in combs
qufcs <- lapply(1:nrow(combs),
  function(idxsub){

    setting <- combs[idxsub]

    if(setting[, "method"] == "leave-one-out"){

      start_year <- min_year
    } else {

      start_year <- min_year + window_length
    }

    subdat <- fcdat |>
      .d(source == setting[, "source"]) |>
      .d(target == setting[, "target"]) |>
      .d(, source := NULL)


    empFC(subdat,
          target_year = start_year:max_year,
          tv_release = tv_release,
          error_method = setting[, "error_method"],
          method = setting[, "method"],
          window_length = window_length,
          ci_levels = cis) |>
      .d(, source := setting[, "source"]) |>
      .d(, error_method := setting[, "error_method"]) |>
      .d(, method := setting[, "method"])

    }
  ) |>
  rbindlist() |>
  setnames(paste0("tv_", tv_release), "true_value") |>
  .d(!is.na(true_value))


directional_expand <- qufcs |>
  .d(error_method == "directional") |>
  .d(quantile %in% c(0.1+fperror(type = 0.1), 0.25, 0.75, 0.9)) |>
  .d(, .(country, target, target_year, horizon, imf_pp, true_value, error, quantile, error_prediction, source, method)) |>
  .d(, quantile := paste0("q", quantile)) |>
  dcast(country + target + target_year + horizon + error + imf_pp + true_value + source + method ~ quantile,
         value.var = "error_prediction") |>
  .d(,length_ci0.9 := q0.9 - q0.1) |>
  .d(,length_ci0.5 := q0.75 - q0.25) |>
  .d(, q0.1 := -length_ci0.9/2) |>
  .d(, q0.9 := length_ci0.9/2) |>
  .d(, q0.25 := -length_ci0.5/2) |>
  .d(, q0.75 := length_ci0.5/2) |>
  .d(, length_ci0.9 := NULL) |>
  .d(, length_ci0.5 := NULL) |>
  melt(measure.vars = c(paste0("q", c(0.1, 0.25, 0.75, 0.9))),
       value.name = "error_prediction",
       variable.name = "quantile") |>
  .d(, quantile := as.numeric(substring(quantile, 2))) |>
  .d(, error_method := "symmetric") |>
  .d(, prediction := imf_pp + error_prediction)


data.table::fwrite(qufcs, here("quantile_forecasts", "quantile_forecasts.csv"))
data.table::fwrite(directional_expand, here("quantile_forecasts", "qufcs_directionalsymmetric.csv"))
