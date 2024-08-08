library(here)
library(data.table)
devtools::load_all()

source(here("specs", "specs.R"))

.d <- `[`

max_year_imf <- specs$max_year_imf
max_year_others <- specs$max_year_others
min_year <- specs$min_year
tv_release <- specs$tv_release
quantype <- specs$qutype

cis <- specs$ci_levels_make

#make all combinations of settings
combs <- data.table::fread(here("quantile_forecasts", "setting_combinations.csv"))


fcdat <- data.table::fread(here("data", "point_forecasts.csv"))

#make quantile forecasts for all combinations in combs
qufcs <- lapply(1:nrow(combs),
  function(idxsub){

    setting <- combs[idxsub]

    if(setting[, "method"] == "leave-one-out"){

      start_year <- min_year
    } else {

      start_year <- min_year + 11 # 11 is max value for R
    }

    subdat <- fcdat |>
      .d(source == setting[, "source"]) |>
      .d(target == setting[, "target"]) |>
      .d(, source := NULL)

    if (setting[, "source"] %in% c("bvar", "ar")) {
      max_year <- specs$max_year_others
    } else {

      max_year <- specs$max_year_imf
    }

    empFC(subdat,
          target_year = start_year:max_year,
          tv_release = tv_release,
          error_method = setting[, "error_method"],
          method = setting[, "method"],
          window_length = window_length,
          ci_levels = cis,
          qutype = quantype) |>
      .d(, source := setting[, "source"]) |>
      .d(, error_method := setting[, "error_method"]) |>
      .d(, method := setting[, "method"])

    }
  ) |>
  rbindlist() |>
  setnames(paste0("tv_", tv_release), "true_value") #|>
  #.d(!is.na(true_value))

mainfcs <- qufcs |>
  .d(error_method == "absolute" & method == "rolling window") |>
  empFC_pava()

mainfcs_ew <- qufcs |>
  .d(error_method == "absolute" & method == "expanding window") |>
  empFC_pava()

mainfcs <- rbind(mainfcs, mainfcs_ew)

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
data.table::fwrite(mainfcs, here("quantile_forecasts", "quantile_forecasts_pava.csv"))
