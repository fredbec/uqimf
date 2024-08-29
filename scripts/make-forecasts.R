library(here)
library(data.table)
devtools::load_all()

source(here("specs", "specs.R"))

.d <- `[`

max_year_imf <- specs$max_year_imf
max_year_others <- specs$max_year_others
min_year <- specs$min_year
tv_release <- specs$tv_release
window_length <- specs$window_length
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

      start_year <- min_year + window_length
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

data.table::fwrite(qufcs, here("quantile_forecasts", "quantile_forecasts.csv"))
data.table::fwrite(mainfcs, here("quantile_forecasts", "quantile_forecasts_pava.csv"))
