library(data.table)
library(here)
library(ggplot2)
library(MetBrewer)
library(patchwork)
devtools::load_all()

.d <- `[`


cis <- specs$ci_levels_eval
qus <- specs$qu_levels
vis_begin_yr <- current_yr_season$forecast_year - 8

weodat <- fread(here(location_download, "weodat.csv")) |>
  data.table::setnames(paste0("tv_", tv_release), "true_value")

realized_vals <- weodat |>
  .d(target_year > vis_begin_yr) |>
  .d(, .(country, target, target_year, true_value)) |>
  .d(!is.na(true_value)) |>
  unique()

point_forecasts <- weodat |>
  .d(source == "IMF") |>
  .d(,forecast_season := ifelse(horizon %% 1 == 0, "F", "S")) |>
  .d(ceiling(target_year - horizon) <= current_yr_season$forecast_year) |>
  .d(horizon %in% current_yr_season$available_horizons) |>
  .d(forecast_year == current_yr_season$forecast_year) |>
  .d(, target := ifelse(target == "ngdp_rpch", "gdp_growth", "inflation")) |>
  .d(, .(country, target, target_year, prediction))

data.table::fwrite(realized_vals, here(path_dest, "imf-data", paste0("historicvalues_", current_yr_season$identifier, ".csv")))
data.table::fwrite(realized_vals, here(path_dest, "imf-data", paste0("pointforecasts_", current_yr_season$identifier, ".csv")))

