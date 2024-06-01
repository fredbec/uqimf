library(data.table)
library(here)
library(ggplot2)
library(MetBrewer)
library(patchwork)
devtools::load_all()

.d <- `[`

cis <- c(0.5, 0.8)
qus <- c(0.1, 0.25, 0.75, 0.9)
filter_yr <- 2024
vis_begin_yr <- 2016
release <- "Spring2024"
release_season <- "S"

qufcs <- fread(here("quantile_forecasts", "quantile_forecasts.csv")) |>
  .d(source == "IMF") |>
  .d(error_method == "absolute") |>
  .d(method == "rolling window") |>
  .d(quantile %in% c(0.1, 0.25, 0.75, 0.9)) |>
  .d(,forecast_season := ifelse(horizon %% 1 == 0, "F", "S")) |>
  .d(, forecast_year := target_year - floor(horizon)) |>
  .d(, .(country, target, forecast_year, forecast_season, target_year, quantile, prediction, horizon, true_value)) |>
  .d(, target := ifelse(target == "ngdp_rpch", "gdp_growth", "inflation"))

qufcs_to_save <- qufcs |>
  .d(, fltr := ceiling(target_year - horizon)) |>
  .d(fltr == filter_yr & forecast_season  == release_season) |>
  .d(, .(country, target, forecast_year, forecast_season, target_year, quantile, prediction))

realized_vals <- qufcs |>
  .d(target_year > vis_begin_yr) |>
  .d(, .(country, target, target_year, true_value)) |>
  .d(!is.na(true_value)) |>
  unique()

point_forecasts <- fread(here("data", "point_forecasts.csv")) |>
  .d(source == "IMF") |>
  .d(,forecast_season := ifelse(horizon %% 1 == 0, "F", "S")) |>
  .d(, fltr := ceiling(target_year - horizon)) |>
  .d(fltr == filter_yr & forecast_season  == release_season) |>
  .d(, target := ifelse(target == "ngdp_rpch", "gdp_growth", "inflation")) |>
  .d(, .(country, target, target_year, prediction))


data.table::fwrite(qufcs_to_save, file = here("..", "MacroPI_check", "MacroPI", "forecasts", paste0("forecasts_", release, ".csv")))
data.table::fwrite(realized_vals, file = here("..", "MacroPI_check", "MacroPI", "imf-data", paste0("historicvalues_", release, ".csv")))
data.table::fwrite(point_forecasts, file = here("..", "MacroPI_check", "MacroPI", "imf-data", paste0("pointforecasts_", release, ".csv")))
