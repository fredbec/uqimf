library(here)
library(MetBrewer)
library(ggplot2)
global_file_prefix <- ""
prefix <- ""
.d <- `[`
library(data.table)
devtools::load_all()

qufcs <- data.table::fread(here("quantile_forecasts",
                                paste0(global_file_prefix, "toscore", prefix, "_quantile_forecasts_ho.csv"))) |>
  .d(source == "IMF")

qufcs_bvar <- data.table::fread(here("benchmarks",
                                     paste0(global_file_prefix, "toscore", prefix, "_bvar_direct_quantile_forecasts_ho.csv"))) |>
  setnames(paste0("tv_1"), "true_value") |>
  .d(source == "bvar_const") |>
  .d(quantile %in% c(0.1, 0.25, 0.75, 0.9))


realized_vals <- data.table::fread(here("real_time_publication", "imf-data", paste0("historicvalues_", "Fall2024", ".csv"))) |>
  setDT()

point_fcs <- data.table::fread(  here("real_time_publication", "imf-data", paste0("pointforecasts_", "Fall2024", ".csv"))) |>
  setDT()

trgt <- "pcpi_pch"
forecast_season <- "S"

linerange_dat_imf <- qufcs |>
  .d(target == trgt) |>
  .d(horizon == 0.5) |>
  .d(target_year %in% c(2020, 2021)) |>
  .d(, target_year := target_year - 0.25) |>
  .d(,quantile := paste0("quantile", quantile)) |>
  .d(, forecast_season := ifelse(horizon %% 1 == 0, "F", "S")) |>
  .d(, forecast_year := target_year - floor(horizon)) |>
  .d(,season_helper := ifelse(forecast_season == "S", 0.5, 0)) |>
  .d(,horizon := (target_year - forecast_year) + season_helper) |>
  .d(, .(country, target, target_year, horizon, quantile, prediction)) |>
  dcast(country + target + target_year + horizon ~ quantile, value.var = "prediction")

linerange_dat_bvar <- qufcs_bvar |>
  .d(target == trgt) |>
  .d(horizon == 0.5) |>
  .d(target_year %in% c(2020, 2021)) |>
  .d(, target_year := target_year + 0.25) |>
  .d(,quantile := paste0("quantile", quantile)) |>
  .d(, forecast_season := ifelse(horizon %% 1 == 0, "F", "S")) |>
  .d(, forecast_year := target_year - floor(horizon)) |>
  .d(,season_helper := ifelse(forecast_season == "S", 0.5, 0)) |>
  .d(,horizon := (target_year - forecast_year) + season_helper) |>
  .d(, .(country, target, target_year, horizon, quantile, prediction)) |>
  dcast(country + target + target_year + horizon ~ quantile, value.var = "prediction")

realized_vals_infl <- realized_vals |>
  .d(target == "inflation")
point_fcs_infl <- point_fcs |>
  .d(target == "inflation")

dashed_line <- rbind(point_fcs_infl,
                     realized_vals_infl |>
                       copy() |>
                       setnames("true_value", "prediction") |>
                       .d(target_year >= 2020 - 1)
)

cis <- c(0.5, 0.8)
qus <- c(0.1, 0.25, 0.75, 0.9)
cols <- paste0("quantile", qus)
release_year <- 2024

colors <- met.brewer("Hokusai1", 7)
names(colors) <- unique(qufcs$country)
shinyplot(realized_vals_infl, linerange_dat_imf, linerange_dat_bvar, point_fcs_infl, dashed_line, "CAN", colors, cis)
