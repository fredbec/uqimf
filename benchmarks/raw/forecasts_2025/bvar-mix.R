#this file "feeds" of of master having run up to exclude from scoring
# not included in master since it's currently only run on demand
bvar_fc_mix <- data.table::fread(here("benchmarks", paste0(global_file_prefix, "toscore_bvar_direct_quantile_forecasts_ho.csv"))) |>
  .d(source %in% c("bvar_qu", "bvar_const")) |>
  .d(, ind1 := ifelse(forecast_year <= 2019 & source == "bvar_qu", 1, 0)) |>
  .d(, ind2 := ifelse(forecast_year > 2019 & source == "bvar_const", 1, 0)) |>
  .d(, ind := ind1 + ind2) |>
  .d(ind == 1) |>
  .d(, ind1 := NULL) |>
  .d(, ind2 := NULL) |>
  .d(, ind := NULL) |>
  .d(, source := "bvar_mix")

#check that forecast instances are still identical
#base BVAR
check1 <- data.table::fread(here("benchmarks", paste0(global_file_prefix, "toscore_bvar_direct_quantile_forecasts_ho.csv"))) |>
  .d(source %in% c("bvar_qu")) |>
  .d(, c("target", "country", "forecast_year", "horizon", "quantile"))

check2 <- bvar_fc_mix |>
  copy() |>
  .d(, c("target", "country", "forecast_year", "horizon", "quantile"))

all.equal(check1, check2, ignore.row.order = TRUE) #row order does not matter for equality

bvar_fcs <- rbind(bvar_fc_mix,
                  data.table::fread(here("benchmarks", paste0(global_file_prefix,"toscore_bvar_direct_quantile_forecasts_ho.csv"))))

data.table::fwrite(bvar_fcs,
                   here("benchmarks", paste0(global_file_prefix, "toscore_bvar_direct_quantile_forecasts_ho.csv")))


#point forecasts
bvar_fc_mix <-  data.table::fread(here("quantile_forecasts", paste0(global_file_prefix, "toscore_quantile_forecasts_ho.csv"))) |>
  .d(source %in% c("bvar", "bvar_const")) |>
  .d(, forecast_year := target_year - floor(horizon)) |>
  .d(, ind1 := ifelse(forecast_year <= 2019 & source == "bvar", 1, 0)) |>
  .d(, ind2 := ifelse(forecast_year > 2019 & source == "bvar_const", 1, 0)) |>
  .d(, ind := ind1 + ind2) |>
  .d(ind == 1) |>
  .d(, ind1 := NULL) |>
  .d(, ind2 := NULL) |>
  .d(, ind := NULL) |>
  .d(, source := "bvar_mix")


check1 <- data.table::fread(here("quantile_forecasts", paste0(global_file_prefix, "toscore_quantile_forecasts_ho.csv"))) |>
  .d(source %in% c("bvar")) |>
  .d(, forecast_year := target_year - floor(horizon)) |>
  .d(, c("target", "country", "forecast_year", "horizon", "quantile"))

check2 <- bvar_fc_mix |>
  copy() |>
  .d(, c("target", "country", "forecast_year", "horizon", "quantile"))

all.equal(check1, check2, ignore.row.order = TRUE) #row order does not matter for equality

bvar_fcs <- rbind(bvar_fc_mix |> .d(, forecast_year := NULL),
                  data.table::fread(here("quantile_forecasts", paste0(global_file_prefix,"toscore_quantile_forecasts_ho.csv"))))

data.table::fwrite(bvar_fcs,
                   here("quantile_forecasts", paste0(global_file_prefix,"toscore_quantile_forecasts_ho.csv")))
