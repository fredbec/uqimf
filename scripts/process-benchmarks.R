library(here)
library(data.table)
devtools::load_all()

#load specs list
source(here("specs", "specs.R"))

.d <- `[`

min_year <- specs$min_year
max_year <- specs$max_year
holdout_split <- specs$holdout_split
window_length <- specs$window_length
flag_imputetv05as1 <- specs$flag_imputetv05as1

###############################################################################
#extract truth and horizon values from WEO forecasts
#horizon is not coded explicitly in benchmark forecasts
hordat <- data.table::fread(
  here("data", "weodat.csv")
) |>
  .d(, .(target_year, forecast_year, forecast_season, horizon)) |>
  unique() |>
  setnames("forecast_season", "season")

oecd_truth <- data.table::fread(here("oecd_data", "oecd_actuals.csv")) |>
  setnames("var", "target") |>
  .d(, target := ifelse(target == "cpi", "pcpi_pch", "ngdp_rpch")) |>
  setnames("truth_oecd", "tv_oecd") |>
  .d(target_year >= min_year)


truth <- data.table::fread(
  here("data", "weodat.csv")
) |>
  .d(, .(country, target, target_year, tv_0.5, tv_1, tv_1.5, tv_2)) |>
  .d(oecd_truth, on = c("country", "target", "target_year"))


if(flag_imputetv05as1){
  cyear <- format(Sys.Date(), "%Y") |> as.numeric()

  truth <- truth |>
    copy() |>
    #.d(target_year == cyear - 1) |>
    .d(is.na(tv_1) & target_year == cyear - 1,tv_1 := tv_0.5)

  #truth <- truth |>
  #  .d(target_year!= cyear - 1) |>
  #  rbind(tv_impute)

}

truth <- truth |>
  #.d(!is.na(tv_0.5)) |>
  .d(!is.na(get(paste0("tv_", tv_release)))) |>
  unique()


################################################################################
#point forecasts
#read in data, rename target
fcdat <- data.table::fread(
  here("benchmarks", "raw", "forecasts_March2024.csv")
) |>
  .d(quantile_level == 0.5) |> #only point forecast for this dataset
  .d(, target := ifelse(var == "cpi", "pcpi_pch", "ngdp_rpch")) |>
  .d(, var := NULL)


#merge with horizon data, remove and remerge with truth value
#remove values above max_year
benchmark_fc <- hordat[fcdat, on = c("target_year", "forecast_year", "season")] |>
  .d(method %in% c("ar", "bvar")) |>
  .d(!is.na(horizon)) |>
  setnames("value", "prediction") |>
  split(by = c("method")) |>
  lapply(function(dt)
    truth[dt, on = c("target", "target_year", "country")]
    ) |>
  rbindlist(idcol = "source") |>
  .d(order(source, target, country, forecast_year, horizon)) |>
  .d(, .(source, target, country, forecast_year, horizon, target_year,
         prediction, get(paste0("tv_", tv_release))
  )
  ) |>
  setnames("V8", paste0("tv_", tv_release))


#read in WEO forecasts, process the same way and merge with benchmarks
weodat <- fread(here("data", "weodat.csv")) |>
  .d(, source := "IMF") |>
  .d(order(source, target, country, forecast_year, horizon)) |>
  split(by = c("source")) |>
  lapply(function(dt)
    truth[dt, on = c("target", "target_year", "country")]
  ) |>
  rbindlist(idcol = "source") |>
  .d(, .(source, target, country, forecast_year, horizon, target_year,
         prediction, get(paste0("tv_", tv_release)))) |>
  setnames("V8", paste0("tv_", tv_release)) |>
  rbind(benchmark_fc)


################################################################################
#quantile forecasts
bvar_qufcs <- data.table::fread(
  here("benchmarks", "raw", "forecasts_March2024.csv")
  ) |>
  .d(method %in% c("bvar", "bvar_ciss")) |> #remove Truth and IMF forecasts
  .d(, target := ifelse(var == "cpi", "pcpi_pch", "ngdp_rpch")) |>
  .d(, var := NULL) |>
  setnames("method", "source") |>
  .d(, source := ifelse(source == "bvar", "bvar_qu", "bvar_ciss"))

bvar_fc <- hordat[bvar_qufcs, on = c("target_year", "forecast_year", "season")] |>
  .d(!is.na(horizon)) |>
  setnames("value", "prediction") |>
  setnames("quantile_level", "quantile")

bvar_fc <- truth[bvar_fc, on = c("target", "target_year", "country") ]  |>
  .d(order(source, target, country, forecast_year, horizon)) |>
  .d(, .(source, target, country, forecast_year, horizon, target_year,
         prediction, get(paste0("tv_", tv_release)), quantile)) |>
  setnames("V8", paste0("tv_", tv_release)) #|>
  #setnames(paste0("tv_", tv_release), "true_value")

bvar_fc_train <- bvar_fc |>
  copy() |>
  .d(target_year >= min_year & target_year < holdout_split)

bvar_fc_ho <- bvar_fc |>
  copy() |>
  .d(target_year >= holdout_split & target_year <= max_year)

################################################################################
#save data
data.table::fwrite(benchmark_fc, here("benchmarks", paste0(global_file_prefix, "point_benchmarks_processed.csv")))
data.table::fwrite(bvar_fc_train, here("benchmarks", paste0(global_file_prefix, "bvar_direct_quantile_forecasts.csv")))
data.table::fwrite(bvar_fc_ho, here("benchmarks", paste0(global_file_prefix, "bvar_direct_quantile_forecasts_ho.csv")))
data.table::fwrite(weodat, here("data", paste0(global_file_prefix, "point_forecasts.csv")))
