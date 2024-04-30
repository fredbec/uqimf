library(here)
library(data.table)

.d <- `[` #for piping data.table operations

#benchmark data
benchmarks <- data.table::fread(here("data", "point_forecasts.csv"))

#data if there were no missing predictions
perfect_avail_dat <- expand.grid(
  country = unique(benchmarks$country),
  target = unique(benchmarks$target),
  forecast_year = unique(benchmarks$forecast_year),
  horizon = unique(benchmarks$horizon),
  source = unique(benchmarks$source)
) |>
  setDT()


#join data to find  out which predictions are missing
missingdat <- benchmarks[perfect_avail_dat, on = c("country", "target", "forecast_year", "horizon", "source")] |>
  .d(is.na(target_year), target_year := forecast_year + floor(horizon))

data.table::fwrite(missingdat, here("data", "point_forecasts.csv"))
