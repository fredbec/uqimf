library(here)
source(here("benchmarks", "raw","forecasts_2025", "ar_forecast_procs.R"))


daty <- rnorm(20)

ar_p_fcst(daty, p = 10)
