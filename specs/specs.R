library(here)
library(data.table)

#set project wide specifications

specs <- list(
  tv_release = 1,
  window_length = 9,
  score_max_year = 2012,
  min_year = 1990,
  ci_levels_eval = c(0.5, 0.8),
  ci_levels_make =  seq(0.1, 0.9, by = 0.1)
)


error_method <- c("directional", "absolute")
method <- c("rolling window", "expanding window", "leave-one-out")
target <- c("pcpi_pch", "ngdp_rpch")
source <- c("IMF", "bvar", "ar")

#make all combinations of settings
combs <- data.table::CJ(source, target, error_method, method)
data.table::fwrite(combs, here("quantile_forecasts", "setting_combinations.csv"))
