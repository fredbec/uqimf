library(here)
library(data.table)

#set project wide specifications

min_year <- 1990
window_length <- 11

specs <- list(
  tv_release = 1,
  window_length = window_length,
  score_max_year = 2012,
  max_year_imf = 2024, #for MAKING forecasts, won't affect scoring at all
  max_year_others = 2024, #for MAKING forecasts, won't affect scoring at all
  min_year = min_year,
  #score_min_year = 2013,
  score_min_year = min_year + window_length,
  ci_levels_eval = c(0.5, 0.8),
  ci_levels_eval_su = c(50, 80), #for scoringutils wrapper
  ci_levels_make =  seq(0.1, 0.9, by = 0.1),
  qu_levels = c(0.1, 0.25, 0.75, 0.9),
  qutype = 7,

  flag_imputetv05as1 = TRUE,

  instances_to_exclude = list(

    #no benchmarks for JAPAN after this time (due to missing inflation data)
    i1 = list(
      country = c("JPN"),
      target_year = 2021:2100 #2100 is just a random maximum value
    )
  )
)


error_method <- c("directional", "absolute")
method <- c("rolling window", "expanding window", "leave-one-out")
target <- c("pcpi_pch", "ngdp_rpch")
source <- c("IMF", "bvar", "ar")

#make all combinations of settings
combs <- data.table::CJ(source, target, error_method, method)
data.table::fwrite(combs, here("quantile_forecasts", "setting_combinations.csv"))


rm(min_year)
rm(window_length)
