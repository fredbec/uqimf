library(here)
library(data.table)

#set project wide specifications
specs <- list(
  holdout_split = 2013,

  cset = "base", #"extended" for alternative countries, as defined in next line
  cset_list = c("KOR", "AUS", "ESP", "NLD", "CHE", "CHN", "IND", "RUS", "BRA", "IDN"),

  ciset = "extended", #"extended" for more CI levels (10,20,...,90)

  tv_release = 1,
  window_length = 11,
  max_year = 2023,
  min_year = 1990,
  #score_min_year = ifelse(holdout, 2013, min_year + window_length),
  #score_min_year = min_year + window_length,
  ci_levels_eval = c(0.5, 0.8),
  ci_levels_eval_su = c(50, 80), #for scoringutils wrapper
  ci_levels_make = c(0.5, 0.8),
  qu_levels = c(0.1, 0.25, 0.75, 0.9),
  ci_levels_eval_extended = seq(0.1, 0.9, by = 0.1),
  ci_levels_eval_su_extended = seq(10, 90, by = 10), #for scoringutils wrapper
  ci_levels_make_extended = seq(0.1, 0.9, by = 0.1),
  qu_levels_extended = seq(0.05, 0.95, by = 0.05),

  qutype = 7,

  flag_imputetv05as1 = TRUE,

  instances_to_exclude = list(

    #no benchmarks for Japan after 2021 (due to missing inflation data)
    i1 = list(
      country = c("JPN"),
      target_year = 2021:2100 #2100 is just a random maximum value
    ),

    #don't score any years prior to 2001 (due to 11 being the default window length)
    i2 = list(
      target_year = 1900:2000 #1900 is just a random minimum value
    )
  ),

  #exclude Japan and Canada for comparing BVAR specs, as there is no CISS
  #data available for these countries
  instances_to_exclude_bvarspecs = list(

    i1 = list(country = c("JPN", "CAN"))
  )
)


error_method <- c("directional", "absolute")
method <- c("rolling window")
target <- c("pcpi_pch", "ngdp_rpch")
source <- c("IMF", "bvar", "ar", "ar_bic", "bvar_const")

#make all combinations of settings
combs <- data.table::CJ(source, target, error_method, method)
data.table::fwrite(combs, here("quantile_forecasts", "setting_combinations.csv"))

