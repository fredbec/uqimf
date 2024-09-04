library(here)
library(data.table)

#set project wide specifications
specs <- list(
  error_method = "absolute",
  method = "rolling window",
  window_length = 11,
  tv_release = 1,
  min_year = 1990,
  ci_levels_eval = c(0.5, 0.8),
  ci_levels_eval_su = c(50, 80), #for scoringutils wrapper
  ci_levels_make =  c(0.1, 0.25, 0.75, 0.9),
  qu_levels = c(0.1, 0.25, 0.75, 0.9),
  qutype = 7,

  flag_imputetv05as1 = TRUE
)
