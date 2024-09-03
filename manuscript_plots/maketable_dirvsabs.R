library(here)
library(data.table)
library(kableExtra)
 .d <- `[`

scores_cvgshort_abs <- fread(here("scores", paste0("ci_scores_avgcnt", ".csv"))) |>
  .d(model == "IMF" & method == "rolling window" & error_method == "absolute") |>
  .d(, c("error_method", "target", "horizon", "interval_score", "coverage_50", "coverage_80"))|>
  .d(, interval_score := round(interval_score, 2)) |>
  .d(, coverage_80 := round(coverage_80, 2))|>
  .d(, coverage_50 := round(coverage_50, 2))


scores_cvgshort_dir <- fread(here("scores", paste0("ci_scores_avgcnt", "_directional", ".csv"))) |>
  .d(model == "IMF" & method == "rolling window" & error_method == "directional")|>
  .d(, c("error_method", "target", "horizon", "interval_score", "coverage_50", "coverage_80")) |>
  .d(, interval_score := round(interval_score, 2)) |>
  .d(, coverage_80 := round(coverage_80, 2))|>
  .d(, coverage_50 := round(coverage_50, 2))

allscores <- rbind(scores_cvgshort_abs, scores_cvgshort_dir) |>
  dcast(target  + horizon ~ error_method, value.var = c("interval_score", "coverage_50", "coverage_80"))


kable(allscores, format = "latex", booktabs = TRUE)

