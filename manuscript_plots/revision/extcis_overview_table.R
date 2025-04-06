library(data.table)
library(knitr)
library(kableExtra)

prefix <- "extcis_"


ciscores <- fread(here("scores", paste0(prefix, "ci_scores_avgcnt_ho.csv")))|>
  .d(, dev_50 := 100*coverage_50 - 50) |>
  .d(, dev_80 := 100*coverage_80 - 80) |>
  .d(, c("model", "target", "horizon", "interval_score", "unweighted_interval_score", "interval_score_50", "interval_score_80", "dev_50", "dev_80")) |>
  .d(, target := ifelse(target == "ngdp_rpch", "GDP", "CPI"))
ciscores_base <- fread(here("scores", paste0("ci_scores_avgcnt_ho.csv")))|>
  .d(, c("model", "target", "horizon", "interval_score")) |>
  setnames("interval_score", "base_interval_score") |>
  .d(, target := ifelse(target == "ngdp_rpch", "GDP", "CPI"))
ciss_fakescpres <- ciscores |>
  .d(model == "IMF") |>
  .d(,model := "bvar_ciss") |>
  .d(, c("interval_score", "unweighted_interval_score", "interval_score_50", "interval_score_80", "dev_50", "dev_80") := NA)
ciscores <- rbind(ciscores, ciss_fakescpres)
ciscores_ciss <- fread(here("scores", "_bvarspecs", paste0("ci_scores_avgcnt_ho.csv")))|>
  .d(, dev_50 := 100*coverage_50 - 50) |>
  .d(, dev_80 := 100*coverage_80 - 80) |>
  .d(, c("model", "target", "horizon", "interval_score")) |>
  setnames("interval_score", "ciss_interval_score") |>
  .d(, target := ifelse(target == "ngdp_rpch", "GDP", "CPI"))

ciscores <- merge(ciscores, ciscores_base, by = c("model", "target", "horizon"), all.x = TRUE) |>
  merge(ciscores_ciss,  by = c("model", "target", "horizon"), all.x = TRUE)

crps <- fread(here("scores", paste0(prefix, "crps_values_ho.csv"))) |>
  setnames("source", "model") |>
  setnames("score", "crps") |>
  .d(, c("target", "horizon", "model", "crps")) |>
  .d(, target := ifelse(target == "ngdp_rpch", "GDP", "CPI"))

crps_base <- crps |>
  data.table::copy() |>
  .d(model %in% unique(ciscores$model)) |>
  .d(ciscores , on = c("target", "horizon", "model"))  |>
  .d(, model := gsub("_", "-", model))


ciscores2 <- fread(here("scores", paste0(prefix, "bvar_ci_scores_avgcnt_ho.csv"))) |>
  .d(, dev_50 := 100*coverage_50 - 50) |>
  .d(, dev_80 := 100*coverage_80 - 80) |>
  .d(, c("model", "target", "horizon", "interval_score", "unweighted_interval_score", "interval_score_50", "interval_score_80", "dev_50", "dev_80")) |>
  .d(, target := ifelse(target == "ngdp_rpch", "GDP", "CPI")) |>
  .d(, model := gsub("_", "-", model)) |>
  .d(, model := paste0(model, "-direct"))
ciss_fakescpres2 <- ciscores2 |>
  .d(model == "ar-bic-direct") |>
  .d(,model := "bvar-ciss-direct") |>
  .d(, c("interval_score", "unweighted_interval_score", "interval_score_50", "interval_score_80", "dev_50", "dev_80") := NA)
ciscores2 <- rbind(ciscores2, ciss_fakescpres2)
ciscores2_base <- fread(here("scores", paste0("bvar_ci_scores_avgcnt_ho.csv"))) |>
  .d(, c("model", "target", "horizon", "interval_score")) |>
  setnames("interval_score", "base_interval_score") |>
  .d(, target := ifelse(target == "ngdp_rpch", "GDP", "CPI")) |>
  .d(, model := gsub("_", "-", model)) |>
  .d(, model := paste0(model, "-direct"))
ciscores2_ciss <- fread(here("scores","_bvarspecs", paste0("bvar_ci_scores_avgcnt_ho.csv"))) |>
  .d(, c("model", "target", "horizon", "interval_score")) |>
  setnames("interval_score", "ciss_interval_score") |>
  .d(, target := ifelse(target == "ngdp_rpch", "GDP", "CPI")) |>
  .d(, model := gsub("_", "-", model)) |>
  .d(, model := paste0(model, "-direct"))

ciscores2 <- merge(ciscores2, ciscores2_base, by = c("model", "target", "horizon"),all.x = TRUE)|>
  merge(ciscores2_ciss,  by = c("model", "target", "horizon"), all.x = TRUE)

crps_qus <- crps |>
  data.table::copy() |>
  .d(, model := gsub("_", "-", model)) |>
  .d(model %in% unique(ciscores2$model)) |>
  .d(ciscores2, on = c("target", "horizon", "model"))  |>
  .d(, model := gsub("_", "-", model))

scoredat <- rbind(crps_qus, crps_base)

create_latex_table2 <- function(dat, mrow){

  #round_cols_new <- c("CRPS", "(Weighted) IS",  "Unweighted IS", "IS 50", "IS 80")#,
  #"Deviation 50","Deviation 80")

  round_cols_newer <- c("$\\text{CRPS}^{1)}$", "$\\text{IS}_{W}^{2)}$", "$\\text{IS}_{W,b}^{2)}$", "$\\text{IS}_{U}^{2)}$",
                        "$\\text{IS}_{50}^{2)}$", "$\\text{IS}_{80}^{2)}$", "$\\text{IS}_{\\text{CISS}}^{2)}$")#,
  #"$\\text{Dev}_{50}$", "$\\text{Dev}_{80}$")

  singletab <- lapply(dat, function(dt){

    curr_hor <- dt$horizon
    curr_tgt <- dt$target

    round_cols <- c("crps", "interval_score", "base_interval_score", "unweighted_interval_score", "interval_score_50", "interval_score_80", "ciss_interval_score")#, "dev_50", "dev_80")
    #round_cols_extra <- c("dev_50", "dev_80")

    dt[, (round_cols) := lapply(.SD, round, 3), .SDcols = round_cols]
    #dt[, (round_cols_extra) := lapply(.SD, round, 2), .SDcols = round_cols_extra]

    # Compute minimum values per horizon, with special handling for deviation
    min_vals <- dt[, lapply(.SD, function(x) min(abs(x), na.rm = TRUE)), by = horizon, .SDcols = round_cols]
    setnames(min_vals, old = round_cols, new = paste0("min_", round_cols))  # Rename min columns

    # Merge with original dt
    dt <- merge(dt, min_vals, by = "horizon", all.x = TRUE)

    # Apply bold formatting to the minimum values
    bold_format <- function(x, min_x) ifelse(abs(x) == min_x, paste0("\\textbf{", x, "}"), as.character(x))
    dt[, (round_cols) := Map(bold_format, .SD, min_vals[, .SD, .SDcols = paste0("min_", round_cols)]), .SDcols = round_cols]

    # Drop unnecessary columns
    drop_cols <- c("horizon", paste0("min_", round_cols), "target")
    dt[, (drop_cols) := NULL]


    dt <- dt |>
      .d(, target := curr_tgt) |>
      .d(, horizon := curr_hor) |>
      setnames(round_cols, round_cols_newer)

  }
  )|>
    rbindlist() |>
    dcast(model + horizon ~ target, value.var = round_cols_newer)

  singletab <- singletab |>
    .d(, .SD, .SDcols = c("horizon", "model", paste0(round_cols_newer, "_CPI"),
                          paste0(round_cols_newer, "_GDP"))) |>
    .d(order(horizon)) |>
    .d(model != "bvar") |> #exclude original bvar model due to stability issues
    .d(model != "bvar-qu-direct") |>
    .d(model != "bvar-ciss") |>
    .d(, model := fifelse(model == "ar-direct", "Direct$^{4)}$: AR(1)",
                          fifelse(model == "ar-annual-direct", "Direct$^{4)}$: AR-annual",
                                  fifelse(model == "ar-bic-direct", "Direct$^{4)}$: AR(p)",
                                          fifelse(model == "bvar-const-direct", "Direct$^{4)}$: BVAR$^{3)}$",
                                                  fifelse(model == "bvar-ciss-direct", "Direct$^{4)}$: BVAR-CISS$^{3)}$",
                                                          fifelse(model == "ar", "AR(1)",
                                                                  fifelse(model == "arx-annual-direct", "Direct$^{4)}$: ARX-annual",
                                                                          fifelse(model == "ar-bic", "AR(p)",
                                                                                  fifelse(model == "bvar-ciss", "BVAR-CISS",
                                                                                          fifelse(model == "bvar-const", "BVAR$^{3)}$",
                                                                                                  fifelse(model == "bvar-mix", "BVAR-Mix$^{3)}$",
                                                                                                          fifelse(model == "bvar-mix-direct", "Direct$^{4)}$: BVAR-Mix$^{3)}$",
                                                                                                                  fifelse(model == "mean-ensemble", "ZEnsemble",
                                                                                                                          fifelse(model == "IMF", "AAAIMF", model))))))))))))))) |>
    .d(order(horizon, model)) |>
    .d(, model := fifelse(model == "AAAIMF", "IMF",
                          fifelse(model == "ZEnsemble", "Simple Ensemble$^{5)}$", model))) |>
    .d(, horizon := as.character(horizon))


  maxr <- nrow(singletab)

  singletab <- singletab[1:maxr, horizon := ""] |>
    setnames(paste0(round_cols_newer, "_GDP"), paste0(round_cols_newer, "-cc")) |>
    setnames(paste0(round_cols_newer, "_CPI"), paste0(round_cols_newer, "-gg")) |>
    setnames("horizon", "")  |>
    setnames("model", "")

  #make NA's look cleaner in final table
  singletab[is.na(singletab)] <- "--"

  dt_latex <- kable(singletab, format = "latex", escape = FALSE, booktabs = TRUE, linesep = c('', '', '', '','','','','','','','','', '\\addlinespace\\addlinespace'),
                    caption = "Interval Scores with Minimums Highlighted") %>%
    kable_styling(latex_options = c("hold_position")) |>
    add_header_above(
      c(" " = 2, "{Inflation\\\\hspace*{15mm}}" = 7, "{GDP Growth}" = 7),
      escape = FALSE
    )
  dt_latex <- dt_latex |>
    row_spec(1, extra_latex = paste0("\\parbox[t]{2mm}{\\multirow{13}{*}{\\rotatebox[origin=c]{90}{\\hspace{5mm}Fall, ", mrow, "}}}")) |>
    row_spec(14, extra_latex = paste0("\\parbox[t]{2mm}{\\multirow{13}{*}{\\rotatebox[origin=c]{90}{\\hspace{5mm}Spring, ", mrow, "}}}"))# |>
  # row_spec(23, extra_latex = "\\parbox[t]{2mm}{\\multirow{11}{*}{\\rotatebox[origin=c]{90}{\\hspace{5mm}Fall, Next}}}") |>
  # row_spec(34, extra_latex = "\\parbox[t]{2mm}{\\multirow{11}{*}{\\rotatebox[origin=c]{90}{\\hspace{5mm}Spring, Next}}}")

  highlight_rows <- c(1, 14)
  for (i in highlight_rows) {
    dt_latex <- dt_latex %>%
      row_spec(i, background = "gray!35")  # Set the background color to grey (light grey)
  }
  highlight_rows <- c(13, maxr)
  for (i in highlight_rows) {
    dt_latex <- dt_latex %>%
      row_spec(i, background = "gray!15")  # Set the background color to grey (light grey)
  }

  return(dt_latex)
}




# Generate and return the full LaTeX table with bold highlights
#kable(dt, format = "latex", escape = FALSE, booktabs = TRUE,
#      caption = "Interval Scores with Minimums Highlighted") %>%
#  kable_styling(latex_options = c("hold_position"))
scoredat_current <- scoredat |>
  copy() |>
  .d(horizon < 1) |>
  split(by = c("horizon", "target"))
scoredat_next <- scoredat |>
  copy() |>
  .d(horizon >= 1) |>
  split(by = c("horizon", "target"))

table_current <- create_latex_table2(scoredat_current, mrow = "Current")
table_next <- create_latex_table2(scoredat_next, mrow = "Next")

writeLines(table_current, (here("manuscript_plots", "revision", "extcis_current.tex")))
writeLines(table_current, (here("..", "uqimf-manuscript", "tables", "extcis_current.tex")))

writeLines(table_next, (here("manuscript_plots", "revision", "extcis_next.tex")))
writeLines(table_next, (here("..", "uqimf-manuscript", "tables", "extcis_next.tex")))

