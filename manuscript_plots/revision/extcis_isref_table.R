library(data.table)
library(knitr)
library(kableExtra)

prefix <- "extcis_"


ciscores <- fread(here("scores", paste0(prefix, "ci_scores_avgcnt_ho_is.csv"))) |>
  .d(, dev_50 := 100*coverage_50 - 50) |>
  .d(, dev_80 := 100*coverage_80 - 80) |>
  .d(, c("model", "target", "horizon", "interval_score_30", "interval_score_50", "interval_score_80", "interval_score_90")) |>
  .d(, target := ifelse(target == "ngdp_rpch", "GDP", "CPI")) |>
  .d(model %in% c("IMF", "ar", "bvar_const")) |>
  .d(, model := gsub("_", "-", model))

ciscores2 <- fread(here("scores", paste0(prefix, "bvar_ci_scores_avgcnt_ho_is.csv"))) |>
  .d(, c("model", "target", "horizon", "interval_score_30", "interval_score_50", "interval_score_80", "interval_score_90")) |>
  .d(, target := ifelse(target == "ngdp_rpch", "GDP", "CPI")) |>
  .d(model == "bvar_const") |>
  .d(, model := gsub("_", "-", model)) |>
  .d(, model := paste0(model, "-direct"))

scoredat <- rbind(ciscores, ciscores2)

create_latex_table2 <- function(dat, mrow){

  #round_cols_new <- c("CRPS", "(Weighted) IS",  "Unweighted IS", "IS 50", "IS 80")#,
  #"Deviation 50","Deviation 80")

  round_cols_newer <- c("$\\text{IS}_{30}^{1)}$", "$\\text{IS}_{50}^{1)}$", "$\\text{IS}_{80}^{1)}$", "$\\text{IS}_{90}^{1)}$")#,
  #"$\\text{Dev}_{50}$", "$\\text{Dev}_{80}$")

  singletab <- lapply(dat, function(dt){

    curr_hor <- dt$horizon
    curr_tgt <- dt$target

    round_cols <- c("interval_score_30", "interval_score_50", "interval_score_80", "interval_score_90")#, "dev_50", "dev_80")
    #round_cols_extra <- c("dev_50", "dev_80")

    dt[, (round_cols) := lapply(.SD, round, 2), .SDcols = round_cols]
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
  ) |>
    rbindlist() |>
    dcast(model + horizon ~ target, value.var = round_cols_newer)

  singletab <- singletab |>
    .d(, .SD, .SDcols = c("horizon", "model", paste0(round_cols_newer, "_CPI"),
                          paste0(round_cols_newer, "_GDP"))) |>
    .d(order(horizon)) |>
    .d(model != "bvar") |> #exclude original bvar model due to stability issues
    .d(model != "bvar-qu-direct") |>
    .d(model != "bvar-ciss") |>
    .d(, model := fifelse(model == "ar-direct", "Direct$^{2)}$: AR",
                          fifelse(model == "ar-annual-direct", "Direct$^{2)}$: AR-annual",
                                  fifelse(model == "ar-bic-direct", "Direct$^{2)}$: AR(p)",
                                          fifelse(model == "bvar-const-direct", "Direct$^{2)}$: BVAR",
                                                  fifelse(model == "bvar-ciss-direct", "Direct$^{2)}$: BVAR-CISS",
                                                          fifelse(model == "ar", "AR",
                                                                  fifelse(model == "arx-annual-direct", "Direct$^{2)}$: ARX-annual",
                                                                          fifelse(model == "ar-bic", "AR(p)",
                                                                                  fifelse(model == "bvar-ciss", "BVAR-CISS",
                                                                                          fifelse(model == "bvar-const", "BVAR",
                                                                                                  fifelse(model == "bvar-mix", "BVAR-Mix",
                                                                                                          fifelse(model == "bvar-mix-direct", "Direct$^{2)}$: BVAR-Mix",
                                                                                                                  fifelse(model == "mean-ensemble", "ZEnsemble",
                                                                                                                          fifelse(model == "IMF", "AAAIMF", model))))))))))))))) |>
    .d(order(horizon, model)) |>
    .d(, model := fifelse(model == "AAAIMF", "IMF",
                          fifelse(model == "ZEnsemble", "Simple Ensemble$^{5)}$", model))) |>
    .d(, horizon := as.character(horizon))


  maxr <- nrow(singletab)

  singletab <- singletab[1:maxr, horizon := ""] |>
    setnames(paste0(round_cols_newer, "_CPI"), paste0(round_cols_newer, "-cc")) |>
    setnames(paste0(round_cols_newer, "_GDP"), paste0(round_cols_newer, "-gg")) |>
    setnames("horizon", "")  |>
    setnames("model", "")

  #make NA's look cleaner in final table
  singletab[is.na(singletab)] <- "--"

  dt_latex <- kable(singletab, format = "latex", escape = FALSE, booktabs = TRUE, linesep = c('', '', '', '\\addlinespace','', '', '', '\\addlinespace','', '', '', '\\addlinespace','', '', '')) %>%
    kable_styling(latex_options = c("hold_position")) |>
    add_header_above(
      c(" " = 2, "{Inflation}" = 4, "{GDP Growth}" = 4),
      escape = FALSE
    )
  dt_latex <- dt_latex  |>
    row_spec(1, extra_latex = paste0(
      "\\parbox[t]{2mm}{\\multirow{4}{*}{\\rotatebox[origin=c]{90}{\\parbox{2cm}{\\centering Fall,\\\\Current}}}}"
    )) |>
    row_spec(5, extra_latex = paste0(
      "\\parbox[t]{2mm}{\\multirow{4}{*}{\\rotatebox[origin=c]{90}{\\parbox{2cm}{\\centering Spring,\\\\Current}}}}"
    )) |>
    row_spec(9, extra_latex = paste0(
      "\\parbox[t]{2mm}{\\multirow{4}{*}{\\rotatebox[origin=c]{90}{\\parbox{2cm}{\\centering Fall,\\\\Next}}}}"
    )) |>
    row_spec(13, extra_latex = paste0(
      "\\parbox[t]{2mm}{\\multirow{4}{*}{\\rotatebox[origin=c]{90}{\\parbox{2cm}{\\centering Spring,\\\\Next}}}}"
    ))
  # row_spec(23, extra_latex = "\\parbox[t]{2mm}{\\multirow{11}{*}{\\rotatebox[origin=c]{90}{\\hspace{5mm}Fall, Next}}}") |>
  # row_spec(34, extra_latex = "\\parbox[t]{2mm}{\\multirow{11}{*}{\\rotatebox[origin=c]{90}{\\hspace{5mm}Spring, Next}}}")

  #highlight_rows <- c(1, 5, 9, 13)
  #for (i in highlight_rows) {
  #  dt_latex <- dt_latex %>%
  #    row_spec(i, background = "gray!35")  # Set the background color to grey (light grey)
  #}
  return(dt_latex)
}




# Generate and return the full LaTeX table with bold highlights
#kable(dt, format = "latex", escape = FALSE, booktabs = TRUE,
#      caption = "Interval Scores with Minimums Highlighted") %>%
#  kable_styling(latex_options = c("hold_position"))
scoredat_current <- scoredat |>
  copy() |>
  split(by = c("horizon", "target"))

table_current <- create_latex_table2(scoredat_current, mrow = "Current")
#table_next <- create_latex_table2(scoredat_next, mrow = "Next")

writeLines(table_current, (here("manuscript_plots", "revision", "results", "extcis_isscores.tex")))
writeLines(table_current, (here("..", "uqimf-manuscript", "tables", "extcis_isscores.tex")))

#writeLines(table_next, (here("manuscript_plots", "revision", "results", "extcis_next.tex")))
#writeLines(table_next, (here("..", "uqimf-manuscript", "tables", "extcis_next.tex")))

