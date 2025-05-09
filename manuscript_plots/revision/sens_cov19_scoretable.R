library(data.table)
library(knitr)
library(kableExtra)
library(here)
.d <- `[`

prefix <- ""


ciscores <- fread(here("manuscript_plots", "revision", "results", "cov19_scores", paste0(prefix, "ci_scores_avgcnt_ho.csv"))) |>
  .d(, dev_50 := 100*coverage_50 - 50) |>
  .d(, dev_80 := 100*coverage_80 - 80) |>
  .d(, c("model", "target", "horizon", "interval_score", "unweighted_interval_score", "interval_score_50", "interval_score_80", "dev_50", "dev_80")) |>
  .d(, target := ifelse(target == "ngdp_rpch", "GDP", "CPI")) |>
  .d(model %in% c("IMF", "bvar_const", "ar")) |>
  .d(, model := gsub("_", "-", model))



ciscores2 <- fread(here("manuscript_plots", "revision", "results", "cov19_scores", paste0(prefix, "bvar_ci_scores_avgcnt_ho.csv"))) |>
  .d(, dev_50 := 100*coverage_50 - 50) |>
  .d(, dev_80 := 100*coverage_80 - 80) |>
  .d(, c("model", "target", "horizon", "interval_score", "unweighted_interval_score", "interval_score_50", "interval_score_80", "dev_50", "dev_80")) |>
  .d(, target := ifelse(target == "ngdp_rpch", "GDP", "CPI")) |>
  .d(, model := gsub("_", "-", model)) |>
  .d(, model := paste0(model, "-direct")) |>
  .d(model %in% c("bvar-const-direct"))#, "ar-direct"))

scoredat <- rbind(ciscores, ciscores2)


create_latex_table2 <- function(dat, tgt){

  round_cols_new <- c("(Weighted) IS", "Unweighted IS", "IS 50", "IS 80")#,
  #"Deviation 50","Deviation 80")

  round_cols_newer <- c("$\\text{IS}_{W,b}^{1)}$", "$\\text{IS}_{U}^{1)}$",
                        "$\\text{IS}_{50}^{1)}$", "$\\text{IS}_{80}^{1)}$",
                        "$\\text{Dev}_{50}^{2)}$", "$\\text{Dev}_{80}^{2)}$")#,
  #"$\\text{Dev}_{50}$", "$\\text{Dev}_{80}$")

  singletab <- lapply(dat, function(dt){

    curr_hor <- dt$horizon
    curr_tgt <- dt$target

    round_cols <- c("interval_score", "unweighted_interval_score", "interval_score_50", "interval_score_80", "dev_50", "dev_80")
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

  if(tgt == "CPI"){
    singletab <- singletab |>
      .d(, .SD, .SDcols = c("horizon", "model", paste0(round_cols_newer, "_CPI")))
  } else {
    singletab <- singletab |>
      .d(, .SD, .SDcols = c("horizon", "model",
                            paste0(round_cols_newer, "_GDP")))
  }
  singletab <- singletab |>
    .d(order(horizon)) |>
    .d(, model := fifelse(model == "ar-direct", "Direct: AR",
                          fifelse(model == "ar-annual-direct", "Direct: AR-annual",
                                  fifelse(model == "ar-bic-direct", "Direct: AR-BIC",
                                          fifelse(model == "bvar-const-direct", "Direct: BVAR",
                                                  fifelse(model == "bvar-qu-direct", "Direct: BVAR-SV",
                                                          fifelse(model == "ar", "AR",
                                                                  fifelse(model == "ar-bic", "AR-BIC",
                                                                          fifelse(model == "bvar", "BVAR-SV",
                                                                                  fifelse(model == "bvar-const", "BVAR",
                                                                                          fifelse(model == "arx-annual-direct", "Direct: ARX-annual",
                                                                                                  fifelse(model == "mean-ensemble", "AAEnsemble",
                                                                                                          fifelse(model == "IMF", "AAAIMF", model))))))))))))) |>
    .d(order(horizon, model)) |>
    .d(, model := fifelse(model == "AAAIMF", "IMF",
                          fifelse(model == "AAEnsemble", "Ensemble", model))) |>
    .d(, horizon := as.character(horizon))




  singletab <- singletab[1:nrow(singletab), horizon := ""] |>
    setnames(paste0(round_cols_newer, "_GDP"), paste0(round_cols_newer, "-gg"), skip_absent = TRUE) |>
    setnames(paste0(round_cols_newer, "_CPI"), paste0(round_cols_newer, "-cc"), skip_absent = TRUE) |>
    setnames("horizon", "")  |>
    setnames("model", "")

  cpon <- ifelse(tgt == "CPI",
                 "Scores for Target Years 2020 - 2022 (COVID period), Inflation",
                 "Scores for Target Years 2020 - 2022 (COVID period), GDP Growth")

  dt_latex <- kable(singletab, format = "latex", escape = FALSE, booktabs = TRUE, linesep = c('','', '', '\\addlinespace', '','','',  '\\addlinespace','','','', '\\addlinespace','',''),
                    caption = cpon) %>%
    kable_styling(latex_options = c("hold_position"))


  dt_latex <- dt_latex |>
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



  return(dt_latex)
}
#  kable_styling(latex_options = c("hold_position"))
scoredat_cpi <- scoredat |>
  copy() |>
  .d(target == "CPI") |>
  split(by = c("horizon", "target"))

table_cpi <- create_latex_table2(scoredat_cpi, "CPI")

scoredat_gdp <- scoredat |>
  copy() |>
  .d(target == "GDP") |>
  split(by = c("horizon", "target"))

table_gdp <- create_latex_table2(scoredat_gdp, "GDP")

writeLines(table_cpi, (here("manuscript_plots", "revision", "results", "sens_cov19_cpi.tex")))
writeLines(table_gdp, (here("manuscript_plots", "revision", "results", "sens_cov19_gdp.tex")))

