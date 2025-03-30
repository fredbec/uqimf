library(data.table)
library(knitr)
library(kableExtra)

prefix <- "extcntry_"


ciscores <- fread(here("scores", paste0(prefix, "ci_scores_avgcnt_ho.csv")))|>
  .d(, dev_50 := 100*coverage_50 - 50) |>
  .d(, dev_80 := 100*coverage_80 - 80) |>
  .d(, c("model", "target", "horizon", "interval_score", "unweighted_interval_score", "interval_score_50", "interval_score_80", "dev_50", "dev_80")) |>
  .d(, target := ifelse(target == "ngdp_rpch", "GDP", "CPI"))

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

crps_qus <- crps |>
  data.table::copy() |>
  .d(, model := gsub("_", "-", model)) |>
  .d(model %in% unique(ciscores2$model)) |>
  .d(ciscores2, on = c("target", "horizon", "model"))  |>
  .d(, model := gsub("_", "-", model))

scoredat <- rbind(crps_qus, crps_base)

create_latex_table2 <- function(dat, tgt){

  round_cols_new <- c("CRPS", "(Weighted) IS", "Unweighted IS", "IS 50", "IS 80")#,
  #"Deviation 50","Deviation 80")

  round_cols_newer <- c("$\\text{CRPS}$", "$\\text{IS}_{W}$", "$\\text{IS}_{U}$",
                        "$\\text{IS}_{50}$", "$\\text{IS}_{80}$")#,
  #"$\\text{Dev}_{50}$", "$\\text{Dev}_{80}$")

  singletab <- lapply(dat, function(dt){

    curr_hor <- dt$horizon
    curr_tgt <- dt$target

    round_cols <- c("crps", "interval_score", "unweighted_interval_score", "interval_score_50", "interval_score_80")#, "dev_50", "dev_80")
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
                                          fifelse(model == "bvar-const-direct", "Direct: BVAR-CP",
                                                  fifelse(model == "bvar-qu-direct", "Direct: BVAR-SV",
                                                          fifelse(model == "ar", "AR",
                                                                  fifelse(model == "ar-bic", "AR-BIC",
                                                                          fifelse(model == "bvar", "BVAR-SV",
                                                                                  fifelse(model == "bvar-const", "BVAR-CP",
                                                                                          fifelse(model == "arx-annual-direct", "Direct: ARX-annual",
                                                                                                  fifelse(model == "mean-ensemble", "AAEnsemble",
                                                                                                          fifelse(model == "IMF", "AAAIMF", model))))))))))))) |>
    .d(order(horizon, model)) |>
    .d(, model := fifelse(model == "AAAIMF", "IMF",
                          fifelse(model == "AAEnsemble", "Ensemble", model))) |>
    .d(, horizon := as.character(horizon))




  singletab <- singletab[1:nrow(singletab), horizon := ""] |>
    setnames(paste0(round_cols_newer, "_GDP"), paste0(round_cols_newer, "-g"), skip_absent = TRUE) |>
    setnames(paste0(round_cols_newer, "_CPI"), paste0(round_cols_newer, "-c"), skip_absent = TRUE) |>
    setnames("horizon", "")  |>
    setnames("model", "")

  dt_latex <- kable(singletab, format = "latex", escape = FALSE, booktabs = TRUE, linesep = c('','', '\\addlinespace', '','', '\\addlinespace','','','\\addlinespace','',''),
                    caption = "Interval Scores with Minimums Highlighted") %>%
    kable_styling(latex_options = c("hold_position"))


  dt_latex <- dt_latex |>
    row_spec(1, extra_latex = "\\multirow{3}{*}{Fall, Current}") |>
    row_spec(4, extra_latex = "\\multirow{3}{*}{Spring, Current}") |>
    row_spec(7, extra_latex = "\\multirow{3}{*}{Fall, Next}") |>
    row_spec(10, extra_latex = "\\multirow{3}{*}{Spring, Next}")
  # row_spec(23, extra_latex = "\\parbox[t]{2mm}{\\multirow{11}{*}{\\rotatebox[origin=c]{90}{\\hspace{5mm}Fall, Next}}}") |>
  # row_spec(34, extra_latex = "\\parbox[t]{2mm}{\\multirow{11}{*}{\\rotatebox[origin=c]{90}{\\hspace{5mm}Spring, Next}}}")



  return(dt_latex)
}




# Generate and return the full LaTeX table with bold highlights
#kable(dt, format = "latex", escape = FALSE, booktabs = TRUE,
#      caption = "Interval Scores with Minimums Highlighted") %>%
#  kable_styling(latex_options = c("hold_position"))
varvar <- "GDP"
scoredat1 <- scoredat |>
  copy() |>
  .d(target == varvar) |>
  split(by = c("horizon", "target"))

myvals <- create_latex_table2(scoredat1, varvar)
myvals
