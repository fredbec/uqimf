
####################################read in quantile forecasts########################
qufcs <- data.table::fread(here("quantile_forecasts", paste0("toscore", "_quantile_forecasts.csv")))

qufcs <- qufcs |>
  .d(,.(country, target, horizon, target_year, true_value, prediction, quantile, method, error_method, source)) |>
  setnames("source", "model") |>
  .d(country == "DEU" & target == "pcpi_pch" & horizon == 0 & target_year == 2008 & model == "IMF")

#qufcs$prediction <- c(1,1.5,2.5,3)
#qufcs$true_value <- 2.7

sc150 <- qufcs |>
  compute_is(50, by = c("country", "target", "horizon", "model"))

sc180 <-  qufcs |>
  compute_is(80, by = c("country", "target", "horizon", "model"))


diffmed <- abs(2.754203 - ((3.118599     -2.762137)/2 + 2.762137))
#diffmed2 <- abs(2.754203 -  ((3.225969-2.654767)/2 + 2.654767))

sc1 <- merge(sc150, sc180, by = c("country", "target", "horizon", "model")) |>
  .d(,iscore := 1/2 * (0.25*interval_score_50 + 0.1 * interval_score_80))

sc2 <-scoreempQu(qufcs, cvg_rg = c(50,80),
                 by = c("model", "country", "target", "horizon"))
sc2
