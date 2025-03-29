library(data.table)
.d <- `[`
library(MetBrewer)
library(ggplot2)

mydat <- fread(here("scores", "extcntry_bvar_ci_scores_ho.csv")) |>
  .d(, c("model", "country", "target", "horizon", "interval_score"))

myfat <- fread(here("scores", "extcntry_ci_scores_ho.csv")) |>
  .d(, c("model", "country", "target", "horizon", "interval_score"))
weodat <- data.table::fread(here("data", "weodat.csv"))

allscores <- rbind(mydat, myfat) |>
  dcast(country+target+horizon~model, value.var = "interval_score") |>
  .d(, relscores := ar_annual / IMF) |>
  .d(relscores > 3)


truthdat <- weodat |>
  .d(country == "RUS" & target == "pcpi_pch") |>
  .d(horizon == 0.5) |>
  .d(target_year <= 2023)

ardat <- fread(here("benchmarks","raw","forecasts_2025","fcsts_ar_2025", "fcst_RUS_S_annual.csv")) |>
  .d(var == "cpi") |>
  .d(quantile_level == 0.5) |>
  .d(forecast_year == target_year)

mycols <- met.brewer("Hokusai3", 2)

ggplot() +
  geom_line(aes(x = target_year, y = tv_1), data = truthdat |> .d(target_year > 2012), color = "black") +
  geom_line(aes(x = target_year, y = prediction), data = truthdat |> .d(target_year > 2012), color = mycols[1]) +
  geom_line(aes(x = target_year, y = value), data = ardat, color = mycols[2]) +
  theme_uqimf()

ggplot() +
  geom_line(aes(x = target_year, y = tv_1), data = truthdat, color = "black") +
  geom_line(aes(x = target_year, y = prediction), data = truthdat, color = mycols[1]) +
  geom_line(aes(x = target_year, y = value), data = ardat, color = mycols[2]) +
  theme_uqimf()
