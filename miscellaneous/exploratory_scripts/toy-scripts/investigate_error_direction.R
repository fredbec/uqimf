
library(here)
library(data.table)
library(ggplot2)

library(patchwork)

.d <- `[`

qufcs <- data.table::fread(here("quantile_forecasts", "quantile_forecasts.csv"))
scores <- data.table::fread(here("quantile_forecasts", "ci_scores.csv"))

mydat <- qufcs |> .d(source == "IMF" & country == "USA" & horizon == 1.0 & quantile %in% c(0.1, 0.25, 0.75, 0.9)) |> .d(target == "pcpi_pch") |>
.d(method %in% c("rolling window", "leave-one-out"))


pldat <- mydat |>
  .d(method %in% c("rolling window", "leave-one-out")) |>
  .d(,.(country, target, horizon, quantile,prediction, target_year, error_method, method)) |>
  .d(, quantile := paste0("quant", quantile)) |>
  data.table::dcast(country + target + horizon + target_year + error_method + method~ quantile,
                    value.var = "prediction")


pldat0 <- mydat |>
  .d(quantile == 0.25) |>
  .d(error_method == "directional" & method == "leave-one-out")

qu_lvls <- list(
  c(0.1, 0.9, 0.25),
  c(0.25, 0.75, 0.5)
)

plot1 <- ggplot() +
  geom_line(aes(x = target_year, y = true_value), data = mydat |> .d(quantile == 0.25), color = "steelblue") +
  geom_point(aes(x = target_year, y = imf_pp), data = mydat |> .d(quantile == 0.25), color = "darkgreen") +
  lapply(qu_lvls, function(qupr){
    geom_linerange(
      aes(x = target_year,
          ymin = get(paste0("quant", qupr[1])),
          ymax = get(paste0("quant", qupr[2])),
          color = country),
      data = pldat,
      lwd = 1.5, alpha = qupr[3], show.legend = TRUE)
  }) +
  facet_wrap(~error_method + method)


transdat <- mydat |>
  .d(,error := ifelse(error_method == "absolute", abs(error), error))

plot2 <- ggplot() +
  geom_boxplot(aes(x = error_method, y = error), data = transdat)

plot0 <- ggplot() +
  geom_line(aes(x = target_year, y = error), data = pldat0) +
  geom_hline(aes(yintercept =0), linetype = "dashed")

ovrplt <-
  (plot2 + plot0) /
  (plot1) +
  plot_layout(guides = "collect",
              heights = c(1, 1)) &
  plot_annotation(tag_levels = 'I')  &
  theme(legend.position = 'bottom',
        legend.box="vertical", legend.margin=margin())

ovrplt



fildat <- scores |>
  copy() |>
  .d(model == "IMF") |>
  .d(error_method == "directional") |>
  .d(method %in% c("rolling window", "leave-one-out")) |>
  .d(,.(method, country, target, horizon, coverage_50, coverage_80)) |>
  dcast(country + target + horizon ~ method, value.var = c("coverage_50", "coverage_80")) |>
  setnames(c("coverage_50_leave-one-out", "coverage_80_leave-one-out", "coverage_50_rolling window", "coverage_80_rolling window"), c("c50loo", "c80loo", "c50rw", "c80rw")) |>
  .d(, val1 := c50loo - c50rw) |>
  .d(, val2 := c80loo - c80rw) |>
  .d(val1 == max(val1) | val2 == max(val2))




