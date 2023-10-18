library(data.table)
library(here)
library(ggplot2)
library(MetBrewer)
devtools::load_all()

.d <- `[`

trgt <- "pcpi_pch"
cis <- c(0.5, 0.8)
qus <- c(0.1, 0.25, 0.75, 0.9)

qufcs <- fread(here("quantile_forecasts", "quantile_forecasts.csv"))

linerange_dat <- qufcs |>
  .d(source == "IMF") |>
  .d(error_method == "absolute") |>
  .d(method == "rolling window") |>
  .d(target == trgt) |>
  .d(quantile %in% qus) |>
  .d(,quantile := paste0("quantile", quantile)) |>
  .d(,fltr := target_year - horizon) |>
  .d(fltr == 2021.5) |>
  .d(, .(country, target, target_year, horizon, quantile, prediction)) |>
  dcast(country + target + target_year + horizon ~ quantile, value.var = "prediction")


point_forecasts <- fread(here("data", "point_forecasts.csv")) |>
  .d(source == "IMF") |>
  .d(target == trgt) |>
  .d(,fltr := target_year - horizon) |>
  .d(fltr == 2021.5) |>
  .d(, .(country, target, target_year, prediction))


realized_vals <- qufcs |>
  .d(target_year > 2013) |>
  .d(target == trgt) |>
  .d(, .(country, target, target_year, true_value)) |>
  .d(!is.na(true_value)) |>
  unique()


dashed_line <- rbind(point_forecasts, realized_vals |> copy() |> setnames("true_value", "prediction") |> .d(target_year > 2020))


###########################################################################
cols <- paste0("quantile", qus)

labeldat_2022 <- linerange_dat |>
  .d(target_year == 2022) |>
  .d(, x := 2016.25) |>
  .d(, y := 8.2) |>
  .d(, (cols) := lapply(.SD, function(val) as.character(round(val, 1))), .SDcols = cols) |>
  .d(, (cols) := lapply(.SD, function(val) ifelse(grepl("[.]", val), val, paste0(val, ".0"))), .SDcols = cols) |>
  .d(, label := paste0("2022\n", "50% PI: ", quantile0.25, " - ", quantile0.75, "\n",
                       "80% PI: ", quantile0.1, " - ", quantile0.9))

labeldat_2023 <- linerange_dat |>
  .d(target_year == 2023) |>
  .d(, x := 2016.75) |>
  .d(, y := 5.0) |>
  .d(, (cols) := lapply(.SD, function(val) as.character(round(val, 1))), .SDcols = cols) |>
  .d(, (cols) := lapply(.SD, function(val) ifelse(grepl("[.]", val), val, paste0(val, ".0"))), .SDcols = cols) |>
  .d(, label := paste0("2023\n", "50% PI: ", quantile0.25, " - ", quantile0.75, "\n",
                       "80% PI: ", quantile0.1, " - ", quantile0.9))
###########################################################################

colors <- met.brewer("Hokusai1", 7)
names(colors) <- unique(qufcs$country)


qus_list <- qu_lvls(cis)

ggplot() +
  geom_line(
    aes(x = target_year, y = true_value,
        group = country, color = country),
    data = realized_vals,
    lwd = 0.75) +
  geom_point(
    aes(x = target_year, y = true_value,
        group = country, color = country),
    data = realized_vals,
    size = 0.95) +
  ggtitle(paste0("Actual Series, with forecast for year ", 2023)) +
  ylab("True value") +
  ylim(-0.5, 9.5) +

  facet_wrap(~country,
             labeller = as_labeller(plot_country_label()),
             nrow = 2, ncol = 4) +
  xlab("Target Year") +

  scale_color_met_d("Hokusai1") +
  theme_uqimf() %+replace%
  theme(legend.position = "none")+

  lapply(qus_list, function(qupr){
    geom_linerange(
      aes(x = target_year,
          ymin = get(paste0("quantile", qupr[1])),
          ymax = get(paste0("quantile", qupr[2])),
          color = country),
      data = linerange_dat,
      lwd = 2.15, alpha = qupr[3], show.legend = TRUE)
  }) +

  geom_point(
    aes(x = target_year, y = prediction, color = country),
    data = point_forecasts,
    size = 2.5
  ) +

  geom_line(
    aes(x = target_year, y = prediction, color = country),
    data = dashed_line,
    linetype = "dashed"
  ) +
  geom_label(data=labeldat_2022, aes(x=x, y=y, label=label, color = country),
             size=3.25 , angle=45, fontface="bold") +
  geom_label(data=labeldat_2023, aes(x=x, y=y, label=label, color = country),
             size=3.25 , angle=45, fontface="bold")

