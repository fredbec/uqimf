#why do directional errors give lower coverage?

library(here)
library(data.table)
library(ggplot2)
library(MetBrewer)

devtools::load_all()

.d <- `[`

cnt <- "CAN"
trgtyr <- 2010


qus <- ci_to_quantiles(as.numeric(c(.5,.8)), "directional") #always directional

n_qus <- length(qus)

no_pairs <- floor(n_qus/2)


alpha_vals <- seq(0.5, 0.75, length.out = no_pairs)

#inner and outer quantile levels
qu_lvls <- lapply(seq_along(1:no_pairs),
       function(ind) c(qus[ind], qus[(n_qus)-(ind-1)], alpha_vals[ind]))




sub_weodat <- data.table::fread(here("WEOforecasts_tidy.csv")) |>
  .d(g7 == 1) |>
  .d(horizon < 2)

fcdat <- data.table::fread(here("quantile_forecasts", "quantile_forecasts.csv"))

fcdat <- bvarq



pldat <- fcdat |>
  .d(source == "IMF") |>
  .d(method == "leave-one-out") |>
  .d(,.(country, target, horizon, quantile,prediction, target_year, error_method)) |>
  .d(, quantile := paste0("quant", quantile)) |>
  data.table::dcast(country + target + horizon + error_method + target_year ~ quantile,
                    value.var = "prediction")


fctodis <- fcdat |>
  .d(target_year == trgtyr) |>
  .d(country == cnt) |>
  .d(source == "IMF") |>
  .d(horizon < 2)


year_range <- unique(fcdat$target_year)

ggplot() +
  geom_line(
    aes(x = target_year, y = get(paste0("tv_", 1)),
        group = country, color = country),
    data = sub_weodat |>
      .d(ISOAlpha_3Code == cnt) |>
      .d(target_year %in% year_range)) +
  ggtitle(paste0("Actual Series, with forecast for year ", trgtyr)) +
  ylab("True value") +

  lapply(qu_lvls, function(qupr){
    geom_linerange(
      aes(x = target_year,
          ymin = get(paste0("quant", qupr[1])),
          ymax = get(paste0("quant", qupr[2])),
          color = country),
      data = pldat |>
        .d(country == cnt) |>
        .d(target_year == trgtyr),
      lwd = 1.5, alpha = qupr[3], show.legend = TRUE)
  }) +

  geom_point(
    aes(x = target_year, y = imf_pp, color = country),
    data = fctodis
  ) +

  scale_color_met_d("Monet") +

  facet_wrap(~horizon + target + error_method) +

  xlab("Target Year") +

  theme_uqimf()
