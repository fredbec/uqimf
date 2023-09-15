#why do directional errors give lower coverage?

library(here)
library(data.table)
library(ggplot2)

devtools::load_all()

.d <- `[`

cnt <- "ITA"
trgtyr <- 2010


qus <- ci_to_quantiles(as.numeric(c(.5,.8)), "directional") #always directional

n_qus <- length(qus)

no_pairs <- floor(n_qus/2)


alpha_vals <- seq(0.5, 0.75, length.out = no_pairs)

#inner and outer quantile levels
qu_lvls <- lapply(seq_along(1:no_pairs),
                  function(ind) c(qus[ind], qus[(n_qus)-(ind-1)], alpha_vals[ind]))


fcdat <- bvar_qus |>
  .d(!is.na(horizon)) |>
  .d(quantile %in% c(0.1, 0.25, 0.75, 0.9)) |>
  .d(target == "pcpi_pch")



pldat <- fcdat |>
  .d(,.(country, target, horizon, quantile,prediction, target_year)) |>
  .d(, quantile := paste0("quant", quantile)) |>
  data.table::dcast(country + target + horizon  + target_year ~ quantile,
                    value.var = "prediction")


fctodis <- fcdat |>
  .d(target_year == trgtyr) |>
  .d(country == cnt) |>
  .d(horizon < 2)


year_range <- unique(fcdat$target_year)

ggplot() +
  geom_line(
    aes(x = target_year, y = true_value,
        group = country, color = country),
    data = fcdat |>
      .d(country == cnt) |>
      .d(target_year %in% year_range)) +
  ggtitle(paste0("BVAR predictions, for ", cnt)) +
  ylab("True value") +

  lapply(qu_lvls, function(qupr){
    geom_linerange(
      aes(x = target_year,
          ymin = get(paste0("quant", qupr[1])),
          ymax = get(paste0("quant", qupr[2])),
          color = country),
      data = pldat |>
        .d(country == cnt) ,
      lwd = 1.5, alpha = qupr[3], show.legend = TRUE)
  }) +

  #geom_point(
  #  aes(x = target_year, y = imf_pp, color = country),
  #  data = fctodis
  #) +

  scale_color_met_d("Monet") +

  facet_wrap(~horizon) +

  xlab("Target Year") +

  theme_uqimf()
