library(data.table)
library(here)
library(ggplot2)
suffix <- "_directional"

qudat <- fread(here("quantile_forecasts", paste0("quantile_forecasts", suffix, ".csv"))) |>
  .d(target_year %in% c(2008:2010) & horizon == 1 & source == "IMF" & target == "ngdp_rpch") |>
  .d(, error := -1*error) |>
  .d(, countrynum := (as.numeric(factor(country))-8)*(-1)) |>
  .d(, plotpos := countrynum + (target_year - 2009) * -0.175)

linerangedat50 <- qudat |>
  .d(, c("country", "target_year", "plotpos", "error_prediction", "quantile")) |>
  .d(quantile %in% c(0.25, 0.75)) |>
  .d(, quantile := paste0("q", 100*quantile)) |>
  dcast(country + plotpos + target_year~ quantile, value.var = "error_prediction") |>
  .d(, flag := q75 > abs(q25))

linerangedat80 <- qudat |>
  .d(, c("country", "target_year", "plotpos", "error_prediction", "quantile")) |>
  .d(quantile %in% c(0.1, 0.9)) |>
  .d(, quantile := paste0("q", 100*quantile)) |>
  dcast(country + target_year +plotpos~ quantile, value.var = "error_prediction") |>
  .d(, flag := q90 > abs(q10))

ggplot() +
  geom_point(aes(x = error, y = plotpos, color = country),
             data = qudat, size = 2.5) +
  scale_x_continuous(lim = c(-6,6)) +
  geom_linerange(aes(xmin = q25, xmax = q75, y = plotpos,
                     color = country),
                 data = linerangedat50, lwd = 2.25, alpha = 0.8) +
  geom_linerange(aes(xmin = q10, xmax = q90, y = plotpos,
                     color = country),
                 data = linerangedat80, alpha = 0.4, lwd = 2.25) +
  theme_uqimf() %+replace%
  theme(
    axis.ticks.y = element_blank(),  # Removes y-axis tick marks
    axis.text.y = element_blank()    # Removes y-axis tick labels
  ) +
  scale_color_met_d("Hokusai1") +
  xlab("Forecast Error") +
  ylab("") +
  guides(color = "none") +
  annotate("label", x = 5, y = 1, label = "USA", size = 4, fill = "white", color = "black",family = "serif") + #ypos_lab for hor 1
  annotate("label", x = 5, y = 2, label = "Japan", sie = 4, fill = "white", color = "black",family = "serif")+
  annotate("label", x = 5, y = 3, label = "Italy", sie = 4, fill = "white", color = "black",family = "serif")+
  annotate("label", x = 4.9, y = 4, label = "United Kingdom", sie = 4, fill = "white", color = "black",family = "serif")+
  annotate("label", x = 5, y = 5, label = "France", sie = 4, fill = "white", color = "black",family = "serif")+
  annotate("label", x = 5, y = 6, label = "Germany", sie = 4, fill = "white", color = "black",family = "serif")+
  annotate("label", x = 5, y = 7, label = "Canada", sie = 4, fill = "white", color = "black",family = "serif")+
  annotate("text", x = -6, y = 7.175, label = "2008", size = 3, fill = "white", color = "black",family = "serif")+
  annotate("text", x = -6, y = 7, label = "2009", size = 3, fill = "white", color = "black",family = "serif")+
  annotate("text", x = -6, y = 6.825, label = "2010", size = 3, fill = "white", color = "black",family = "serif")+
annotate("text", x = -6, y = 6.175, label = "2008", size = 3, fill = "white", color = "black",family = "serif")+
  annotate("text", x = -6, y = 6, label = "2009", size = 3, fill = "white", color = "black",family = "serif")+
  annotate("text", x = -6, y = 5.825, label = "2010", size = 3, fill = "white", color = "black",family = "serif")+
  annotate("text", x = -6, y = 5.175, label = "2008", size = 3, fill = "white", color = "black",family = "serif")+
  annotate("text", x = -6, y = 5, label = "2009", size = 3, fill = "white", color = "black",family = "serif")+
  annotate("text", x = -6, y = 4.825, label = "2010", size = 3, fill = "white", color = "black",family = "serif")+
  annotate("text", x = -6, y = 4.175, label = "2008", size = 3, fill = "white", color = "black",family = "serif")+
  annotate("text", x = -6, y = 4, label = "2009", size = 3, fill = "white", color = "black",family = "serif")+
  annotate("text", x = -6, y = 3.825, label = "2010", size = 3, fill = "white", color = "black",family = "serif")+
  annotate("text", x = -6, y = 3.175, label = "2008", size = 3, fill = "white", color = "black",family = "serif")+
  annotate("text", x = -6, y = 3, label = "2009", size = 3, fill = "white", color = "black",family = "serif")+
  annotate("text", x = -6, y = 2.825, label = "2010", size = 3, fill = "white", color = "black",family = "serif")+
  annotate("text", x = -6, y = 2.175, label = "2008", size = 3, fill = "white", color = "black",family = "serif")+
  annotate("text", x = -6, y = 2, label = "2009", size = 3, fill = "white", color = "black",family = "serif")+
  annotate("text", x = -6, y = 1.825, label = "2010", size = 3, fill = "white", color = "black",family = "serif")+
  annotate("text", x = -6, y = 1.175, label = "2008", size = 3, fill = "white", color = "black",family = "serif")+
  annotate("text", x = -6, y = 1, label = "2009", size = 3, fill = "white", color = "black",family = "serif")+
  annotate("text", x = -6, y = 0.825, label = "2010", size = 3, fill = "white", color = "black",family = "serif")

ggsave(here("manuscript_plots", "revision", "results", paste0("financialcrisis", suffix, ".pdf")), width = 5, height = 5)
