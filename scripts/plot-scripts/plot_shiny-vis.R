library(data.table)
library(here)
library(ggplot2)
library(MetBrewer)
library(patchwork)
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
shinyplot <- function(realized_series,
                      linerange_data,
                      labels_currentyear,
                      labels_nextyear,
                      plot_country,
                      colorscale){
  ggplot() +
    geom_line(
      aes(x = target_year, y = true_value),
      color = colorscale[plot_country],
      data = realized_vals |> .d(country == plot_country),
      lwd = 0.75) +
    geom_point(
      aes(x = target_year, y = true_value),
      color = colorscale[plot_country],
      data = realized_vals |> .d(country == plot_country),
      size = 0.95) +
    ggtitle(paste0("Actual Series, with forecast for year ", 2023)) +
    ylab(plot_target_label()[trgt]) +
    ylim(-0.5, 9.5) +
    xlab("Target Year") +
    ggtitle(plot_country_label()[plot_country]) +
    scale_color_met_d("Hokusai1") +
    theme_uqimf() %+replace%
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5,
                                    vjust = 3)) +

    lapply(qus_list, function(qupr){
      geom_linerange(
        aes(x = target_year,
            ymin = get(paste0("quantile", qupr[1])),
            ymax = get(paste0("quantile", qupr[2]))),
        color = colorscale[plot_country],
        data = linerange_dat |> .d(country == plot_country),
        lwd = 2.15, alpha = qupr[3], show.legend = TRUE)
    }) +

    geom_point(
      aes(x = target_year, y = prediction),
      color = colorscale[plot_country],,
      data = point_forecasts |> .d(country == plot_country),
      size = 2.5
    ) +

    geom_line(
      aes(x = target_year, y = prediction),
      color = colorscale[plot_country],
      data = dashed_line |> .d(country == plot_country),
      linetype = "dashed"
    ) +
    geom_label(data=labeldat_2022 |> .d(country == plot_country), aes(x=x, y=y, label=label),
               color = colorscale[plot_country],
               size=3.25 , angle=45, fontface="bold") +
    geom_label(data=labeldat_2023 |> .d(country == plot_country), aes(x=x, y=y, label=label),
               color = colorscale[plot_country],
               size=3.25 , angle=45, fontface="bold")

}

plotlist <- lapply(as.list(unique(qufcs$country)),
                   function(pltc) shinyplot(realized_vals, linerange_dat, labeldat_2022, labeldat_2023, pltc, colors))



design <- "AAAAAAAA
          #BBBBBB#
          #CCCCCC#"


wrap_elements(full = plotlist[[1]] + plotlist[[2]] + plotlist[[3]]) +
wrap_elements(plotlist[[4]] + plotlist[[5]]) +
  wrap_elements(plotlist[[6]]+ plotlist[[7]]) +
  plot_layout(design = design)


