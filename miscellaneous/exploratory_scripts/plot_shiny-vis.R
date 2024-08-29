library(data.table)
library(here)
library(ggplot2)
library(MetBrewer)
library(patchwork)
library(lubridate)
devtools::load_all()

.d <- `[`

trgt <- "pcpi_pch"
season <- "S"

cis <- c(0.5, 0.8)
qus <- c(0.1, 0.25, 0.75, 0.9)

horshift <- ifelse(season == "S", 0.5, 0)
cyear <- lubridate::year(Sys.Date())

qufcs <- fread(here("quantile_forecasts", "quantile_forecasts.csv"))

linerange_dat <- qufcs |>
  .d(source == "IMF") |>
  .d(error_method == "absolute") |>
  .d(method == "rolling window") |>
  .d(target == trgt) |>
  .d(quantile %in% qus) |>
  .d(,quantile := paste0("quantile", quantile)) |>
  .d(,fltr := target_year - horizon) |>
  .d(fltr == cyear - horshift) |>
  .d(, .(country, target, target_year, horizon, quantile, prediction)) |>
  dcast(country + target + target_year + horizon ~ quantile, value.var = "prediction")


point_fcs <- fread(here("data", "point_forecasts.csv")) |>
  .d(source == "IMF") |>
  .d(target == trgt) |>
  .d(,fltr := target_year - horizon) |>
  .d(fltr == cyear - horshift) |>
  .d(, .(country, target, target_year, prediction))


realized_vals <- qufcs |>
  .d(target_year > 2013) |>
  .d(target == trgt) |>
  .d(, .(country, target, target_year, true_value)) |>
  .d(!is.na(true_value)) |>
  unique()


dashed_line <- rbind(point_fcs, realized_vals |> copy() |> setnames("true_value", "prediction") |> .d(target_year > 2020))


###########################################################################
cols <- paste0("quantile", qus)

labeldat_2022 <- linerange_dat |>
  .d(target_year == cyear) |>
  .d(, x := 2016.25) |>
  .d(, y := 8.2) |>
  .d(, (cols) := lapply(.SD, function(val) as.character(round(val, 1))), .SDcols = cols) |>
  .d(, (cols) := lapply(.SD, function(val) ifelse(grepl("[.]", val), val, paste0(val, ".0"))), .SDcols = cols) |>
  .d(, label := paste0("2022\n", "50% PI: ", quantile0.25, " - ", quantile0.75, "\n",
                       "80% PI: ", quantile0.1, " - ", quantile0.9))

labeldat_2023 <- linerange_dat |>
  .d(target_year == cyear) |>
  .d(, x := 2016.75) |>
  .d(, y := 5.0) |>
  .d(, (cols) := lapply(.SD, function(val) as.character(round(val, 1))), .SDcols = cols) |>
  .d(, (cols) := lapply(.SD, function(val) ifelse(grepl("[.]", val), val, paste0(val, ".0"))), .SDcols = cols) |>
  .d(, label := paste0("2023\n", "50% PI: ", quantile0.25, " - ", quantile0.75, "\n",
                       "80% PI: ", quantile0.1, " - ", quantile0.9))
###########################################################################

colors <- met.brewer("Hokusai1", 7)
names(colors) <- unique(qufcs$country)


plotlist <- lapply(as.list(unique(qufcs$country)),
                   function(pltc) shinyplot(realized_series = realized_vals,
                                            linerange_data = linerange_dat,
                                            point_forecasts = point_fcs,
                                            future_realized = dashed_line,
                                            labeldat_list = list(labeldat_2022, labeldat_2023),
                                            plot_country =  pltc,
                                            colorscale = colors,
                                            cis = cis))


spacer <- 150
spacer2 <- 150

design <- "122
           345
           678"

text2 <- wrap_elements(grid::textGrob("Here we'll place some explanatory text and possibly also the legend"))

pdf(here("miscellaneous", "exploratory_scripts", "shiny-vis.pdf"), width =9, height = 9)
plotlist[[1]] + text2 + plotlist[[2]] + plotlist[[3]] + plotlist[[4]] + plotlist[[5]] + plotlist[[6]] + plotlist[[7]]+
  plot_layout(guides = "collect",
              design = design) &
  theme(legend.position='bottom')
dev.off()

