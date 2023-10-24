library(data.table)
library(here)
library(ggplot2)
library(MetBrewer)
library(patchwork)
devtools::load_all()

.d <- `[`

trgt <- "inflation"
cis <- c(0.5, 0.8)
qus <- c(0.1, 0.25, 0.75, 0.9)

qufcs <- fread(here("test-macropi", "forecasts", "forecasts_Spring2022.csv"))

linerange_dat <- qufcs |>
  .d(target == trgt) |>
  .d(,quantile := paste0("quantile", quantile)) |>
  .d(,season_helper := ifelse(forecast_season == "S", 0.5, 0)) |>
  .d(,horizon := (target_year - forecast_year) + season_helper) |>
  .d(, .(country, target, target_year, horizon, quantile, prediction)) |>
  dcast(country + target + target_year + horizon ~ quantile, value.var = "prediction")

realized_vals <- fread(here("test-macropi", "extra-data", "historicvalues_Spring2022.csv"))|>
  .d(target == trgt)
point_fcs <- fread(here("test-macropi", "extra-data", "pointforecasts_Spring2022.csv"))|>
  .d(target == trgt)

dashed_line <- rbind(point_fcs, realized_vals |> copy() |> setnames("true_value", "prediction") |> .d(target_year > 2020))


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


plotlist <- lapply(as.list(unique(qufcs$country)),
                   function(pltc) shinyplot(realized_vals, linerange_dat, point_fcs, dashed_line, labeldat_2022, labeldat_2023, pltc, colors, cis) + ylab(trgt))


spacer <- 150
spacer2 <- 150

design <- "122
           345
           678"

text2 <- wrap_elements(grid::textGrob("Here we'll place some explanatory text and possibly also the legend"))

pdf(here("plots", "shiny-vis.pdf"), width =9, height = 9)
plotlist[[1]] + text2 + plotlist[[2]] + plotlist[[3]] + plotlist[[4]] + plotlist[[5]] + plotlist[[6]] + plotlist[[7]]+
  plot_layout(guides = "collect",
              design = design) &
  theme(legend.position='bottom')
dev.off()

