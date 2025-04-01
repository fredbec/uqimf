library(ggplot2)
library(MetBrewer)
library(data.table)
library(gghighlight)
library(patchwork)
library(here)
devtools::load_all()

.d <- `[`

set.seed(2922)

ctry <- "USA"
tgt <- "pcpi_pch"
yr <- 2001
hr <- 1
rw <- 11

qufcs_dir <- data.table::fread(here("quantile_forecasts",
                                    paste0("toscore", "", "_quantile_forecasts_directional.csv"))) |>
  .d(country == ctry & target == tgt & horizon == hr) |>
  .d(target_year == yr) |>
  .d(source == "IMF")

errordat2 <- fread(here("data", "weodat.csv")) |>
  .d(, fc_error := prediction - tv_1) |>
  .d(country == ctry & target == tgt & horizon == hr) |>
  .d(target_year %in% seq(yr-(rw+floor(hr)), yr+3, 1))

qufcs_dir <- data.table::fread(here("quantile_forecasts",
                                    paste0("toscore", "", "_quantile_forecasts_directional.csv"))) |>
  .d(country == ctry & target == tgt & horizon == hr) |>
  .d(target_year == yr) |>
  .d(source == "IMF")

getlinerangedat <- function(dat, xpos){
  val80_ur <- dat[quantile == 0.9, "prediction"] |> unname() |> unlist()
  val50_ur <- dat[quantile == 0.75, "prediction"]|> unname() |> unlist()
  val80_lwr <- dat[quantile == 0.1, "prediction"] |> unname() |> unlist()
  val50_lwr <- dat[quantile == 0.25, "prediction"]|> unname() |> unlist()
  linerangedat_lr <- data.frame(
    type = rep(c(xpos), each = 1),
    upper80 = c(val80_ur),
    lower80 = c(val80_lwr),
    upper50 = c(val50_ur),
    lower50 = c(val50_lwr)
  ) |>
    setDT()

  return(linerangedat_lr)
}

errordat3 <- errordat2 |> .d(target_year <= 2002)
errordat3$fc_error |> sort() |> quantile(c(0.1, 0.25, 0.75, 0.9))

lrdir <- getlinerangedat(qufcs_dir, 1)
ggplot() +
  geom_line(aes(x = target_year, y = prediction), color = "red", data = errordat2) +
  geom_line(aes(x = target_year, y = tv_1), color = "black", data = errordat2) +
  geom_linerange(
    aes(x = yr,
        ymin = lower80,
        ymax = upper80),
    color = "red",
    data = lrdir,
    lwd = 5,
    alpha = 0.5
  ) +
  geom_linerange(
    aes(x = yr,
        ymin = lower50,
        ymax = upper50),
    color = "red",
    data = lrdir,
    lwd = 5
  ) +
  theme_uqimf()
