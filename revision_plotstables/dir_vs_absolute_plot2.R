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
  .d(target_year %in% seq(yr-(rw+floor(hr)), yr+5, 1))

qufcs_dir <- data.table::fread(here("quantile_forecasts",
                                    paste0("toscore", "", "_quantile_forecasts_directional.csv"))) |>
  .d(country == ctry & target == tgt & horizon == hr) |>
  .d(target_year %in% yr:(yr+5)) |>
  .d(source == "IMF")

getlinerangedat2 <- function(dat, xpos){

  linerangedat_lr <- dat |>
    .d(, c("country", "target", "target_year", "quantile", "prediction")) |>
    .d(, quantile := paste0("qu", quantile)) |>
    dcast(country + target + target_year ~ quantile, value.var = "prediction")

  return(linerangedat_lr)
  #dat50 <- linerangedat_lr|>
  #  .d(quantile %in% c(0.25, 0.75))

  #dat80 <- linerangedat_lr|>
  #  .d(quantile %in% c(0.1, 0.9))


  #return(list(dat50 = dat50, dat80=dat80))
}

errordat3 <- errordat2 |> .d(target_year <= 2002)
errordat3$fc_error |> sort() |> quantile(c(0.1, 0.25, 0.75, 0.9))

lrdir <- getlinerangedat2(qufcs_dir, 1)
plot2 <- ggplot() +
  geom_line(aes(x = target_year, y = prediction), color = "red", data = errordat2, alpha = 0.4) +
  geom_point(aes(x = target_year, y = prediction), color = "red", data = errordat2) +
  geom_line(aes(x = target_year, y = tv_1), color = "black", data = errordat2) +
  geom_point(aes(x = target_year, y = tv_1), color = "black", data = errordat2) +
  geom_linerange(
    aes(x = target_year,
        ymin = qu0.1,
        ymax = qu0.9),
    color = "red",
    data = lrdir,
    lwd = 5,
    alpha = 0.5
  ) +
  geom_linerange(
    aes(x = target_year,
        ymin = qu0.25,
        ymax = qu0.75),
    color = "red",
    data = lrdir,
    lwd = 5,
    alpha = 0.5
  ) +
  theme_uqimf() +
  ggtitle("US, horizon Fall Next, Inflation")
ggsave(here("revision_plotstables", "dirvsabs_plot2.pdf"), plot2, width = 10, height = 7)
