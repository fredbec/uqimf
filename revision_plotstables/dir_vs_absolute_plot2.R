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
textsize_y <- 18
suffix <- "_directional"

qufcs_dir <- data.table::fread(here("quantile_forecasts",
                                    paste0("toscore", "", "_quantile_forecasts", suffix, ".csv"))) |>
  .d(country == ctry & target == tgt & horizon == hr) |>
  .d(target_year == yr) |>
  .d(source == "IMF")

errordat2 <- fread(here("data", "weodat.csv")) |>
  .d(, fc_error := prediction - tv_1) |>
  .d(country == ctry & target == tgt & horizon == hr) |>
  .d(target_year %in% seq(yr-(rw+floor(hr)), yr+11, 1))

qufcs_dir <- data.table::fread(here("quantile_forecasts",
                                    paste0("toscore", "", "_quantile_forecasts", suffix, ".csv"))) |>
  .d(country == ctry & target == tgt & horizon == hr) |>
  .d(target_year %in% (yr+1):(yr+11)) |>
  .d(source == "IMF")

#Define segment function
geom_segment_arrow <- function(x, y, xend, yend = y,
                               lineend = "butt", linejoin = "round",
                               color = "grey80", alpha = 0.8,
                               linetype = "dashed", size = 0.25,
                               arrow = NULL) {

  geom_segment(
    aes(x = x, y = y, xend = xend, yend = yend),
    lineend = "butt", linejoin = "round",
    color = "black", alpha = 1,
    size = 0.5, arrow = arrow(length = unit(0.075, "inches"))
  )
}

mycol <- met.brewer("Hokusai1", n = 7)[7]

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
  geom_line(aes(x = target_year, y = prediction), color = mycol, data = errordat2, alpha = 0.5) +
  geom_point(aes(x = target_year, y = prediction), color = mycol, data = errordat2) +
  geom_line(aes(x = target_year, y = tv_1), color = "black", data = errordat2, alpha = 0.5) +
  geom_point(aes(x = target_year, y = tv_1), color = "black", data = errordat2) +
  geom_linerange(
    aes(x = target_year,
        ymin = qu0.1,
        ymax = qu0.9),
    color = mycol,
    data = lrdir,
    lwd = 5,
    alpha = 0.4
  ) +
  geom_linerange(
    aes(x = target_year,
        ymin = qu0.25,
        ymax = qu0.75),
    color = mycol,
    data = lrdir,
    lwd = 5,
    alpha = 0.8
  ) +
  geom_segment_arrow(x = 1991, xend = 2000, y = -1.4) +
  xlab("Target Year") +
  ylab("Inflation") +
  annotate("label", x = 1995, y = -1.2, label = "Initial estimation window", size = 4, fill = "white", color = "black",family = "serif") +
  theme_uqimf() %+replace%
  theme(#axis.text.x = element_blank(),
    axis.text.x = element_text(size = textsize_y),
    #strip.text = element_text(size = 8),
    axis.text.y = element_text(size = textsize_y),

    axis.title.x = element_text(size = textsize_y),
    axis.title.y = element_text(size = textsize_y, angle = 90, vjust = 2),
    strip.text = element_text(size=textsize_y),
    legend.text=element_text(size=textsize_y),
    text = element_text(family = "serif"),
    legend.title=element_blank(),
    plot.margin = margin(t=10,b=10,r=10,l=10, unit = "pt"))# +
  #ggtitle("US, horizon Fall Next, Inflation")
ggsave(here("revision_plotstables", paste0("dirvsabs_plotUSA", suffix, ".pdf")), plot2, width = 10, height = 5)
