library(ggplot2)
library(MetBrewer)
library(data.table)
library(gghighlight)
library(patchwork)
library(here)
devtools::load_all()

.d <- `[`

set.seed(2922)
w <- 11

ctry <- "USA"
tgt <- "pcpi_pch"
yr <- 2001
hr <- 1
rw <- 11

getlinerangedat <- function(dat, xpos){
  val80_ur <- dat[quantile == 0.9, "error_prediction"] |> unname() |> unlist()
  val50_ur <- dat[quantile == 0.75, "error_prediction"]|> unname() |> unlist()
  val80_lwr <- dat[quantile == 0.1, "error_prediction"] |> unname() |> unlist()
  val50_lwr <- dat[quantile == 0.25, "error_prediction"]|> unname() |> unlist()
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

makecovplot <- function(ctry, tgt, yr, hr, rw){
  labelvec <- c("raw error\nvalues", "Directional", "absolute\nerror values", "IMF")

  fancycols <- met.brewer("Hokusai3", n = 4)
  ######################################CURRENT YEAR#####################################
  makedata <- function(ctry, tgt, yr, hr, rw, pos = -1){
    errordat2 <- fread(here("data", "weodat.csv")) |>
      .d(, fc_error := tv_1 - prediction) |>
      .d(country == ctry & target == tgt & horizon == hr) |>
      .d(target_year %in% seq(yr-(rw+floor(hr)), yr-(1+floor(hr)), 1)) #need extra shift if hor > 1
    qufcs_imf<- data.table::fread(here("quantile_forecasts",
                                       paste0("toscore", "", "_quantile_forecasts.csv"))) |>
      .d(country == ctry & target == tgt & horizon == hr) |>
      .d(target_year == yr) |>
      .d(source == "IMF")

    qufcs_dir <- data.table::fread(here("quantile_forecasts",
                                       paste0("toscore", "", "_quantile_forecasts_directional.csv"))) |>
      .d(country == ctry & target == tgt & horizon == hr) |>
      .d(target_year == yr) |>
      .d(source == "IMF")

    errors <- errordat2$fc_error
    abs_errors <- errors |> abs()
    val80 <- quantile(abs_errors, 0.8)
    val50 <- quantile(abs_errors, 0.5)

    ##make data
    errordat <- data.frame(
      type = rep(1 + pos*0.2, w),
      value = errors,
      col = 1:11
    )
    errordat_abs <- data.frame(
      type = rep(3 + pos*0.2, w),
      value = abs_errors,
      col = 1:11
    )
    errordat <- rbind(errordat, errordat_abs) |>
      setDT() |>
      .d(, value := ifelse(type == 2, value, value + 1e-09))

    pos_imf <- ifelse(pos == 1, 4.2, 3.8)
    pos_dir <- ifelse(pos == 1, 2.2, 1.8)

    linerangedat_imf <- getlinerangedat(qufcs_imf, pos_imf) |>
      .d(, pltcol := fancycols[1])
    linerangedat_dir <- getlinerangedat(qufcs_dir, pos_dir) |>
      .d(, pltcol := fancycols[2])


    return(list(errordat = errordat, listdatsets = list(linerangedat_imf, linerangedat_dir)))
  }

  val50 <- 0
  val80 <- 0
  ######################################NEXT YEAR#####################################
  myres_next <- makedata(ctry = ctry, tgt = tgt, yr = yr + 1, hr = hr, rw = rw, pos = 1)
  nextyrerror <- myres_next$errordat
  nextyrdats <- myres_next$listdatsets

  myres_current <- makedata(ctry = ctry, tgt = tgt, yr = yr, hr = hr, rw = rw, pos = -1)
  errordat <- myres_current$errordat
  datasets <- myres_current$listdatsets


  minvalplot <- min(c(nextyrerror$value, errordat$value))
  maxvalplot <- max(c(nextyrerror$value, errordat$value))
  #Define segment function
  geom_segment_hor <- function(x, y, xend, yend = y,
                               lineend = "butt", linejoin = "round",
                               color = "grey80", alpha = 0.8,
                               linetype = "dashed", size = 0.25,
                               arrow = NULL) {
    geom_segment(
      aes(x = x, y = y, xend = xend, yend = yend),
      lineend = lineend, linejoin = linejoin,
      color = color, alpha = alpha,
      linetype = linetype, size = size,
      arrow = arrow
    )
  }
  #Define segment function
  geom_segment_arrow <- function(x, y, xend, yend = y,
                                 lineend = "butt", linejoin = "round",
                                 color = "grey80", alpha = 0.8,
                                 linetype = "dashed", size = 0.25,
                                 arrow = NULL) {

    geom_segment(
      aes(x = x, y = y, xend = xend, yend = yend),
      lineend = "butt", linejoin = "round",
      color = "grey80", alpha = 0.8,
      size = 0.5, arrow = arrow(length = unit(0.075, "inches"))
    )
  }

  #datasets <- list(linerangedat_imf, linerangedat_ar, linerangedat_bvar, linerangedat_tulip)

  lrcols <- met.brewer("Hokusai2", 2)
  rects <- data.frame(xstart = seq(1,4,1), xend = seq(1.4,4.4,1), col = "grey40")
  quantvis <- ggplot() +
    geom_point(aes(x = type, y = value), color = "grey65", data = errordat, size = 4, alpha = 0.5) +
    geom_point(aes(x = type, y = value), color = "grey65", data = nextyrerror, size = 4, alpha = 0.5) +
    #gghighlight(value %in% c(val50, val80)) +
    scale_color_met_d("Hokusai2") +
    geom_hline(aes(yintercept = 0), linetype = "solid", color = "grey80", alpha = 0.7, size = 0.3) +
    guides(color = "none") +
    geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf), fill = "grey70", alpha = 0.2) +
    theme_uqimf() %+replace%
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "right",
          plot.title = element_text(hjust = 0.5, size = 14),
          text = element_text(family = "serif"),
          #axis.text.x = element_text(size = textsize_y, angle = 90, hjust = .5, vjust = .5, face = "plain"),
          #strip.text = element_text(size = 8),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          strip.text = element_text(size=12),
          legend.text=element_text(size=12),
          legend.title=element_blank(),
          plot.margin = margin(t=5,b=5,r=5,l=5, unit = "pt")) +

    lapply(datasets, function(datset){
      geom_linerange(
        aes(x = type,
            ymin = lower80,
            ymax = upper80),
        color = datset$pltc,
        data = datset,
        lwd = 5,
        alpha = 0.5
      )
    }) +
    lapply(datasets, function(datset){
      geom_linerange(
        aes(x = type,
            ymin = lower50,
            ymax = upper50),
        color = datset$pltc,
        data = datset,
        lwd = 5
      )
    }) +
    lapply(nextyrdats, function(datset){
      geom_linerange(
        aes(x = type,
            ymin = lower80,
            ymax = upper80),
        color = datset$pltc,
        data = datset,
        lwd = 5,
        alpha = 0.5
      )
    }) +
    lapply(nextyrdats, function(datset){
      geom_linerange(
        aes(x = type,
            ymin = lower50,
            ymax = upper50),
        color = datset$pltc,
        data = datset,
        lwd = 5
      )
    }) +
    #geom_segment_arrow(x = 1.15, xend = 1.85, y = -2.4) +
    #geom_segment_arrow(x = 2.15, xend = 2.85, y = -2.4) +
    geom_segment_hor(x = 2.05, xend = 2.685, y = val50) +
    geom_segment_hor(x = 2.05, xend = 2.685, y = val80) +
    #geom_segment_hor(x = 3.065, xend = 3.255, y = val50) +
    #geom_segment_hor(x = 3.065, xend = 3.255, y = val80) +
    #annotate(
    #  "text", label = "80% Interval\n",
    #  x = 3.37, y = val80, size = 2.75, colour = "grey60"
    #) +
    #annotate(
    #  "text", label = "50% Interval\n",
    #  x = 3.37, y = val50, size = 2.75, colour = "grey60"
    #) +
    #geom_point(aes(x = type, y = pointval), data = pointdat, color = lrcols[2], size = 1.5, pch = 23) +
    scale_y_continuous(limits = c(minvalplot -2, maxvalplot)) +
    scale_x_continuous(breaks = c(1,2,3,4),
                       labels = labelvec,
                       limits = c(0.55, 4.8)) +
    ylab("") +
    xlab("") +
    annotate("label", x = 0.8, y = minvalplot - 1, label = "I", size = 5, fill = "white", color = "black",family = "serif") +
    annotate("label", x = 1.8, y = minvalplot - 1, label = "I", size = 5, fill = "white", color = "black",family = "serif")+
    annotate("label", x = 2.8, y = minvalplot - 1, label = "I", size = 5, fill = "white", color = "black",family = "serif")+
    annotate("label", x = 3.8, y = minvalplot - 1, label = "I", size = 5, fill = "white", color = "black",family = "serif")+
    annotate("label", x = 1.2, y = minvalplot - 1, label = "II", size = 5, fill = "white", color = "black",family = "serif")+
    annotate("label", x = 2.2, y = minvalplot - 1, label = "II", size = 5, fill = "white", color = "black",family = "serif")+
    annotate("label", x = 3.2, y = minvalplot - 1, label = "II", size = 5, fill = "white", color = "black",family = "serif")+
    annotate("label", x = 4.2, y = minvalplot - 1, label = "II", size = 5, fill = "white", color = "black",family = "serif")
  return(quantvis)
}

plot_de <- makecovplot(ctry = "USA", tgt = "pcpi_pch", yr = 2004, hr = hr, rw = rw)

if(FALSE){
plot_fr <- makecovplot(ctry = "FRA", tgt = tgt, yr = yr, hr = hr, rw = rw)+
  ggtitle("GDP Growth, France")
plot_it <- makecovplot(ctry = "ITA", tgt = tgt, yr = yr, hr = hr, rw = rw)+
  ggtitle("GDP Growth, Italy")
plot_ca <- makecovplot(ctry = "CAN", tgt = tgt, yr = yr, hr = hr, rw = rw)+
  ggtitle("GDP Growth, Canada")
plot_us <- makecovplot(ctry = "USA", tgt = tgt, yr = yr, hr = hr, rw = rw)+
  ggtitle("GDP Growth, USA")
plot_uk <- makecovplot(ctry = "GBR", tgt = tgt, yr = yr, hr = hr, rw = rw)+
  ggtitle("GDP Growth, United Kingdom")

ovrplot <- (plot_de + plot_ca) /
  (plot_fr + plot_us) /
  (plot_it + plot_uk)
}
