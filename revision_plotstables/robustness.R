library(ggplot2)
library(MetBrewer)
library(data.table)
library(gghighlight)
library(here)
devtools::load_all()

.d <- `[`

set.seed(2922)
w <- 11

ctry <- "FRA"
tgt <- "ngdp_rpch"
yr <- 2021
hr <- 1
rw <- 11

getlinerangedat <- function(dat, xpos){
  val80_lr <- dat[quantile == 0.9, "error_prediction"] |> unname() |> unlist()
  val50_lr <- dat[quantile == 0.75, "error_prediction"]|> unname() |> unlist()
  linerangedat_lr <- data.frame(
    type = rep(c(xpos), each = 1),
    upper80 = c(val80_lr),
    lower80 = c( -val80_lr),
    upper50 = c(val50_lr),
    lower50 = c(-val50_lr)
  ) |>
    setDT()

  return(linerangedat_lr)
}

labelvec <- c("raw error\nvalues", "error values,\nabsolute transformed", "prediction intervals, \ncentered around\n zero for illustration")

fancycols <- met.brewer("Hokusai3", n = 4)
######################################CURRENT YEAR#####################################
makedata <- function(ctry, tgt, yr, hr, rw, pos = -1){
  errordat2 <- fread(here("data", "weodat.csv")) |>
    .d(, fc_error := prediction - tv_1) |>
    .d(country == ctry & target == tgt & horizon == hr) |>
    .d(target_year %in% seq(yr-(rw+floor(hr)), yr-(1+floor(hr)), 1)) #need extra shift if hor > 1
  qufcs_imf<- data.table::fread(here("quantile_forecasts",
                                         paste0("toscore", "", "_quantile_forecasts_ho.csv"))) |>
    .d(country == ctry & target == tgt & horizon == hr) |>
    .d(target_year == yr) |>
    .d(source == "IMF")
  qufcs_ar <- data.table::fread(here("benchmarks",
                                        paste0("extcis_", "toscore", "", "_bvar_direct_quantile_forecasts_ho.csv"))) |>
    setnames(paste0("tv_1"), "true_value") |>
    .d(source == "ar")|>
    .d(country == ctry & target == tgt & horizon == hr) |>
    .d(target_year == yr) |>
    .d(quantile %in% c(0.1, 0.25, 0.75, 0.9)) |>
    .d(, mean := mean(prediction)) |>
    .d(, error_prediction := prediction-mean)
  qufcs_bvar <- data.table::fread(here("benchmarks",
                                     paste0("extcis_", "toscore", "", "_bvar_direct_quantile_forecasts_ho.csv"))) |>
    setnames(paste0("tv_1"), "true_value") |>
    .d(source == "bvar_mix")|>
    .d(country == ctry & target == tgt & horizon == hr) |>
    .d(target_year == yr) |>
    .d(quantile %in% c(0.1, 0.25, 0.75, 0.9)) |>
    .d(, mean := mean(prediction)) |>
    .d(, error_prediction := prediction-mean)

  print(qufcs_bvar)

  errors <- errordat2$fc_error
  msee <- sqrt(mean(errors^2))
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
    type = rep(2 + pos*0.2, w),
    value = abs_errors,
    col = 1:11
  )
  errordat <- rbind(errordat, errordat_abs) |>
    setDT() |>
    .d(, value := ifelse(type == 2, value, value + 1e-09))

  pos_imf <- ifelse(pos == 1, 3.1, 2.6)
  pos_bvar <- ifelse(pos == 1, 3.2, 2.7)
  pos_ar <- ifelse(pos == 1, 3.3, 2.8)
  pos_tulip <- ifelse(pos == 1, 3.4, 2.9)


  linerangedat_imf <- getlinerangedat(qufcs_imf, pos_imf) |>
    .d(, pltcol := fancycols[1])
  linerangedat_ar <- getlinerangedat(qufcs_ar, pos_ar) |>
    .d(, pltcol := fancycols[2])
  linerangedat_bvar <- getlinerangedat(qufcs_bvar, pos_bvar)|>
    .d(, pltcol := fancycols[4])

  linerangedat_tulip <- data.frame(
    type = rep(pos_tulip, each = 1),
    upper80 = c(qnorm(0.9, 0, msee)),
    lower80 = c( -qnorm(0.9, 0, msee)),
    upper50 = c(qnorm(0.75, 0, msee)),
    lower50 = c(-qnorm(0.75, 0, msee))
  ) |>
    setDT()|>
    .d(, pltcol := "firebrick4")


  return(list(errordat = errordat, listdatsets = list(linerangedat_imf, linerangedat_ar, linerangedat_bvar, linerangedat_tulip)))
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
rects <- data.frame(xstart = seq(1,3,1), xend = seq(1.5,3.5,1), col = "grey50")
quantvis <- ggplot() +
  geom_point(aes(x = type, y = value), color = "grey55", data = errordat, size = 2) +
  geom_point(aes(x = type, y = value), color = "grey55", data = nextyrerror, size = 2) +
  #gghighlight(value %in% c(val50, val80)) +
  scale_color_met_d("Hokusai2") +
  geom_hline(aes(yintercept = 0), linetype = "solid", color = "grey80", alpha = 0.7, size = 0.3) +
  guides(color = "none") +
  geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf), fill = "grey70", alpha = 0.2) +
  theme_uqimf() %+replace%
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "right",
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
      lwd = 3,
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
      lwd = 3
    )
  }) +
  lapply(nextyrdats, function(datset){
    geom_linerange(
      aes(x = type,
          ymin = lower80,
          ymax = upper80),
      color = datset$pltc,
      data = datset,
      lwd = 3,
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
      lwd = 3
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
  scale_y_continuous(limits = c(-9.5, 9.5)) +
  scale_x_continuous(breaks = c(1,2,3), labels = labelvec, limits = c(0.55, 3.8)) +
  ylab("") +
  xlab("")


quantvis
#ggsave(here("..", "uqimf-manuscript", "figures", "illustration_quantileextraction.pdf"), quantvis, width = 7, height = 4.5)

