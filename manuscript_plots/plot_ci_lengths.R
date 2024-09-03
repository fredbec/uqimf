library(here)
library(data.table)
library(ggplot2)
library(MetBrewer)
devtools::load_all()

source(here("specs", "specs.R"))

.d <- `[`

max_year <- specs$score_max_year
min_year <- specs$min_year
score_min_year <- specs$score_min_year
tv_release <- specs$tv_release
window_length <- specs$window_length
suffix <- "_pava"


cstate <- readRDS("currentscorestate.RDS")
if(cstate == "train set 2001-2012"){
  prefix <- ""
} else if(cstate == "holdout set 2013-2023"){
  prefix <- "ho_"
} else {
  stop("something wrong here, master did not run through")
}


imf_fc <- fread(here("quantile_forecasts", paste0("quantile_forecasts", suffix, ".csv"))) |>
  .d(target_year <= max_year) |>
  .d(source == "IMF") |>
  .d(error_method == "absolute") |>
  .d(method == "rolling window") |>
  .d(quantile %in% c(0.1, 0.25, 0.75, 0.9)) |>
  .d(, quantile := paste0("q", quantile)) |>
  .d(, .(country, target, target_year, horizon, quantile, prediction)) |>
  dcast(country + target + target_year + horizon ~ quantile) |>
  .d(, cil5 := q0.75-q0.25) |>
  .d(, cil9 := q0.9 - q0.1) |>
  .d(, meancil5 := round(mean(cil5),1), by = c("horizon", "target")) |>
  .d(, meancil9 := round(mean(cil9),1), by = c("horizon", "target")) |>
  .d(, .(target, horizon, meancil5, meancil9)) |>
  unique() |>
  #.d(, target := factor(ifelse(target == "pcpi_pch", "Inflation", "GDP Growth")),)
  .d(, target := factor(target,
                        levels = c("pcpi_pch", "ngdp_rpch"),
                        labels = c("Inflation", "GDP Growth"),
                        ordered = "TRUE") )

baseval <- 0

imf_fc <- imf_fc |>
  .d(, q0.1 := baseval -meancil9/2) |>
  .d(, q0.25 := baseval -meancil5/2) |>
  .d(, q0.75 := baseval + meancil5/2) |>
  .d(, q0.9 := baseval + meancil9/2) |>
  .d(, .(target, horizon, q0.1, q0.25, q0.75, q0.9)) #|>
  #melt(id.vars = c("target", "horizon"), value.name = "prediction", variable.name = "quantile")

imf_fc$target_year <- factor(rep(1:4, 2),
                             levels = 1:4,
                             labels = c("Fall, Current", "Spring, Current",
                             "Fall, Next", "Spring, Next"),
                            ordered = TRUE)

#imf_fc$target_year <- rep(1:4, times = 2)



qu_lvlss <- function(lvls){

  qus <- ci_to_quantiles(lvls, "directional") #always directional

  n_qus <- length(qus)

  no_pairs <- floor(n_qus/2)


  alpha_vals <- seq(0.5, 0.75, length.out = no_pairs)

  #inner and outer quantile levels
  lapply(seq_along(1:no_pairs),
         function(ind) c(qus[ind], qus[(n_qus)-(ind-1)], alpha_vals[ind]))


}

cilengthplot <- ggplot(aes(x = target_year, group = target, color = target), data = imf_fc) +
  geom_linerange(
    aes(x = target_year,
        ymin = q0.25,
        ymax = q0.75,
        alpha = "50% Interval"),
    position = position_dodge(width = 0.5),
   #alpha = 0.8,
    lwd = 3
  ) +
  geom_linerange(
    aes(x = target_year,
        ymin = q0.1,
        ymax = q0.9,
        alpha = "80% Interval"),
    position = position_dodge(width = 0.5),
    #alpha = 0.5,
    lwd = 3
  ) +
  xlab("") +
  scale_color_met_d("Hokusai3") +
  #geom_hline(aes(yintercept = 0), linetype = "dashed") +
  theme_uqimf() %+replace%
  theme(
  text = element_text(family = "serif"),
  legend.margin=margin(0,0,0,0),
legend.box.margin=margin(-15,-10,15,-10)) +
  scale_alpha_manual(name = "",
                     breaks = c("50% Interval", "80% Interval"),
                     values = c("50% Interval" = 0.8, "80% Interval" = 0.4) )
ggsave(here("..", "uqimf-manuscript", "figures", paste0(global_file_prefix, prefix, "cilength_byhorizon.pdf")), cilengthplot, width = 4.75, height = 3.25)

