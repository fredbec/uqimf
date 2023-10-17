library(here)
library(data.table)
devtools::load_all()

source(here("specs", "specs.R"))

.d <- `[`

max_year <- specs$score_max_year
min_year <- specs$min_year
score_min_year <- specs$score_min_year
tv_release <- specs$tv_release
window_length <- specs$window_length

imf_fc <- fread(here("quantile_forecasts", "quantile_forecasts.csv")) |>
  .d(target_year == 2023) |>
  .d(source == "IMF") |>
  .d(target == "ngdp_rpch") |>
  .d(country == "DEU") |>
  .d(error_method == "absolute") |>
  .d(method == "rolling window") |>
  .d(quantile %in% c(0.1, 0.25, 0.75, 0.9)) |>
  .d(, quantile := paste0("q", quantile)) |>
  .d(, .(country, target, target_year, horizon, quantile, error_prediction)) |>
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

print(imf_fc)

baseval <- 0.1

imf_fc <- imf_fc |>
  .d(, q0.1 := baseval -meancil9/2) |>
  .d(, q0.25 := baseval -meancil5/2) |>
  .d(, q0.75 := baseval + meancil5/2) |>
  .d(, q0.9 := baseval + meancil9/2) |>
  .d(, .(target, horizon, q0.1, q0.25, q0.75, q0.9)) #|>
#melt(id.vars = c("target", "horizon"), value.name = "prediction", variable.name = "quantile")

imf_fc$target_year <- factor(rep(1:4, 1),
                             levels = 1:4,
                             labels = c("Fall, same year", "Spring, same year",
                                        "Fall, next year", "Spring, next year"),
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

colors = met.brewer(name="Hokusai1", n=4)

if(FALSE){
  qu_lvls <- qu_lvlss(c(0.5, 0.8))

  ggplot(aes(group = target), data = imf_fc) +
    lapply(qu_lvls, function(qupr){
      geom_linerange(
        aes(x = target_year,
            ymin = get(paste0("q", qupr[1])),
            ymax = get(paste0("q", qupr[2]))),
        data = imf_fc |> .d(target == "ngdp_rpch"),
        lwd = 3.5, alpha = qupr[3], show.legend = TRUE,
        color =colors[2],
        position = position_dodge(width = 0.5))
    }) +
    lapply(qu_lvls, function(qupr){
      geom_linerange(
        aes(x = target_year,
            ymin = get(paste0("q", qupr[1])),
            ymax = get(paste0("q", qupr[2]))),
        data = imf_fc |> .d(target == "pcpi_pch"),
        lwd = 2.5, alpha = qupr[3], show.legend = TRUE,
        color =colors[1],
        position = position_dodge(width = 0.5))
    }) +
    xlab("") +
    geom_hline(aes(yintercept = 0), linetype = "dashed") +
    theme_uqimf()
}


pdf(file = here("presentation", "figures", "horizon_uncc_germany2023_current.pdf"), width = 5, height = 3.35)
ggplot(aes(x = target_year, group = target, color = target), data = imf_fc) +
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
  geom_hline(aes(yintercept = 0, linetype = "\"Recession\"")) +
  xlab("") +
  scale_color_met_d("Hokusai1") +
  #geom_hline(aes(yintercept = 0), linetype = "dashed") +
  theme_uqimf() %+replace%
  theme(
    legend.margin=margin(0,0,0,0),
    legend.box.margin=margin(-15,-10,15,-10)) +
  scale_alpha_manual(name = "",
                     breaks = c("50% Interval", "80% Interval"),
                     values = c("50% Interval" = 0.8, "80% Interval" = 0.4) ) +
  scale_linetype_manual(name = "",
                        breaks = c("\"Recession\""),
                        values = c("\"Recession\"" = "dashed"))
dev.off()
