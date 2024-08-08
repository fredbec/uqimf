library(here)
library(data.table)
library(MetBrewer)
library(ggplot2)

devtools::load_all()
.d <- `[`

R_values <- 4:11
chosen_em <- "directional"
chosen_score <- "coverage_80"

filenames <- c("wis", "c80", "c50")
names(filenames) <- c("interval_score", "coverage_80", "coverage_50")

Rdats <- vector(mode = "list", length = length(4:11))

for(window_length in R_values){

  k <- window_length - 3 #only for starting counting at 1

  Rdats[[k]] <- data.table::fread(here("scores", "R_value_scores", paste0("ci_scores_avgcnt", window_length, ".csv"))) |>
    .d(, R:= window_length)
}


Rdats <- rbindlist(Rdats) ##|>

if(chosen_score == "interval_score"){
  maxvals_gdp <- Rdats |>
    copy() |>
    .d(model == "IMF" & method == "rolling window" & target == "ngdp_rpch")


  maxvals_infl <- Rdats |>
    copy() |>
    .d(model == "IMF" & method == "rolling window" & target == "pcpi_pch")

  xlims_ngdp <- c(0, max(maxvals_gdp$interval_score) + 0.025)
  xlims_pcpi <- c(0, max(maxvals_infl$interval_score) + 0.025)
} else {
  xlims_ngdp <- c(0, 1)
  xlims_pcpi <- c(0, 1)
}

Rdats <- Rdats |>
  .d(model == "IMF" & error_method == chosen_em & method == "rolling window") |>
  .d(, horizon := factor(horizon,
                            levels = c(0, 0.5, 1, 1.5),
                            labels = c("Fall, Current",
                                       "Spring, Current",
                                       "Fall, Next",
                                       "Spring, Next")))

relscores <- Rdats |>
  copy() |>
  .d(, R := paste0("R", R)) |>
  dcast(target + horizon ~ R, value.var = chosen_score) |>
  .d(,cmpto11 := R11) |>
  melt(id.vars = c("target", "horizon", "cmpto11"), variable.name = "R", value.name = chosen_score) |>
  .d(, relscore := get(chosen_score) - cmpto11)


Rlineplot <- function(chosen_target, xlims){
  Rplot <- ggplot(aes(x = R, y = get(chosen_score), group = horizon),
                  data = Rdats |> .d(target == chosen_target)) +
    geom_line(aes(color = horizon), lwd = 0.65) +
    scale_color_met_d("Hokusai3") +
    theme_uqimf() %+replace%
    theme(text = element_text(family = "serif"),
          axis.text.y = element_text(size = 18),
          axis.text.x = element_text(size = 18),
          axis.title.y = element_text(size = 18, angle = 90, vjust = 3),
          axis.title.x = element_text(size = 18, vjust = -2),
          legend.text=element_text(size=18),
          legend.title=element_blank(),
          plot.margin = margin(t=10,b=10,r=10,l=10, unit = "pt")) +
    xlab("Window Length R") +
    ylab("Weighted Interval Score") +
    scale_x_continuous(breaks = 4:11) +
    scale_y_continuous(limits = xlims)

  return(Rplot)
}


gdpplot <- Rlineplot("ngdp_rpch", xlims = xlims_ngdp)
ggplot2::ggsave(here("..", "uqimf-manuscript", "figures", paste0("Rvalues_", filenames[chosen_score], "_", "ngdp_rpch", "_", chosen_em, ".pdf")),
                gdpplot, width = 9, height = 8)
cpiplot <- Rlineplot("pcpi_pch", xlims = xlims_pcpi )
ggplot2::ggsave(here("..", "uqimf-manuscript", "figures", paste0("Rvalues_", filenames[chosen_score], "_", "pcpi_pch", "_", chosen_em, ".pdf")),
                cpiplot, width = 9, height = 8)
