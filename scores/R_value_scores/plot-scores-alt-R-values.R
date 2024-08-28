library(here)
library(data.table)
library(MetBrewer)
library(ggplot2)
library(patchwork)
devtools::load_all()
.d <- `[`

R_values <- 4:11

chosen_ems <- c("absolute", "directional")
chosen_scores <- c("interval_score", "coverage_50", "coverage_80")

all_plotcombs <- expand.grid(
  chosen_em = chosen_ems,
  chosen_score = chosen_scores
)

cpi_plots <- vector(mode = "list", length = 6)
gdp_plots <- vector(mode = "list", length = 6)


for(m in 1:nrow(all_plotcombs)){

  current_comb <- all_plotcombs[m,]

  chosen_em <- as.character(current_comb$chosen_em)
  chosen_score <- as.character(current_comb$chosen_score)



  filenames <- c("wis", "c80", "c50")
  ylabs <- c("Interval Score", "80% Interval Coverage", "50% Interval Coverage")

  names(filenames) <- c("interval_score", "coverage_80", "coverage_50")
  names(ylabs) <- c("interval_score", "coverage_80", "coverage_50")

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

  txtsize <- 20

  if(chosen_em == "directional"){
    titlep <- " - Directional"
  } else {
    titlep <- " - Absolute"
  }

  if(chosen_score == "coverage_80"){
    hlineval <- 0.8
    hlinecol <- "black"
  } else if(chosen_score == "coverage_50"){
    hlineval <- 0.5
    hlinecol <- "black"
  } else {
    hlineval <- 0
    hlinecol <- "white"
  }


  Rlineplot <- function(chosen_target, xlims, chosen_scr){
    Rplot <- ggplot(aes(x = R, y = get(chosen_scr), group = horizon),
                    data = Rdats |> .d(target == chosen_target)) +

      geom_hline(yintercept = hlineval, linetype = "dashed", color = hlinecol, lwd = 1) +
      geom_line(aes(color = horizon), lwd = 1.4) +
      scale_color_met_d("Hokusai3") +
      theme_uqimf() %+replace%
      theme(text = element_text(family = "serif"),
            axis.text.y = element_text(size = txtsize),
            plot.title = element_text(hjust = 0.5, size = txtsize +2),
            axis.text.x = element_text(size = txtsize),
            axis.title.y = element_text(size = txtsize, angle = 90, vjust = 3),
            axis.title.x = element_text(size = txtsize, vjust = -0.1),
            legend.text=element_text(size=txtsize + 2),
            legend.title.align=-30,
            legend.title=element_blank(),
            plot.margin = margin(t=10,b=20,r=20,l=20, unit = "pt")) +
      xlab("Window Length R") +
      ylab(ylabs[chosen_scr]) +
      scale_x_continuous(breaks = 4:11) +
      scale_y_continuous(limits = xlims)

    return(Rplot)
  }


  gdp_plots[[m]] <- Rlineplot("ngdp_rpch", xlims = xlims_ngdp, chosen_scr = chosen_score)

  if(chosen_score == "interval_score"){
    gdp_plots[[m]] <- gdp_plots[[m]] +
      theme(
        plot.title = element_text(hjust = 0.5, vjust = 6, size = txtsize +8),
        plot.margin = margin(t=40,b=30,r=10,l=10, unit = "pt")) +
      ggtitle(paste0("GDP Growth", titlep))
  }
  cpi_plots[[m]] <- Rlineplot("pcpi_pch", xlims = xlims_pcpi, chosen_scr = chosen_score )

  if(chosen_score == "interval_score"){
    cpi_plots[[m]] <- cpi_plots[[m]]+
      theme(
        plot.title = element_text(hjust = 0.5, vjust = 6, size = txtsize + 8),
        plot.margin = margin(t=40,b=30,r=10,l=10, unit = "pt")) +
      ggtitle(paste0("Inflation", titlep))
  }

}

ovr_plot <-
  gdp_plots[[1]] + gdp_plots[[2]] + cpi_plots[[1]] + cpi_plots[[2]] + gdp_plots[[3]] + gdp_plots[[4]] + cpi_plots[[3]] + cpi_plots[[4]] + gdp_plots[[5]] + gdp_plots[[6]] + cpi_plots[[5]] + cpi_plots[[6]] +
    plot_layout(guides = "collect", ncol = 4) &
  theme(legend.position = 'bottom',
        legend.box="vertical", legend.margin=margin())


ggplot2::ggsave(here("..", "uqimf-manuscript", "figures", paste0("Rvalues_","allmetrics", ".pdf")),
                ovr_plot, width = 20, height = 12)
