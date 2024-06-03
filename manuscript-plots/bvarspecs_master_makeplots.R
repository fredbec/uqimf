library(data.table)
library(here)
library(ggplot2)
library(patchwork)
library(MetBrewer)
library(tidyverse)
devtools::load_all()
.d <- `[`

#load plot files
source(here("manuscript-plots", "bvarspecs_wis_plot.R"))
source(here("manuscript-plots", "bvarspecs_coverage_plots.R"))


cstate <- readRDS("currentscorestate.RDS")
if(cstate == "train set 2001-2012"){
  prefix <- ""
} else if(cstate == "holdout set 2013-2023"){
  prefix <- "ho_"
} else {
  stop("something wrong here, master did not run through")
}

cstate_truth <- readRDS("currenttruthstate.RDS")
if(is.numeric(cstate_truth)){
  prefix <- paste0(prefix, "")
} else if(cstate_truth == "oecd"){
  prefix <- paste0(prefix, "oecd_")
} else {
  stop("something wrong here, master did not run through")
}

prefix <- paste0(prefix, "bvarspecs_")

chosen_method <- "rolling window"
chosen_em <- "absolute"
chosen_target <- "pcpi_pch"



############Inflation##########################
wis_plot <- wis_plot_paper_bvarspecs(chosen_method = chosen_method,
                           chosen_em = chosen_em,
                           chosen_target = chosen_target,
                           prefix = prefix)
ggplot2::ggsave(here("..", "uqimf-manuscript", "figures", paste0(prefix, "wis_", chosen_target,"_", chosen_em, "_", gsub(" ", "",chosen_method), ".pdf")),
                wis_plot, width = 9, height = 8)

cvg_plot <- coverage_plot_paper_bvarspecs(chosen_method = chosen_method,
                                          chosen_em = chosen_em,
                                          chosen_target = chosen_target,
                                          prefix = prefix)
ggplot2::ggsave(here("..", "uqimf-manuscript", "figures", paste0(prefix, "coverage_", chosen_target,"_", chosen_em, "_", gsub(" ", "",chosen_method), ".pdf")),
                cvg_plot, width = 12, height = 4.3)



############GDP Growth##########################
chosen_target <- "ngdp_rpch"
wis_plot <- wis_plot_paper_bvarspecs(chosen_method = chosen_method,
                                     chosen_em = chosen_em,
                                     chosen_target = chosen_target,
                                     prefix = prefix)
ggplot2::ggsave(here("..", "uqimf-manuscript", "figures", paste0(prefix, "wis_", chosen_target,"_", chosen_em, "_", gsub(" ", "",chosen_method), ".pdf")),
                wis_plot, width = 9, height = 8)

cvg_plot <- coverage_plot_paper_bvarspecs(chosen_method = chosen_method,
                                chosen_em = chosen_em,
                                chosen_target = chosen_target,
                                prefix = prefix)
ggplot2::ggsave(here("..", "uqimf-manuscript", "figures", paste0(prefix, "coverage_", chosen_target,"_", chosen_em, "_", gsub(" ", "",chosen_method), ".pdf")),
                cvg_plot, width = 12, height = 4.3)


#############Histogram Relative Scores#################

bvar_twospec <- fread(here("scores", "bvar_ci_scores.csv")) |>
  .d(model %in% c("bvar_qu", "bvar_ciss")) |>
  dcast(target + horizon  + country ~ model, value.var = "interval_score") |>
  .d(!is.na(bvar_ciss)) |> # no data for CAN, JPN
  .d(, relwis := bvar_ciss/bvar_qu)


#both targets
relwis <- ggplot(aes(x = relwis), data = bvar_twospec) +
  geom_histogram(binwidth = 0.13345, color = "steelblue4", fill = "steelblue3") +
  theme_uqimf() +
  xlab("Relative WIS: (BVAR - CISS) / (BVAR - direct)")
ggplot2::ggsave(here("..", "uqimf-manuscript", "figures", paste0(prefix, "relativeWIS_", ".pdf")),
                relwis, width = 7, height = 4)

#inflation only
relwis <- ggplot(aes(x = relwis), data = bvar_twospec |> copy() |> .d(target == "pcpi_pch")) +
  geom_histogram(binwidth = 0.13345, color = "steelblue4", fill = "steelblue3") +
  theme_uqimf() +
  xlab("Relative WIS: (BVAR - CISS) / (BVAR - direct)")
ggplot2::ggsave(here("..", "uqimf-manuscript", "figures", paste0(prefix, "relativeWIS_pcpi_pch", ".pdf")),
                relwis, width = 7, height = 4)


#inflation only
relwis <- ggplot(aes(x = relwis), data = bvar_twospec |> copy() |> .d(target == "ngdp_rpch")) +
  geom_histogram(binwidth = 0.13345, color = "steelblue4", fill = "steelblue3") +
  theme_uqimf() +
  xlab("Relative WIS: (BVAR - CISS) / (BVAR - direct)")
ggplot2::ggsave(here("..", "uqimf-manuscript", "figures", paste0(prefix, "relativeWIS_ngdp_rpch", ".pdf")),
                relwis, width = 7, height = 4)
