library(data.table)
library(here)
library(ggplot2)
library(patchwork)
library(MetBrewer)
library(tidyverse)
devtools::load_all()
.d <- `[`

#load plot files
source(here("manuscript_plots", "bvarspecs_wis_plot.R"))
source(here("manuscript_plots", "bvarspecs_coverage_plots.R"))


splits <- c("", "ho")
chosen_targets <- c("ngdp_rpch", "pcpi_pch")
prefix <- "bvarspecs_"

all_plotcombs <- expand.grid(
  dataset_suffix = splits,
  chosen_target = chosen_targets
)


for(m in 1:nrow(all_plotcombs)){

  current_comb <- all_plotcombs[m,]

  dataset_suffix <- as.character(current_comb$dataset_suffix)
  chosen_target <- as.character(current_comb$chosen_target)

  wis_plot <- wis_plot_paper_bvarspecs(dataset_suffix = dataset_suffix,
                                       chosen_target = chosen_target)
  ggplot2::ggsave(here("..", "uqimf-manuscript", "figures", paste0(prefix, "wis_", chosen_target, ".pdf")),
                  wis_plot, width = 9, height = 8)

  cvg_plot <- coverage_plot_paper_bvarspecs(dataset_suffix = dataset_suffix,
                                            chosen_target = chosen_target)
  ggplot2::ggsave(here("..", "uqimf-manuscript", "figures", paste0(prefix, "coverage_", chosen_target, ".pdf")),
                  cvg_plot, width = 12, height = 4.3)

  #source(here("manuscript-plots", "coverage_plots.R"))
}

############Inflation##########################




############GDP Growth##########################
#chosen_target <- "ngdp_rpch"
#wis_plot <- wis_plot_paper_bvarspecs(chosen_method = chosen_method,
#                                     chosen_em = chosen_em,
#                                     chosen_target = chosen_target,
#                                     prefix = prefix)
#ggplot2::ggsave(here("..", "uqimf-manuscript", "figures", paste0(prefix, "wis_", chosen_target,"_", chosen_em, "_", gsub(" ", "",chosen_method), ".pdf")),
#                wis_plot, width = 9, height = 8)#

#cvg_plot <- coverage_plot_paper_bvarspecs(chosen_method = chosen_method,
#                                chosen_em = chosen_em,
#                                chosen_target = chosen_target,
#                                prefix = prefix)
#ggplot2::ggsave(here("..", "uqimf-manuscript", "figures", paste0(prefix, "coverage_", chosen_target,"_", chosen_em, "_", gsub(" ", "",chosen_method), ".pdf")),
#                cvg_plot, width = 12, height = 4.3)


#############Histogram Relative Scores#################

bvar_twospec <- fread(here("scores", "_bvarspecs", "bvar_ci_scores_ho.csv")) |>
  .d(model %in% c("bvar_qu", "bvar_ciss")) |>
  dcast(target + horizon  + country ~ model, value.var = "interval_score") |>
  .d(!is.na(bvar_ciss)) |> # no data for CAN, JPN
  .d(, relwis := bvar_ciss/bvar_qu)

prefix <- "new"

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
