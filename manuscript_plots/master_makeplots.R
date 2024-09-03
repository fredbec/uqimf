library(data.table)
library(here)
library(ggplot2)
library(patchwork)
library(MetBrewer)
library(tidyverse)
devtools::load_all()
.d <- `[`

#load plot files
source(here("manuscript_plots", "wis_plots.R"))
source(here("manuscript_plots", "coverage_plots.R"))


splits <- c("", "directional", "ho")
chosen_targets <- c("ngdp_rpch", "pcpi_pch")

all_plotcombs <- expand.grid(
  dataset_suffix = splits,
  chosen_target = chosen_targets
)

for(m in 1:nrow(all_plotcombs)){

  current_comb <- all_plotcombs[m,]

  dataset_suffix <- as.character(current_comb$dataset_suffix)
  chosen_target <- as.character(current_comb$chosen_target)

  wis_plot <- wis_plot_paper(dataset_suffix = dataset_suffix,
                             chosen_target = chosen_target)
  ggplot2::ggsave(here("..", "uqimf-manuscript", "figures", paste0("wis_", chosen_target,"_", dataset_suffix, "_", ".pdf")),
                  wis_plot, width = 9, height = 8)

  cvg_plot <- coverage_plot_paper(dataset_suffix = dataset_suffix,
                                  chosen_target = chosen_target)
  ggplot2::ggsave(here("..", "uqimf-manuscript", "figures", paste0("coverage_", chosen_target,"_", dataset_suffix, "_", ".pdf")),
                  cvg_plot, width = 12, height = 4.3)

  #source(here("manuscript-plots", "coverage_plots.R"))
}

