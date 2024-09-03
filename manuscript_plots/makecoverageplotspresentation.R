library(data.table)
library(here)
library(ggplot2)
library(patchwork)
library(MetBrewer)
library(tidyverse)
devtools::load_all()
.d <- `[`

#load plot files
source(here("manuscript_plots", "coverage_plots_presentation.R"))

dataset_suffix <- "ho"
chosen_targets <- c("ngdp_rpch", "pcpi_pch")

all_plotcombs <- expand.grid(
  dataset_suffix = dataset_suffix,
  chosen_target = chosen_targets
)

for(m in 1:nrow(all_plotcombs)){

  current_comb <- all_plotcombs[m,]

  dataset_suffix <- as.character(current_comb$dataset_suffix)
  chosen_target <- as.character(current_comb$chosen_target)


  if(chosen_target == "ngdp_rpch"){
    plottitle <- "GDP Growth"
  } else {
    plottitle <- "Inflation"
  }

  cvg_plot <- coverage_plot_paper(dataset_suffix = dataset_suffix,
                                  chosen_target = chosen_target) +
    ggtitle(plottitle) +
    theme(plot.title = element_text(hjust = 0.5, size = 20))
  ggplot2::ggsave(here("miscellaneous", "presentation", "figures", paste0("coverage_", chosen_target,"_", dataset_suffix, ".pdf")),
                  cvg_plot, width = 5, height = 4.3)



  #source(here("manuscript-plots", "coverage_plots.R"))
}

