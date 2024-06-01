library(data.table)
library(here)
library(ggplot2)
library(patchwork)
library(MetBrewer)
library(tidyverse)
devtools::load_all()
.d <- `[`

#load plot files
source(here("manuscript-plots", "wis_plots.R"))
source(here("manuscript-plots", "coverage_plots.R"))


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

chosen_methods <- c("rolling window", "expanding window")
chosen_ems <- c("absolute", "directional")
chosen_targets <- c("ngdp_rpch", "pcpi_pch")

all_plotcombs <- expand.grid(
  chosen_method = chosen_methods,
  chosen_em = chosen_ems,
  chosen_target = chosen_targets
)

for(m in 1:nrow(all_plotcombs)){

  current_comb <- all_plotcombs[m,]

  chosen_method <- as.character(current_comb$chosen_method)
  chosen_em <- as.character(current_comb$chosen_em)
  chosen_target <- as.character(current_comb$chosen_target)

  if(chosen_em == "directional" & chosen_method == "expanding window"){
    next
  }

  wis_plot <- wis_plot_paper(chosen_method = chosen_method,
                             chosen_em = chosen_em,
                             chosen_target = chosen_target,
                             prefix = prefix)
  ggplot2::ggsave(here("..", "uqimf-manuscript", "figures", paste0(prefix, "wis_", chosen_target,"_", chosen_em, "_", gsub(" ", "",chosen_method), ".pdf")),
                  wis_plot, width = 9, height = 8)

  cvg_plot <- coverage_plot_paper(chosen_method = chosen_method,
                             chosen_em = chosen_em,
                             chosen_target = chosen_target,
                             prefix = prefix)
  ggplot2::ggsave(here("..", "uqimf-manuscript", "figures", paste0(prefix, "coverage_", chosen_target,"_", chosen_em, "_", gsub(" ", "",chosen_method), ".pdf")),
                  cvg_plot, width = 12, height = 4.3)

  #source(here("manuscript-plots", "coverage_plots.R"))
}

chosen_method <- "expanding window"
chosen_em <- "absolute"
chosen_target <- "pcpi_pch"
