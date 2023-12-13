library(data.table)
library(here)
library(ggplot2)
library(patchwork)
library(MetBrewer)
library(purrr)
library(lattice)
library(gt)
library(rlang)
library(tidyverse)
devtools::load_all()
.d <- `[`

source(here("presentation", "presentation_plot_functions.R"))

chosen_method <- "expanding window"
chosen_em <- "absolute"

#############################################################################
#############################################################################
###########################  WIS PLOTS ######################################
#############################################################################
#############################################################################
scores <- fread(here("scores", "ci_scores.csv"))

wis_scores <- fread(here("scores", "ci_scores_avgcnt.csv")) |>
  data.table::copy() |>
  .d(, .(model, error_method, method, target, horizon, interval_score, dispersion, underprediction, overprediction)) |>
  setnames("model", "source")

bvar_wis_scores <- fread(here("scores", "bvar_ci_scores_avgcnt.csv")) |>
  data.table::copy() |>
  .d(, .(model, target, horizon, interval_score, dispersion, underprediction, overprediction)) |>
  .d(,model := "bvar_qu") |>
  setnames("model", "source") |>
  .d(,error_method := chosen_em) |>
  .d(,method := chosen_method) #change later


wis_scores <- rbind(wis_scores, bvar_wis_scores) |>
  .d(, source := factor(source, levels = c("IMF", "bvar", "bvar_qu", "ar"),
                        label = c("IMF", "BVAR", "BVAR - direct", "AR"))) |>
  .d(method == chosen_method) |>
  .d(error_method == chosen_em)


#generate plots
cpi_plot <- wis_plot_pres(wis_scores, "pcpi_pch")
gdp_plot <- wis_plot_pres(wis_scores, "ngdp_rpch")


#paste together and save as pdf
pdf(file = here("presentation", "figures", "wis_cpigdp_new_ew.pdf"), width = 9.5, height = 4)
ovr_plot <-
  (cpi_plot | gdp_plot) +
  plot_layout(guides = "collect",
              heights = c(1)) &
  plot_annotation(tag_levels = 'I')  &
  theme(legend.position = 'right',
        legend.box="vertical", legend.margin=margin())
ovr_plot
dev.off()



#############################################################################
#############################################################################
######################  COVERAGE PLOTS  #####################################
#############################################################################
#############################################################################

scores <- fread(here("scores", "ci_scores.csv"))
scores_cvgshort <- fread(here("scores", "cvg_pooled.csv")) |>
  rbind(fread(here("scores", "bvar_cvg_pooled.csv")) |>
          .d(, error_method := chosen_em) |>
          .d(, method := chosen_method))

cvg_rg <- c(50, 80)

cols <- paste0("coverage_", cvg_rg)

large_cvgdat <- scores |>
  setnames("model", "source") |>
  .d(, c("country", "horizon", "target", "error_method", "method", "source", cols), with = FALSE) |>
  melt(id.vars = c("country", "horizon", "target", "error_method", "method", "source"),
       variable.name = "pilvl",
       value.name = "coverage") |>
  .d(, pilvl := gsub("^.*?coverage_","",pilvl)) |>
  .d(, pilvl := as.numeric(pilvl)/100) |>
  .d(, idcol := paste0(horizon, country, method)) |>
  .d(, country := NULL) |>
  .d(, horizon := NULL) |>
  .d(error_method == chosen_em) |>
  .d(method == chosen_method)


cvgdat <- scores_cvgshort |>
  setnames("model", "source") |>
  .d(, c( "target", "error_method", "method", "source", cols), with = FALSE) |>
  melt(id.vars = c("target", "error_method", "method", "source"),
       variable.name = "pilvl",
       value.name = "coverage") |>
  .d(, pilvl := gsub("^.*?coverage_","",pilvl)) |>
  .d(, pilvl := as.numeric(pilvl)/100) |>
  .d(, source := factor(source, levels = c("IMF", "bvar", "bvar_qu", "ar"),
                        label = c("IMF", "BVAR", "BVAR - direct", "AR")))


cvgdat_cpi <- cvgdat |>
  .d(target == "pcpi_pch")|>
  .d(error_method == chosen_em) |>
  .d(method == chosen_method)

cvgdat_gdp <- cvgdat |>
  .d(target == "ngdp_rpch")|>
  .d(error_method == chosen_em) |>
  .d(method == chosen_method)


plot_cpi <- coverage_plot_pres(cvgdat_cpi, "pcpi_pch", large_cvgdat)
plot_gdp <- coverage_plot_pres(cvgdat_gdp, "ngdp_rpch", large_cvgdat)


pdf(file = here("presentation", "figures", "coverage_ew.pdf"), width = 9.5, height = 4)
ovr_plot <-
  (plot_cpi | plot_gdp) +
  plot_layout(guides = "collect",
              heights = c(1)) &
  plot_annotation(tag_levels = 'I')  &
  theme(legend.position = 'right',
        legend.box="vertical", legend.margin=margin())

ovr_plot
dev.off()
