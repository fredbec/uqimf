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


chosen_method <- "rolling window"
chosen_em <- "directional"

######## WIS Data
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


#bvar_wis_scores <- rbind(bvar_wis_scores, bvar_wis_scores)# |> copy() |> .d(, error_method := chosen_em))

wis_scores <- rbind(wis_scores, bvar_wis_scores) |>
  .d(, source := factor(source, levels = c("IMF", "bvar", "bvar_qu", "ar"),
                        label = c("IMF", "BVAR", "BVAR - direct", "AR"))) |>
  .d(method == chosen_method) |>
  .d(error_method == chosen_em)


####Coverage Data
scores_cvgshort <- fread(here("scores", "ci_scores_avgcnt.csv")) |>
  setnames("model", "source", skip_absent = TRUE)


bvar_scores_cvgshort <- fread(here("scores", "bvar_ci_scores_avgcnt.csv")) |>
  setnames("model", "source", skip_absent = TRUE) |>
  .d(, source := "bvar_qu") |>
  .d(, method := chosen_method) |>
  .d(, error_method := chosen_em)


bvar_scores_cvgshort <- rbind(bvar_scores_cvgshort, bvar_scores_cvgshort |> copy() |> .d(, error_method := "directional"))


point_scores <- data.table::fread(here("scores", "pointfc_scores.csv"))

scores_cvgshort <- rbind(scores_cvgshort, bvar_scores_cvgshort)

cpi_plot <- ovr_assessment_plot(wis_scores, "pcpi_pch")
gdp_plot <- ovr_assessment_plot(wis_scores, "ngdp_rpch")



pdf(file = here("presentation", "figures", "wis_cpigdp_new_directional.pdf"), width = 9.5, height = 4)
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



#############block to merge with cvgdat symmetric

#cvgdat_cpin <- data.table::fread(here("scores", "cvg_pooled_directionalsymmetric.csv")) |>
#  setnames("model", "source") |>
#  .d(, c( "target", "error_method", "method", "source", cols), with = FALSE) |>
#  melt(id.vars = c("target", "error_method", "method", "source"),
#       variable.name = "pilvl",
#       value.name = "coverage") |>
#  .d(, pilvl := gsub("^.*?coverage_","",pilvl)) |>
#  .d(, pilvl := as.numeric(pilvl)/100) |>
#  .d(target == "pcpi_pch") |>
#  rbind(cvgdat_cpi)

#cvgdat_cpi <- cvgdat_cpin |>
#  .d(error_method == "absolute") |>
#  .d(method == "rolling window")


#cvgdat_gdpn <- data.table::fread(here("scores", "cvg_pooled_directionalsymmetric.csv")) |>
#  setnames("model", "source") |>
#  .d(, c( "target", "error_method", "method", "source", cols), with = FALSE) |>
#  melt(id.vars = c("target", "error_method", "method", "source"),
#       variable.name = "pilvl",
#       value.name = "coverage") |>
#  .d(, pilvl := gsub("^.*?coverage_","",pilvl)) |>
#  .d(, pilvl := as.numeric(pilvl)/100) |>
#  .d(target == "ngdp_rpch") |>
#  rbind(cvgdat_gdp)

#cvgdat_gdp <- cvgdat_gdpn|>
#  .d(error_method == "absolute") |>
#  .d(method == "rolling window")




plot_cpi <- ggplot() +
  geom_line(aes(x = pilvl,
                y = coverage,
                group = idcol),
            data = large_cvgdat |> .d(target == "pcpi_pch" & source == "IMF"),
            alpha = 0.4,
            color = "gray") +
  geom_point(aes(x = pilvl,
                 y = coverage,
                 color = source),
             data = cvgdat_cpi,
             size = 3) +
  geom_line(aes(x = pilvl,
                y = coverage,
                color = source),
            data = cvgdat_cpi,
            lwd = 1.25) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1),
               color = "black",
               linetype = "dashed",
               data = cvgdat_cpi) +
  #scale_color_brewer(palette = "Set1") +

  ggtitle("Inflation") +

  scale_color_met_d("Hokusai1") +

  ylab("Empirical Coverage Level (Central PI)") +
  xlab("Nominal Coverage (Central PI)") +
  theme_uqimf()


plot_gdp <- ggplot() +
  geom_line(aes(x = pilvl,
                y = coverage,
                group = idcol),
            data = large_cvgdat |> .d(target == "ngdp_rpch" & source == "IMF"),
            alpha = 0.4,
            color = "gray") +
  geom_point(aes(x = pilvl,
                 y = coverage,
                 color = source),
             data = cvgdat_gdp,
             size = 3) +
  geom_line(aes(x = pilvl,
                y = coverage,
                color = source),
            data = cvgdat_gdp,
            lwd = 1.25) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1),
               color = "black",
               linetype = "dashed",
               data = cvgdat_cpi) +
  #scale_color_brewer(palette = "Set1") +

  ggtitle("GDP Growth") +

  scale_color_met_d("Hokusai1") +

  ylab("Empirical Coverage Level (Central PI)") +
  xlab("Nominal Coverage (Central PI)") +
  theme_uqimf()


pdf(file = here("presentation", "figures", "coverage_directional.pdf"), width = 9.5, height = 4)
ovr_plot <-
  (plot_cpi | plot_gdp) +
  plot_layout(guides = "collect",
              heights = c(1)) &
  plot_annotation(tag_levels = 'I')  &
  theme(legend.position = 'right',
        legend.box="vertical", legend.margin=margin())

ovr_plot
dev.off()
