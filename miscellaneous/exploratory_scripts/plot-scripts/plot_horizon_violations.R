library(data.table)
library(here)
library(ggplot2)
library(patchwork)
devtools::load_all()

.d <- `[`


plot_width <- 7
plot_height <- 10

make_ovrplot <- function(plot1, plot2, plot3){
  ovrplot <-
    (plot1) /
    (plot2) /
    (plot3) +
    plot_layout(guides = "collect",
                heights = c(1, 1, 1)) &
    plot_annotation(tag_levels = 'I')  &
    theme(legend.position = 'bottom',
          legend.box="vertical", legend.margin=margin())
  ovrplot
}


qufcs <- fread(here("quantile_forecasts", "quantile_forecasts.csv"))

#################CI 0.8#########################################
plot_0 <- horizon_violators(qufcs, 0.8, 0, exclude_h1.5 = FALSE)
plot_wo15 <- horizon_violators(qufcs, 0.8, 0, exclude_h1.5 = TRUE)
plot_025 <- horizon_violators(qufcs, 0.8, 0.25, exclude_h1.5 = FALSE)

pdf(file = here("plots", "horizon_violations_ci8_windowlength9.pdf"), width = plot_width, height = plot_height)
make_ovrplot(plot_0, plot_wo15, plot_025) +
   plot_annotation(
    title = 'CI 80, Window Length 9')
dev.off()

#################CI 0.5#########################################
plot_0 <- horizon_violators(qufcs, 0.5, 0, exclude_h1.5 = FALSE)
plot_wo15 <- horizon_violators(qufcs, 0.5, 0, exclude_h1.5 = TRUE)
plot_025 <- horizon_violators(qufcs, 0.5, 0.25, exclude_h1.5 = FALSE)

pdf(file = here("plots", "horizon_violations_ci5_windowlength9.pdf"), width = plot_width, height = plot_height)
make_ovrplot(plot_0, plot_wo15, plot_025) +
  plot_annotation(
    title = 'CI 50, Window Length 9')
dev.off()


qufcs <- fread(here("quantile_forecasts", "quantile_forecasts21.csv"))

#################CI 0.8#########################################
plot_0 <- horizon_violators(qufcs, 0.8, 0, exclude_h1.5 = FALSE)
plot_wo15 <- horizon_violators(qufcs, 0.8, 0, exclude_h1.5 = TRUE)
plot_025 <- horizon_violators(qufcs, 0.8, 0.25, exclude_h1.5 = FALSE)

pdf(file = here("plots", "horizon_violations_ci8_windowlength21.pdf"), width = plot_width, height = plot_height)
make_ovrplot(plot_0, plot_wo15, plot_025) +
  plot_annotation(
    title = 'CI 80, Window Length 21')
dev.off()

#################CI 0.5#########################################
plot_0 <- horizon_violators(qufcs, 0.5, 0, exclude_h1.5 = FALSE)
plot_wo15 <- horizon_violators(qufcs, 0.5, 0, exclude_h1.5 = TRUE)
plot_025 <- horizon_violators(qufcs, 0.5, 0.25, exclude_h1.5 = FALSE)

pdf(file = here("plots", "horizon_violations_ci5_windowlength21.pdf"), width = plot_width, height = plot_height)
make_ovrplot(plot_0, plot_wo15, plot_025) +
  plot_annotation(
    title = 'CI 50, Window Length 21')
dev.off()
