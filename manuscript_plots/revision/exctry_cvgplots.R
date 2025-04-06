library(data.table)
library(knitr)
library(kableExtra)
library(ggplot2)
library(MetBrewer)
library(patchwork)
library(here)
devtools::load_all()

prefix <- "extcntry_"


ciscores <- fread(here("scores", paste0(prefix, "ci_scores_ho.csv")))|>
  .d(, c("model", "target", "country", "horizon", "coverage_50", "coverage_80"))


ciscores2 <- fread(here("scores", paste0(prefix, "bvar_ci_scores_ho.csv"))) |>
  .d(, c("model", "target", "country", "horizon", "coverage_50", "coverage_80")) |>
  .d(, model := ifelse(model=="ar_annual", "Direct: AR-Annual", model))|>
  .d(, model := ifelse(model=="arx_annual", "Direct: ARX-Annual", model)) |>
  .d(, model := factor(model, levels = c("IMF", "Direct: AR-Annual", "Direct: ARX-Annual")))
ciscores <- rbind(ciscores, ciscores2)|>
  .d(, error_method := "absolute") |>
  .d(, method := "rolling window")

ciscores_avg <- fread(here("scores", paste0(prefix, "cvg_pooled_ho.csv"))) |>
  .d(, c("model", "target", "coverage_50", "coverage_80"))
ciscores_avg2 <- fread(here("scores", paste0(prefix, "bvar_cvg_pooled_ho.csv"))) |>
  .d(, c("model", "target", "coverage_50", "coverage_80")) |>
  .d(, model := ifelse(model=="ar_annual", "Direct: AR-Annual", model))|>
  .d(, model := ifelse(model=="arx_annual", "Direct: ARX-Annual", model))|>
  .d(, model := factor(model, levels = c("IMF", "Direct: AR-Annual", "Direct: ARX-Annual")))
ciscores_avg <- rbind(ciscores_avg, ciscores_avg2) |>
  .d(, error_method := "absolute") |>
  .d(, method := "rolling window")



plot_marg <- 5
fancycols <- met.brewer("Hokusai3", n = length(unique(ciscores_avg$model)))[2:length(unique(ciscores_avg$model))]  # Get 10 colors from VanGogh1 palette
imfcol <-  met.brewer("Hokusai3", n = 4)[1]

modnames <- unique(ciscores_avg$model)
modnames <- modnames[modnames != "IMF"]

colors_manual <- c("IMF" = imfcol, setNames(fancycols, modnames))


tgt <- "pcpi_pch"
cvgplot <- coverage_plot_aggregate(ciscores_avg,
                                   ciscores,
                                   cvg_rg = c(50,80),
                                   tgt,
                                   textsize_y = 17,
                                   plot_title = "",
                                   manual_scale = colors_manual,
                                   metcolor = "Hokusai3",
                                   minusvalpos = 2) +
  theme(plot.margin = margin(t=0,b=0,r=plot_marg,l=plot_marg, unit = "pt"))

cvgplot
ggsave(here("manuscript_plots", "revision", "results", paste0("extctry_cvgplot_", tgt, ".pdf")), width = 8.3, height = 4.5)
ggsave(here("..", "uqimf-manuscript", "figures", paste0("extctry_cvgplot_", tgt, ".pdf")), width = 8.3, height = 4.5)


tgt <- "ngdp_rpch"
cvgplot <- coverage_plot_aggregate(ciscores_avg,
                                   ciscores,
                                   cvg_rg = c(50,80),
                                   tgt,
                                   textsize_y = 17,
                                   plot_title = "",
                                   manual_scale = colors_manual,
                                   metcolor = "Hokusai3") +
  theme(plot.margin = margin(t=0,b=0,r=plot_marg,l=plot_marg, unit = "pt"))


ggsave(here("manuscript_plots", "revision", "results", paste0("extctry_cvgplot_", tgt, ".pdf")),plot = cvgplot, width = 8.3, height = 4.5)
ggsave(here("..", "uqimf-manuscript", "figures", paste0("extctry_cvgplot_", tgt, ".pdf")), plot = cvgplot, width = 8.3, height = 4.5)
