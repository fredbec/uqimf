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


scores_cvgshort <- fread(here("scores", paste0(prefix, "ci_scores_avgcnt_ho.csv"))) |>
  .d(model %in% c("IMF")) ##|>
#setnames("model", "source", skip_absent = TRUE)

bvar_scores_cvgshort <- fread(here("scores", paste0(prefix, "bvar_ci_scores_avgcnt_ho.csv"))) |>
  #.d(, model := ifelse(model=="ar_annual", "Direct: AR-Annual", model))|>
  #.d(, model := ifelse(model=="arx_annual", "Direct: ARX-Annual", model)) |>
  .d(,error_method := unique(scores_cvgshort$error_method)) |>
  .d(,method := unique(scores_cvgshort$method))

scores_cvgshort <- rbind(scores_cvgshort, bvar_scores_cvgshort) |>
  #setnames("model", "source", skip_absent = TRUE) |>
  .d(, model := factor(model, levels = c("IMF", "ar_annual", "arx_annual"),
                       label = c("IMF", "Direct: AR-Annual", "Direct: ARX-Annual"),
                       ordered = TRUE))



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
                                   minusvalpos = 2,
                                   font_family = "sans") +
  theme(plot.margin = margin(t=0,b=0,r=plot_marg,l=plot_marg, unit = "pt"))


scores_cvgshort_cpi <- scores_cvgshort |>
  .d(target == tgt)

horpathplot50 <- horizon_path_plot(scores_cvgshort_cpi,
                                   "coverage_50", "50% PI - Coverage", manual_scale = colors_manual, metcolor = "Hokusai3", lintype = "88") +
  geom_hline(aes(yintercept = 0.5),
             #lwd = 0.75,
             linetype = "solid") +
  scale_x_continuous(breaks = c(0, 0.5, 1, 1.5),
                     labels = c("Fall,\nCurrent", "Spring,\nCurrent", "Fall,\nNext", "Spring,\nNext")) +
  scale_y_continuous(#labels = paste0(seq(0, 100, by = 25), "%"),
    labels = paste0(seq(0, 1, by = 0.25)),
    breaks = seq(0,1, by = 0.25),
    limits = c(0,1)) +
  theme(plot.margin = margin(t=0,b=0,r=plot_marg,l=plot_marg, unit = "pt"))

horpathplot80 <- horizon_path_plot(scores_cvgshort_cpi,
                                   "coverage_80", "80% PI - Coverage",manual_scale = colors_manual, metcolor = "Hokusai3", lintype = "88") +
  geom_hline(aes(yintercept = 0.8),
             #lwd = 0.75,
             linetype = "solid") +
  scale_x_continuous(breaks = c(0, 0.5, 1, 1.5),
                     labels = c("Fall,\nCurrent", "Spring,\nCurrent", "Fall,\nNext", "Spring,\nNext")) +
  scale_y_continuous(#labels = paste0(seq(0, 100, by = 25), "%"),
    labels = paste0(seq(0, 1, by = 0.25)),
    breaks = seq(0,1, by = 0.25),
    limits = c(0,1))+
  theme(plot.margin = margin(t=0,b=0,r=plot_marg,l=plot_marg, unit = "pt"))


ovr_plot <-
  (cvgplot + horpathplot50 + horpathplot80) +
  plot_layout(guides = "collect",
              heights = c(1)) &
  plot_annotation(tag_levels = 'I')  &
  theme(legend.position = 'bottom',
        legend.box="vertical", legend.margin=margin())
cvgplot

ggsave(here(paste0("extctry_cvgplot_", tgt, ".pdf")), width = 5.2, height = 4)
#ggsave(here("..", "uqimf-manuscript", "figures", paste0("extctry_cvgplot_", tgt, ".pdf")), width = 12, height = 4.3)


tgt <- "ngdp_rpch"
cvgplot <- coverage_plot_aggregate(ciscores_avg,
                                   ciscores,
                                   cvg_rg = c(50,80),
                                   tgt,
                                   textsize_y = 17,
                                   plot_title = "",
                                   manual_scale = colors_manual,
                                   metcolor = "Hokusai3",
                                   minusvalpos = 2,
                                   font_family = "sans") +
  theme(plot.margin = margin(t=0,b=0,r=plot_marg,l=plot_marg, unit = "pt"))


scores_cvgshort_gdp <- scores_cvgshort |>
  .d(target == tgt)

horpathplot50 <- horizon_path_plot(scores_cvgshort_gdp,
                                   "coverage_50", "50% PI - Coverage", manual_scale = colors_manual, metcolor = "Hokusai3", lintype = "88") +
  geom_hline(aes(yintercept = 0.5),
             #lwd = 0.75,
             linetype = "solid") +
  scale_x_continuous(breaks = c(0, 0.5, 1, 1.5),
                     labels = c("Fall,\nCurrent", "Spring,\nCurrent", "Fall,\nNext", "Spring,\nNext")) +
  scale_y_continuous(#labels = paste0(seq(0, 100, by = 25), "%"),
    labels = paste0(seq(0, 1, by = 0.25)),
    breaks = seq(0,1, by = 0.25),
    limits = c(0,1)) +
  theme(plot.margin = margin(t=0,b=0,r=plot_marg,l=plot_marg, unit = "pt"))

horpathplot80 <- horizon_path_plot(scores_cvgshort_gdp,
                                   "coverage_80", "80% PI - Coverage",manual_scale = colors_manual, metcolor = "Hokusai3", lintype = "88") +
  geom_hline(aes(yintercept = 0.8),
             #lwd = 0.75,
             linetype = "solid") +
  scale_x_continuous(breaks = c(0, 0.5, 1, 1.5),
                     labels = c("Fall,\nCurrent", "Spring,\nCurrent", "Fall,\nNext", "Spring,\nNext")) +
  scale_y_continuous(#labels = paste0(seq(0, 100, by = 25), "%"),
    labels = paste0(seq(0, 1, by = 0.25)),
    breaks = seq(0,1, by = 0.25),
    limits = c(0,1))+
  theme(plot.margin = margin(t=0,b=0,r=plot_marg,l=plot_marg, unit = "pt"))

ovr_plot <-
  (cvgplot + horpathplot50 + horpathplot80) +
  plot_layout(guides = "collect",
              heights = c(1)) &
  plot_annotation(tag_levels = 'I')  &
  theme(legend.position = 'bottom',
        legend.box="vertical", legend.margin=margin())
cvgplot

ggsave(here(paste0("extctry_cvgplot_", tgt, ".pdf")), width = 5.2, height = 4)
#ggsave(here("..", "uqimf-manuscript", "figures", paste0("extctry_cvgplot_", tgt, ".pdf")), width = 12, height = 4.3)
