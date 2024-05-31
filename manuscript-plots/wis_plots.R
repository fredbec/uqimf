library(data.table)
library(here)
library(ggplot2)
library(patchwork)
library(MetBrewer)
devtools::load_all()
.d <- `[`


chosen_method <- "rolling window"
chosen_em <- "directional"
chosen_target <- "ngdp_rpch"
#prefix <- "ho_"

cscale <- "Hokusai3"


#when using absolute errors, read in scores of pava corrected forecasts
if(chosen_em == "absolute"){
  pasteon_filename <- "_pava"
} else {
  pasteon_filename <- ""
}


######## WIS Data
scores <- fread(here("scores", "ci_scores_pava.csv"))


wis_scores <- fread(here("scores", paste0("ci_scores_avgcnt", pasteon_filename, ".csv"))) |>
  data.table::copy() |>
  .d(, .(model, error_method, method, target, horizon, interval_score, dispersion, underprediction, overprediction)) |>
  setnames("model", "source")


bvar_wis_scores <- fread(here("scores", "bvar_ci_scores_avgcnt.csv")) |>
  data.table::copy() |>
  .d(, .(model, target, horizon, interval_score, dispersion, underprediction, overprediction)) |>
  .d(model == "bvar_qu") |>
  setnames("model", "source") |>
  .d(,error_method := chosen_em) |>
  .d(,method := chosen_method) #change later


wis_scores <- rbind(wis_scores, bvar_wis_scores) |>
  .d(, source := factor(source, levels = c("IMF", "bvar", "bvar_qu", "ar"),
                        label = c("IMF", "BVAR", "BVAR - direct", "AR"))) |>
  .d(method == chosen_method) |>
  .d(error_method == chosen_em)

colors_manual <- met.brewer(cscale, 4)
names(colors_manual) <- unique(wis_scores$source)


wis_plot <- wis_plot_new(wis_scores, manual_scale = colors_manual, chosen_target, plot_name = "", textsize_y = 17) +
  theme(axis.text.x = element_blank(),
        #axis.text.x = element_text(size = textsize_y, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        #strip.text = element_text(size = 8),
        axis.text.y = element_text(size = 24),
        axis.title.y = element_text(size = 24),
        strip.text = element_text(size=24),
        legend.text=element_text(size=24),
        text = element_text(family = "serif"),
        legend.title=element_blank(),
        plot.margin = margin(t=5,b=5,r=5,l=5, unit = "pt"))


pdf(here("..", "uqimf-manuscript", "figures", paste0(prefix, "wis_", chosen_target,"_", chosen_em, "_", gsub(" ", "",chosen_method), ".pdf")),
    height = 8, width = 9)
wis_plot
dev.off()
