source(here("R", "plot.R"))

wis_plot_paper <- function(dataset_suffix,
                           chosen_target,
                           prefix,
                           cscale = "Hokusai3"){


  ######## WIS Data
  #scores <- fread(here("scores", "ci_scores_pava.csv"))

  if(nchar(dataset_suffix) > 0){

    dataset_suffix <- paste0("_", dataset_suffix)
  }

  if (dataset_suffix == "_directional"){

    bvar_dataset_suffix <- ""
  } else {

    bvar_dataset_suffix <- dataset_suffix
  }

  wis_scores <- fread(here("scores", paste0("ci_scores_avgcnt", dataset_suffix, ".csv"))) |>
    data.table::copy() |>
    .d(, .(model, error_method, method, target, horizon, interval_score, dispersion, underprediction, overprediction)) |>
    setnames("model", "source")


  bvar_wis_scores <- fread(here("scores", paste0("bvar_ci_scores_avgcnt", bvar_dataset_suffix, ".csv"))) |>
    data.table::copy() |>
    .d(, .(model, target, horizon, interval_score, dispersion, underprediction, overprediction)) |>
    .d(model == "bvar_qu") |>
    setnames("model", "source") |>
    .d(,error_method := unique(wis_scores$error_method)) |>
    .d(,method := unique(wis_scores$method))


  wis_scores <- rbind(wis_scores, bvar_wis_scores) |>
    .d(, source := factor(source, levels = c("IMF", "bvar", "bvar_qu", "ar"),
                          label = c("IMF", "BVAR", "BVAR - direct", "AR")))

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

  return(wis_plot)
}
