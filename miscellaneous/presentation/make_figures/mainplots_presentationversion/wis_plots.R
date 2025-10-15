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

  wis_scores <- fread(here("scores", paste0(global_file_prefix, "ci_scores_avgcnt", dataset_suffix, ".csv"))) |>
    data.table::copy() |>
    .d(, .(model, error_method, method, target, horizon, interval_score, dispersion, underprediction, overprediction)) |>
    setnames("model", "source") |>
    .d(source %in% c("IMF", "bvar_mix", "mean_ensemble"))


  bvar_wis_scores <- fread(here("scores", paste0(global_file_prefix, "bvar_ci_scores_avgcnt", bvar_dataset_suffix, ".csv"))) |>
    data.table::copy() |>
    .d(, .(model, target, horizon, interval_score, dispersion, underprediction, overprediction)) |>
    .d(model %in% c("ar", "bvar_mix", "arx_annual")) |>
    .d(, model := paste0(model, "-direct")) |>
    setnames("model", "source") |>
    .d(,error_method := unique(wis_scores$error_method)) |>
    .d(,method := unique(wis_scores$method))


  wis_scores <- rbind(wis_scores, bvar_wis_scores) |>
    .d(, source := factor(source, levels = c("IMF", "bvar_mix", "bvar_mix-direct", "ar-direct", "arx_annual-direct", "mean_ensemble"),
                          label = c("IMF", "BVAR-Mix", "BVAR-Mix - direct", "AR - direct", "AR-X", "Ensemble")))

  colors_manual <- met.brewer(cscale, 4)[1]
  colors_manual2 <- met.brewer("Hokusai2", 5)
  colors_manual <- c(colors_manual, colors_manual2)
  names(colors_manual) <- unique(wis_scores$source)


  wis_plot <- wis_plot_new(wis_scores, manual_scale = colors_manual, chosen_target, plot_name = "", textsize_y = 17,
                           font_family = "sans") +
    theme(axis.text.x = element_blank(),
          #axis.text.x = element_text(size = textsize_y, angle = 90, hjust = .5, vjust = .5, face = "plain"),
          #strip.text = element_text(size = 8),
          axis.text.y = element_text(size = 24),
          axis.title.y = element_text(size = 24),
          strip.text = element_text(size=24),
          legend.text=element_text(size=24),
          text = element_text(family = "sans"),
          legend.title=element_blank(),
          plot.margin = margin(t=5,b=5,r=5,l=5, unit = "pt"))

  return(wis_plot)
}
