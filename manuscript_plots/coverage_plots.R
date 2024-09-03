coverage_plot_paper <- function(dataset_suffix,
                                chosen_target,
                                cscale = "Hokusai3"){

  if(nchar(dataset_suffix) > 0){

    dataset_suffix <- paste0("_", dataset_suffix)
  }

  if (dataset_suffix == "_directional"){

    bvar_dataset_suffix <- ""
  } else {

    bvar_dataset_suffix <- dataset_suffix
  }

  ####Read and filter coverage data
  #Scores by average country
  scores_cvgshort <- fread(here("scores", paste0(global_file_prefix, "ci_scores_avgcnt", dataset_suffix, ".csv"))) ##|>
    #setnames("model", "source", skip_absent = TRUE)

  bvar_scores_cvgshort <- fread(here("scores", paste0(global_file_prefix, "bvar_ci_scores_avgcnt", bvar_dataset_suffix, ".csv"))) |>
    #setnames("model", "source", skip_absent = TRUE) |>
    .d(model == "bvar_qu") |>
    .d(,error_method := unique(scores_cvgshort$error_method)) |>
    .d(,method := unique(scores_cvgshort$method))

  scores_cvgshort <- rbind(scores_cvgshort, bvar_scores_cvgshort) |>
    #setnames("model", "source", skip_absent = TRUE) |>
    .d(target == chosen_target) |>
    .d(, model := factor(model, levels = c("IMF", "bvar", "bvar_qu", "ar"),
                         label = c("IMF", "BVAR", "BVAR - direct", "AR"),
                         ordered = TRUE))



  scores_cvgaggregate <- fread(here("scores", paste0(global_file_prefix, "cvg_pooled", dataset_suffix, ".csv"))) |>
    rbind(fread(here("scores", paste0(global_file_prefix, "bvar_cvg_pooled", bvar_dataset_suffix, ".csv"))) |>
            .d(,error_method := unique(scores_cvgshort$error_method)) |>
            .d(,method := unique(scores_cvgshort$method)) |>
            .d(model == "bvar_qu")) |>
    .d(, model := factor(model, levels = c("IMF", "bvar", "bvar_qu", "ar"),
                          label = c("IMF", "BVAR", "BVAR - direct", "AR"),
                         ordered = TRUE))


  #non aggregated
  scores <- fread(here("scores", paste0(global_file_prefix, "ci_scores", dataset_suffix, ".csv"))) |>
    rbind(fread(here("scores", paste0(global_file_prefix, "bvar_ci_scores", bvar_dataset_suffix, ".csv"))) |>
            .d(,error_method := unique(scores_cvgshort$error_method)) |>
            .d(,method := unique(scores_cvgshort$method)) |>
                 .d(model == "bvar_qu")) |>
    .d(, model := factor(model, levels = c("IMF", "bvar", "bvar_qu", "ar"),
                          label = c("IMF", "BVAR", "BVAR - direct", "AR"),
                         ordered = TRUE))

  #############################################################################
  plot_marg <- 5
  colors_manual <- met.brewer(cscale, 4)
  names(colors_manual) <- unique(scores_cvgaggregate$model)


  cvgplot <- coverage_plot_aggregate(scores_cvgaggregate,
                           scores,
                           cvg_rg = c(50,80),
                           chosen_target,
                           textsize_y = 17,
                           plot_title = "",
                           manual_scale = colors_manual,
                           metcolor = "Hokusai3") +
    theme(plot.margin = margin(t=0,b=0,r=plot_marg,l=plot_marg, unit = "pt"))


  horpathplot50 <- horizon_path_plot(scores_cvgshort,
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

  horpathplot80 <- horizon_path_plot(scores_cvgshort,
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

  return(ovr_plot)

#pdf(here("..", "uqimf-manuscript", "figures", paste0(prefix, "coverage_", chosen_target,"_", chosen_em, "_", gsub(" ", "",chosen_method), ".pdf")), width = 12, height = 4.3)

#ovr_plot
#dev.off()
}
