coverage_plot_paper <- function(chosen_method,
                                chosen_em,
                                chosen_target,
                                prefix,
                                cscale = "Hokusai3"){

  #when using absolute errors, read in scores of pava corrected forecasts
  if(chosen_em == "absolute"){
    pasteon_filename <- "_pava"
  } else {
    pasteon_filename <- ""
  }

  ####Read and filter coverage data
  #Scores by average country
  scores_cvgshort <- fread(here("scores", paste0("ci_scores_avgcnt", pasteon_filename, ".csv"))) ##|>
    #setnames("model", "source", skip_absent = TRUE)

  bvar_scores_cvgshort <- fread(here("scores", "bvar_ci_scores_avgcnt.csv")) |>
    #setnames("model", "source", skip_absent = TRUE) |>
    .d(model == "bvar_qu") |>
    .d(, method := chosen_method) |>
    .d(, error_method := chosen_em)

  scores_cvgshort <- rbind(scores_cvgshort, bvar_scores_cvgshort) |>
    #setnames("model", "source", skip_absent = TRUE) |>
    .d(error_method == chosen_em & method == chosen_method & target == chosen_target) |>
    .d(, model := factor(model, levels = c("IMF", "bvar", "bvar_qu", "ar"),
                         label = c("IMF", "BVAR", "BVAR - direct", "AR"),
                         ordered = TRUE))



  scores_cvgaggregate <- fread(here("scores", paste0("cvg_pooled", pasteon_filename, ".csv"))) |>
    rbind(fread(here("scores", "bvar_cvg_pooled.csv")) |>
            .d(, error_method := chosen_em) |>
            .d(, method := chosen_method) |>
            .d(model == "bvar_qu")) |>
    .d(error_method == chosen_em) |>
    .d(method == chosen_method) |>
    .d(, model := factor(model, levels = c("IMF", "bvar", "bvar_qu", "ar"),
                          label = c("IMF", "BVAR", "BVAR - direct", "AR"),
                         ordered = TRUE))


  #non aggregated
  scores <- fread(here("scores", paste0("ci_scores", pasteon_filename, ".csv"))) |>
    rbind(fread(here("scores", "bvar_ci_scores.csv"))|>
            .d(, error_method := chosen_em) |>
            .d(, method := chosen_method) |>
            .d(model == "bvar_qu")) |>
    .d(error_method == chosen_em) |>
    .d(method == chosen_method) |>
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
