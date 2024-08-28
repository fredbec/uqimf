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

  return(cvgplot)

  #pdf(here("..", "uqimf-manuscript", "figures", paste0(prefix, "coverage_", chosen_target,"_", chosen_em, "_", gsub(" ", "",chosen_method), ".pdf")), width = 12, height = 4.3)

  #ovr_plot
  #dev.off()
}
