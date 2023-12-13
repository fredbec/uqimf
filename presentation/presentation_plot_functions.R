wis_plot_pres <- function(wis_scoredat,
                         trgt = c("pcpi_pch", "ngdp_rpch"),
                         xmax = NULL,
                         plot_name = NULL){

  if(length(unique(wis_scoredat$error_method)) > 1){
    stop("Only one error method allowed. Prefilter data")
  }

  if(length(unique(wis_scoredat$method)) > 1){
    stop("Only one method allowed. Prefilter data")
  }

  if(length(trgt) > 1){
    stop("need to supply single value for trgt")
  }

  if(is.null(plot_name)){
    plot_name <- plot_target_label()[trgt]
  }

  decomp_data <- wis_scoredat |>
    .d(target == trgt) |>
    .d(, target := NULL) |>
    .d(, method := NULL) |>
    .d(, error_method := NULL)


  #prespecify xmax (otherwise, largest values are sometimes cut off)
  if(is.null(xmax)){
    xmax <- max(decomp_data$interval_score) + 0.02
  }

  decomp_data <- decomp_data |>
    .d(, .(source, horizon, dispersion, underprediction, overprediction)) |>
    data.table::melt(id.vars = c("source", "horizon"),
                     value.name = "score",
                     variable.name = "type") |>
    # is technically already filtered out, but just to make sure
    .d(type != "interval_score")



  decomp_plot <- ggplot(decomp_data, aes(x = source,
                                         fill = source,
                                         y = score,
                                         alpha = type)) +
    geom_bar(position="stack", stat="identity") +
    facet_wrap(~ horizon,
               labeller = as_labeller(plot_horizon_label_short(4)),
               nrow = 1) +
    scale_alpha_discrete(range = c(0.35,1),
                         labels = c("Overprediction",
                                    "Underprediction",
                                    "Dispersion"),
                         name = "Components of the WIS") +
    scale_x_discrete(name = "",
                     breaks = c("IMF", "BVAR", "BVAR - direct", "AR"),
                     labels = c("IMF", "BVAR", "BVAR - direct", "AR")) +

    scale_y_continuous(name = "Weighted Interval Score", limits = c(0, xmax)) +
    scale_fill_met_d("Hokusai1",
                     breaks = c("IMF", "BVAR", "BVAR - direct", "AR"),
                     labels = c("IMF", "BVAR", "BVAR - direct", "AR")) +
    xlab("") +
    theme_uqimf() %+replace%
    theme(axis.text.x = element_text(size = 8, angle = 90, hjust = .5, vjust = .5, face = "plain"),
          strip.text = element_text(size = 8)) +
    ggtitle(plot_name) +
    theme(legend.title=element_blank())# +
  #guides(fill = "none")

  return(decomp_plot)

}


coverage_plot_pres <- function(cvgdat_target,
                               target_name,
                               all_cvg){



  cvgplot <- ggplot() +
    geom_line(aes(x = pilvl,
                  y = coverage,
                  group = idcol),
              data = all_cvg |> .d(target == target_name & source == "IMF"),
              alpha = 0.4,
              color = "gray") +
    geom_point(aes(x = pilvl,
                   y = coverage,
                   color = source),
               data = cvgdat_target,
               size = 3) +
    geom_line(aes(x = pilvl,
                  y = coverage,
                  color = source),
              data = cvgdat_target,
              lwd = 1.25) +
    geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1),
                 color = "black",
                 linetype = "dashed",
                 data = cvgdat_target) +
    #scale_color_brewer(palette = "Set1") +

    ggtitle(plot_target_label()[target_name]) +

    scale_color_met_d("Hokusai1") +

    ylab("Empirical Coverage Level (Central PI)") +
    xlab("Nominal Coverage (Central PI)") +
    theme_uqimf()

  return(cvgplot)
}
