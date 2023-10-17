#' Plot decomposition of WIS by horizon and forecast source
#'
#' @param score_data score data that includes WIS values, prefiltered! by desired
#' error_method and method
#' @param trgt target variable for which the plot should be produced
#' @param tolerance deviation to be exceeded, to be considered violator
#' @param exclude_h1.5 if TRUE, excludes the largest horizon to calculate violators
#' @param fcsource forecast source for which to calculate violators
#' @param upper_lim upper limit for plotting legend
#' @return nothing, saves tidied data in directory
#' @export
#'
wis_plot_new <- function(wis_scoredat,
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
                     breaks = c("IMF", "bvar", "bvar_qu", "ar"),
                     labels = c("IMF", "BVAR", "BVAR_QU", "AR")) +

    scale_y_continuous(name = "Weighted Interval Score", limits = c(0, xmax)) +
    scale_fill_met_d("Hokusai1",
                     breaks = c("IMF", "bvar", "bvar_qu", "ar"),
                     labels = c("IMF", "BVAR", "BVAR_QU", "AR")) +
    xlab("") +
    theme_uqimf() %+replace%
    theme(axis.text.x = element_text(size = 8, angle = 90, hjust = .5, vjust = .5, face = "plain"),
          strip.text = element_text(size = 8)) +
    ggtitle(plot_name) +
    theme(legend.title=element_blank()) +
    guides(fill = "none")

    return(decomp_plot)

}



#' Plot proportions of 'monotonic horizon uncertainty' violators
#'
#' @param fcdat forecast data, as output from make-forecasts.R
#' Note: this can be DIRECTLY used with quantile_forecasts.csv, no preprocess needed
#' @param cilvl CI level for which to calculate violations
#' @param tolerance deviation to be exceeded, to be considered violator
#' @param exclude_h1.5 if TRUE, excludes the largest horizon to calculate violators
#' @param fcsource forecast source for which to calculate violators
#' @param upper_lim upper limit for plotting legend
#' @return nothing, saves tidied data in directory
#' @export
#'

horizon_violators <- function(fcdat,
                              cilvl,
                              tolerance,
                              exclude_h1.5 = FALSE,
                              fcsource = "IMF",
                              upper_lim = 0.601){

  #get violatorname (for pipe later on )
  if(exclude_h1.5){
    violator_name <- "violator_minor"
    plot_title <- "Without horizon 'Spring, year ahead'"
  } else {
    violator_name <- "violator_all"
    plot_title <- "All horizons"
  }

  qus <- ci_to_quantiles(cilvl, "directional") |>
    #needed because of floating point error
    as.character() |>
    as.numeric()


  violations <- fcdat |>
    #filter relevant quantiles
    .d(quantile %in% qus) |>
    #only keep relevant variables
    .d(, .(country, target, target_year, horizon, error_prediction,
           source, error_method, method, quantile)) |>
    .d(, quantile := paste0("q", quantile)) |>
    #wide formal
    dcast(country + target + target_year + horizon +
            error_method + method + source ~ quantile,
          value.var = "error_prediction") |>

    #calculate CI length
    .d(, cil := get(paste0("q", qus[2])) - get(paste0("q", qus[1]))) |>
    .d(, .(country, target, target_year, horizon, source, error_method, method, cil)) |>

    #bring into wide formal by horizon
    .d(, horizon := paste0("h", horizon))|>
    dcast(country + target + target_year +
            error_method + method + source ~ horizon,
          value.var = "cil") |>

    #calculate violators
    .d(, violator_all := ifelse(h0.5 < h0 - tolerance |
                              h1 < h0.5 - tolerance |
                              h1.5 < h1 - tolerance, 1, 0)) |>
    #minor violators exclude largest horizon
    .d(, violator_minor := ifelse(h0.5 < h0 - tolerance |
                                  h1 < h0.5 - tolerance,  1, 0)) |>

    #calculate proportion of violators
    .d(, meanviol := mean(get(violator_name)),
       by = c("target", "error_method", "method", "source")) |>

    #keep only relevant variables and get distinct values
    .d(, .(target , source, error_method, method, meanviol)) |>
    unique() |>
    .d(source == fcsource)



  #############################
  viol_plot <- ggplot(aes(x = error_method, y = method, fill = meanviol), data = violations) +
    geom_tile() +
    facet_wrap(~target,
               labeller = as_labeller(plot_target_label())) +
    ggtitle(paste0("Deviation tolerance = ", tolerance, "; ", plot_title)) +
    scale_fill_continuous(limits = c(0, upper_lim)) +
    xlab("") +
    ylab("") +
    theme_uqimf()

  return(viol_plot)

}


#' Plot values of scoring rule / function / other metric over horizon, by
#' forecast source (color)
#'
#' @param score_data data that contains scores, as output from score-forecasts).
#' Can either by score data or coverage data, but for each need to supply version
#' that is averaged by country
#' @param score_rule name of the scoring rule to plot (must be in score_data, obvs)
#' @param ylab name for plot y axis (score_rule by default)
#' @param facet optional, variable to facet by
#' @param title optional, title for plot
#' @param metcolor colorscale from Metcolorbrewer package
#' @return plot
#' @export
#' Note to self: currently used in manuscript.Rmd
#'
horizon_path_plot <- function(score_data,
                              score_rule,
                              ylab = score_rule,
                              facet = NULL,
                              title = NULL,
                              metcolor = "Hokusai1"){

  path_plot <- ggplot(score_data,
                      aes(x = horizon,
                          y = get(score_rule),
                          color = source)) +

    scale_color_met_d(metcolor, breaks = c("IMF", "bvar", "ar", "bvar_qu"),
                      labels = c("IMF", "BVAR", "AR", "BVAR_QU")) +
    geom_line() +
    geom_point() +
    ylab(ylab) +
    xlab("Forecast Horizon") +
    guides(fill = "none", colour = "none") +
    theme_uqimf() %+replace%
    theme(legend.title=element_blank(),
          legend.position = "none") +
    ggtitle(title)

  if(!is.null(facet)){

    path_plot <- path_plot +
      facet_wrap(~ get(facet),
                 scales = "free")
  }

  return(path_plot)

}
