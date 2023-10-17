
ovr_assessment_plot <- function(wis_scoredat, trgt){


  decomp_data <- wis_scoredat |>
    #.d(method == "rolling window") |>
    .d(target == trgt) |>
    .d(, target := NULL) |>
    .d(, method := NULL) #|>


  xmax <- max(decomp_data$interval_score) + 0.05

  decomp_data <- decomp_data |>
    data.table::melt(id.vars = c("source", "error_method", "horizon"),
                     value.name = "score",
                     variable.name = "type") |>
    .d(type != "interval_score") # filter out total score, only interested in decomposition




  #return(decomp_data)
  dir_wis_plot <- wis_plot(decomp_data, "directional", xmax)
  abs_wis_plot <- wis_plot(decomp_data, "absolute", xmax)



  #ovr_plot <-
  #  (dir_wis_plot | abs_wis_plot) +
  #  plot_layout(guides = "collect",
  #              heights = c(0.2, 0.05, 0.2, 0.05, 0.2)) &
  #  plot_annotation(tag_levels = 'I')  &
  #  theme(legend.position = 'bottom',
  #        legend.box="vertical", legend.margin=margin())



  return(dir_wis_plot)
}


#check arguments of function!
wis_plot <- function(wis_scoredat, trgt, error_m){

  if(length(unique(wis_scoredat$error_method)) > 1){
    stop("only one error method allowed")
  }

  if(length(unique(wis_scoredat$method)) > 1){
    stop("only one method allowed")
  }

  if(trgt == "ngdp_rpch"){
    plot_name <- "GDP Growth"
  } else {
    plot_name <- "Inflation"
  }

  maxval <- max(wis_scoredat$interval_score) + 0.05

  decomp_data <- wis_scoredat |>
    .d(target == trgt) |>
    .d(, target := NULL) |>
    .d(, method := NULL)  |>
    setnames("model", "source") |>
    .d(, .(source, error_method, horizon, dispersion, underprediction, overprediction)) |>
    data.table::melt(id.vars = c("source", "error_method", "horizon"),
                     value.name = "score",
                     variable.name = "type") |>
    .d(type != "interval_score")



  plt <- ggplot(decomp_data,
                aes(x = source,
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
    scale_x_discrete(name = "") +
    #breaks = c("IMF", "bvar", "bvar_qu", "ar"),
    #labels = c("IMF", "BVAR", "BVAR_QU", "AR")) +
    scale_y_continuous(name = "Weighted Interval Score", limits = c(0, maxval)) +
    scale_fill_met_d("Hokusai1") +
    xlab("") +
    theme_uqimf() %+replace%
    theme(axis.text.x = element_text(size = 8, angle = 90, hjust = .5, vjust = .5, face = "plain"),
          strip.text = element_text(size = 8)) +
    ggtitle(plot_name) +
    theme(legend.position = "none")

  return(plt)

}





#' Plot proportions of 'monotonic horizon uncertainty' violators
#'
#' @param fcdat forecast data, as output from make-forecasts.R
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

