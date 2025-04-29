#' Convert CI levels to quantiles for empQU function
#'
#' @param ci_levels desired confidence interval levels
#' @param error_method either 'directional' or 'absolute'
#' @return vector with quantiles
#' @export
#'

ci_to_quantiles <- function(ci_levels,
                            error_method){

  if(error_method == "directional"){

    qu_down <- 0.5 - ci_levels / 2
    qu_up <- 0.5 + ci_levels / 2

    qus <- c(qu_down, qu_up) |>
      unique() |> #in case of median
      sort()

  } else if (error_method == "absolute"){

    qus <- ci_levels
  }

  return(round(qus, 2))
}




#' Produce year sets depending on respective estimation method
#'
#' @param target_year year for which estimation set should be produced
#' @param avail_years all available years
#' @param method estimation method, one of 'leave-one-out', 'rolling window',
#' 'expanding window'
#' @return vector with years that should enter estimation set for given target_year
#' @export
#'
year_set <- function(target_year,
                     avail_years,
                     method = c("leave-one-out", "rolling window", "expanding window"),
                     window_length = NULL){

  if(length(method) != 1L){
    stop("need to specify method. Available options: 'leave-one-out', 'rolling window', 'expanding window'")
  }

  if( !all(order(avail_years) == 1:length(avail_years))){

    message("avail_years will be put in order")
    avail_years <- sort(avail_years)
  }

  idx_target = which(avail_years == target_year)

  if(method == "rolling window"){

    if(is.null(window_length)){
      stop("no value for window_length")
    }

    if(idx_target < (window_length + 1)){

      stop("not enough available years for given window_length")
    }
  }

  if(method == "leave-one-out"){

    avail_years[-idx_target]
  } else if(method == "rolling window"){

    avail_years[(idx_target-window_length):(idx_target-1)]
  } else if(method == "expanding window"){

    avail_years[1:(idx_target-1)]
  }
}


#' @title ugimf ggplot2 theme
#'
#' @description
#' A theme for ggplot2 plotss
#' @return A ggplot2 theme
#' @importFrom ggplot2 theme theme_minimal element_line `%+replace%`
#' @export
#'
theme_uqimf <- function() {
  theme_minimal() %+replace%
    theme(axis.line = element_line(colour = "grey80"),
          axis.ticks = element_line(colour = "grey80"),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.position = "bottom",
          legend.title=element_blank())
}


custom_labeller <- function(variable, value) {
  if (variable == "season") {
    if (value == 0) {
      return("Fall forecast, same year")
    } else if (value == 0.5) {
      return("Spring forecast, same year")
    }
  }
  return(as.character(value))  # Default labelling for other variables
}

fperror <- function(type = 0.1){

  if(type == 0.1){

    return(-2.77e-17)
  }
}



plot_horizon_label <- function(length.out = 4){

  phl <- c(`0` = "Fall forecast, same year",
           `0.5` = "Spring forecast, same year",
           `1`= "Fall forecast, year ahead",
           `1.5`= "Spring forecast, year ahead")

  return(phl[1:length.out])
}


plot_horizon_label_short <- function(length.out = 4){

  phl <- c(`0` = "F, same year",
           `0.5` = "S, same year",
           `1`= "F, year ahead",
           `1.5`= "S, year ahead")

  return(phl[1:length.out])
}

plot_horizon_label_shorter <- function(length.out = 4){

  phl <- c(`0` = "Fall,\nCurrent",
           `0.5` = "Spring\nCurrent",
           `1`= "Fall,\nNext",
           `1.5`= "Spring,\nNext")

  return(phl[1:length.out])
}

plot_target_label <- function(){

  ptl <- c(`pcpi_pch` = "Inflation",
          `ngdp_rpch` = "Real GDP Growth")

  return(ptl)
}


plot_country_label <- function(){

  pcl <- c(`CAN` = "Canada",
           `DEU` = "Germany",
           `FRA` = "France",
           `GBR` = "United Kingdom",
           `ITA` = "Italy",
           `JPN` = "Japan",
           `USA` = "United States")

  return(pcl)
}

#' @title
#'
#' @description
#' Function to generate a list of quantiles and alpha values for geom_linerange
#'
qu_lvls <- function(cis){

  qus <- ci_to_quantiles(cis, "directional") #always directional

  n_qus <- length(qus)

  no_pairs <- floor(n_qus/2)


  alpha_vals <- seq(0.5, 0.75, length.out = no_pairs)

  #inner and outer quantile levels
  lapply(seq_along(1:no_pairs),
         function(ind) c(qus[ind], qus[(n_qus)-(ind-1)], alpha_vals[ind]))


}

#' @title
#'
#' @description
#' Function to exclude instances from data table based on a list of AND-exclusions
#' in a list instexcl
#' used before scoring
#'
exclude_rows <- function(dt, instexcl) {
  condition <- rep(TRUE, nrow(dt))  # Initialize condition to include all rows

  # Iterate over the names in instexcl
  for (var_name in names(instexcl)) {
    # Check if the variable exists in dt
    if (var_name %in% names(dt)) {
      condition <- condition & (dt[[var_name]] %in% instexcl[[var_name]])
    }
  }

  # Return the filtered data.table
  return(dt[!condition])
}


make_ensemble <- function(data,
                          summary_function = median,
                          model_name = NULL,
                          extra_excl = NULL,
                          incl = NULL,
                          strat = c("country", "error_method", "method", "quantile",
                                    "horizon", "target", "target_year"),
                          avail_threshold = NULL,
                          old_call = FALSE){

  #extract function name to make model name
  if(is.null(model_name)){
    model_name <- deparse(summary_function) |>
      paste(collapse = " ") |>
      (\(x) gsub(".*\"(.*)\".*", "\\1", x = x))() |>
      paste0("_ensemble")
  }


  truthdat <- data |>
    .d(, .SD, .SDcols = c(strat, "true_value")) |>
    unique()

      #Input checks
  if(model_name %in% c("weighted.mean_ensemble", "weighted.median_ensemble") & is.null(data$weights)){
    stop("can't compute weighted mean or median without weights")
  }
  if(model_name %in% c("median_ensemble", "mean_ensemble") & !is.null(data$weights)){
    warning("There are weights in the data, but an unweighted summary function was chosen. Was this intended?")
  }

  #check if weights are alright
  if(!is.null(data$weights)){

    if(any(is.na(data$weights))){
      stop("some weights are missing")
    }

    sum_weights <- data |>
      select(source, target_type, location, forecast_date, weights) |>
      distinct() |>
      group_by(target_type, location, forecast_date) |>
      summarise(weight_sum = round(sum(weights), digits = 3),
                .groups = "drop") |>
      select(weight_sum) |>
      distinct() |>
      pull()

    if(length(sum_weights) > 1){
      print(sum_weights)
      message("At the level of one forecast instance, not all weights sum to the same value. Is this intended?")
    } else if (round(sum_weights,3) != 1){
      message("At the level of one forecast instance, weights do not sum to 1. Due to automatic correction,
              this is not necessarily a problem - but check if this is intended")
    }

  }

  #######make model vector based on incl/excl arguments#####

  models <- incl

  #check if any ensembles in models (this should in general not be the case)
  is_ensemble <- grepl(".*ensemble.*", models)

  if(any(is_ensemble)){
    warning(paste0("There seems to be at least one ensemble (\"" ,
                   paste(models[is_ensemble], collapse = "\", \""),
                   "\") that is not in the list of excluded models. Is this intended?"))
  }


  #############make mean/median forecast################

  ensemble <- data |>
    .d(source %in% models) |>
    .d(, .(ensemble_prediction = summary_function(prediction)), by = strat) |>
    .d(, .SD, .SDcols = c(strat, "ensemble_prediction")) |>
    setnames("ensemble_prediction", "prediction") |>
    .d(, source := model_name) |>
    .d(, .SD, .SDcols = c("source", strat, "prediction")) |>
    .d(truthdat, on = strat)

  ensemble[truthdat, on = strat]

  if(!is.null(data$weights)){
    data <- data |>
      select(-weights)
  }

  return(ensemble)
}
