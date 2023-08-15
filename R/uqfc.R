#' Extract historical empirical quantiles from WEO data
#'
#' @param weodat (subset of) WEO data as returned by download.process.weo()
#' function in package imfpp
#' @param error_fct function to feed error through
#' @param tv_release which release of the true value in the WEO data shall be
#' used. Must be one of c(0.5, 1, 1.5, 2)
#'
#' @return data.table with empirical quantiles
#' @export
#'

empQU <- function(weodat,
                  #horizon = 1,
                  error_fct = identity,
                  tv_release = 0.5,
                  quantiles = c(0.1, 0.25, 0.5, 0.75, 0.9)){

  .d <- `[`

  #####Input checks####
  if (!(tv_release %in% c(0.5, 1, 1.5, 2))){

    stop("tv_release must be one of c(0.5, 1, 1.5, 2)")
  }

  #small helper function for calculating quantiles
  calculate_quantiles <- function(x, quantiles) {
    quantile(x, probs = quantiles)
  }

  quants <-
    weodat |>
    #.d(horizon == horizon) |>
    .d(, error := error_fct(prediction - get(paste0("tv_", tv_release)))) |>
    .d(!is.na(error)) |>
    .d(, {
      quantile_vals <- calculate_quantiles(error, quantiles)
      .(quantile = quantiles,
        setNames(quantile_vals, paste0("quant", quantiles)))
    }, by = c("country", "target", "horizon")) |>
    data.table::setnames("V2", "prediction")


  return(quants)
}


empFC <- function(weodat,
                  target_years,
                  tv_release,
                  error_method = c("directional", "absolute"),
                  method = c("leave-one-out", "rolling window", "expanding window"),
                  ci_levels = c(0.5, 0.8),
                  quantiles = NULL,
                  window_length = NULL,
                  include_truevals = TRUE){

  .d <- `[`



  #####Input checks####
  if (!(tv_release %in% c(0.5, 1, 1.5, 2))){

    stop("tv_release must be one of c(0.5, 1, 1.5, 2)")
  }

  if(length(method) != 1L){
    stop("need to specify method. Available options: 'leave-one-out', 'rolling window', 'expanding window'")
  }

  if(length(error_method) != 1L){
    stop("need to specify error method. Available options: 'directional', 'absolute'")
  }

  if(is.null(window_length)){

    if(method == "rolling window"){
      stop("have to specify window length when using a rolling window")
    }
  }

  if(!is.null(ci_levels)){

    if(!is.null(quantiles)){
      warning("quantiles supplied, but ci_levels not set to NULL. Dropping ci_levels")

      ci_levels <- NULL

    } else {

      quantiles <- ci_to_quantiles(ci_levels, error_method = error_method)
    }
  } else {
    if(is.null(quantiles)){
      stop("must supply one of ci_levels or quantiles")
    }
  }


  ###Assign error function
  if(error_method == "directional"){

    error_fct <- identity
  } else if (error_method == "absolute"){

    error_fct <- abs
  } else {

    stop("error_method must be either 'directional' or 'absolute'")
  }

  target_years_list <- as.list(target_years)

  weo_years <- unique(weodat[["target_year"]])

  if(include_truevals){

    truevals <- weodat |>
      .d(, .(country, target, target_year, prediction, horizon, get(paste0("tv_", tv_release)))) |>
      data.table::setnames("V6", paste0("tv_", tv_release)) |>
      .d(, error := prediction - get(paste0("tv_", tv_release))) |>
      data.table::setnames("prediction", "imf_pp")
  }


  quantileFC <- lapply(target_years_list, function(target_year){

    #get set of years for current estimation
    yearset <- year_set(target_year,
                        weo_years,
                        method = method,
                        window_length = window_length)


    #get empirical quantiles
    quSet <- weodat |>
      .d(target_year %in% yearset) |>
      empQU(error_fct = error_fct,
            quantiles = quantiles) |>
      .d(, target_year := target_year)

    if(error_method == "absolute"){

      quSet_pos <- quSet |>
        data.table::copy() |>
        .d(, quantile := 0.5 + quantile / 2)

      quSet_neg <- quSet |>
        data.table::copy() |>
        .d(, quantile := 0.5 - quantile / 2) |>
        .d(, prediction := - prediction)

      quSet <- rbind(quSet_neg,
                     quSet_pos) |>
        .d(order(target, country, target_year, horizon, quantile))

    }

    return(quSet)

  }) |>
    data.table::rbindlist()


  if(include_truevals){

    #return(list(quantileFC, truevals))
    quantileFC <- truevals[quantileFC, on = c("country", "target", "target_year", "horizon")]
  }

  return(quantileFC)
}


#' Score quantile forecasts
#'
#' @param empquants data containing empirical quantiles
#' @param weodat (subset of) WEO data containing relevant truth data
#'
#' @return nothing, saves tidied data in directory
#' @export
#'
score_quants <- function(empquants,
                         weodat,
                         sRule = scoringRules::crps_sample){

  sapply(weodat, function(dp) sRule(dp, empquants))

}

#' Score quantile forecasts
#'
#' @param fcdat forecast data, containing both true values and quantile predictions
#' @importFrom scoringutils score
#' @importFrom scoringutils summarise_scores
#' @return nothing, saves tidied data in directory
#' @export
#'
scoreempQu <- function(fcdat,
                       by = c("country", "target", "horizon")){

  .d <- `[`

  scores <- fcdat |>
    data.table::copy() |>
    data.table::setnames("error", "true_value") |>
    scoringutils::score() |>
    scoringutils::summarise_scores(by = by)

  return(scores)

}

