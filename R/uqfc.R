#' Extract historical empirical quantiles from WEO data
#'
#' @param weodat (subset of) WEO data as returned by download.process.weo()
#' function in package imfpp
#' @param horizon horizon for which quantiles should be extracted
#' @param tv_release which release of the true value in the WEO data shall be
#' used. Must be one of c(0.5, 1, 1.5, 2)
#'
#' @return data.table with empirical quantiles
#' @export
#'

empQU <- function(weodat,
                  #horizon = 1,
                  tv_release = 0.5,
                  quantiles = c(0.1, 0.25, 0.5, 0.75, 0.9)){

  .d <- `[`

  #####Input checks####
  if (!(tv_release %in% c(0.5, 1, 1.5, 2))){

    stop("tv_release must be one of c(0.5, 1, 1.5, 2)")
  }

  #if(!(horizon %in% seq(0, 5, by = 0.5))){
#
 #   stop("invalid value for horizon")
#  }


  #small helper function for calculating quantiles
  calculate_quantiles <- function(x) {
    quantile(x, probs = quantiles)
  }

  quants <-
    weodat |>
    #.d(horizon == horizon) |>
    .d(, error := prediction - get(paste0("tv_", tv_release))) |>
    .d(!is.na(error)) |>
    .d(, {
      quantile_vals <- calculate_quantiles(error)
      .(quantile = quantiles,
        setNames(quantile_vals, paste0("quant", quantiles)))
    }, by = c("country", "target", "horizon")) |>
    setnames("V2", "prediction")


  return(quants)
}


empFC <- function(weodat,
                  target_years,
                  tv_release,
                  method = c("leave-one-out", "rolling window", "expanding window"),
                  quantiles = c(0.1, 0.25, 0.5, 0.75, 0.9),
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

  if(is.null(window_length)){

    if(method == "rolling window"){
      stop("have to specify window length when using a rolling window")
    }
  }

  target_years_list <- as.list(target_years)

  weo_years <- unique(weodat[["target_year"]])

  if(include_truevals){

    truevals <- weodat |>
      .d(, .(country, target, target_year, prediction, horizon, get(paste0("tv_", tv_release)))) |>
      setnames("V6", paste0("tv_", tv_release)) |>
      .d(, error := prediction - get(paste0("tv_", tv_release))) |>
      setnames("prediction", "imf_pp")
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
      empQU() |>
      .d(, target_year := target_year)

    return(quSet)

  }) |>
    data.table::rbindlist()

  if(include_truevals){

    #return(list(quantileFC, truevals))
    quantileFC <- truevals[quantileFC, on = c("country", "target", "target_year", "horizon")]
  }

  return(quantileFC)
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


#' Score quantile forecasts
#'
#' @param empquants
#' @return nothing, saves tidied data in directory
#' @export
#'
score_quants <- function(empquants,
                         weodat,
                         sRule = scoringRules::crps_sample){

  sapply(weodat, function(dp) sRule(dp, empquants))

}


scoreempQu <- function(fcdat,
                             by = c("country", "target", "horizon")){

  .d <- `[`

  scores <- fcdat |>
    copy() |>
    setnames("error", "true_value") |>
    scoringutils::score() |>
    scoringutils::summarise_scores(by = by)

  return(scores)

}

