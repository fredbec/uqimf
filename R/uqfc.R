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
    .d(, error := error_fct(get(paste0("tv_", tv_release)) - prediction)) |>
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
                  only_errorquants = FALSE,
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

  #return(weo_years)
  quantileFC <- lapply(target_years_list, function(target_year){

    #get set of years for current estimation
    yearset <- year_set(target_year,
                        weo_years,
                        method = method,
                        window_length = window_length)


    #get empirical quantiles
    quSet <- weodat |>
      #####This block is for not incorrectly sampling target_years when horizon >=1
      #e.g. when horizon = 1, this means making a forecast for e.g. 2020 in fall
      #of 2019, so in a real-time setting you wouldn't have access to the resolution
      #of the horizon-1 forecast for 2019 (so you need your error sample to start in 2018)
      #this code is set so that the window methods honor this, but the leave-one-out
      #method doesnt't (since it has access to the future anyways)
      .d(,target_method := method) |>
      .d(,helper_horizon := ifelse(target_method == "leave-one-out",
                                   0, #no shift in loo method
                                   horizon)) |>
      .d(, effective_target_year := target_year + floor(helper_horizon)) |>
      .d(effective_target_year %in% yearset) |>
      .d(, c("target_method", "helper_horizon", "effective_target_year") := NULL) |>
      #####And now back to your regularly scheduled program
      #.d(target_year %in% yearset) |>
      empQU(error_fct = error_fct,
            quantiles = quantiles,
            tv_release = tv_release) |>
      .d(, target_year := target_year)

    if(error_method == "absolute"){
      #with absolute errors, the extracted quantile must be transformed:
      #e.g. when extracting a 50% quantile from the absolute errors, this value
      #must be added on to the prediction to get the 75% quantile and subtracted
      #from the prediction to get the 25% quantile

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

  if(!only_errorquants){

    imfpp <- truevals |>
      data.table::copy() |>
      .d(, .(country, target, target_year, horizon, imf_pp))


    quantileFC <- imfpp[quantileFC,  on = c("country", "target", "target_year", "horizon")] |>
      .d(, error_prediction := prediction) |>
      .d(, prediction := imf_pp + error_prediction) |>
      .d(, imf_pp := NULL)

  }

  if(include_truevals){

    quantileFC <- truevals[quantileFC, on = c("country", "target", "target_year", "horizon")]
  }


  return(quantileFC)
}


