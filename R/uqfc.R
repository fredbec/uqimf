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
                  quantiles = c(0.1, 0.25, 0.5, 0.75, 0.9),
                  qutype = 7){

  .d <- `[`

  #####Input checks####
  if (!(tv_release %in% c(0.5, 1, 1.5, 2))){

    stop("tv_release must be one of c(0.5, 1, 1.5, 2)")
  }

  #small helper function for calculating quantiles
  calculate_quantiles <- function(x, quantiles, qutype) {
    quantile(x, probs = quantiles, type = qutype)
  }

  quants <-
    weodat |>
    #.d(horizon == horizon) |>
    .d(, error := error_fct(get(paste0("tv_", tv_release)) - prediction)) |>
    .d(!is.na(error)) |>
    .d(, {
      quantile_vals <- calculate_quantiles(error, quantiles, qutype = qutype)
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
                  include_truevals = TRUE,
                  qutype = 7){

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
            tv_release = tv_release,
            qutype = qutype) |>
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

##########NOTE: THIS FUNCTION IS HARDCODED FOR CIs 50 & 80
empFC_pava <- function(fcdat,
                       ci_levels = c(0.5, 0.8),
                       qutype = 7){

  .d <- `[`

  qus <- ci_to_quantiles(ci_levels, "directional") |>
    #needed because of floating point error
    as.character() |>
    as.numeric()


  extra_cols <- fcdat |>
    .d(, .(imf_pp, true_value, error, country, target, target_year, horizon,
           source, error_method, method)) |>
    unique()

  violations <- fcdat |>
    #filter relevant quantiles
    .d(quantile %in% c(0.1+fperror(), 0.25, 0.75, 0.9)) |>
    #only keep relevant variables
    .d(, .(country, target, target_year, horizon, error_prediction,
           source, error_method, method, quantile)) |>


    #wide formal
    .d(, horizon := paste0("h", 10*horizon)) |>
    dcast(country + target + target_year + quantile +
            error_method + method + source ~ horizon,
          value.var = "error_prediction") |>

    .d(, .(country, target, source, method, error_method, target_year, quantile, h0, h5, h10, h15)) |>
    .d(order(country, target, target_year, error_method, method, source, quantile)) |>
    .d(, qtype := ifelse(quantile < 0.5, 1, -1)) |>

    ##Pooling for
    .d(, viol := ifelse( qtype*h5 > qtype*h0, TRUE, FALSE)) |>
    .d(, pool05 := any(viol), by = .(country, target, target_year, error_method, method, source)) |>
    .d(pool05 == TRUE, c("h0", "h5") := (h0+h5)/2) |>

    .d(, viol := ifelse( qtype*h10 > qtype*h5, TRUE, FALSE)) |>
    .d(, pool10 := any(viol), by = .(country, target, target_year, error_method, method, source)) |>
    .d(pool10 == TRUE, c("h5", "h10") := (h5+h10)/2) |>

    #check if ordering is still upheld
    .d(, viol := ifelse( qtype*h5 > qtype*h0, TRUE, FALSE)) |>
    .d(, pool05twice := any(viol), by = .(country, target, target_year, error_method, method, source)) |>
    .d(pool05twice == TRUE, c("h0", "h5", "h10") := (h0+h5+h10)/3) |>

    .d(, viol := ifelse( qtype*h15 > qtype*h10, TRUE, FALSE)) |>
    .d(, pool15 := any(viol), by = .(country, target, target_year, error_method, method, source)) |>
    .d(pool15 == TRUE, c("h10", "h15") := (h10+h15)/2) |>

    .d(, viol := ifelse( qtype*h10 > qtype*h5, TRUE, FALSE)) |>
    .d(, pool10twice := any(viol), by = .(country, target, target_year, error_method, method, source)) |>
    .d(pool10twice == TRUE, c("h5", "h10", "h15") := (h5+h10+h15)/3) |>

    #check if ordering is still upheld
    .d(, viol := ifelse( qtype*h5 > qtype*h0, TRUE, FALSE)) |>
    .d(, pool05thrice := any(viol), by = .(country, target, target_year, error_method, method, source)) |>
    .d(pool05thrice == TRUE, c("h0", "h5", "h10", "h15") := (h0+h5+h10+h15)/4) |>

    .d(, .(country, target, source, method, error_method, target_year, quantile, h0, h5, h10, h15)) |>
    melt(id.vars = c("country", "target", "source", "method", "error_method", "target_year", "quantile"),
         variable.name = "horizon",
         value.name = "error_prediction") |>
    .d(,horizon := as.numeric(substr(horizon, 2, 100))/10)

  fcs <- extra_cols[violations, on = c("country", "target", "target_year", "horizon",
                                       "source", "error_method", "method")] |>
    .d(, prediction := imf_pp + error_prediction) |>
    .d(, .(country, target, target_year, imf_pp, horizon, true_value, error, quantile, prediction, error_prediction, source, error_method, method))


  return(fcs)

}



##########NOTE: THIS FUNCTION IS HARDCODED FOR CIs 50 & 80
##########NOTE: UNFINISHED CODE
empFC_pava_cilength <- function(fcdat,
                       ci_levels = c(0.5, 0.8),
                       qutype = 7){

  .d <- `[`

  qus <- ci_to_quantiles(ci_levels, "directional") |>
    #needed because of floating point error
    as.character() |>
    as.numeric()


  extra_cols <- fcdat |>
    .d(, .(imf_pp, true_value, error, country, target, target_year, horizon,
           source, error_method, method)) |>
    unique()

  violations <- fcdat |>
    #filter relevant quantiles
    .d(quantile %in% qus) |>
    #only keep relevant variables
    .d(, .(country, target, target_year, horizon, error_prediction,
           source, error_method, method, quantile)) |>


    #wide formal
    .d(, horizon := paste0("h", 10*horizon)) |>
    dcast(country + target + target_year + quantile +
            error_method + method + source ~ horizon,
          value.var = "error_prediction") |>

    .d(, .(country, target, source, method, error_method, target_year, quantile, h0, h5, h10, h15)) |>
    .d(order(country, target, target_year, error_method, method, source, quantile)) |>
    melt(measure.vars = paste0("h", c(0,5,10,15)), value.name = "error_prediction", variable.name = "horizon") |>
    dcast(country + target + target_year + horizon +
            error_method + method + source ~ quantile,
          value.var = "error_prediction") |>
    .d(, ci50 := `0.75`- `0.25`) |>
    .d(, ci80 := `0.9` - `0.1`) |>
    melt(measure.vars = c("0.1", "0.25", "0.75", "0.9", "ci50", "ci80"), value.name = "error_prediction", variable.name = "quantile") |>
    dcast(country + target + target_year + quantile +
            error_method + method + source ~ horizon,
          value.var = "error_prediction") |>

    .d(quantile %in% c("ci50", "ci80"), viol := ifelse( h0 > h5, TRUE, FALSE)) |>
    .d(, viol := ifelse(is.na(viol), FALSE, viol)) |>
    .d(, pool05 := any(viol), by = .(country, target, target_year, error_method, method, source))

  return(violations)


}
