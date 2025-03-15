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
                       tv_release = NULL,
                       by = c("country", "target", "horizon"),
                       cvg_rg = NULL){

  .d <- `[`

  #Determine available CI's
  if(is.null(cvg_rg)){
    qus <- unique(fcdat$quantile) |>
      sort()

    no_pairs <- floor(length(qus)/2)

    cvg_ranges <- sapply(
      1:no_pairs,
      function(idx) qus[length(qus)-(idx-1)] - qus[idx]
      ) |>
      sort()

    #floating point bullshit
    cvg_ranges <- cvg_ranges * 10000
    cvg_rg <- as.integer(cvg_ranges) / 100

    cvg_rg <- round(cvg_rg)
  }

  if(is.null(tv_release) & is.null(fcdat$true_value)){

    stop("need to supply a value for tv_release")
  }


  if(is.null(fcdat$model)){

    fcdat$model <- "model1"
  }

  fcdat <- fcdat |>
    data.table::copy() |>
    data.table::setnames(paste0("tv_", tv_release), "observed", skip_absent = TRUE) |>
    data.table::setnames("true_value", "observed", skip_absent = TRUE) |>
    data.table::setnames("prediction", "predicted", skip_absent = TRUE) |>
    data.table::setnames("quantile", "quantile_level", skip_absent = TRUE) |>
    .d(, .SD, .SDcols = union(by, c("country", "model", "horizon", "target", "target_year", "observed", "predicted", "quantile_level"))) |>
    scoringutils::as_forecast_quantile()

  scorenames <- scoringutils::get_metrics(fcdat) #to select scoring functions in next step

  is_scores <- fcdat |>
    data.table::copy() |>
    scoringutils::score(metrics = list("wis" = scorenames$wis,
                                       "overprediction" = scorenames$overprediction,
                                       "underprediction" = scorenames$underprediction,
                                       "dispersion" = scorenames$dispersion)) |>
    scoringutils::summarise_scores(by = by)

  coverage_dat <- fcdat |>
    scoringutils::get_coverage(by = by)

  dcast_eqn <- paste(by, collapse = " + ")

  cvg_interval <- coverage_dat |>
    .d(, .SD, .SDcols = c(by, "interval_range", "interval_coverage")) |>
    unique() |>
    .d(, interval_range := paste0("coverage_", interval_range)) |>
    dcast(as.formula(paste(dcast_eqn, "~ interval_range")), value.var = "interval_coverage")

  cvg_quantile <- coverage_dat |>
    .d(, .SD, .SDcols = c(by, "quantile_level", "quantile_coverage")) |>
    unique() |>
    .d(, quantile_level := paste0("qucoverage_", quantile_level*100)) |>
    dcast(as.formula(paste(dcast_eqn, "~ quantile_level")), value.var = "quantile_coverage")

  all_scores <- cvg_interval[cvg_quantile, on = by] |>
    .d(is_scores, on = by) |>
    setnames("wis", "interval_score") |>
    .d(, .SD,
       .SDcols = c(by,
           "interval_score", "dispersion", "underprediction", "overprediction",
           paste0("coverage_", seq(10, 90, by = 10)),
           paste0("qucoverage_", c(seq(5,45, by = 5), seq(55, 95, by = 5))))
       ) |>
    .d(order(model, target))
  #data.table::setnames("observed", "true_value", skip_absent = TRUE) |>
  #data.table::setnames("predicted", "prediction", skip_absent = TRUE) |>
  #data.table::setnames("quantile_level", "quantile", skip_absent = TRUE) |>

  #scoringutils::add_coverage(by = by, ranges = cvg_rg) |>
  #scoringutils::summarise_scores(by = by)

  return(all_scores)

}




score_sample <- function(weodat,
                         target_years,
                         tv_release,
                         country,
                         target,
                         horizon,
                         error_method = c("directional", "absolute"),
                         method = c("leave-one-out", "rolling window", "expanding window"),
                         window_length = NULL){

  .d <- `[`



  #####Input checks####
  if (!(tv_release %in% c(0.5, 1, 1.5, 2, "oecd"))){

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

  if(length(unique(weodat$country)) > 1 | length(unique(weodat$target)) > 1 | length(unique(weodat$horizon)) > 1){
    stop("use wrapper score_by_crps instead")
  }


  ###Assign error function
  if(error_method == "directional"){

    error_fct <- identity
  } else if (error_method == "absolute"){

    error_fct <- abs
  } else {

    stop("error_method must be either 'directional' or 'absolute'")
  }

  if(horizon >=1 ){target_years <- target_years[-1]}
  target_years_list <- as.list(target_years)

  weo_years <- unique(weodat[["target_year"]])


  weodat <- weodat |>
    .d(, error := error_fct(prediction - get(paste0("tv_", tv_release)))) |>
    .d(!is.na(error))

  quantileFC <- lapply(target_years_list, function(tget_year){

    trgt_val <- weodat |>
      .d(target_year == tget_year) |>
      .d(, error)

    #get set of years for tget_year target sample
    yearset <- year_set(tget_year,
                        weo_years,
                        method = method,
                        window_length = window_length)

    #get empirical scores
    scSet <- weodat |>
      .d(target_year %in% yearset) |>
      .d(, error)

    score <- scoringRules::crps_sample(trgt_val, scSet)

    scoredat <- data.table(
      country = country,
      target = target,
      horizon = horizon,
      target_year = tget_year,
      score = score
    )
  }) |>
    data.table::rbindlist()



  return(quantileFC)
}


score_by_crps <- function(weodat,
                          target_years,
                          tv_release,
                          error_method = c("directional", "absolute"),
                          method = c("leave-one-out", "rolling window", "expanding window"),
                          window_length = NULL){

  combs <- data.table::CJ(unique(weodat$country), unique(weodat$target), unique(weodat$horizon))


  lapply(1:nrow(combs), function(idx){
    comb <- combs[idx,] |> unlist()


    subdat <- weodat |>
      .d(country == comb[1]) |>
      .d(target == comb[2]) |>
      .d(horizon == comb[3])


    score_sample(subdat,
                 target_years = target_years,
                 tv_release = tv_release,
                 country = comb[1],
                 target = comb[2],
                 horizon = comb[3],
                 error_method = error_method,
                 method = method,
                 window_length = window_length)
  }
  ) |>
    data.table::rbindlist()

}
