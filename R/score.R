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
                       cvg_rg = NULL,
                       extis = FALSE){

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

  if(extis){

    is_30 <- fcdat |>
      data.table::copy() |>
      compute_is(30, by = by)

    is_90 <- fcdat |>
      data.table::copy() |>
      compute_is(90, by = by)
  } else {

    is_30 <- NULL
    is_90 <- NULL
  }

  is_50 <- fcdat |>
    data.table::copy() |>
    compute_is(50, by = by)

  is_80 <- fcdat |>
    data.table::copy() |>
    compute_is(80, by = by)

  cvg_rg_list <- as.list(cvg_rg)

  is_unweighted <- lapply(cvg_rg_list, function(cvgrg){

    fcdat |>
      data.table::copy() |>
      compute_is(cvgrg, by = by) |>
      setnames(paste0("interval_score_", cvgrg), "iscore") |>
      .d(, iscore_rg := cvgrg)
  }) |>
    data.table::rbindlist()

  if(length((unique(is_unweighted$iscore_rg))) != length(cvg_rg)){
    warning("not the correct number of interval scores calculated. Might be an fp error in interval score function")
  }

  is_unweighted <- is_unweighted |>
    .d(, .(unweighted_interval_score = mean(iscore)), by = by)

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

  qulvlss <- 100 * ci_to_quantiles(cvg_rg/100, "directional")

  all_scores <- cvg_interval[cvg_quantile, on = by] |>
    .d(is_scores, on = by) |>
    .d(is_50, on = by) |>
    .d(is_80, on = by) #|>

  scorenames <- c("interval_score", "dispersion", "underprediction", "overprediction",
                  "interval_score_50", "interval_score_80", "unweighted_interval_score",
                  paste0("coverage_", cvg_rg),
                  paste0("qucoverage_", qulvlss))
  if(extis){
    all_scores <- all_scores |>
      .d(is_30, on = by) |>
      .d(is_90, on = by)
    scorenames <- c(scorenames, "interval_score_30","interval_score_90")
  }


  all_scores <- all_scores |>
    .d(is_unweighted, on = by) |>
    setnames("wis", "interval_score") |>
    .d(, .SD,
       .SDcols = c(by, scorenames)
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


  #weodat <- weodat |>
  #  .d(, error := error_fct(prediction - get(paste0("tv_", tv_release)))) |>
  #  .d(!is.na(error))

  quantileFC <- lapply(target_years_list, function(tget_year){

    trgt_val <- weodat |>
      .d(target_year == tget_year) |>
      .d(, .SD, .SDcols = paste0("tv_", tv_release)) |>
      unlist()

    #get set of years for tget_year target sample
    yearset <- year_set(tget_year,
                        weo_years,
                        method = method,
                        window_length = window_length)

    #get empirical scores
    scSet <- weodat |>
      .d(target_year %in% yearset) |>
      .d(, prediction) |>
      unlist()

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

    if(comb[1] == "JPN"){
      target_years_sub <- min(target_years):2020
    } else {
      target_years_sub <- target_years

    }


    score_sample(subdat,
                 target_years = target_years_sub,
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



score_by_crps_quants <- function(weodat,
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

    if(comb[1] == "JPN" & max(weodat$target_year)>2020){
      target_years_sub <- min(target_years):2020
    } else {
      target_years_sub <- target_years
    }

    if(unique(subdat$model %in% c("ar_annual-direct", "arx_annual-direct"))){
      if(comb[1] == "JPN" & max(target_years)>2020){
        target_years_sub <- 2013:2020
      } else {
        target_years_sub <- 2013:2023
      }
    }

    score_sample_quants(subdat,
                 target_years = target_years_sub,
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




score_sample_quants <- function(weodat,
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

  #if(horizon >=1 ){target_years <- target_years[-1]}
  target_years_list <- as.list(target_years)


  weo_years <- unique(weodat[["target_year"]])


  #weodat <- weodat |>
  #  .d(, error := error_fct(prediction - get(paste0("tv_", tv_release)))) |>
  #  .d(!is.na(error))

  quantileFC <- lapply(target_years_list, function(tget_year){

    trgt_val <- weodat |>
      .d(target_year == tget_year) |>
      .d(, c("true_value")) |>
      unique() |>
      unlist()
    #get set of years for tget_year target sample
    #yearset <- year_set(tget_year,
    #                    weo_years,
    #                    method = method,
    #                    window_length = window_length)

    #get empirical scores
    scSet <- weodat |>
      .d(target_year == tget_year) |>
      .d(, prediction) |>
      unlist()

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


#function to compute the interval score
compute_is <- function(fcdata, cilevel, by){

  lower_br <- 50 - cilevel/2
  upper_br <- 50 + cilevel/2
  alph_br <- (1 - (cilevel/100))
  fact_br <- 2/alph_br

  is_cmpt <- fcdata |>
    data.table::copy() |>
    .d(,quantile := round(100 * quantile)) |>
    .d(round(quantile) %in% c(lower_br, upper_br))

  if(length(unique(is_cmpt$quantile)) != 2){
    message(paste0("CI level ", cilevel))
    stop("Quantile filtering did not work")
  }


  dcast_vars <- c("model", "country", "target", "horizon", "target_year", "true_value")
  subset_vars <- c("model", "country", "target", "horizon", "target_year")

  if(is.null(fcdata$error_method)){

    dcast_eqnbv <- paste(dcast_vars, collapse = " + ")
  } else {

    dcast_eqnbv <- paste(c(dcast_vars, "method", "error_method"), collapse = " + ")
    subset_vars <- c(subset_vars, "method", "error_method")
  }


  is_cmpt <- is_cmpt |>
    .d(,quantile := ifelse(quantile < 50, "lower", "upper")) |>
    dcast(as.formula(paste(dcast_eqnbv, "~ quantile")), value.var = "prediction") |>
    .d(, lower_ind := as.numeric(true_value < lower)) |>
    .d(, upper_ind := as.numeric(true_value > upper)) |>
    .d(, lower_dist := lower - true_value) |>
    .d(, upper_dist := true_value - upper) |>
    .d(, underpred := (fact_br * lower_ind * lower_dist)) |>
    .d(, overpred := (fact_br * upper_ind * upper_dist)) |>
    .d(, disp := upper-lower) |>
    .d(, iscore := disp + underpred + overpred) |>
    .d(, .SD, .SDcols = c(subset_vars, "iscore")) |>
    .d(, .(interval_score = mean(iscore)), by = by) |>
    setnames("interval_score", paste0("interval_score_", cilevel))

  return(is_cmpt)
}
