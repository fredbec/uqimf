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
                       tv_release,
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
  }

  print(cvg_rg)

  if(is.null(fcdat$model)){

    fcdat$model <- "model1"
  }

  scores <- fcdat |>
    data.table::copy() |>
    data.table::setnames(paste0("tv_", tv_release), "true_value") |>
    scoringutils::score() |>
    scoringutils::add_coverage(by = by, ranges = cvg_rg) |>
    scoringutils::summarise_scores(by = by)

  return(scores)

}
