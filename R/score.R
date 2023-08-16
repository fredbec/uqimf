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
                       by = c("country", "target", "horizon")){

  .d <- `[`

  #Determine available CI's
  qus <- unique(fcdat$quantile) |>
    sort()

  no_pairs <- floor(length(qus)/2)

  cvg_ranges <- sapply(
    1:no_pairs,
    function(idx) qus[length(qus)-(idx-1)] - qus[idx]
    ) |>
    sort()

  scores <- fcdat |>
    data.table::copy() |>
    data.table::setnames(paste0("tv_", tv_release), "true_value") |>
    scoringutils::score() |>
    scoringutils::add_coverage(by = by, ranges = cvg_ranges * 100) |>
    scoringutils::summarise_scores(by = by)

  return(scores)

}
