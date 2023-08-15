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
