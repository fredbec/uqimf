#' Function to extract historical empirical quantiles from WEO data
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

empqu <- function(weodat,
                  horizon = 1,
                  tv_release = 0.5,
                  quantiles = c(0.1, 0.25, 0.5, 0.75, 0.9)){

  .d <- `[`

  #####Input checks####
  if (!(tv_release %in% c(0.5, 1, 1.5, 2))){

    stop("tv_release must be one of c(0.5, 1, 1.5, 2)")
  }

  if(!(horizon %in% seq(0, 5, by = 0.5))){

    stop("invalid value for horizon")
  }


  #small helper function for calculating quantiles
  calculate_quantiles <- function(x) {
    quantile(x, probs = quantiles)
  }

  quants <-
    weodat |>
    .d(horizon == horizon) |>
    .d(, error := prediction - get(paste0("tv_", tv_release))) |>
    .d(!is.na(error)) |>
    .d(, {
      quantile_vals <- calculate_quantiles(error)
      .(quantile = quantiles,
        setNames(quantile_vals, paste0("quant", quantiles)))
    }, by = country) |>
    setnames("V2", "value")


  return(quants)
}

#' Function to download, tidy and save WEO data
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
