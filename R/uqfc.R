#' Function to download, tidy and save WEO data
#'
#' @param weodat (subset of) WEO data as returned by download.process.weo()
#' function in package imfpp
#' @param horizon horizon for which quantiles should be extracted
#' @param tv_release which release of the true value in the WEO data shall be
#' used. Must be one of c(0.5, 1, 1.5, 2)
#'
#' @return nothing, saves tidied data in directory
#' @export
#'

empqu <- function(weodat,
                  horizon = 1,
                  tv_release = 0.5,
                  quantiles = c(0.1, 0.25, 0.5, 0.75, 0.9)){

  .d <- `[`

  if (!(tv_release %in% c(0.5, 1, 1.5, 2))){

    stop("tv_release must be one of c(0.5, 1, 1.5, 2)")
  }

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
      .(quantile = paste0("quant", quantiles),
        setNames(quantile_vals, paste0("quant", quantiles)))
    }, by = country) |>
    setnames("V2", "value")
    #dplyr::group_by(country) |>
    #dplyr::summarise(quantile = paste0("quant", quantiles),
    #                 myval = quantile(error,
    #                           quantiles))
    #.d(, .(myval = quantile(error, quantiles)), by = c("country"))
    #.d(, `q0.5` := quantile(error, 0.5), by = c("country"))

  return(quants)
}
