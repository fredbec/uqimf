#' Convert CI levels to quantiles for empQU function
#'
#' @param ci_levels desired confidence interval levels
#' @param error_method either 'directional' or 'absolute'
#' @return vector with quantiles
#' @export
#'

ci_to_quantiles <- function(ci_levels,
                            error_method){

  if(error_method == "directional"){

    qu_down <- 0.5 - ci_levels / 2
    qu_up <- 0.5 + ci_levels / 2

    qus <- c(qu_down, qu_up) |>
      unique() |> #in case of median
      sort()

  } else if (error_method == "absolute"){

    qus <- ci_levels
  }

  return(qus)
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

    if(is.null(window_length)){
      stop("no value for window_length")
    }

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


#' @title ugimf ggplot2 theme
#'
#' @description
#' A theme for ggplot2 plotss
#' @return A ggplot2 theme
#' @importFrom ggplot2 theme theme_minimal element_line `%+replace%`
#' @export
#'
theme_uqimf <- function() {
  theme_minimal() %+replace%
    theme(axis.line = element_line(colour = "grey80"),
          axis.ticks = element_line(colour = "grey80"),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.position = "bottom",
          legend.title=element_blank())
}


custom_labeller <- function(variable, value) {
  if (variable == "season") {
    if (value == 0) {
      return("Fall forecast, same year")
    } else if (value == 0.5) {
      return("Spring forecast, same year")
    }
  }
  return(as.character(value))  # Default labelling for other variables
}
