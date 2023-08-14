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
