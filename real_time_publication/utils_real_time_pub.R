get_current_year_season <- function(weodat){

  forecast_season_dictionary <- c("Spring", "Fall")
  names(forecast_season_dictionary) <- c("S", "F")

  forecast_year <- weodat |>
    .d(forecast_year == max(forecast_year))

  forecast_year <- max(weodat$forecast_year)

  forecast_season <- weodat |>
    .d(forecast_year == max(forecast_year)) |>
    .d(!is.na(prediction)) |>
    .d(horizon == min(horizon)) |>
    .d(, "forecast_season") |>
    unique()

  forecast_season <- forecast_season$forecast_season

  if(forecast_season == "F"){
    available_horizons <- c(0, 1)
  } else if (forecast_season == "S"){

    available_horizons <- c(0.5, 1.5)
  } else {

    stop("season has non available value")
  }

  return(
    list(forecast_year = forecast_year,
         forecast_season = forecast_season,
         identifier = paste0(
           forecast_season_dictionary[forecast_season],
           forecast_year),
         available_horizons = available_horizons)
  )
}


make_qufcs <- function(weodat,
                       target_type,
                       target_year_start,
                       target_year_end,
                       tv_release,
                       error_method,
                       method,
                       window_length,
                       ci_levels,
                       qutype,
                       current_yr_season){

  quantiles <- ci_to_quantiles(ci_levels, error_method)

  target_qufcs <- empFC(weodat |> copy() |> .d(target == target_type),
        target_year = target_year_start:2025,
        tv_release = tv_release,
        error_method = error_method,
        method = method,
        window_length = window_length,
        ci_levels = NULL,
        quantiles = quantiles,
        qutype = quantype) |>
    setnames(paste0("tv_", tv_release), "true_value")

  return(target_qufcs)
}

apply_pava_and_clean <- function(qufcs,
                                 method,
                                 error_method,
                                 current_yr_season){
  qufcs <- qufcs |>
    #add in placeholder
    .d(, method := method) |>
    .d(, error_method := error_method) |>
    .d(, source := "placeholder") |>
    .d(ceiling(target_year - horizon) == current_yr_season$forecast_year) |>
    empFC_pava()|>
    .d(, method := NULL) |>
    .d(, error_method := NULL) |>
    .d(, source := NULL) |>
    .d(order(country, target, target_year, horizon, quantile)) |>
    .d(ceiling(target_year - horizon) <= current_yr_season$forecast_year) |>
    .d(horizon %in% current_yr_season$available_horizons)  |>
    .d(,forecast_season := current_yr_season$forecast_season) |>
    .d(, forecast_year := target_year - floor(horizon)) |>
    .d(forecast_year == current_yr_season$forecast_year) |>
    .d(, .(country, target, forecast_year, forecast_season, target_year, quantile, prediction)) |>
    .d(, target := ifelse(target == "ngdp_rpch", "gdp_growth", "inflation"))


  return(qufcs)
}
