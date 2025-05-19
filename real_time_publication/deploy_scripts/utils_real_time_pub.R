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
        target_year = target_year_start:target_year_end,
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
    empFC_pava(ci_levels = c(0.5, 0.8)) |>
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

empFC_pava_rtb_spring <- function(fcdat,
                       ci_levels = c(0.5, 0.8),
                       qutype = 7){

  .d <- `[`

  qus <- ci_to_quantiles(ci_levels, "directional") |>
    #needed because of fp error
    round(8)
  #needed because of floating point error
  #as.character() |>
  #as.numeric()

  #to merge again later
  extra_cols <- fcdat |>
    .d(, forecast_year := ceiling(target_year - horizon)) |>
    .d(, .(imf_pp, true_value, error, country, target, forecast_year, target_year, horizon,
           source, error_method, method)) |>
    unique()

  #sometimes there is a floating point error that I can't quite figure out
  #so here's a bit of lazy code to address it
  #fcdat <- fcdat |>
  # .d(round(quantile,8) %in% seq(0.05, 0.95, by = 0.05))


  #if(length(unique(fcdat_update$quantile))<4){
  #  message("addressing floating point error")

  #fcdat <-  fcdat |>
  #    .d(quantile %in%  c(0.1, 0.25, 0.75, 0.9))
  #}

  #check again if all quantiles are there
  if(length(unique(fcdat$quantile))!=2*length(ci_levels)){
    #message("change this part")
    stop("Wrong number of quantiles")
  }

  violations <- fcdat |>

    #only keep relevant variables
    .d(, .(country, target, target_year, horizon, error_prediction,
           source, error_method, method, quantile)) |>

    .d(, forecast_year := ceiling(target_year - horizon)) |>

    #wide formal
    .d(, horizon := paste0("h", 10*horizon)) ##|>
    dcast(country + target + forecast_year + quantile +
            error_method + method + source ~ horizon,
          value.var = "error_prediction") |>

    .d(, .(country, target, source, method, error_method, forecast_year, quantile, h5, h15)) |>
    .d(order(country, target, forecast_year, error_method, method, source, quantile)) |>

    #define helper variable: upper quantiles will need to be smaller with rising horizon to
    #constitute a violation, lower quantiles similarly need to be bigger
    #this variable will be used so the same comparison can be performed for both
    #quantile types (see next line of code)
    .d(, qtype := ifelse(quantile < 0.5, 1, -1)) |>

    .d(, viol := ifelse( qtype*h15 > qtype*h5, TRUE, FALSE)) |>
    .d(, poolS := any(viol), by = .(country, target, forecast_year, error_method, method, source)) |>
    .d(poolS == TRUE, c("h5", "h15") := (h5+h15)/2) |>

    .d(, .(country, target, source, method, error_method, forecast_year, quantile, h5, h15)) |>
    melt(id.vars = c("country", "target", "source", "method", "error_method", "forecast_year", "quantile"),
         variable.name = "horizon",
         value.name = "error_prediction") |>
    .d(,horizon := as.numeric(substr(horizon, 2, 100))/10) |>
    .d(!is.na(error_prediction))

  fcs <- extra_cols[violations, on = c("country", "target", "forecast_year", "horizon",
                                       "source", "error_method", "method")] |>
    .d(, prediction := imf_pp + error_prediction) |>
    .d(, quantile := round(quantile, 8)) |>
    .d(, .(country, target, target_year, imf_pp, horizon, true_value, error, quantile, prediction, error_prediction, source, error_method, method))


  return(fcs)

}
