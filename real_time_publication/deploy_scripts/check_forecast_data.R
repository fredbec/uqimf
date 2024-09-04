qufcs <- data.table::fread(here(path_dest,
                                "forecasts",
                                paste0("forecasts_",
                                       current_yr_season$identifier,
                                       ".csv")))

#check that forecasts stem from a single origin
qufcs_forecast_year <- unique(qufcs$forecast_year)
qufcs_forecast_season <- unique(qufcs$forecast_season)

if(length(qufcs_forecast_year) > 1){
  warning("Forecast data seems to contain more than one forecast origin. Concretely, data contains more than one unique entry for forecast_year")
}

if(length(qufcs_forecast_season) > 1){
  warning("Forecast data seems to contain more than one forecast origin. Concretely, data contains more than one unique entry for forecast_season")
}


#check that forecast year is correct
current_year <- lubridate::year(Sys.Date())

if(current_year != qufcs_forecast_year){
  warning(paste0("Forecasts don't seem to stem from current year. Expected ", current_year, ", got ", qufcs_forecast_year))
}


#check that target years are correct
qufcs_target_years <- sort(unique(qufcs$target_year))
expected_target_years <- c(current_year, current_year+1)

if(any(qufcs_target_years != expected_target_years)){
  warning(paste0("Forecasts don't seem to contain the correct target years. Expected ", expected_target_years[1], expected_target_years[2], ", got ", qufcs_target_years[1],qufcs_target_years[2]))
}


#check that forecast season is correct
current_month <- lubridate::month(Sys.Date())
current_season <- ifelse(current_month %in% 4:8, "S", "F")

if(current_season != qufcs_forecast_season){
  warning(paste0("Forecasts don't seem to stem from current forecast season. Expected ", current_season, ", got ", qufcs_forecast_season))
}

#check that quantile levels are correct
qufcs_qu_levels <- sort(unique(qufcs$quantile))
qu_levels_expected <- c(0.1, 0.25, 0.75, 0.9)

if(any(qufcs_qu_levels != qu_levels_expected)){
  warning(paste0("Forecasts don't seem to contain the correct quantile levels. Expected ", qu_levels_expected, ", got ", qufcs_qu_levels))
}

#any NAS
if(any(is.na(qufcs))){
  warning("Forecasts contain NA values")
}

#correct countries
expected_countries <- c("CAN", "FRA", "ITA", "DEU", "GBR", "JPN", "USA")
if(any(!qufcs$country %in% expected_countries)){
  warning("Unexpected country names")
}

#correct countries
expected_targets <- c("inflation", "gdp_growth")
if(any(!qufcs$target %in% expected_targets)){
  warning("Unexpected target names")
}

#same length for inflation and gdp
length_inflation <- nrow(qufcs |> .d(target == "inflation"))
length_gdp<- nrow(qufcs |> .d(target == "gdp_growth"))

if(length_inflation != length_gdp){
  warning("Not the same number of instances for GDP Growth and Inflation.")
}

#length of data
expected_length <- 7*2*2*4 #7 countries, 2 targets, 2 target years per release, 4 quantile levels
qufcs_length <- nrow(qufcs)

if(expected_length != qufcs_length){
  warning(paste0("Forecast data does not have the expected number of instances, Expected ", expected_length, " rows, got ", qufcs_length))
}

#value ranges
expected_value_range <- c(-2,6) #very broad of course, can only catch major mistakes
qufcs_value_range <- c(min(qufcs$prediction), max(qufcs$prediction))

if(qufcs_value_range[1] < expected_value_range[1]){
  warning("There exist prediction values below the expected lower range. Ignore if this makes sense.")
}

if(qufcs_value_range[2] > expected_value_range[2]){
  warning("There exist prediction values over the expected upper range. Ignore if this makes sense.")
}

#check that quantiles are monotonic
qufcs_monqufcs <- qufcs |>
  copy() |>
  .d(, lag_value:=c(NA, prediction[-.N]), by=c("country", "target", "target_year")) |>
  .d(, diff_value := prediction-lag_value)

#some values are by lag design na, exclude these for check
vals <- qufcs_monqufcs$diff_value[!is.na(qufcs_monqufcs$diff_value)]

if(any(vals < 0)){
  warning("Quantiles don't seem to be monotonic within each instance.")
  message("listing instances where quantiles are non-monotonic")
  qufcs_monqufcs <- qufcs_monqufcs |>
    .d(, flag := any(diff_value < 0),  by=c("country", "target", "target_year")) |>
    .d(, lag_value := NULL) |>
    .d(, diff_value := NULL)

  qufcs_monqufcs |> .d(flag == TRUE)

}
