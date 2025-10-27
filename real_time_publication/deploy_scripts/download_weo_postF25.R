library(httr)
library(jsonlite)
library(data.table)
library(here)

DT <- `[`

#list of countries to download (G7)
countries <- c("CAN", "DEU", "GBR", "FRA", "ITA", "JPN", "USA")
#transform for URL
cget <- paste(countries, collapse = "+")
startyr <- 1990 #don't need this many years, but can't hurt to have the data
endyr <- cyear + 6

dict_target_names <- c("PCPIPCH", "NGDP_RPCH")
names(dict_target_names) <- c("pcpi_pch", "ngdp_rpch")

#minimal function that only depends on the target
#could also pull both targets with same query, but this way seems
#less prone to coding errors
download_target <- function(target_name){

  target_url_name <- dict_target_names[target_name]

  url <- paste0("https://api.imf.org/external/sdmx/2.1/data/",
                "WEO/", #World Economic Outlook data
                cget, #countries
                ".", target_url_name, #name of target variable
                ".A?", #annual
                "startPeriod=", startyr, "&endPeriod=", endyr) #start and end of period to query

  resp <- GET(url)
  stop_for_status(resp)

  data_json <- content(resp, "text", encoding = "UTF-8")
  data_list <- fromJSON(data_json)


  queried_data <- lapply(seq_along(data_list$dataSets$series),
                  function(k){

    data.table(target_year = data_list$structure$dimensions$observation$values[[1]]$id,
               value = unlist(data_list$dataSets$series[[k]]$observations),
               country = countries[k])
  }) |>
    rbindlist() |>
    DT(, target_year := as.numeric(target_year)) |>
    DT(, value := as.numeric(value)) |>
    DT(, forecast_season := cseason) |>
    DT(, forecast_year := cyear) |>
    DT(, target := target_name) |>
    DT(, .SD, .SDcols = c("country", "target", "target_year", "forecast_year", "forecast_season", "value"))

  return(queried_data)
}

cdat_gdp <- download_target("ngdp_rpch")
cdat_inf <- download_target("pcpi_pch")

weocedition <- rbind(cdat_gdp, cdat_inf)


name_tv <- ifelse(cseason == "S", "tv_0.5", "tv_1")
alttruth <- ifelse(cseason == "S", "tv_1", "tv_0.5")

truth_data <- weocedition |>
  .d(target_year == (cyear - 1)) |>
  setnames("value", name_tv)  |>
  .d(, altval := as.numeric(NA)) |>
  setnames("altval", alttruth) |>
  .d(, forecast_season := NULL) |>
  .d(, forecast_year := NULL)

forecast_data <- weocedition |>
  .d(target_year %in% c(cyear, cyear + 1)) |>
  setnames("value", "prediction") |>
  .d(, forecast_year := cyear) |>
  .d(, forecast_season := cseason) |>
  .d(, season_helper := ifelse(forecast_season == "S", 0.5, 0)) |>
  .d(, horizon := (target_year - forecast_year) + season_helper) |>
  .d(, season_helper := NULL) |>
  .d(, c("tv_0.5", "tv_1", "tv_1.5", "tv_2") := as.numeric(NA))  # explicitly code true value as missing


data.table::fwrite(truth_data, here(location_download, paste0("weodat_truth", cseason, cyear, ".csv")))
data.table::fwrite(forecast_data, here(location_download, paste0("weodat_fcsts", cseason, cyear, ".csv")))

