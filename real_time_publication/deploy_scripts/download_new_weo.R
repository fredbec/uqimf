library(arrow)

cyear <- format(Sys.Date(), "%Y") |> as.numeric()
cday <- format(Sys.Date(), "%j") |> as.numeric()
#set April 10 and October 10 as first days to check for new release
cseason <- ifelse(cday > 100 & cday < 283, "S", "F")
cyear <- ifelse(cday < 100, cyear - 1, cyear)

#get file and folder names depending on season
cmonthlong <- ifelse(cseason == "S", "april", "october")
cmonthshort <- ifelse(cseason == "S", "Apr", "Oct")

weourl <- paste0("https://www.imf.org/-/media/Files/Publications/WEO/WEO-Database/",
                 cyear, "/",
                 cmonthlong, "/",
                 "WEO",
                 cmonthshort,
                 cyear,
                 "all.ashx")

#mode "wb" (otherwise won't be readable)
if(FALSE){
download.file(weourl,
              here(location_download, paste0("WEOforecasts", cseason, cyear, "_full.tsv")),
              mode = "wb")
}

weocedition <- read.delim(
  here(location_download, paste0("WEOforecasts", cseason, cyear, "_full.tsv")),
  sep = "\t", fileEncoding = "UTF-16LE", stringsAsFactors = FALSE
  ) |>
  as.data.table()

write_parquet(weocedition, here(location_download, paste0("WEOforecasts", cseason, cyear, "_full.parquet")))

subyears <- paste0("X", 1980:(cyear+5)) #all years in data
weocedition <- read_parquet(
  here(location_download,
       paste0("WEOforecasts", cseason, cyear, "_full.parquet"))
  ) |>
  as.data.table() |>
  .d(WEO.Subject.Code %in% c("NGDP_RPCH", "PCPIPCH")) |>
  .d(, .SD, .SDcols = c("ISO", "WEO.Subject.Code", subyears)) |>
  melt(id.vars = c("ISO", "WEO.Subject.Code"),
       value.name = "prediction", variable.name = "target_year") |>
  .d(ISO %in% c("DEU", "USA", "ITA", "JPN", "FRA", "CAN", "GBR")) |>
  setnames("ISO", "country") |>
  setnames("WEO.Subject.Code", "target") |>
  .d(, target_year := as.numeric(gsub("X", "", target_year))) |>
  .d(, target := ifelse(target == "NGDP_RPCH", "ngdp_rpch", "pcpi_pch")) |>
  .d(target_year %in% c(cyear - 1, cyear, cyear + 1)) |>
  .d(, prediction := as.numeric(prediction)) |>
  #reorder columns
  .d(, .SD, .SDcols = c("country", "target", "target_year", "prediction"))

name_tv <- ifelse(cseason == "S", "tv_0.5", "tv_1")
alttruth <- ifelse(cseason == "S", "tv_1", "tv_0.5")

truth_data <- weocedition |>
  .d(target_year == (cyear - 1)) |>
  setnames("prediction", name_tv)  |>
  .d(, altval := as.numeric(NA)) |>
  setnames("altval", alttruth)

forecast_data <- weocedition |>
  .d(target_year %in% c(cyear, cyear + 1)) |>
  .d(, forecast_year := cyear) |>
  .d(, forecast_season := cseason) |>
  .d(, season_helper := ifelse(forecast_season == "S", 0.5, 0)) |>
  .d(, horizon := (target_year - forecast_year) + season_helper) |>
  .d(, season_helper := NULL) |>
  .d(, c("tv_0.5", "tv_1", "tv_1.5", "tv_2") := as.numeric(NA))  # explicitly code true value as missing

data.table::fwrite(truth_data, here(location_download, paste0("weodat_truth", cseason, cyear, ".csv")))
data.table::fwrite(forecast_data, here(location_download, paste0("weodat_fcsts", cseason, cyear, ".csv")))
