library(here)
library(data.table)
source(here("specs", "specs_real_time_publication.R"))
source(here("real_time_publication", "deploy_scripts", "utils_real_time_pub.R"))

devtools::load_all()
.d <- `[`

#final location for publishing
#i.e. a local repo that has kitmetricslab/macroPI set as remote
ready_to_publish <- FALSE
publish_dest <- here("..", "MacroPI")

download_data <- FALSE

cset <- "base"

#to determine current year and season
cyear <- format(Sys.Date(), "%Y") |> as.numeric()
cday <- format(Sys.Date(), "%j") |> as.numeric()
#set April 10 and October 10 as first days to check for new release
cseason <- ifelse(cday > 100 & cday < 283, "S", "F")
cyear <- ifelse(cday < 100, cyear - 1, cyear)


#folder to save forecasts into
path_dest <- here("real_time_publication")

#set location to download data into
location_download <- here("real_time_publication", "downloaded_data")
tv_release <- specs$tv_release


if(download_data){
  #source(here("scripts", "download-data.R"))
  source(here("real_time_publication", "deploy_scripts", "download_weo_postF25.R"))
}
source(here("scripts", "process-weo.R"))
hist_weodat <- fread(here(location_download, "weodat.csv"))
#write as preprocess
data.table::fwrite(hist_weodat, here(location_download, "weodat_preprocess.csv"))

#this is just quick and dirty for now, move all of this to an external script
#and keep a proper database incl. log
hist_weodat <- fread(here(location_download, "weodat_preprocess.csv"))
c_weodat_truth <- fread(here(location_download, paste0("weodat_truth", cseason, cyear, ".csv"))) |>
  setnames("tv_1", "new_tv_1") |>
  setnames("tv_0.5", "new_tv_0.5")

#update with new truth data
hist_weodat <- c_weodat_truth[hist_weodat, on = c("country", "target", "target_year")] |>
  .d(target_year == (cyear-1), tv_1 := ifelse(is.na(tv_1), new_tv_1)) |>
  .d(target_year == (cyear-1), tv_0.5 := ifelse(is.na(tv_0.5), new_tv_0.5)) |>
  .d(, new_tv_1 := NULL) |>
  .d(, new_tv_0.5 := NULL)
c_weodat_fcsts <- fread(here(location_download, paste0("weodat_fcsts", cseason, cyear, ".csv")))

weodat <- rbind(hist_weodat, c_weodat_fcsts)
current_yr_season <- get_current_year_season(weodat) #for file names

data.table::fwrite(weodat, here(location_download, "weodat_preprocess.csv"))

#process further (change row order, change some column names)
source(here("real_time_publication", "deploy_scripts", "process_weo_real_time_pub.R"))
weodat <- fread(here(location_download, "weodat.csv"))

source(here("real_time_publication", "deploy_scripts", "make_forecasts.R"))
source(here("real_time_publication", "deploy_scripts", "make_extra_data.R"))

source(here("real_time_publication", "deploy_scripts", "check_forecast_data.R"))


if(ready_to_publish){

  forecasts <- data.table::fread(here(path_dest, "forecasts", paste0("forecasts_", current_yr_season$identifier, ".csv")))
  data.table::fwrite(forecasts, here(publish_dest, "forecasts", paste0("forecasts_", current_yr_season$identifier, ".csv")))

  realized_vals <- data.table::fread(here(path_dest, "imf-data", paste0("historicvalues_", current_yr_season$identifier, ".csv")))
  data.table::fwrite(realized_vals, here(publish_dest, "imf-data", paste0("historicvalues_", current_yr_season$identifier, ".csv")))

  point_forecasts <- data.table::fread(here(path_dest, "imf-data", paste0("pointforecasts_", current_yr_season$identifier, ".csv")))
  data.table::fwrite(point_forecasts, here(publish_dest, "imf-data", paste0("pointforecasts_", current_yr_season$identifier, ".csv")))

}
