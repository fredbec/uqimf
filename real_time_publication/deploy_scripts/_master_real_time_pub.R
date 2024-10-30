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

download_data <- TRUE


#folder to save forecasts into
path_dest <- here("real_time_publication")

#set location to download data into
location_download <- here("real_time_publication", "downloaded_data")
tv_release <- specs$tv_release


if(download_data){
  source(here("scripts", "download-data.R"))
}
source(here("scripts", "process-weo.R"))
weodat <- fread(here(location_download, "weodat.csv"))
current_yr_season <- get_current_year_season(weodat)

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
