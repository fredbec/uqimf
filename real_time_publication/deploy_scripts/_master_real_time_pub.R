library(here)
library(data.table)
source(here("specs", "specs_real_time_publication.R"))
source(here("real_time_publication", "deploy_scripts", "utils_real_time_pub.R"))

devtools::load_all()
.d <- `[`


#folder to save forecasts into
path_dest <- here("real_time_publication")

#set location to download data into
location_download <- here("real_time_publication", "downloaded_data")
tv_release <- specs$tv_release


#source(here("scripts", "download-data.R"))
source(here("scripts", "process-weo.R"))
weodat <- fread(here(location_download, "weodat.csv"))
current_yr_season <- get_current_year_season(weodat)

#process further
source(here("real_time_publication", "deploy_scripts", "process_weo_real_time_pub.R"))
weodat <- fread(here(location_download, "weodat.csv"))

source(here("real_time_publication", "deploy_scripts", "make_forecasts.R"))
source(here("real_time_publication", "deploy_scripts", "make_extra_data.R"))

source(here("real_time_publication", "deploy_scripts", "check_forecast_data.R"))
