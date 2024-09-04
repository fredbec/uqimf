library(here)
library(data.table)
source(here("specs", "specs_real_time_publication.R"))
source(here("real_time_publication", "utils_real_time_pub.R"))

devtools::load_all()
.d <- `[`


location_download <- "real_time_publication"
tv_release <- specs$tv_release


####setting current year and season. This is done by checking the currently
####downloaded WEO forecast data based on the repo structure used. Feel free
####to set these manually, as you see fit


#source(here("scripts", "download-data.R"))
source(here("scripts", "process-weo.R"))
weodat <- fread(here("real_time_publication", "weodat.csv"))
current_yr_season <- get_current_year_season(weodat)

#process further
source(here("real_time_publication", "process_weo_real_time_pub.R"))
weodat <- fread(here("real_time_publication", "weodat.csv"))

