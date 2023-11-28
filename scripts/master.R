library(here)
library(data.table)

devtools::load_all()
.d <- `[`

#first check if new download is/should be available
download <- FALSE #could be automated, but don't see the need right now

if(download){
  source(here("scripts", "download-data.R"))
  source(here("scripts", "process-weo.R"))
  source(here("scripts", "process-benchmarks.R"))
}
source(here("scripts", "make-forecasts.R"))
source(here("scripts", "score-forecasts.R"))

