library(here)
library(data.table)


source(here("specs", "specs.R"))

tv_release <- specs$tv_release
cset <- specs$cset
ciset <- specs$ciset
global_file_prefix <- paste0(ifelse(tv_release == "oecd", "oecd_", ""),
                             ifelse(cset == "extended", "extcntry_", ""),
                             ifelse(ciset == "extended", "extcis_", ""))

location_download <- "data"

devtools::load_all()
.d <- `[`

#first check if new download is/should be available
download <- FALSE #could be automated, but don't see the need right now

if(download){
  source(here("scripts", "download-data.R"))
}
source(here("scripts", "process-weo.R"))
source(here("scripts", "process-benchmarks.R"))
source(here("scripts", "encode-missing-predictions.R"))
source(here("scripts", "make-forecasts.R"))
source(here("scripts", "exclude-from-scoring.R"))
#run on demand (produces bvar_mix forecasts)
bvar_prefix <- ""
source(here("benchmarks", "raw", "forecasts_2025", "bvar-mix.R"))
bvar_prefix <- "_bvarspecs"
source(here("benchmarks", "raw", "forecasts_2025", "bvar-mix.R"))
prefix <- ""
source(here("scripts", "score-forecasts.R"))

if(cset == "base" & ciset == "base"){
  prefix <- "_bvarspecs"
  source(here("scripts", "score-forecasts.R"))
}
