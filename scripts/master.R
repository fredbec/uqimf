library(here)
library(data.table)

devtools::load_all()
.d <- `[`

#first check if new download is/should be available
download <- FALSE #could be automated, but don't see the need right now

#set token that master file started to "started but not finished"
saveRDS("started master, but did not finish. Check if something is wrong.", here("currentscorestate.RDS"))
saveRDS("started master, but did not finish. Check if something is wrong.", here("currenttruthstate.RDS"))

if(download){
  source(here("scripts", "download-data.R"))
}
source(here("scripts", "process-weo.R"))
source(here("scripts", "process-benchmarks.R"))
source(here("scripts", "encode-missing-predictions.R"))
source(here("scripts", "make-forecasts.R"))
source(here("scripts", "exclude-from-scoring.R"))
source(here("scripts", "score-forecasts.R"))


#set token that master file started to "started but not finished"
saveRDS(specs$scoreset, here("currentscorestate.RDS"))
saveRDS(specs$tv_release, here("currenttruthstate.RDS"))
