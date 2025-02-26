library(here)
library(data.table)
library(arrow)


.d <- `[`

#short script to process WEO data for this project

weodat <- read_parquet(here(location_download, "WEOforecasts_prefilter.parquet"))

if(cset == "extended"){
  weodat <- weodat |>
    .d(ISOAlpha_3Code %in% specs$cset_list) #decide what to do
} else { #in base version, only doing g7
  weodat <- weodat |>
    .d(g7 == 1)
}

weodat <- weodat |>
  .d(, .(ISOAlpha_3Code, target, target_year, forecast_season, horizon,
         forecast_year, prediction, tv_0.5, tv_1, tv_1.5, tv_2)) |>
  .d(horizon < 2) |>
  setnames("ISOAlpha_3Code", "country")


data.table::fwrite(weodat, here(location_download, "weodat.csv"))
