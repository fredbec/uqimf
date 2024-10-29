library(here)
library(data.table)
library(arrow)


.d <- `[`

#short script to process WEO data for this project

weodat <- read_parquet(here(location_download, "WEOforecasts_prefilter.parquet")) |>
  .d(g7 == 1) |> #only keep g7 countries
  .d(, .(ISOAlpha_3Code, target, target_year, forecast_season, horizon,
         forecast_year, prediction, tv_0.5, tv_1, tv_1.5, tv_2)) |>
  .d(horizon < 2) |>
  setnames("ISOAlpha_3Code", "country")


data.table::fwrite(weodat, here(location_download, "weodat.csv"))
