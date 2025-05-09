flag_imputetv05as1 <- specs$flag_imputetv05as1

###############################################################################
#extract truth values from WEO forecasts
truth <- data.table::fread(
  here(location_download, "weodat.csv")
) |>
  .d(, .(country, target, target_year, tv_0.5, tv_1, tv_1.5, tv_2))


if(flag_imputetv05as1){
  cyear <- format(Sys.Date(), "%Y") |> as.numeric()

  truth <- truth |>
    copy() |>
    .d(is.na(tv_1) & target_year == cyear - 1,tv_1 := tv_0.5)

}

truth <- truth |>
  .d(!is.na(get(paste0("tv_", tv_release)))) |>
  unique()


#read in WEO forecasts
weodat <- fread(here(location_download, "weodat.csv")) |>
  .d(, source := "IMF") |>
  .d(order(source, target, country, forecast_year, horizon)) |>
  split(by = c("source")) |>
  lapply(function(dt)
    truth[dt, on = c("target", "target_year", "country")]
  ) |>
  rbindlist(idcol = "source") |>
  .d(, .(source, target, country, forecast_year, horizon, target_year,
         prediction, get(paste0("tv_", tv_release)))) |>
  setnames("V8", paste0("tv_", tv_release))

data.table::fwrite(weodat, here(location_download, paste0("weodat.csv")))
