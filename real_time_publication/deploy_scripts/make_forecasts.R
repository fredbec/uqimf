library(data.table)
library(here)
library(ggplot2)
library(MetBrewer)
library(patchwork)
library(lubridate)
devtools::load_all()

.d <- `[`

qus <- c(0.1, 0.25, 0.75, 0.9)


####setting current year and season. This is done by checking the currently
####downloaded WEO forecast data based on the repo structure used. Feel free
####to set these manually, as you see fit
weodat <- fread(here(location_download, "weodat.csv"))

window_length <- specs$window_length
emethod <- specs$error_method
rmethod <- specs$method
quantype <- specs$qutype
cis <- specs$ci_levels_eval
qus <- specs$qu_levels

target_year_start <- current_yr_season$forecast_year
target_year_end <- current_yr_season$forecast_year + 1


qufcs_pcpi <- make_qufcs(weodat,
                         "pcpi_pch",
                         target_year_start,
                         target_year_end,
                         tv_release,
                         emethod,
                         rmethod,
                         window_length,
                         cis,
                         qutype,
                         current_yr_season) |>
  apply_pava_and_clean(error_method = emethod,
                       method = rmethod,
                       current_yr_season = current_yr_season) |>
  .d(, prediction := round(prediction, 3))


qufcs_ngdp <- make_qufcs(weodat,
                          "ngdp_rpch",
                          target_year_start,
                          target_year_end,
                          tv_release,
                          emethod,
                          rmethod,
                          window_length,
                          cis,
                          qutype,
                          current_yr_season) |>
  #add in placeholder
  apply_pava_and_clean(error_method = emethod,
                       method = rmethod,
                       current_yr_season = current_yr_season) |>
  .d(, prediction := round(prediction, 3))


full_qufcs <- rbind(qufcs_pcpi, qufcs_ngdp)

data.table::fwrite(full_qufcs, here(path_dest, "forecasts", paste0("forecasts_", current_yr_season$identifier, ".csv")))
