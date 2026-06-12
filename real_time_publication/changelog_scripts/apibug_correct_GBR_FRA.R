library(here)
library(data.table)
source(here("specs", "specs_real_time_publication.R"))

devtools::load_all()
.d <- `[`


cyear <- 2025
cseason <- "F"


location_download <- here("real_time_publication", "downloaded_data")

Fall25_truth <- fread(here(location_download, paste0("weodat_truth", cseason, cyear, ".csv")))
for (tgt in c("ngdp_rpch", "pcpi_pch")) {

  gbr_val <- Fall25_truth[country == "GBR" & target == tgt & target_year == 2024, tv_1]
  fra_val <- Fall25_truth[country == "FRA" & target == tgt & target_year == 2024, tv_1]

  # Guard: confirm we're in the wrong state before correcting
  if(tgt == "ngdp_rpch"){
    stopifnot(gbr_val == 1.102)
    stopifnot(fra_val == 1.101)
  } else if (tgt == "pcpi_pch"){
    stopifnot(gbr_val == 2.317)
    stopifnot(fra_val == 2.530)
  }

  Fall25_truth[country == "GBR" & target == tgt & target_year == 2024, tv_1 := fra_val]
  Fall25_truth[country == "FRA" & target == tgt & target_year == 2024, tv_1 := gbr_val]

}
setorder(Fall25_truth, target, country, target_year)
fwrite(Fall25_truth, here(location_download, paste0("weodat_truth", cseason, cyear, ".csv")))


Fall25_fcsts <- fread(here(location_download, paste0("weodat_fcsts", cseason, cyear, ".csv")))
for (tgt in c("ngdp_rpch", "pcpi_pch")) {

  gbr_val_25 <- Fall25_fcsts[country == "GBR" & target == tgt & target_year == 2025, prediction]
  fra_val_25 <- Fall25_fcsts[country == "FRA" & target == tgt & target_year == 2025, prediction]
  gbr_val_26 <- Fall25_fcsts[country == "GBR" & target == tgt & target_year == 2026, prediction]
  fra_val_26 <- Fall25_fcsts[country == "FRA" & target == tgt & target_year == 2026, prediction]

  # Guard: confirm we're in the wrong state before correcting
  if(tgt == "ngdp_rpch"){
    stopifnot(gbr_val_25 == 0.671)
    stopifnot(fra_val_25 == 1.308)

    stopifnot(gbr_val_26 == 0.911)
    stopifnot(fra_val_26 == 1.264)
  } else if (tgt == "pcpi_pch"){
    stopifnot(gbr_val_25 == 1.143)
    stopifnot(fra_val_25 == 3.402)

    stopifnot(gbr_val_26 == 1.526)
    stopifnot(fra_val_26 == 2.546)
  }

  Fall25_fcsts[country == "GBR" & target == tgt & target_year == 2025, prediction := fra_val_25]
  Fall25_fcsts[country == "FRA" & target == tgt & target_year == 2025, prediction := gbr_val_25]
  Fall25_fcsts[country == "GBR" & target == tgt & target_year == 2026, prediction := fra_val_26]
  Fall25_fcsts[country == "FRA" & target == tgt & target_year == 2026, prediction := gbr_val_26]

}
setorder(Fall25_fcsts, target, country, target_year)
fwrite(Fall25_fcsts, here(location_download, paste0("weodat_fcsts", cseason, cyear, ".csv")))


cyear <- 2026
cseason <- "S"
Spring26_truth <- fread(here(location_download, paste0("weodat_truth", cseason, cyear, ".csv")))
for (tgt in c("ngdp_rpch", "pcpi_pch")) {

  gbr_val <- Spring26_truth[country == "GBR" & target == tgt & target_year == 2025, tv_0.5]
  fra_val <- Spring26_truth[country == "FRA" & target == tgt & target_year == 2025, tv_0.5]

  # Guard: confirm we're in the wrong state before correcting
  if(tgt == "ngdp_rpch"){
    stopifnot(gbr_val == 0.928126)
    stopifnot(fra_val == 1.323384)
  } else if (tgt == "pcpi_pch"){
    stopifnot(gbr_val == 0.926027)
    stopifnot(fra_val == 3.373037)
  }

  Spring26_truth[country == "GBR" & target == tgt & target_year == 2025, tv_0.5 := fra_val]
  Spring26_truth[country == "FRA" & target == tgt & target_year == 2025, tv_0.5 := gbr_val]

}
setorder(Spring26_truth, target, country, target_year)
fwrite(Spring26_truth, here(location_download, paste0("weodat_truth", cseason, cyear, ".csv")))


Spring26_fcsts <- fread(here(location_download, paste0("weodat_fcsts", cseason, cyear, ".csv")))
for (tgt in c("ngdp_rpch", "pcpi_pch")) {

  gbr_val_26 <- Spring26_fcsts[country == "GBR" & target == tgt & target_year == 2026, prediction]
  fra_val_26 <- Spring26_fcsts[country == "FRA" & target == tgt & target_year == 2026, prediction]
  gbr_val_27 <- Spring26_fcsts[country == "GBR" & target == tgt & target_year == 2027, prediction]
  fra_val_27 <- Spring26_fcsts[country == "FRA" & target == tgt & target_year == 2027, prediction]

  # Guard: confirm we're in the wrong state before correcting
  if(tgt == "ngdp_rpch"){
    stopifnot(gbr_val_26 == 0.858141)
    stopifnot(fra_val_26 == 0.796780)

    stopifnot(gbr_val_27 == 0.876108)
    stopifnot(fra_val_27 == 1.296787)
  } else if (tgt == "pcpi_pch"){
    stopifnot(gbr_val_26 == 1.843886)
    stopifnot(fra_val_26 == 3.202959)

    stopifnot(gbr_val_27 == 1.721117)
    stopifnot(fra_val_27 == 2.417978)
  }

  Spring26_fcsts[country == "GBR" & target == tgt & target_year == 2026, prediction := fra_val_26]
  Spring26_fcsts[country == "FRA" & target == tgt & target_year == 2026, prediction := gbr_val_26]
  Spring26_fcsts[country == "GBR" & target == tgt & target_year == 2027, prediction := fra_val_27]
  Spring26_fcsts[country == "FRA" & target == tgt & target_year == 2027, prediction := gbr_val_27]

}
setorder(Spring26_fcsts, target, country, target_year)
fwrite(Spring26_fcsts, here(location_download, paste0("weodat_fcsts", cseason, cyear, ".csv")))
