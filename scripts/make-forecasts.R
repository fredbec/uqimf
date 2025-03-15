library(here)
library(data.table)
devtools::load_all()

source(here("specs", "specs.R"))

.d <- `[`

max_year <- specs$max_year
min_year <- specs$min_year
holdout_split <- specs$holdout_split
window_length <- specs$window_length
quantype <- specs$qutype

if(specs$ciset == "extended"){
  cis <- specs$ci_levels_make_extended
} else {
  cis <- specs$ci_levels_make
}

#make all combinations of settings
combs <- data.table::fread(here("quantile_forecasts", "setting_combinations.csv"))


fcdat <- data.table::fread(here("data", paste0(global_file_prefix, "point_forecasts.csv")))

#make quantile forecasts for all combinations in combs
qufcs <- lapply(1:nrow(combs),
  function(idxsub){

    setting <- combs[idxsub]

    start_year <- min_year + window_length

    subdat <- fcdat |>
      .d(source == setting[, "source"]) |>
      .d(target == setting[, "target"]) |>
      .d(, source := NULL)

    empFC(subdat,
          target_year = start_year:max_year,
          tv_release = tv_release,
          error_method = setting[, "error_method"],
          method = setting[, "method"],
          window_length = window_length,
          ci_levels = cis,
          qutype = quantype) |>
      .d(, source := setting[, "source"]) |>
      .d(, error_method := setting[, "error_method"]) |>
      .d(, method := setting[, "method"])

    }
  ) |>
  rbindlist() |>
  setnames(paste0("tv_", tv_release), "true_value") |>
  .d(, holdout := ifelse(target_year >= holdout_split, "_ho", ""))
  #.d(!is.na(true_value))




################  save forecasts  ################
#first, split into respective datasets
qufcs <- qufcs |>
  split(by = c("error_method", "holdout"))


#then save data, do pava correction and make ensemble
emptycontainer <- lapply(qufcs, function(dat){

  em_suffix <- unique(dat$error_method)
  split_suffix <- unique(dat$holdout)

  #remove holdout column
  dat <- dat |>
    .d(, holdout := NULL)

  #Absolute forecasts
  if(em_suffix == "absolute"){
    #pava correction for absolute forecasts
    dat <- dat |>
      empFC_pava(ci_levels = cis)

    ensdat <- dat |>
      make_ensemble(summary_function = mean,
                    incl = c("IMF", "ar", "bvar")) |>
      .d(, error_prediction := NA) |>
      .d(, error := NA) |>
      .d(, imf_pp := NA)

    dat <- rbind(dat, ensdat)

    data.table::fwrite(dat,
                       here("quantile_forecasts",
                            paste0(global_file_prefix, "quantile_forecasts",split_suffix, ".csv")))


    ##Directional forecasts
  } else if (em_suffix == "directional"){

    if(split_suffix == ""){
      ensdat <- dat |>
        make_ensemble(summary_function = mean,
                      incl = c("IMF", "ar", "bvar")) |>
        .d(, error_prediction := NA) |>
        .d(, error := NA) |>
        .d(, imf_pp := NA)

      dat <- rbind(dat, ensdat)

      data.table::fwrite(dat,
                         here("quantile_forecasts",
                              paste0(global_file_prefix, "quantile_forecasts",split_suffix, "_", em_suffix, ".csv")))
    } else {
      #not saved, as we're not using directional method on the holdout set
    }
  }

})
