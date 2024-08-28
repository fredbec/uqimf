library(data.table)
library(here)
devtools::load_all()
#this file was run once before and once after changing the code in uqfc.R
#for 'correctly'/honestly sampling with respect to horizon
#only last line (name of saved file) was changed

source(here("specs", "specs.R"))

.d <- `[`

max_year_imf <- specs$max_year_imf
max_year_others <- specs$max_year_others
min_year <- 2000
tv_release <- specs$tv_release
window_length <- specs$window_length

cis <- specs$ci_levels_make

#make all combinations of settings
combs <- data.table::fread(here("quantile_forecasts", "setting_combinations.csv"))


fcdat <- data.table::fread(here("data", "point_forecasts.csv")) |>
  .d(country %in% c("CAN", "JAP"))

#make quantile forecasts for all combinations in combs
testqus <- lapply(1:nrow(combs),
                function(idxsub){

                  setting <- combs[idxsub]

                  if(setting[, "method"] == "leave-one-out"){

                    start_year <- min_year
                  } else {

                    start_year <- min_year + window_length
                  }

                  subdat <- fcdat |>
                    .d(source == setting[, "source"]) |>
                    .d(target == setting[, "target"]) |>
                    .d(, source := NULL)

                  if (setting[, "source"] %in% c("bvar", "ar")) {
                    max_year <- specs$max_year_others
                  } else {

                    max_year <- specs$max_year_imf
                  }

                  empFC(subdat,
                        target_year = start_year:max_year,
                        tv_release = tv_release,
                        error_method = setting[, "error_method"],
                        method = setting[, "method"],
                        window_length = window_length,
                        ci_levels = cis) |>
                    .d(, source := setting[, "source"]) |>
                    .d(, error_method := setting[, "error_method"]) |>
                    .d(, method := setting[, "method"])

                }
) |>
  rbindlist() |>
  setnames(paste0("tv_", tv_release), "true_value")

#fwrite(testqus, here("qufcs_beforehorshift.csv"))
fwrite(testqus, here("qufcs_afterhorshift.csv"))












######################Comparison####### (after saving)
after <- fread(here("qufcs_afterhorshift.csv"))
before<- fread(here("qufcs_beforehorshift.csv"))

nrow(after)
nrow(before)

all.equal(after|>
            .d(horizon >= 1),
          before |>
            .d(horizon >= 1))


after <- after |>
  .d(horizon >= 1) |>
  .d(source == "IMF") |>
  .d(error_method == "absolute") |>
  .d(method != "leave-one-out") |>
  .d(, .(country, target, target_year, horizon, quantile, prediction))



before <- before |>
  .d(horizon >= 1) |>
  .d(source == "IMF") |>
  .d(error_method == "absolute") |>
  .d(method != "leave-one-out") |>
  .d(, .(country, target, target_year, horizon, quantile, prediction))


together <- after[before, on = c("country", "target", "target_year", "horizon", "quantile")] |>
  .d(, diff := prediction - i.prediction)


