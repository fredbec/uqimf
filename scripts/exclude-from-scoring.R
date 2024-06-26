library(here)
library(data.table)
devtools::load_all()

.d <- `[`

source(here("specs", "specs.R"))
instances_to_exclude <- specs$instances_to_exclude
instances_to_exclude_bvarspecs <- specs$instances_to_exclude_bvarspecs




####################################read in quantile forecasts########################
qufcs <- data.table::fread(here("quantile_forecasts", "quantile_forecasts.csv"))
bvar_qus <- data.table::fread(here("benchmarks", "quantile_benchmarks_processed.csv"))
qufcs_pava <- data.table::fread(here("quantile_forecasts", "quantile_forecasts_pava.csv"))
fcdat <- data.table::fread(here("data", "point_forecasts.csv"))

for(k in 1:length(instances_to_exclude)){

  inst <- instances_to_exclude[[k]]
  qufcs <- exclude_rows(qufcs, inst)
  bvar_qus <- exclude_rows(bvar_qus, inst)
  qufcs_pava <- exclude_rows(qufcs_pava, inst)
  fcdat <- exclude_rows(fcdat, inst)
}


data.table::fwrite(qufcs, here("quantile_forecasts", "quantile_forecasts_toscore.csv"))
data.table::fwrite(bvar_qus, here("benchmarks", "quantile_benchmarks_processed_toscore.csv"))
data.table::fwrite(qufcs_pava, here("quantile_forecasts", "quantile_forecasts_pava_toscore.csv"))
data.table::fwrite(fcdat, here("data", "point_forecasts_toscore.csv"))


###again for scoring the bvar specifications
####################################read in quantile forecasts########################
qufcs <- data.table::fread(here("quantile_forecasts", "quantile_forecasts.csv"))
bvar_qus <- data.table::fread(here("benchmarks", "quantile_benchmarks_processed.csv"))
qufcs_pava <- data.table::fread(here("quantile_forecasts", "quantile_forecasts_pava.csv"))
fcdat <- data.table::fread(here("data", "point_forecasts.csv"))

for(k in 1:length(instances_to_exclude_bvarspecs)){

  inst <- instances_to_exclude_bvarspecs[[k]]
  qufcs <- exclude_rows(qufcs, inst)
  bvar_qus <- exclude_rows(bvar_qus, inst)
  qufcs_pava <- exclude_rows(qufcs_pava, inst)
  fcdat <- exclude_rows(fcdat, inst)
}


data.table::fwrite(qufcs, here("quantile_forecasts", "quantile_forecasts_toscore_bvarspecs.csv"))
data.table::fwrite(bvar_qus, here("benchmarks", "quantile_benchmarks_processed_toscore_bvarspecs.csv"))
data.table::fwrite(qufcs_pava, here("quantile_forecasts", "quantile_forecasts_pava_toscore_bvarspecs.csv"))
data.table::fwrite(fcdat, here("data", "point_forecasts_toscore_bvarspecs.csv"))
