library(here)
library(data.table)
devtools::load_all()

.d <- `[`

source(here("specs", "specs.R"))
instances_to_exclude <- specs$instances_to_exclude
instances_to_exclude_bvarspecs <- specs$instances_to_exclude_bvarspecs


####################################read in quantile forecasts########################
qufcs <- data.table::fread(here("quantile_forecasts", "quantile_forecasts.csv"))
qufcs_ho <- data.table::fread(here("quantile_forecasts", "quantile_forecasts_ho.csv"))
qufcs_directional <- data.table::fread(here("quantile_forecasts", "quantile_forecasts_directional.csv"))
bvar_qus <- data.table::fread(here("benchmarks", "bvar_direct_quantile_forecasts.csv"))
bvar_qus_ho <- data.table::fread(here("benchmarks", "bvar_direct_quantile_forecasts_ho.csv"))
fcdat <- data.table::fread(here("data", "point_forecasts.csv"))

for(k in 1:length(instances_to_exclude)){

  inst <- instances_to_exclude[[k]]
  qufcs <- exclude_rows(qufcs, inst)
  qufcs_ho <- exclude_rows(qufcs_ho, inst)
  qufcs_directional <- exclude_rows(qufcs_directional, inst)
  bvar_qus <- exclude_rows(bvar_qus, inst)
  bvar_qus_ho <- exclude_rows(bvar_qus_ho, inst)
  fcdat <- exclude_rows(fcdat, inst)
}


data.table::fwrite(qufcs, here("quantile_forecasts", "toscore_quantile_forecasts.csv"))
data.table::fwrite(qufcs_directional, here("quantile_forecasts", "toscore_quantile_forecasts_directional.csv"))
data.table::fwrite(qufcs_ho, here("quantile_forecasts", "toscore_quantile_forecasts_ho.csv"))
data.table::fwrite(bvar_qus, here("benchmarks", "toscore_bvar_direct_quantile_forecasts.csv"))
data.table::fwrite(bvar_qus_ho, here("benchmarks", "toscore_bvar_direct_quantile_forecasts_ho.csv"))
data.table::fwrite(fcdat, here("data", "toscore_point_forecasts.csv"))


####################################read in quantile forecasts########################
qufcs <- data.table::fread(here("quantile_forecasts", "quantile_forecasts.csv"))
qufcs_ho <- data.table::fread(here("quantile_forecasts", "quantile_forecasts_ho.csv"))
qufcs_directional <- data.table::fread(here("quantile_forecasts", "quantile_forecasts_directional.csv"))
bvar_qus <- data.table::fread(here("benchmarks", "bvar_direct_quantile_forecasts.csv"))
bvar_qus_ho <- data.table::fread(here("benchmarks", "bvar_direct_quantile_forecasts_ho.csv"))

for(k in 1:length(instances_to_exclude_bvarspecs)){

  inst <- instances_to_exclude_bvarspecs[[k]]
  qufcs <- exclude_rows(qufcs, inst)
  qufcs_ho <- exclude_rows(qufcs_ho, inst)
  qufcs_directional <- exclude_rows(qufcs_directional, inst)
  bvar_qus <- exclude_rows(bvar_qus, inst)
  bvar_qus_ho <- exclude_rows(bvar_qus_ho, inst)
  fcdat <- exclude_rows(fcdat, inst)
}


data.table::fwrite(qufcs, here("quantile_forecasts", "toscore_bvarspecs_quantile_forecasts.csv"))
data.table::fwrite(qufcs_directional, here("quantile_forecasts", "toscore_bvarspecs_quantile_forecasts_directional.csv"))
data.table::fwrite(qufcs_ho, here("quantile_forecasts", "toscore_bvarspecs_quantile_forecasts_ho.csv"))
data.table::fwrite(bvar_qus, here("benchmarks", "toscore_bvarspecs_bvar_direct_quantile_forecasts.csv"))
data.table::fwrite(bvar_qus_ho, here("benchmarks", "toscore_bvarspecs_bvar_direct_quantile_forecasts_ho.csv"))
data.table::fwrite(fcdat, here("data", "toscore_bvarspecs_point_forecasts.csv"))


