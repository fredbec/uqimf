library(here)
library(data.table)
devtools::load_all()

.d <- `[`

source(here("specs", "specs.R"))
instances_to_exclude <- specs$instances_to_exclude




####################################read in quantile forecasts########################
qufcs <- data.table::fread(here("quantile_forecasts", "quantile_forecasts.csv"))
bvar_qus <- data.table::fread(here("benchmarks", "quantile_benchmarks_processed.csv"))
qufcs_pava <- data.table::fread(here("quantile_forecasts", "quantile_forecasts_pava.csv"))

for(k in 1:length(instances_to_exclude)){

  inst <- instances_to_exclude[[k]]
  qufcs <- exclude_rows(qufcs, inst)
  bvar_qus <- exclude_rows(bvar_qus, inst)
  qufcs_pava <- exclude_rows(qufcs_pava, inst)
}



data.table::fwrite(qufcs, here("quantile_forecasts", "quantile_forecasts.csv"))
data.table::fwrite(bvar_qus, here("benchmarks", "quantile_benchmarks_processed.csv"))
data.table::fwrite(qufcs_pava, here("quantile_forecasts", "quantile_forecasts_pava.csv"))

