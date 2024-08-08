library(here)
library(data.table)

devtools::load_all()
.d <- `[`

R_values <- 4:11

for(window_length in R_values){

  source(here("scores", "R_value_scores", "make-forecasts.R"))
  source(here("scores", "R_value_scores", "score-forecasts.R"))
}

