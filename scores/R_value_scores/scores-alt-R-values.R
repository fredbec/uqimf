library(here)
library(data.table)

devtools::load_all()
.d <- `[`

R_values <- 4:11
min_score_yr_forall <- 2001 #will be used to filter in score_forecasts

##NOTE: IF THERE SHOULD EVER BE ANY EXCLUSIONS ON THE TRAINING SET (NOT THE CASE
##CURRENTLY), WE HAVE TO CHANGE THIS TO ALSO RUN EXCLUDEFROMSCORING

for(window_length in R_values){

  source(here("scores", "R_value_scores", "make-forecasts.R"))

  source(here("scores", "R_value_scores", "score-forecasts.R"))
}

