knitr::opts_chunk$set(echo = TRUE)
library(data.table)
.d <- `[`
set.seed(42)


###SETTINGS##########################
nits <- 10000
valsn <- exp(seq(2,10, by = 0.05)) |> floor() |> as.list()
qumeth <- c(1, 7, 8)



###FUNCTION##########################
simcilength <- function(n, nits, lvl = 0.8, qumeth = c(1,7,8)){
  res <- vector(mode = "list", length = nits)
  res_cmp <- vector(mode = "list", length = nits)

  if(lvl == 0.8){
    upper <- 0.9
    lower <- 0.1
  } else if (lvl == 0.5){
    upper <- 0.75
    lower <- 0.25
  }

  for(i in 1:nits){

    resvec <- rep(NA, 2*length(qumeth))
    names(resvec) <- apply(expand.grid(c("absolute", "directional"), paste0("q", qumeth)), 1, paste, collapse="_")

    resvec_cmp <- rep(NA, length(qumeth))
    names(resvec) <- paste0("q", qumeth)


    dat <- rnorm(n = n)


    for(k in seq_along(qumeth)){
      resdir <-  quantile(dat, upper, type = qumeth[k]) - quantile(dat, lower, type = qumeth[k])
      resabs <-  2* quantile(abs(dat), type = qumeth[k], lvl)
      resvec[2*k - 1] <- resdir
      resvec[2*k] <- resabs

      resvec_cmp[k] <- (resabs - resdir) > 0
    }

    res[[i]] <- data.table(t(resvec))
    res_cmp[[i]] <- data.table(t(resvec_cmp))
  }
  res <- data.table::rbindlist(res)
  res_cmp <- data.table::rbindlist(res_cmp)
  names(res) <- apply(expand.grid(c("absolute", "directional"), paste0("q", qumeth)), 1, paste, collapse="_")
  names(res_cmp) <- paste0("q", qumeth)

  result <- apply(res_cmp, 2, mean) |> t() |> as.data.table()

  return(list(res, res_cmp, result))
}


###SIMULATE 80% CI##########################
simdat <- lapply(valsn, function(nval){
  simdat_both <- simcilength(nval, nits, lvl = 0.8)

  simdat_props <- simdat_both[[3]] |>
    melt(measure.vars = paste0("q", c(1,7,8)), variable.name = "quantile_function",
         value = "proportion") |>
    .d(,n := nval) |>
    .d(, quantile_function := factor(quantile_function,
                                     levels = c("q1", "q7", "q8"),
                                     labels = c("type = 1", "type = 7", "type = 8")))

  simdat_vals <- simdat_both[[1]] |>
    melt(measure.vars = apply(expand.grid(c("absolute", "directional"), paste0("q", qumeth)), 1, paste, collapse="_"), variable.name = "quantile_function",
         value = "proportion") |>
    .d(,n := nval)

  return(list(simdat_props, simdat_vals))
}
)

simdat_props <- lapply(1:length(simdat), function(k) simdat[[k]][[1]]) |>
  rbindlist()

simdat_vals <- lapply(1:length(simdat), function(k) simdat[[k]][[2]]) |>
  rbindlist() |>
  .d(, c("PX", "PY") := tstrsplit(quantile_function, "_", fixed=TRUE)) |>
  .d(, quantile_function := NULL) |>
  setnames(c("PX", "PY"), c("method", "quantile_function")) |>
  .d(, quantile_function := factor(quantile_function,
                                   levels = c("q1", "q7", "q8"),
                                   labels = c("type = 1", "type = 7", "type = 8")))


data.table::fwrite(simdat_props, here("simulations", "results", "simdat_props80.csv"))
data.table::fwrite(simdat_vals, here("simulations", "results", "simdat_vals80.csv"))
#data.table::fwrite(simdat_vals,  "simdat_vals.csv")


###SIMULATE 50% CI##########################
simdat <- lapply(valsn, function(nval){
  simdat_both <- simcilength(nval, nits, lvl = 0.5)

  simdat_props <- simdat_both[[3]] |>
    melt(measure.vars = paste0("q", c(1,7,8)), variable.name = "quantile_function",
         value = "proportion") |>
    .d(,n := nval) |>
    .d(, quantile_function := factor(quantile_function,
                                     levels = c("q1", "q7", "q8"),
                                     labels = c("type = 1", "type = 7", "type = 8")))

  simdat_vals <- simdat_both[[1]] |>
    melt(measure.vars = apply(expand.grid(c("absolute", "directional"), paste0("q", qumeth)), 1, paste, collapse="_"), variable.name = "quantile_function",
         value = "proportion") |>
    .d(,n := nval)

  return(list(simdat_props, simdat_vals))
}
)

simdat_props <- lapply(1:length(simdat), function(k) simdat[[k]][[1]]) |>
  rbindlist()

simdat_vals <- lapply(1:length(simdat), function(k) simdat[[k]][[2]]) |>
  rbindlist() |>
  .d(, c("PX", "PY") := tstrsplit(quantile_function, "_", fixed=TRUE)) |>
  .d(, quantile_function := NULL) |>
  setnames(c("PX", "PY"), c("method", "quantile_function")) |>
  .d(, quantile_function := factor(quantile_function,
                                   levels = c("q1", "q7", "q8"),
                                   labels = c("type = 1", "type = 7", "type = 8")))


data.table::fwrite(simdat_props, here("simulations", "results", "simdat_props50.csv"))
data.table::fwrite(simdat_vals, here("simulations", "results", "simdat_vals50.csv"))
