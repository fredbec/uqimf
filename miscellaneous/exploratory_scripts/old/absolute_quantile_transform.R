# make toy data to check function
library(here)
library(data.table)
.d <- `[`

toyweo <- data.table::fread(here("WEOforecasts_tidy.csv")) |>
  .d(ISOAlpha_3Code %in% c("USA", "GER")) |>
  .d(horizon %in% c(0)) |>
  .d(target_year <= 2011) |>
  .d(, .(ISOAlpha_3Code, target, target_year, horizon, forecast_year, prediction, tv_1)) |>
  #setnames("tv_1", "true_value") |>
  .d(, tv_1 := 0) |>
  setnames("ISOAlpha_3Code", "country")

step <- diff(seq(-2, 2, length.out = 21))[1]

toyweo$prediction <- rep(c(seq(-2, 2, length.out = 21), 2+step) , 2)


empFC(toyweo,
      target_years = 2010,
      tv_release = 1,
      error_method = "absolute",
      method = "leave-one-out",
      ci_levels = c(0.5)
    )


errors <- seq(-10, 10, length.out = 51)

errors <- errors[which(abs(errors)>4)]

errors <- res[[1000]]

quantile(errors, 0.75) - quantile(errors, 0.25)
quantile(abs(errors), 0.5) * 2

identical(unname(quantile(errors, c(0.25, 0.75))),
          unname(c(-quantile(abs(errors),  c(0.5)), quantile(abs(errors),  c(0.5)))))




numits <- 100000
res <- vector(mode = "list", length = numits)
res1 <- vector(mode = "list", length = numits)

set.seed(2912)
for (i in 1:numits){


  #n <- round(runif(1, 11, 20))

  n <- 33

  sig <- round(runif(1, 1, 5))

  errors <- rnorm(n, 0, 1)
  #errors <- runif(n, -sig, sig)

  #if(i < 10){
  #  print(errors)
  #}


  #if(length(errors[which(abs(errors)>0.5)]) < 9){
  #  next
  #}

  qus_d <- quantile(errors, c(0.25, 0.75), type = 1)

  qus_a <- quantile(abs(c(errors)), 0.5, type = 1) - min(abs(errors))

  l_d <- qus_d[2] - qus_d[1]
  l_a <- (qus_a) * 2

  if(l_a < l_d){
    res[[i]] <- errors
  }

  if(l_a > l_d ){
    res1[[i]] <- errors
  }
}


resl <- lapply(res, length)
mean(unlist(resl)>0)


plot(sort(res[[999]]), 1:length(res[[999]]))

par(mfrow=c(2,1))

myfun <- function(vec){

  q2 <- quantile(vec, 0.25)
  q5 <- quantile(abs(vec), 0.5)
  q7 <- quantile(vec, 0.75)



  plot(c(vec, max(abs(vec))), rep(1, length(vec)+1))
  abline(v = q2)
  abline(v = q7)

  plot(c(min(vec), abs(vec)), rep(1, length(vec)+1))
  abline(v = q5)

  qs <- c(q2, q5, q7)
  names(qs) <- paste0("q", c(2,5,7))


  qs2 <- c(q7-q2, 2*q5)
  print(qs)
  print(qs2)


}

myfun(res1[[2]])
