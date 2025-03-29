rm(list = ls())

library(sandwich)

# Diebold-Mariano test implementation, similar to function t_hac here:
# https://github.com/FK83/forecastcalibration/blob/main/R/R_procs.R
dm_test <- function(loss1, loss2){
  # make series of loss differences
  # loss1 is for model 1, loss2 is for model 2
  # smaller loss is better
  d <- loss1 - loss2
  # mean loss 
  m <- mean(d)
  # variance estimated via Newey-West method
  # regression of d on intercept
  fit <- lm(d~1)
  # robust standard error
  s <- NeweyWest(fit) |> unname() |> sqrt() |> as.numeric()
  # t statistic
  # t > 0 indicates that model 2 is better
  # t < 0 indicates that model 1 is better
  t <- m/s
  # p-value (two-sided)
  p <- 2*pnorm(-abs(t))
  # lag length used by estimator
  k <- bwNeweyWest(fit)
  # output
  list(t_stat = t, p_val = p, lag_length = k)
}

# Application example: simulate data from AR(1) model, 
# compare true (cond. mean) forecast to random walk forecast, 
# using SE loss
n <- 200
a <- .8 # ar parameter
y <- arima.sim(model = list(ar = a), n = n)
cm <- a*y[-n] # cond. mean forecast for obs. t = 2,...,n
rw <- y[-n] # random walk forecast for obs. t = 2,...,n
loss_cm <- (y[-1]-cm)^2 # loss for obs. t = 2,...,n
loss_rw <- (y[-1]-rw)^2 # loss for obs. t = 2,...,n
# run DM test
# power should be high for large values of n and 
# small values of a (such that random walk is implausible)
dm_test(loss1 = loss_cm, loss2 = loss_rw)

