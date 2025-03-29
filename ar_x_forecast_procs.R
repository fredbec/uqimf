# fit an AR(p) model to data, use it to compute path forecast
ar_x_fcst <- function(dat, dat_z, lag = 1, max_h = 8){
  T <- length(dat) - 1 # sample size (not counting initial values)
  x <- rep(1, T)
  x <- cbind(x, dat[1:(length(dat)-1)])
  y <- dat[(2):length(dat)]
  z <- dat_z[1:length(dat)-1]
  fit <- lm(y~x+z-1)
  #print(summary(fit))
  # divide by T, as in R's predict.ar function
  # (alternative would be to divide by T-p-1, i.e. d.o.f. correction)
  Sigma <- sum(residuals(fit)^2)/T
  A <- coefficients(fit)["x"]
  B <- coefficients(fit)["z"]
  nu <- coefficients(fit)[1]

  datz1 <- dat_z[length(dat_z)]

  var_path_forecast(nu = nu, Sigma = Sigma, B = B, A = A, dat0 = dat,
                    datz = datz1,
                    max_h = max_h)
}


# fit an AR(p) model to data, use it to compute path forecast
ar_x_fcst_lag <- function(dat, dat_z, lag = 1, max_h = 8){
  T <- length(dat) - lag # sample size (not counting initial values)
  x <- rep(1, T)
  x <- cbind(x, dat[1:(length(dat)-lag)])
  y <- dat[(1+lag):length(dat)]
  z <- dat_z[(1+1):(length(dat)-1)]
  fit <- lm(y~x+z-1)
  #print(summary(fit))
  # divide by T, as in R's predict.ar function
  # (alternative would be to divide by T-p-1, i.e. d.o.f. correction)
  Sigma <- sum(residuals(fit)^2)/T
  A <- coefficients(fit)["x"]
  B <- coefficients(fit)["z"]
  nu <- coefficients(fit)[1]


  datz1 <- dat_z[length(dat_z)]

  var_path_forecast(nu = nu, Sigma = Sigma, B = B, A = A, dat0 = dat,
                    datz = datz1,
                    max_h = max_h)
}

# helper function from bvarsv package, see bvarsv:::matmult
matmult <- function(mat, mult){
  if (mult == 0){
    out <- diag(nrow(mat))
  } else if (mult == 1){
    out <- mat
  } else {
    out <- mat
    for (c in 1:(mult-1)) out <- out %*% mat
  }
  return(out)
}

make_bigY <- function(dat, p){
  K <- ncol(dat)
  out <- rep(0, p*K)
  for (jj in 1:p){
    out[(((jj-1)*K)+1):(jj*K)] <- dat[nrow(dat)-jj+1,]
  }
  out
}
# construct path forecast (mean and VCV) of a VAR(p) model
# requires to transform VAR(p) into VAR(1) representation first
# see Luetkepohl (2005, Section 2.1.1); use same notation here
# dat0 contains conditioning data: newest observation is in last row
# construct path forecast (mean and VCV) of a VAR(p) model
# requires to transform VAR(p) into VAR(1) representation first
# see Luetkepohl (2005, Section 2.1.1); use same notation here
# dat0 contains conditioning data: newest observation is in last row
var_path_forecast <- function(nu, A, B, Sigma, dat0, datz,
                              max_h = 8, which_var = 1){

  A <- matrix(A, nrow = 1)
  dat0 <- matrix(dat0, ncol = 1)
  datz <- matrix(datz, ncol = 1)
  Sigma <- matrix(Sigma, 1, 1)

  p <- ncol(A) # number of lags
  # intercept vector
  # AR coefficient matrix
  phi <- A
  # residual covariance matrix
  Sigma_U <- matrix(Sigma, 1, 1)
  # data matrix
  bigY <- dat0[length(dat0)]
  bigZ <- datz
  #print(paste0("Y:", bigY))
  #print(paste0("Z:", bigZ))
  # make mean forecast
  fc_mean <- nu + phi %*% bigY + B %*% bigZ
  # construct hugeA matrix needed for forecast covariance matrix
  # make complete covariance matrix
  fc_vcv <- Sigma_U
  # construct relevant indices
  # return path forecast mean and covariance
  list(fc_mean = fc_mean, fc_vcv = fc_vcv)
}
