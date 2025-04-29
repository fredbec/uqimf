# fit an AR(p) model to data, use it to compute path forecast
ar1_fcst <- function(dat, p = 1, max_h = 8){
  T <- length(dat) - 1 # sample size (not counting initial values)
  x <- rep(1, T)
  y <- dat[2:length(dat)]

  x <- cbind(x, dat[(p):(length(dat)-1)])
  fit <- lm(y~x-1)
  # divide by T, as in R's predict.ar function
  # (alternative would be to divide by T-p-1, i.e. d.o.f. correction)
  Sigma <- sum(residuals(fit)^2)/T
  A <- coefficients(fit)[-1]
  nu <- coefficients(fit)[1]
  var_path_forecast(nu = nu, Sigma = Sigma, A = A, dat0 = dat,
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
  out <- 0
  out[1] <- dat[nrow(dat),]
  out
}
# construct path forecast (mean and VCV) of a VAR(p) model
# requires to transform VAR(p) into VAR(1) representation first
# see Luetkepohl (2005, Section 2.1.1); use same notation here
# dat0 contains conditioning data: newest observation is in last row
var_path_forecast <- function(nu, A, Sigma, dat0,
                              max_h = 8, which_var = 1){
  if (is.vector(A)){
    A <- matrix(A, nrow = 1)
  }
  if (is.vector(dat0)){
    dat0 <- matrix(dat0, ncol = 1)
  }
  if (!is.matrix(Sigma)){
    if (length(Sigma) != 1){
      stop("Wrong format of Sigma")
    } else {
      Sigma <- matrix(Sigma, 1, 1)
    }
  }
  p <- ncol(A) # number of lags
  # intercept vector
  bignu <- nu
  # AR coefficient matrix
  bigA <- A
  # residual covariance matrix
  Sigma_U <- matrix(0, 1,1)
  Sigma_U[1, 1] <- Sigma
  # data matrix
  bigY <- make_bigY(dat = dat0, p = 1)
  # make mean forecast
  fc_mean <- rep(0, max_h)
  tmp <- matrix(0, 1, 1)
  for (jj in 1:max_h){
    tmp <- tmp + matmult(bigA, jj-1)
    fc_mean[((jj-1)+1):(jj)] <- tmp %*% bignu + matmult(bigA, jj) %*% bigY
  }
  # construct hugeA matrix needed for forecast covariance matrix
  hugeA <- diag(max_h)
  for (jj in 1:(max_h-1)){

    for (ll in 1:(max_h-jj)){
      row_inds <- (jj+ll-1)+(1)
      col_inds <- (jj-1)+(1)

      print(row_inds)
      print(col_inds)
      hugeA[row_inds, col_inds] <-
        matmult(bigA, ll)
    }
  }
  # make complete covariance matrix
  fc_vcv <- Sigma_U
  # construct relevant indices
  ind <- seq(from = which_var, by = 1, length.out = max_h)
  # return path forecast mean and covariance
  list(fc_mean = fc_mean[ind], fc_vcv = fc_vcv[ind, ind])
}
