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
var_path_forecast <- function(nu, A, Sigma, dat0,
                              max_h = 8, which_var = 1){
  K <- length(nu) # dimension of VAR
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
  p <- ncol(A)/K  # number of lags
  # intercept vector
  bignu <- rep(0, K*p)
  bignu[1:K] <- nu
  # AR coefficient matrix
  bigA <- A
  if (p > 1){
    bigA <- rbind(A, cbind(diag((p-1)*K), matrix(0, (p-1)*K, K)))
  }
  # residual covariance matrix
  Sigma_U <- matrix(0, K*p, K*p)
  Sigma_U[1:K, 1:K] <- Sigma
  # data matrix
  bigY <- make_bigY(dat = dat0, p = p)
  #print(bigY)
  # make mean forecast
  fc_mean <- rep(0, K*p*max_h)
  tmp <- matrix(0, K*p, K*p)
  for (jj in 1:max_h){
    tmp <- tmp + matmult(bigA, jj-1)
    fc_mean[((jj-1)*K*p+1):(jj*K*p)] <- tmp %*% bignu + matmult(bigA, jj) %*% bigY
  }
  # construct hugeA matrix needed for forecast covariance matrix
  hugeA <- diag(max_h*K*p)
  for (jj in 1:(max_h-1)){
    for (ll in 1:(max_h-jj)){
      row_inds <- (jj+ll-1)*K*p+(1:(K*p))
      col_inds <- (jj-1)*K*p+(1:(K*p))
      hugeA[row_inds, col_inds] <-
        matmult(bigA, ll)
    }
  }
  # make complete covariance matrix
  fc_vcv <- hugeA %*% (diag(max_h) %x% Sigma_U) %*% t(hugeA)
  # construct relevant indices
  ind <- seq(from = which_var, by = K*p, length.out = max_h)
  # return path forecast mean and covariance
  list(fc_mean = fc_mean[ind], fc_vcv = fc_vcv[ind, ind])
}

# simulate data from AR(p) model. Function is used to check code above.
ar_sim <- function(nu = 1, A = c(.4, .1), Sigma = 2, dat0 = NULL,
                       max_h = 10, n_iter = 1e4){
  p <- length(A)
  res <- matrix(0, max_h, max_h)
  dat <- matrix(0, max_h + length(A), n_iter)
  if (!is.null(dat)){
    dat[1:p,] <- matrix(dat0, p, n_iter)
  }
  for (jj in 1:max_h){
    tmp <- rep(0, n_iter)
    for (ll in 1:p){
      tmp <- tmp + A[ll]*dat[jj-ll+p,]
    }
    dat[jj+p, ] <- rep(nu, n_iter) + tmp + rnorm(n_iter, sd = sqrt(Sigma))
  }
  dat <- dat[(p+1):nrow(dat), ]
  list(fc_mean = rowMeans(dat), fc_vcv = var(t(dat)))
}

# function to choose lag length via Schwarz information criterion
# consider lag lengths 1, 2, ..., max_p
ar_choose_p <- function(dat, max_p = 4){
  T <- length(dat) - max_p # sample size (not counting initial values)
  x <- rep(1, T)
  y <- dat[(max_p+1):length(dat)]
  bic <- cbind(1:max_p, NA)
  for (jj in 1:max_p){
    x <- cbind(x, dat[(max_p+1-jj):(length(dat)-jj)])
    fit_tmp <- lm(y~x-1)
    # Schwarz info criterion, see equation (4.3.9) in Luetkepohl 2005 book
    bic[jj, 2] <- log(mean(residuals(fit_tmp)^2)) + (log(T)/T)*jj
  }
  list(bic = bic, best_p = which.min(bic[,2]))
}

# fit an AR(p) model to data, use it to compute path forecast
ar_p_fcst <- function(dat, p = 1, max_h = 8){
  T <- length(dat) - p # sample size (not counting initial values)
  x <- rep(1, T)
  y <- dat[(p+1):length(dat)]
  for (jj in 1:p){
    x <- cbind(x, dat[(p+1-jj):(length(dat)-jj)])
  }
  fit <- lm(y~x-1)
  # divide by T, as in R's predict.ar function
  # (alternative would be to divide by T-p-1, i.e. d.o.f. correction)
  Sigma <- sum(residuals(fit)^2)/T
  A <- coefficients(fit)[-1]
  nu <- coefficients(fit)[1]
  var_path_forecast(nu = nu, Sigma = Sigma, A = A, dat0 = dat,
                    max_h = max_h)
}
# function to check the above code (by comparison to base R)
ar_p_fcst_check <- function(dat, p = 1, max_h = 8){
  fit_ar <- ar(dat, order.max = p, aic = FALSE, method = "ols")
  predict(fit_ar, n.ahead = max_h)
}
