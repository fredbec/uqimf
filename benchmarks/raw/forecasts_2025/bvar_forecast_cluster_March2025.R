rm(list = ls())
#setwd("/home/fabian/Schreibtisch/macro_paper/OECD/")

library(dplyr)
library(zoo)
library(bvarsv)
#library(tidyr)

# setting 
g7_oecd <- c("CAN", "FRA", "DEU", "ITA", "JPN", "GBR", "USA")
vnms_oecd <- c("cpi", "gdp")
n_draws <- 2e4
weights <- c(1:4, 3:1)/4
weights_mat <- matrix(weights, n_draws, 7, byrow = TRUE)
quantile_grid = seq(from = .01, to = .99, by = .01)
n_q <- length(quantile_grid)

# data frame holding all settings to be run
all_settings <- data.frame(country = rep(g7_oecd, each = 2), 
                           season = rep(c("S", "F"), 7)) %>%
  (function(x) rbind(data.frame(x, with_ciss = TRUE, with_sv = TRUE), 
                     data.frame(x, with_ciss = FALSE, with_sv = TRUE),
                     data.frame(x, with_ciss = TRUE, with_sv = FALSE),
                     data.frame(x, with_ciss = FALSE, with_sv = FALSE))) %>%
  filter(! ((country %in% c("CAN", "JPN")) & with_ciss) )

# get iteration (job id)
iter <- as.integer(as.character(Sys.getenv("SLURM_ARRAY_TASK_ID")))
#iter <- 1

# set random seed
set.seed(iter)

cc <- all_settings$country[iter]
fcst_season <- all_settings$season[iter]
with_ciss <- all_settings$with_ciss[iter]
with_sv <- all_settings$with_sv[iter] 
if (with_ciss){
  yrs_fcst <- 2001:2023  
} else {
  yrs_fcst <- 1989:2023
}

# load data
oecd_q <- read.csv("G7_macro_data.csv") %>%
  mutate(dt = as.yearqtr(dt))

# initialize data frame
df_all <- data.frame()

# loop over forecast years
for (yy in yrs_fcst){
  
  # identify latest data
  if (fcst_season == "S"){
    last_q <- yy # up until Q1 of forecast year
  } else {
    last_q <- yy + .5 # up until Q3 of forecast year
  }
  
  # filter data accordingly
  dat0 <- oecd_q %>% filter(ccode == cc, dt <= last_q) %>%
    arrange(dt) 
  if (with_ciss){
    dat1 <- dat0 %>% select(dt, cpi, gdp, ciss) %>% na.omit
  } else {
    dat1 <- dat0 %>% select(dt, cpi, gdp) %>% na.omit
  }
  
  # data checks
  check1 <- tail(dat1$dt, 1) == last_q
  check2 <- all(diff(dat1$dt) == .25)
  
  if (check1 & check2){
    if (with_ciss){
      dat <- dat1 %>% select(cpi, gdp, ciss) %>% as.matrix
    } else {
      dat <- dat1 %>% select(cpi, gdp) %>% as.matrix
    }
    
    # run model
    if (with_sv){
      # use model with time-varying parameters and stochastic volatility
      fit <- bvar.sv.tvp(dat, nf = 7, nrep = n_draws, thinfac = 1)  
    } else {
      # shut off stochastic volatility and time-varying parameters
      # see https://github.com/FK83/bvarsv/blob/master/bvarsv_Nov2015_website.pdf
      # as well as Knueppel et al. (2023)
      fit <- bvar.sv.tvp(dat, nf = 7, nrep = n_draws, thinfac = 1, 
                         k_Q = 1e-5, pQ = 1e3, 
                         k_W = 1e-5, pW = 1e3, k_S = 1e-5, pS = 1e3*(1:3))  
    }
    
    # get forecast draws
    draws_cpi <- t(fit$fc.ydraws[1,,])
    draws_gdp <- t(fit$fc.ydraws[2,,])
    
    # compute forecasts
    if (fcst_season == "S"){
      # current year forecast
      # get relevant past (realized) data
      aux1 <- colSums(tail(dat[,1:2], 4) * cbind(weights, weights)[1:4, ])
      # get relevant forecast draws
      aux2 <- rowSums(draws_cpi[,1:3] * weights_mat[,5:7])
      aux3 <- rowSums(draws_gdp[,1:3] * weights_mat[,5:7])
      # compute quantiles
      cpi_current <- aux1[1] + quantile(aux2, quantile_grid)
      gdp_current <- aux1[2] + quantile(aux3, quantile_grid)
      
      # next year forecast
      # get relevant forecasts
      aux4 <- rowSums(draws_cpi[,1:7] * weights_mat[,1:7])
      aux5 <- rowSums(draws_gdp[,1:7] * weights_mat[,1:7])
      # compute quantiles
      cpi_next <- quantile(aux4, quantile_grid)
      gdp_next <- quantile(aux5, quantile_grid)
    } else {
      # current year forecast
      # get relevant past (realized) data
      aux1 <- colSums(tail(dat[,1:2], 6) * cbind(weights, weights)[1:6, ])
      # get relevant forecast draws
      aux2 <- rowSums(draws_cpi[,1,drop = FALSE] * 
                        weights_mat[,7,drop = FALSE])
      aux3 <- rowSums(draws_gdp[,1,drop = FALSE] * 
                        weights_mat[,7,drop = FALSE])
      # compute quantiles
      cpi_current <- aux1[1] + quantile(aux2, quantile_grid)
      gdp_current <- aux1[2] + quantile(aux3, quantile_grid)
      
      # next year forecast
      aux4 <- colSums(tail(dat[,1:2], 2) * cbind(weights, weights)[1:2, ])
      # get relevant forecast draws
      aux5 <- rowSums(draws_cpi[,1:5] * weights_mat[,3:7])
      aux6 <- rowSums(draws_gdp[,1:5] * weights_mat[,3:7])
      # compute quantiles
      cpi_next <- aux4[1] + quantile(aux5, quantile_grid)
      gdp_next <- aux4[2] + quantile(aux6, quantile_grid)
    }
    
    # store forecasts in data frame
    df_tmp <- data.frame(var = rep(c("cpi", "gdp", "cpi", "gdp"), 
                                   each = n_q), 
                         forecast_year = yy, 
                         target_year = rep(c(yy, yy+1), each = 2*n_q),
                         quantile_level = rep(quantile_grid, 4),
                         value = c(cpi_current, gdp_current, 
                                   cpi_next, gdp_next))
    # append to existing data frame
    df_all <- rbind(df_all, df_tmp)
  }
}

ciss_addon <- if_else(with_ciss, "_with_ciss", "")
sv_addon <- if_else(with_sv, "", "_const")

# save forecasts
write.csv(df_all, 
          file = paste0("bvar_imf_2025/fcst_quantiles_", cc, 
                        "_", fcst_season, ciss_addon, sv_addon, ".csv"), 
          row.names = FALSE)
