rm(list = ls())
setwd("/home/fabian/Schreibtisch/notes_and_ideas/IMF/")

library(dplyr)
library(zoo)
library(bvarsv)
library(tidyr)

# setting 
g7_oecd <- c("CAN", "FRA", "DEU", "ITA", "JPN", "GBR", "USA")
vnms_oecd <- c("cpi", "gdp")
yrs_fcst <- 1989:2021
n_draws <- 2e4
weights <- c(1:4, 3:1)/4
weights_mat <- matrix(weights, n_draws, 7, byrow = TRUE)
quantile_grid = seq(from = .05, to = .95, by = .05)
n_q <- length(quantile_grid)

# data frame holding all settings to be run
all_settings <- data.frame(country = rep(g7_oecd, each = 2), 
                           season = rep(c("S", "F"), 7))

# get iteration (job id)
#iter <- as.integer(as.character(Sys.getenv("SLURM_ARRAY_TASK_ID")))
iter <- 2

# set random seed
set.seed(iter)

cc <- all_settings$country[iter]
fcst_season <- all_settings$season[iter]

# load cpi data, transform date
cpi0 <- read.csv("oecd_cpi_quarterly_new.csv") 
cpi_q <- cpi0 %>%
  transmute(ccode, y = value, 
            dt = as.yearqtr(date)) %>%
  na.omit 
gdp0 <- read.csv("oecd_gdp_quarterly_annual.csv") 
gdp_q <- gdp0 %>% 
  filter(ccode %in% g7_oecd, quarter %in% 1:4, type == "prv_period") %>% 
  transmute(ccode, dt = as.yearqtr(paste0(target_year, "Q", quarter)), 
            y = value)
# Put both into one data frame
oecd_q <- rbind(data.frame(cpi_q, target = "cpi"), 
                data.frame(gdp_q, target = "gdp")) %>%
  pivot_wider(names_from = "target", values_from = "y") %>%
  na.omit %>% filter(dt >= 1962)

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
    arrange(dt) %>% select(dt, cpi, gdp) 
  
  # data checks
  check1 <- tail(dat0$dt, 1) == last_q
  check2 <- all(diff(dat0$dt) == .25)
  
  if (check1 & check2){
    dat <- dat0 %>% select(cpi, gdp) %>% as.matrix
    
    # run model
    fit <- bvar.sv.tvp(dat, nf = 7, nrep = n_draws, thinfac = 1)
    
    # get forecast draws
    draws_cpi <- t(fit$fc.ydraws[1,,])
    draws_gdp <- t(fit$fc.ydraws[2,,])
    
    # compute forecasts
    if (fcst_season == "S"){
      # current year forecast
      # get relevant past (realized) data
      aux1 <- colSums(tail(dat, 4) * cbind(weights, weights)[1:4, ])
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
      aux1 <- colSums(tail(dat, 6) * cbind(weights, weights)[1:6, ])
      # get relevant forecast draws
      aux2 <- rowSums(draws_cpi[,1,drop = FALSE] * 
                        weights_mat[,7,drop = FALSE])
      aux3 <- rowSums(draws_gdp[,1,drop = FALSE] * 
                        weights_mat[,7,drop = FALSE])
      # compute quantiles
      cpi_current <- aux1[1] + quantile(aux2, quantile_grid)
      gdp_current <- aux1[2] + quantile(aux3, quantile_grid)
      
      # next year forecast
      aux4 <- colSums(tail(dat, 2) * cbind(weights, weights)[1:2, ])
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

# save forecasts
write.csv(df_all, 
          file = paste0("bvar_imf/fcst_quantiles", cc, "_", fcst_season, ".csv"), 
          row.names = FALSE)