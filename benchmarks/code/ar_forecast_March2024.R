rm(list = ls())
setwd("/home/fabian/Schreibtisch/OECD/")

library(dplyr)
library(zoo)
library(bvarsv)
library(tidyr)

# setting 
g7_oecd <- c("CAN", "FRA", "DEU", "ITA", "JPN", "GBR", "USA")
vnms_oecd <- c("cpi", "gdp")
weights <- c(1:4, 3:1)/4
yrs_fcst <- 1989:2023

# data frame holding all settings to be run
all_settings <- data.frame(country = rep(g7_oecd, each = 2), 
                           season = rep(c("S", "F"), 7))

# get iteration (job id)
#iter <- as.integer(as.character(Sys.getenv("SLURM_ARRAY_TASK_ID")))
#iter <- 1
for (iter in 1:14){
  
  cc <- all_settings$country[iter]
  fcst_season <- all_settings$season[iter]
  
  # load data, transform date
  oecd_q <- read.csv("G7_macro_data.csv") %>%
    mutate(dt = as.yearqtr(dt)) %>% select(-ciss) %>% 
    na.omit
  
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
      fit_cpi <- ar(dat[,1], aic = FALSE, order.max = 1, 
                    method = "ols")
      pred_cpi <- predict(fit_cpi, n.ahead = 7)
      fit_gdp <- ar(dat[,2], aic = FALSE, order.max = 1, 
                    method = "ols")
      pred_gdp <- predict(fit_gdp, n.ahead = 7)
      pred <- cbind(pred_cpi$pred, pred_gdp$pred)
      
      # compute forecasts
      if (fcst_season == "S"){
        # current year forecast
        # get relevant past (realized) data
        aux1 <- tail(dat, 4)
        # get relevant forecasts
        aux2 <- pred[1:3,]
        # combine and compute weighted sum
        aux3 <- rbind(aux1, aux2)
        fcst_current <- c(sum(weights*aux3[,1]), 
                          sum(weights*aux3[,2]))
        # next year forecast
        # get relevant forecasts
        aux4 <- pred[1:7,] 
        fcst_next <- c(sum(weights*aux4[,1]),
                       sum(weights*aux4[,2]))
      } else {
        # current year forecast
        # get relevant past data
        aux1 <- tail(dat, 6)
        # get relevant forecasts
        aux2 <- pred[1,]
        # combine and compute weighted sum
        aux3 <- rbind(aux1, aux2)
        fcst_current <- c(sum(weights*aux3[,1]),
                          sum(weights*aux3[,2]))
        # next year forecast
        aux4 <- tail(dat, 2)
        # get relevant forecasts
        aux5 <- pred[1:5,]
        aux6 <- rbind(aux4, aux5)
        fcst_next <- c(sum(weights*aux6[,1]), 
                       sum(weights*aux6[,2]))
      }
      
      # store forecasts in data frame
      df_tmp <- data.frame(var = rep(c("cpi", "gdp"), 2), 
                           forecast_year = yy, 
                           target_year = rep(c(yy, yy+1), each = 2),
                           value = c(fcst_current, fcst_next))
      # append to existing data frame
      df_all <- rbind(df_all, df_tmp)
    }
  }
  
  write.csv(df_all, 
            file = paste0("fcsts_ar/fcst_", cc, "_", fcst_season, ".csv"), 
            row.names = FALSE)
  
}