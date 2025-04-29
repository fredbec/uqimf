rm(list = ls())
setwd("/home/fabian/Schreibtisch/macro_paper/OECD/")

library(dplyr)
library(zoo)
library(bvarsv)
library(tidyr)

source("ar_forecast_procs.R")

# setting 
g7_oecd <- c("CAN", "FRA", "DEU", "ITA", "JPN", "GBR", "USA")
vnms_oecd <- c("cpi", "gdp")
weights <- c(1:4, 3:1)/4
yrs_fcst <- 1989:2023
use_bic <- TRUE # use BIC to select AR lag length?
p_max <- 8
bic_addon <- if_else(use_bic, "_bic", "_p=1")

# quantile grid (needed for direct method below)
quantile_grid = seq(from = .01, to = .99, by = .01)
n_q <- length(quantile_grid)

if (use_bic){
  # initialize data frame to record lag length choice
  p_choice_gdp <- p_choice_cpi <- data.frame()    
}

# data frame holding all settings to be run
all_settings <- data.frame(country = rep(g7_oecd, each = 2), 
                           season = rep(c("S", "F"), 7))

# loop through settings
for (iter in 1:14){
  
  cc <- all_settings$country[iter]
  fcst_season <- all_settings$season[iter]
  p_choice_cpi_tmp <- p_choice_gdp_tmp <- rep(NA, length(yrs_fcst))
  
  # load data, transform date
  oecd_q <- read.csv("G7_macro_data.csv") %>%
    mutate(dt = as.yearqtr(dt)) %>% select(-ciss) %>% 
    na.omit
  
  # initialize data frame
  df_all <- data.frame()
  
  # loop over forecast years
  for (yy in yrs_fcst){
    # index for current year
    yy_ind <- which(yrs_fcst == yy)
    
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

      if (!use_bic){
        pred_cpi <- ar_p_fcst(dat[,1], p = 1, max_h = 7)  
      } else {
        p <- p_choice_cpi_tmp[yy_ind] <- 
          ar_choose_p(dat[,1], max_p = 8)$best_p
        pred_cpi <- ar_p_fcst(dat[,1], p = p, max_h = 7)  
      }
      
      if (!use_bic){
        pred_gdp <- ar_p_fcst(dat[,2], p = 1, max_h = 7)  
      } else {
        p <- p_choice_gdp_tmp[yy_ind] <- 
          ar_choose_p(dat[,2], max_p = 8)$best_p
        pred_gdp <- ar_p_fcst(dat[,2], p = p, max_h = 7)  
      }
      
      # assemble mean predictions
      pred <- cbind(pred_cpi$fc_mean, pred_gdp$fc_mean)
      
      # compute annual forecasts
      if (fcst_season == "S"){ # spring season
        # current year forecast
        
        # point forecasts
        # get relevant past (realized) data
        aux1 <- tail(dat, 4)
        # get relevant forecasts
        aux2 <- pred[1:3,]
        # combine and compute weighted sum
        aux3 <- rbind(aux1, aux2)
        fcst_current <- c(sum(weights*aux3[,1]), 
                          sum(weights*aux3[,2]))
        
        # quantile forecasts (for AR-direct method)
        # last three quarters are unknown and thus enter variance
        cpi_v_current <- (t(weights[5:7]) %*% pred_cpi$fc_vcv[1:3, 1:3] %*% 
          weights[5:7]) |> as.numeric()
        cpi_q_current <- fcst_current[1] + sqrt(cpi_v_current)*qnorm(quantile_grid)
        gdp_v_current <- (t(weights[5:7]) %*% pred_gdp$fc_vcv[1:3, 1:3] %*%
          weights[5:7]) |> as.numeric()
        gdp_q_current <- fcst_current[2] + sqrt(gdp_v_current)*qnorm(quantile_grid)
        
        # next year forecast
        
        # point forecasts
        aux4 <- pred[1:7,] 
        fcst_next <- c(sum(weights*aux4[,1]),
                       sum(weights*aux4[,2]))
        
        # quantile forecasts (for AR-direct method)
        # all seven quarters are unknown and thus enter variance
        cpi_v_next <- (t(weights) %*% pred_cpi$fc_vcv %*% weights) |>
          as.numeric()
        cpi_q_next <- fcst_next[1] + sqrt(cpi_v_next)*qnorm(quantile_grid)
        gdp_v_next <- (t(weights) %*% pred_gdp$fc_vcv %*% weights) |> 
          as.numeric()
        gdp_q_next <- fcst_next[2] + sqrt(gdp_v_next)*qnorm(quantile_grid)
        
      } else { # fall season
        # current year forecast
        
        # point forecasts
        # get relevant past data
        aux1 <- tail(dat, 6)
        # get relevant forecasts
        aux2 <- pred[1,]
        # combine and compute weighted sum
        aux3 <- rbind(aux1, aux2)
        fcst_current <- c(sum(weights*aux3[,1]),
                          sum(weights*aux3[,2]))
        
        # quantile forecast (for AR-direct method)
        # last quarter is unknown and enters variance
        cpi_v_current <- (weights[7]^2)*pred_cpi$fc_vcv[7,7]
        cpi_q_current <- fcst_current[1] + sqrt(cpi_v_current)*qnorm(quantile_grid)
        gdp_v_current <- (weights[7]^2)*pred_gdp$fc_vcv[7,7]
        gdp_q_current <- fcst_current[2] + sqrt(gdp_v_current)*qnorm(quantile_grid)
        
        # next year forecast
        
        # point forecasts
        aux4 <- tail(dat, 2)
        # get relevant forecasts
        aux5 <- pred[1:5,]
        aux6 <- rbind(aux4, aux5)
        fcst_next <- c(sum(weights*aux6[,1]), 
                       sum(weights*aux6[,2]))
        
        # quantile forecasts (for AR-direct method)
        # last five quarters are unknown and enter variance
        cpi_v_next <- (t(weights[3:7]) %*% pred_cpi$fc_vcv[1:5, 1:5] %*% 
          weights[3:7]) |> as.numeric()
        cpi_q_next <- fcst_next[1] + sqrt(cpi_v_next)*qnorm(quantile_grid)
        gdp_v_next <- (t(weights[3:7]) %*% pred_gdp$fc_vcv[1:5, 1:5] %*%
          weights[3:7]) |> as.numeric()
        gdp_q_next <- fcst_next[2] + sqrt(gdp_v_next)*qnorm(quantile_grid)
      }
      
      # store forecasts in data frame
      df_tmp <- data.frame(var = rep(c("cpi", "gdp"), 
                                            each = 2*n_q), 
                                  forecast_year = yy, 
                                  target_year = rep(c(yy, yy+1, yy, yy+1), 
                                                    each = n_q),
                                  quantile_level = rep(quantile_grid, 4),
                                  value = c(cpi_q_current, cpi_q_next, 
                                            gdp_q_current, gdp_q_next))
      # append to existing data frame
      df_all <- rbind(df_all, df_tmp)
    }
  }
  
  if (use_bic){
    # update data frame on lag length choice
    p_choice_cpi <- rbind(p_choice_cpi, 
                          data.frame(country = cc, season = fcst_season, 
                                     year = yrs_fcst, value = p_choice_cpi_tmp))
    p_choice_gdp <- rbind(p_choice_gdp, 
                          data.frame(country = cc, season = fcst_season, 
                                     year = yrs_fcst, value = p_choice_gdp_tmp))
  }
  
  write.csv(df_all, 
            file = paste0("fcsts_ar_2025/fcst_", cc, "_", fcst_season, bic_addon, 
                          ".csv"), 
            row.names = FALSE)
  
}
if (use_bic){
  pdf("docu/ar_lag_length_choice.pdf", width = 16, height = 9)
  par(mar = c(5.1, 5.1, 4.1, 2.1))
  # Make plot to summarize lag length choice
  p_choice <- rbind(data.frame(table(p_choice_cpi$value), var = "cpi"), 
                    data.frame(table(p_choice_gdp$value), var = "gdp")) %>%
    mutate(p = Var1)
  cl1 <- grey(.1, 1)
  cl2 <- grey(.6, 1)
  barplot(Freq~var+p, data = p_choice, beside = TRUE, legend = FALSE, 
          xlab = "Lag Length", ylab = "Frequency", 
          col = c(cl1, cl2), cex.axis = 1.7, cex.names = 1.7, 
          cex.lab = 1.7)
  legend("topright", c("Inflation", "GDP growth"), bty = "n", 
         col = c(cl1, cl2), pch = 15, cex = 2)
  dev.off()
}

