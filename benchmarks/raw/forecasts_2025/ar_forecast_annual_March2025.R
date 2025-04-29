rm(list = ls())
setwd("/home/fabian/Schreibtisch/macro_paper/OECD/")

library(scoringRules)
library(dplyr)

source("ar_forecast_procs.R")

# load WEO data, keep only part that is relevant in terms of annual outcome data
dat <- read.csv("weodat.csv") %>% filter(horizon == 0) %>%
  transmute(country, target, target_year, tv_0.5, tv_1)

# setting
g7 <- c("CAN", "FRA", "DEU", "ITA", "JPN", "GBR", "USA")
vnms <- c("ngdp_rpch", "pcpi_pch")
vnms2 <- c("gdp", "cpi")
yrs_fcst <- 2012:2023
forecast_season <- "S" # "S" (spring) or "F" (fall)

# quantile grid
quantile_grid = seq(from = .01, to = .99, by = .01)
n_q <- length(quantile_grid)

for (cc in g7){
  for (forecast_season in c("S", "F")){
    df_all <- data.frame()
    for (vv in vnms){
      # all data for current country and variable
      tmp1 <- dat %>% filter(country == cc, target == vv)
      # loop through forecast years
      for (yy in yrs_fcst){

        # both forecasts (spring and fall)
        tmp2 <- tmp1 %>%
          filter(target_year <= (yy-2)) %>%
          arrange(target_year) %>% pull(tv_1)
        # check data length
        stopifnot(length(tmp2) == (yy-2-1990+1))

        if (forecast_season == "S"){
          # spring forecast only: append first release of last year's obs
          tmp3 <- tmp1 %>%
            filter(target_year == (yy-1)) %>%
            pull(tv_0.5)
          # check data length
          stopifnot(length(tmp3) == 1)

          # data for spring forecast model
          dat_fit <- c(tmp2, tmp3)

        } else if (forecast_season == "F"){

          # fall forecast only: append second release of last year's obs
          tmp4 <- tmp1 %>%
            filter(target_year == (yy-1)) %>%
            pull(tv_1)
          # check data length
          stopifnot(length(tmp4) == 1)

          # data for fall forecast model
          dat_fit <- c(tmp2, tmp4)

        }

        fit <- ar_p_fcst(dat_fit, p = 1, max_h = 2)
        fc_current <- qnorm(p = quantile_grid,
                            mean = fit$fc_mean[1],
                            sd = sqrt(fit$fc_vcv[1,1]))
        fc_next <- qnorm(p = quantile_grid,
                         mean = fit$fc_mean[2],
                         sd = sqrt(fit$fc_vcv[2,2]))

        # expand data frame
        df_tmp <- data.frame(var = rep(vnms2[which(vnms == vv)],
                                       n_q),
                             forecast_year = yy,
                             target_year = rep(c(yy, yy+1),
                                               each = n_q),
                             quantile_level = rep(quantile_grid, 2),
                             value = c(fc_current, fc_next))
        df_all <- rbind(df_all, df_tmp)
      } # end of loop over time
    } # end of loop across variables
    # save results for current country and forecast season
    write.csv(df_all,
            file = paste0("fcsts_ar_2025/fcst_", cc, "_",
                          forecast_season,
                          "_annual.csv"),
            row.names = FALSE)
  } # end loop across seasons
} # end loop across countries
