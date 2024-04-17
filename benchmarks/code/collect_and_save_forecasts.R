rm(list = ls())
setwd("/home/fabian/Schreibtisch/OECD/")

library(dplyr)
library(zoo)
library(bvarsv)
library(tidyr)
library(ggplot2)

# labeling stuff
g7_oecd <- c("CAN", "FRA", "DEU", "ITA", "JPN", "GBR", "USA")
g7_imf <- c("Canada", "France", "Germany", "Italy", "Japan", 
            "United Kingdom", "United States")
vnms_oecd <- c("cpi", "gdp")
vnms_imf <- c("pcpi_pch", "ngdp_rpch")

# collect forecast files (BVAR and AR in separate directories)
df_fcst <- data.frame()
for (gg in c("fcsts_ar", "bvar_imf_2024")){
  fc <- list.files(gg)
  for (ii in fc){
    tmp <- read.csv(paste0(gg, "/", ii))
    if (gg == "bvar_imf_2024"){
      method_name <- if_else(grepl("with_ciss", ii), "bvar_ciss", 
                             "bvar")
      df_fcst <- rbind(df_fcst, 
                       data.frame(tmp, 
                                  country = substr(ii, 16, 18), 
                                  season = substr(ii, 20, 20), 
                                  method = method_name))
    } else if (gg == "fcsts_ar"){
      
      df_fcst <- rbind(df_fcst, 
                       data.frame(tmp, 
                                  country = substr(ii, 6, 8), 
                                  season = substr(ii, 10, 10), 
                                  method = "ar", 
                                  quantile_level = .5))
    }
  }
}

# load WEO forecasts
weo0 <- read.csv("WEOforecasts_tidy.csv", sep = ",")
weo <- weo0 %>%
  filter(country %in% g7_imf) %>%
  select(country, target, forecast_year, target_year, 
         prediction, forecast_season, tv_1) %>%
  transmute(country = g7_oecd[match(country, g7_imf)], 
            var = vnms_oecd[match(target, vnms_imf)], 
            forecast_year, target_year, season = forecast_season, 
            value_imf = prediction, 
            truth = tv_1) %>%
  filter(target_year <= (forecast_year + 1)) %>% 
  # add OECD actuals 
  full_join(read.csv("oecd_actuals.csv"))

# save forecast data frame
dats0 <- weo %>%
  pivot_longer(cols = c("value_imf", "truth", "truth_oecd"), 
               names_to = "method") %>%
  mutate(method = 
           if_else(method == "value_imf", "imf", 
                   if_else(method == "truth", "truth", "truth_oecd")), 
         quantile_level = .5)
dats1 <- df_fcst
dat_save <- rbind(dats0, dats1)
write.csv(dat_save, file = "forecasts_March2024.csv", 
          row.names = FALSE)

