rm(list = ls())

library(dplyr)
library(tidyr)
library(zoo)
library(readxl)

setwd("/home/fabian/Schreibtisch/OECD/")

# Old CPI data (August 2023)
cpi_old <- read.csv("raw data/DP_LIVE_21082023090139698.csv") %>%
  transmute(ccode = LOCATION, 
            target_year = as.integer(substr(TIME, 1, 4)), 
            quarter = as.integer(substr(TIME, 7, 7)), 
            value = Value, 
            ds = "old")

# New CPI data (March 2024)
cpi_new <- read.csv("raw data/PRICES_CPI_18032024164336422.csv") %>%
  filter(MEASURE == "IXOB") %>%
  transmute(ccode = LOCATION, 
            target_year = as.integer(substr(TIME, 1, 4)), 
            quarter = as.integer(substr(TIME, 7, 7)), 
            value = Value, ds = "new") 

# Compare data
cpi_comp <- full_join(cpi_old, cpi_new) %>%  
  pivot_wider(names_from = "ds", values_from = "value") %>%
  mutate(d = new-old) %>% na.omit

plot(cpi_comp$old, cpi_comp$new)
summary(abs(cpi_comp$d))

# Transform and save CPI data
cpi_aux <- cpi_new %>% 
  transmute(ccode, 
            dt = as.yearqtr(paste0(target_year, "Q", quarter)), 
            value) %>%
  group_by(ccode) %>% arrange(dt) %>%
  mutate(value_inf = c(NA, 100*diff(log(value))),
         check = c(NA, diff(dt))) %>% na.omit

if (any(cpi_aux$check != .25)){
  stop("Smth wrong - check again")
} else {
  cpi_final <- cpi_aux %>% transmute(ccode, dt, cpi = value_inf)
  #write.table(cpi_final, "cpi_OECD_March2023.csv", sep = ",", 
  #            row.names = FALSE, col.names = TRUE)
}

# Old GDP data (August 2023)
gdp_old <- read.csv("oecd_gdp_quarterly_annual.csv") %>%
  filter(quarter != "Y", type == "prv_period") %>%
  mutate(quarter = as.integer(quarter), ds = "old") %>%
  select(-type)

# New GDP data (March 2024)
gdp_new <- read.csv("raw data/OECD.SDD.NAD,DSD_NAMAIN1@DF_QNA_EXPENDITURE_CONTRIB,1.0+Q..USA+GBR+JPN+ITA+DEU+FRA+CAN.S1..B1GQ......G1..csv") %>%
  transmute(ccode = REF_AREA, value = OBS_VALUE,
            target_year = as.integer(substr(TIME_PERIOD, 1, 4)),
            quarter = as.integer(substr(TIME_PERIOD, 7, 7))) %>%
  mutate(ds = "new")

# Compare data
gdp_comp <- full_join(gdp_old, gdp_new) %>% 
  pivot_wider(names_from = "ds", values_from = "value") %>%
  mutate(d = new-old)
plot(gdp_comp$old, gdp_comp$new)
summary(abs(gdp_comp$d))

# Final GDP data
gdp_final <- gdp_new %>% 
  transmute(ccode, dt = as.yearqtr(paste0(target_year, "Q", quarter)), 
            gdp = value)

# CISS (Composite Indicator of Systemic Stress, provided by ECB)
ciss0 <- read_excel("raw data/ECB Data Portal long_20240318121830.xlsx", sheet = 2) %>%
  transmute(date_day = as.Date(DATE), dt = as.yearqtr(date_day), 
            value = OBS.VALUE, ccode = substr(`SERIES KEY`, 8, 9)) %>%
  mutate(ccode = 
           if_else(ccode == "DE", "DEU", 
                   if_else(ccode == "GB", "GBR",
                           if_else(ccode %in% c("FR", "IT", "US"), paste0(ccode, "A"),
                                   ccode)))) %>%
  na.omit %>% filter(dt <= 2024) 
ciss_final <- ciss0 %>% group_by(dt, ccode) %>%
  summarise(ciss = mean(value)) 
ciss_plot <- ciss_final %>% pivot_wider(values_from = "ciss", 
                                  names_from = "ccode")
matplot(x = ciss_plot$dt, 
        y = ciss_plot[,-1], type = "l", bty = "n", xlab = "Year", ylab = "CISS")

# Merge all data
dat_all <- full_join(full_join(gdp_final, cpi_final), ciss_final) %>%
  arrange(ccode, dt)

dat_all %>% na.omit %>% group_by(ccode) %>%
  summarise(min_dt = min(dt), max_dt = max(dt))
dat_all %>% select(-ciss) %>% na.omit %>% group_by(ccode) %>%
  summarise(min_dt = min(dt), max_dt = max(dt))

write.table(dat_all, "G7_macro_data.csv", col.names = TRUE, 
            row.names = FALSE, sep = ",")