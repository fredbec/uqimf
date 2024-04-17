rm(list = ls())

setwd("/home/fabian/Schreibtisch/OECD/")

library(zoo)
library(dplyr)
library(tidyr)

dat <- read.csv("G7_macro_data.csv") %>% 
  mutate(dt = as.yearqtr(dt)) %>%
  arrange(ccode, dt)

yrs <- 1989:2023
w <- c(1:4, 3:1)/4
df_all <- data.frame()

for (yy in yrs){
  tmp <- dat %>% filter(dt <= (yy + .75), dt >= (yy - .75)) %>%
    group_by(ccode) %>% arrange(dt) %>%
    summarise(target_year = yy, gdp = sum(gdp*w), cpi = sum(cpi*w))
  df_all <- rbind(df_all, tmp)
}

# Final transformations to align data format with IMF WEO data
df_final <- df_all %>% pivot_longer(c("cpi", "gdp"), names_to = "var") %>%
  transmute(country = ccode, target_year, var, truth_oecd = value)

write.table(df_final, "oecd_actuals.csv", sep = ",", col.names = TRUE, 
            row.names = FALSE)