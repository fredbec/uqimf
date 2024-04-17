rm(list = ls())

library(dplyr)
library(tidyr)
library(zoo)
library(ggplot2)

g7_oecd <- c("CAN", "FRA", "DEU", "ITA", "JPN", "GBR", "USA")

setwd("/home/fabian/Schreibtisch/notes_and_ideas/IMF/")

# rename variables, compute growth rates from CPI level
dat <- read.csv("DP_LIVE_21082023090139698.csv") %>%
  filter(LOCATION %in% g7_oecd) %>%
  transmute(ccode = LOCATION, 
            date = as.yearqtr(gsub("-", "", TIME)), 
            value = Value) %>%
  group_by(ccode) %>% arrange(date) %>%
  mutate(value = c(NA, 100*diff(log(value)))) %>%
  na.omit %>% filter(date >= 1960)

# compare to previous download
comp <- read.csv("oecd_cpi_quarterly_annual.csv") %>%
  filter(ccode %in% g7_oecd, quarter %in% 1:4, 
         type == "prv_period") %>%
  transmute(ccode, value_comp = value, 
            date = as.yearqtr(paste0(target_year, "Q", quarter)))

both <- full_join(dat, comp) 

# summary stats on discrepancies
both %>% na.omit %>% group_by(ccode) %>% 
  summarise(cor = cor(value, value_comp), 
            mae = mean(abs(value-value_comp))) %>%
  ungroup

# plot
both %>% pivot_longer(cols = contains("value")) %>% 
  filter(ccode == "CAN") %>%
  ggplot(aes(x = date, y = value, color = name)) + 
  geom_point() + geom_line() + 
  theme_minimal() + 
  theme(legend.position = "top") + 
  scale_color_discrete(name = "", 
                       breaks = c("value", "value_comp"), 
                       labels = c("new", "old"))

# write data
write.csv(dat, "oecd_cpi_quarterly_new.csv", row.names = FALSE)