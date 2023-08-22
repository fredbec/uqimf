library(here)
library(ggplot2)
library(data.table)
library(dplyr)

oecd <- read.csv(here("oecd_data", "oecd_gdp.csv"))

#read in quarterly data
oecd_q <- oecd |>
  filter(FREQUENCY == "Q",
         SUBJECT == "TOT") |>
  select(-c(INDICATOR, SUBJECT, FREQUENCY, Flag.Codes)) |>
  rename(country = LOCATION,
         measure = MEASURE,
         yrq = TIME,
         value = Value) |>
  mutate(target_year = substr(yrq, 1, 4) |> as.numeric(),
         quarter = substr(yrq, 7, 7) |> as.numeric()) |>
  filter(target_year >= 1960) |>
  rename(ccode = country) |>
  select(-yrq)|>
  mutate(type = ifelse(measure == "PC_CHGPY", "prv_year", "prv_period")) |> #PC_CHGPP refers to previous period, PC_CHGPY refers to same period previous year
  select(-measure)


#read in yearly data
oecd_y <- oecd |>
  filter(FREQUENCY == "A",
         SUBJECT == "TOT") |>
  select(-c(INDICATOR, SUBJECT, FREQUENCY, Flag.Codes)) |>
  rename(country = LOCATION,
         measure = MEASURE,
         target_year = TIME,
         value = Value) |>
  mutate(quarter = "Y") |>
  filter(target_year >= 1960) |>
  rename(ccode = country) |>
  mutate(type = ifelse(measure == "PC_CHGPY", "prv_year", "prv_period")) |> #PC_CHGPP refers to previous period, PC_CHGPY refers to same period previous year
  select(-measure)

#bind the two together
oecd <- rbind(oecd_q, oecd_y)

data.table::fwrite(oecd, here("oecd_data", "oecd_gdp_quarterly_annual.csv"))
