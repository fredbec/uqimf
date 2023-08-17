library(here)
library(ggplot2)
library(data.table)
library(dplyr)


oecd <- read.csv(here("oecd_data", "oecd_cpi.csv"))

oecd_q <- oecd |>
  filter(FREQUENCY == "Q",
         SUBJECT == "TOT") |>
  #filter(LOCATION %in% c("CAN", "FRA", "DEU", "ITA", "JPN", "GBR", "USA")) |>
  select(-c(INDICATOR, SUBJECT, FREQUENCY, Flag.Codes)) |>
  rename(country = LOCATION,
         measure = MEASURE,
         yrq = TIME,
         value = Value) |>
  mutate(target_year = substr(yrq, 1, 4) |> as.numeric(),
         quarter = substr(yrq, 7, 7) |> as.numeric()) |>
  filter(target_year >= 1980) |>
  select(-yrq) |>
  rename(ccode = country)


oecd_y <- oecd |>
  filter(FREQUENCY == "A",
         SUBJECT == "TOT") |>
  #filter(LOCATION %in% c("CAN", "FRA", "DEU", "ITA", "JPN", "GBR", "USA")) |>
  select(-c(INDICATOR, SUBJECT, FREQUENCY, Flag.Codes)) |>
  rename(country = LOCATION,
         measure = MEASURE,
         value = Value,
         target_year = TIME) |>
  mutate(quarter = "Y") |>
  rename(ccode = country)


oecd <- rbind(oecd_q, oecd_y)


data.table::fwrite(oecd, here("oecd_data", "oecd_cpi_quarterly_annual.csv"))
