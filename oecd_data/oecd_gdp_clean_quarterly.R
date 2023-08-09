library(here)
library(ggplot2)
library(data.table)
library(dplyr)

oecd <- read.csv(here("oecd_data", "oecd_gdp.csv"))

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
  filter(target_year >= 1985,
         measure == "PC_CHGPP") |> #PC_CHGPP refers to previous period, PC_CHGPY refers to same period previous year
  rename(ccode = country) |>
  select(-c(measure, yrq))


data.table::fwrite(oecd_q, here("oecd_data", "oecd_gdp_quarterly.csv"))
