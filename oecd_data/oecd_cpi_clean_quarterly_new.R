library(here)
library(ggplot2)
library(data.table)
library(dplyr)


oecd <- read.csv(here("oecd_data", "oecd_cpi_quarter.csv")) |>
  filter(Subject == "CPI: 01-12 - All items",
         MEASURE %in% c("GY", "GP")) |>
  select(-c(Measure, SUBJECT, Subject)) |>
  select(LOCATION, Country, MEASURE, TIME, Unit, Value) |>
  rename(ccode = LOCATION,
         country = Country,
         measure = MEASURE,
         yrq = TIME,
         value = Value) |>
  mutate(target_year = substr(yrq, 1, 4) |> as.numeric(),
         quarter = substr(yrq, 7, 7) |> as.numeric()) |>
  mutate(quarter = ifelse(is.na(quarter), "Y", quarter)) |>
  select(-yrq) |>
  mutate(type = ifelse(measure == "GY", "prv_year", "prv_period")) |>
  mutate(kick = ifelse(type == "prv_year" & quarter == "Y", 1, 0)) |> #redundant
  filter(kick == 0) |>
  select(-kick) |>
  select(-c(Unit, measure)) |> #redundant
  arrange(ccode)

data.table::fwrite(oecd, here("oecd_data", "oecd_cpi_quarterly_annual.csv"))
