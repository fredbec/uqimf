library(here)
library(dplyr)
library(data.table)
library(ggplot2)
DT <- `[`

weodat <- data.table::fread(here("WEOforecasts_tidy.csv"))
cpidat <-
  data.table::fread(here("oecd_data", "oecd_cpi.csv")) |>
  filter(FREQUENCY == "A",
         SUBJECT == "TOT") |>
 # filter(FREQUENCY == "Q", #quarterly
#         SUBJECT == "TOT") |> #total CPI (others are e.g. food, engergy)
  filter(LOCATION %in% c("CAN", "FRA", "DEU", "ITA", "JPN", "GBR", "USA")) |>
  select(-c("INDICATOR", "SUBJECT", "FREQUENCY", "Flag Codes")) |> #redundant
  rename(country = LOCATION,
         measure = MEASURE,
         target_year = TIME,
         value = Value) |>
  mutate(target_year = as.numeric(target_year)) |>
  filter(target_year >= 1990,
         measure == "AGRWTH") |>
  rename(oecdval = value,
         ccode = country) |>
  select(-measure)


weodat_g7 <- weodat |>
  filter(horizon == 0.5,
         !is.na(tv_1),
         g7 == "G7",
         target == "pcpi_pch") |>
  rename(ccode = ISOAlpha_3Code) |>
  select(ccode, country, target_year, tv_1, tv_2) |>
  distinct() |>
  left_join(cpidat, by = c("ccode", "target_year")) |>
  setDT() |>
  melt(id.vars = c("ccode", "country", "target_year"),
       variable.name = "truth_type",
       value.name = "truth_value") |>
  mutate(
    target_year = as.numeric(target_year),
    truth_type = factor(truth_type,
                             levels = c("tv_1", "tv_2", "oecdval"),
                             labels = c("WEO_1yr", "WEO_2yr", "OECD_annual")))

ltdict <- c("solid", "dashed", "solid", "dashed")
coldict <- RColorBrewer::brewer.pal(3, "Set2")[2:3] |> rep(each = 2)
names(ltdict) <- unique(weodat_g7$truth_type)
names(coldict) <- unique(weodat_g7$truth_type)




ggplot(weodat_g7, aes(x = target_year, y = truth_value)) +
  geom_line(aes(linetype = truth_type, color = truth_type)) +
  scale_color_manual(values = coldict) +
  scale_linetype_manual(values = ltdict) +
  facet_wrap(~country, ncol = 2) +
  theme_minimal() %+replace%
  theme(legend.title = element_blank(),
        legend.position = "bottom") +
  ylab("CPI Inflation") +
  xlab("Target Year")


cpidat_q <-
  data.table::fread(here("oecd_data", "oecd_cpi.csv")) |>
  filter(FREQUENCY == "Q", #quarterly
         SUBJECT == "TOT") |> #total CPI (others are e.g. food, engergy)
  filter(LOCATION %in% c("CAN", "FRA", "DEU", "ITA", "JPN", "GBR", "USA")) |>
  select(-c("INDICATOR", "SUBJECT", "FREQUENCY", "Flag Codes")) |> #redundant
  rename(country = LOCATION,
         measure = MEASURE,
         year = TIME,
         value = Value) |>
  mutate(target_year = substr(yrq, 1, 4) |> as.numeric(),
         quarter = substr(yrq, 7, 7) |> as.numeric()) |>
  filter(target_year >= 1990,
         quarter == 4,
         measure == "AGRWTH") |>
  select(-c(yrq, quarter)) |>
  rename(q4val = value,
         ccode = country) |>
  select(-measure)
