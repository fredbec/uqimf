library(arrow)
library(here)

weo_countries <- read_parquet(here("data", "WEOforecasts_prefilter.parquet")) |>
  # the following is not totally clean, but
  # NLD for some reason has NA for economy, but is Advanced, so adding it in here
  .d(, economy := ifelse(ISOAlpha_3Code == "NLD", "Advanced Economies", economy)) |>
  .d(ISOAlpha_3Code != "") |>
  .d(g7 == 0) |>
  .d(,c("ISOAlpha_3Code", "country", "economy")) |>
  unique()

weourl <- "https://www.imf.org/-/media/Files/Publications/WEO/WEO-Database/2024/October/WEOOct2024all.ashx"
#mode "wb" (otherwise won't be readable)

#uncomment next line to downdload data
#then convert from xls to csv in excel, because otherwise it doesnt work....
#download.file(weourl, "WEOeconomies.xls", mode = "wb")

weo_economies <- data.table::fread(here("WEOeconomies.csv"), sep = ";") |>
  #setnames("Subject Descriptor", "sub") |>
  setnames("WEO Subject Code", "sub") |>
  #.d(sub == "Gross domestic product, current prices") |>
  .d(sub == "NGDPD") |>
  .d(, c("ISO", "2023")) |>
  setnames("ISO", "ISOAlpha_3Code")

matchedc <- merge(weo_countries, weo_economies, by = c("ISOAlpha_3Code")) |>
  setnames("2023", "value") |>
  .d(, value := gsub(",", "", value)) |>
  .d(, value := as.numeric(value)) |>
  .d(economy %in% c("Advanced Economies", "Emerging Market Economies")) |>
  .d(order(economy, -value),.SD[1:5], by = economy)
#warning message only pertains to Sri Lanka, where data is missing (but isn't large enough anyways)


print(matchedc |> .d(economy == "Emerging Market Economies") |> .d(, "country"))|> unname() |> unlist()
print(matchedc |> .d(economy == "Advanced Economies") |> .d(, "country"))|> unname() |> unlist()
