library(here)

DT <- `[`

imfpp::download.process.weo(includeGeoData = FALSE)

weodat <- data.table::fread(here("WEOforecasts_tidy.csv"))

N <- nrow(weodat)


#split data into 10 subdatasets (for uploading to GitHub)
weodat |>
  DT(, splitinds := rep(1:10, each = N/10)) |>
  split(by = "splitinds") |>
  lapply(function(dat){

    splitind <- unique(dat$splitinds)

    dat |>
      DT(, splitinds := NULL) |>
      data.table::fwrite(here("data", paste0("WEOforecasts", splitind, ".csv")))
    }
) |>
  unlist() #just for nicer printed output

