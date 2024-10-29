library(here)
library(arrow)

DT <- `[`


#imfpp is an R package that can be downloaded from GitHub with the following command:
#devtools::install_github("https://github.com/fredbec/imfpp")

#then downdload IMF data
imfpp::download.process.weo(includeGeoData = FALSE, includeCountryGroups = TRUE)

#this alos downloads two datafiles, WB_LFP, WB_GDPpC, these can be deleted

weodat <- data.table::fread(here("WEOforecasts_tidy.csv"))

#save as parquet to limit file size for GitHub
write_parquet(weodat, here(location_download, "WEOforecasts_prefilter.parquet"))
file.remove(here("WEOforecasts.csv"))
file.remove(here("WEOforecasts_tidy.csv"))

file.remove(here("FMEconGroup.xlsx"))
