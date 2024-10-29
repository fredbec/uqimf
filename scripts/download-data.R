library(here)
library(arrow)

DT <- `[`


#imfpp is an R package that can be downloaded from GitHub with the following command:
#devtools::install_github("https://github.com/fredbec/imfpp")

#then downdload IMF data
imfpp::download.process.weo(includeGeoData = FALSE)

#this alos downloads two datafiles, WB_LFP, WB_GDPpC, these can be deleted

weodat <- data.table::fread(here("WEOforecasts_tidy.csv"))

#save as parquet to limit file size for GitHub
write_parquet(weodat, here(location_download, "WEOforecasts_prefilter.parquet"))
file.remove(here("WEOforecasts_tidy.csv"))

#also remove extra files that come with the download
file.remove(here("FMEcongroup.xlsx"))
file.remove(here("WB_GDPpC.xls"))
file.remove(here("WB_LFP.xls"))
