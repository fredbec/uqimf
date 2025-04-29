rm(list = ls())
setwd("/home/fabian/Schreibtisch/macro_paper/OECD/")

library(dplyr)
library(tidyr)
library(scoringRules)

horizons <- 0:1
g7 <- c("CAN", "FRA", "DEU", "ITA", "JPN", "GBR", "USA")
vnms <- c("ngdp_rpch", "pcpi_pch")
vnms2 <- c("gdp", "cpi")
seasons <- c("S", "F")
dat_weo <- read.csv("weodat.csv")
df_all <- data.frame()
df_all_micro <- data.frame()

get_flnm <- function(country, forecast_season, model_nr){
  dirs <- c(rep("fcsts_ar_2025/", 3), rep("bvar_imf_2025/", 2))
  if (model_nr < 4){
    nm0 <- paste0("fcst_", country, "_", forecast_season, "_")
  } else {
    nm0 <- paste0("fcst_quantiles_", country, "_", forecast_season)
  }
  if (model_nr == 1){
    nm0 <- paste0(nm0, "annual.csv")
  } else if (model_nr == 2){
    nm0 <- paste0(nm0, "bic.csv")
  } else if (model_nr == 3){
    nm0 <- paste0(nm0, "p=1.csv")
  } else if (model_nr == 4){
    nm0 <- paste0(nm0, ".csv")
  } else if (model_nr == 5){
    nm0 <- paste0(nm0, "_const.csv")
  }
  paste0(dirs[model_nr], nm0)
}
model_labels <- c("ar_annual", "ar_bic", "ar_p=1", "bvar_sv", "bvar_const")


for (fs in seasons){
  for (cc in g7){
    for (mm in 1:5){
      dat <- get_flnm(cc, fs, mm) |> read.csv()
      for (vv in vnms){
        vv2 <- vnms2[which(vnms == vv)]
        for (hh in horizons){
          if (cc == "JPN"){
            max_target_year <- 2020
          } else {
            max_target_year <- 2050
          }
          dat2 <- dat_weo %>% filter(country == cc, target == vv, 
                                     forecast_season == fs, 
                                     target_year == (forecast_year + hh), 
                                     target_year >= (2013 + hh), 
                                     target_year <= max_target_year) %>%
            transmute(target_year, rlz = tv_1)
          dat3 <- dat %>% filter(var == vv2, target_year == (forecast_year + hh)) %>%
            mutate(quantile_level = paste0("q", 100*quantile_level)) %>%
            pivot_wider(names_from = quantile_level) %>% arrange(target_year) %>%
            merge(dat2)
          inds1 <- which(!is.na(dat3$rlz) & !is.na(dat3$q50))
          inds2 <- which(grepl("q", names(dat3)))
          crps_dat <- crps_sample(y = dat3$rlz[inds1], 
                                  dat = as.matrix(dat3[inds1,inds2]))
          df_tmp <- data.frame(country = cc, variable = vv2, season = fs, 
                               horizon = hh, model = model_labels[mm], 
                               n = length(crps_dat), crps = mean(crps_dat))
          df_all <- rbind(df_all, df_tmp)
          
          df_tmp_micro <- data.frame(country = cc, variable = vv2, season = fs, 
                                     horizon = hh, model = model_labels[mm], 
                                     target_year = dat3$target_year[inds1],
                                     crps = crps_dat, rlz = dat3$rlz[inds1], 
                                     pi_len = dat3[inds1,"q90"]-dat3[inds1,"q10"])
          df_all_micro <- rbind(df_all_micro, df_tmp_micro)
          
        }
      }
    }
  }
}

# Compute CRPS relative to AR(1)
crps1 <- df_all %>% group_by(country, variable, season, horizon) %>%
  mutate(check = (sd(n) == 0), crps_rel = crps/crps[model == "ar_p=1"])
# Sanity check (same number of evaluation data points for all models)
all(crps1$check)
# Box plot of relative CRPS values
boxplot(crps_rel~model, data = crps1)
# Performance of new annual AR(1) model, separated across horizons
crps1 %>% filter(model == "ar_annual") %>% 
  group_by(variable, season, horizon) %>%
  summarise(crps_rel = median(crps_rel))

# Performance comparisons between model pairs
crps2 <- df_all %>%
  pivot_wider(names_from = model, values_from = crps)
mean(crps2$bvar_const < crps2$bvar_sv, na.rm = TRUE)

plot(crps2$bvar_const, crps2$bvar_sv, bty = "n", 
     xlab = "BVAR", ylab = "BVAR-SV")
abline(a = 0, b = 1, lty = 2, lwd = .4)

plot(crps2$`ar_p=1`, crps2$bvar_sv, bty = "n", 
     xlab = "AR(1)", ylab = "BVAR-SV")
abline(a = 0, b = 1, lty = 2, lwd = .4)

# CRPS comparisons between two BVAR models over time
# (to check impact of Covid outliers on BVAR-SV model)
plot_df <- df_all_micro %>% filter(country == "FRA", horizon == 0, season == "F", 
                        variable == "gdp", model %in% model_labels[4:5]) %>%
  select(-pi_len) %>%
  pivot_wider(names_from = model, values_from = crps)
matplot(plot_df$target_year, plot_df[,c("bvar_sv", "bvar_const")], 
        bty = "n", pch = 20, xlab = "Year", ylab = "CRPS")
matplot(plot_df$target_year, plot_df[,c("bvar_sv", "bvar_const")], 
        type = "l", lwd = .5, lty = 2, add = TRUE)
