rm(list = ls())
setwd("/home/fabian/Schreibtisch/notes_and_ideas/IMF/")

library(dplyr)
library(zoo)
library(bvarsv)
library(tidyr)
library(ggplot2)

# labeling stuff
g7_oecd <- c("CAN", "FRA", "DEU", "ITA", "JPN", "GBR", "USA")
g7_imf <- c("Canada", "France", "Germany", "Italy", "Japan", 
            "United Kingdom", "United States")
vnms_oecd <- c("cpi", "gdp")
vnms_imf <- c("pcpi_pch", "ngdp_rpch")

# collect forecast files (BVAR and AR in separate directories)
df_all <- data.frame()
for (gg in c("fcsts", "fcsts_ar")){
  fc <- list.files(gg)
  for (ii in fc){
    tmp <- read.csv(paste0(gg, "/", ii)) %>%
      mutate(method = gg)
    df_all <- rbind(df_all, 
                    data.frame(tmp, 
                               country = substr(ii, 6, 8), 
                               season = substr(ii, 10, 10)))
  }
}

# compare BVAR and AR to WEO forecasts
weo0 <- read.csv("WEOforecasts_tidy.csv", sep = ",")
weo <- weo0 %>%
  filter(country %in% g7_imf) %>%
  select(country, target, forecast_year, target_year, 
         prediction, forecast_season, tv_1) %>%
  transmute(country = g7_oecd[match(country, g7_imf)], 
            var = vnms_oecd[match(target, vnms_imf)], 
            forecast_year, target_year, season = forecast_season, 
            value_imf = prediction, 
            truth = tv_1) %>%
  filter(target_year <= (forecast_year + 1))

# Loop over countries
for (cc in g7_oecd){
  # merge time series and imf forecasts
  tmp1 <- df_all %>% filter(country == cc) %>%
    pivot_wider(id_cols = c("var", "forecast_year", 
                            "target_year", "season"), 
                names_from = "method") %>%
    rename(value = fcsts, value_ar = fcsts_ar)
  tmp2 <- weo %>% filter(country == cc)
  tmp3 <- merge(tmp1, tmp2) %>%
    mutate(horizon = 2*(target_year - forecast_year) + 
             as.numeric(season == "S") + 1) %>%
    mutate(horizon = as.character(horizon)) %>%
    na.omit
  # compute mean squared errors
  tmp4 <- tmp3 %>% group_by(var, horizon) %>%
    summarise(mse = mean((truth-value)^2), 
              mse_ar = mean((truth-value_ar)^2),
              mse_imf = mean((truth-value_imf)^2), 
              cor = cor(value, value_imf)) %>%
    ungroup %>% 
    pivot_longer(cols = c("mse", "mse_ar", "mse_imf", "cor"))
  # plot mean squared errors (separately for each variable and country)
  # that is, 2*7 = 14 plots in total
  for (vv in c("cpi", "gdp")){
    tmp4 %>% filter(var == vv, name != "cor") %>% 
      ggplot(aes(x = horizon, y = value, fill = name)) + 
      geom_bar(stat="identity", position = "dodge") + 
      theme_minimal(base_size = 18) + 
      theme(legend.position = "top") + 
      xlab("Forecast Horizon") + ylab("Mean Squared Error") + 
      scale_fill_discrete(name = "", 
                          breaks = c("mse", "mse_ar", "mse_imf"), 
                          labels = c("MSE(BVAR)", 
                                     "MSE(AR)",
                                     "MSE(IMF)"))
    ggsave(paste0("plots/", cc, "_", vv, ".pdf"), 
           width = 16, height = 9)
    # make time series plots of forecasts 
    # (separately for each country, variable and forecast horizon)
    # that is, 7*2*4=56 plots in total
    for (hh in 1:4){
      tmp3 %>% filter(var == vv, horizon == hh) %>%
        select(target_year, contains("value"), "truth") %>%
        pivot_longer(c(contains("value"), "truth")) %>%
        ggplot(aes(x = target_year, y = value, color = name)) + 
        geom_point() + geom_line(linewidth = .5) + 
        scale_color_discrete(name = "", 
                             breaks = c("value", 
                                        "value_ar", 
                                        "value_imf",
                                        "truth"), 
                             labels = c("BVAR", 
                                        "AR",
                                        "IMF", 
                                        "Truth")) + 
        theme_minimal(base_size = 16) + 
        theme(legend.position = "top") + xlab("Year") + 
        ylab("Forecast")
      ggsave(paste0("plots/line_", cc, "_", vv, "_", hh, ".pdf"), 
             width = 16, height = 9)
    }
  }
}

# save forecast data frame
dats0 <- weo %>%
  pivot_longer(cols = c("value_imf", "truth"), 
               names_to = "method") %>%
  mutate(method = if_else(method == "value_imf", "IMF", "Truth"))
dats1 <- df_all %>%
  mutate(method = if_else(method == "fcsts", "bvar", "ar"))
dat_save_long <- rbind(dats0, dats1)
write.csv(dat_save_long, file = "forecast_long.csv")
dat_save_wide <- dat_save_long %>% 
  pivot_wider(id_cols = c("forecast_year", "target_year", 
                          "country", "var", "season"), 
              names_from = "method")
write.csv(dat_save_wide, file = "forecast_wide.csv")