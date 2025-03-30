library(here)
library(data.table)
library(dplyr)
.d <- `[`
source(here("benchmarks", "raw", "forecasts_2025", "ar_x_forecast_procs.R"))
source(here("specs", "specs.R"))

#yy <- 2013
#fsn <- "F"
#ctry <- "CAN"
#tgt <- "pcpi_pch"

if(specs$cset == "base"){
  ctrys <- c("CAN", "FRA", "DEU", "ITA", "JPN", "GBR", "USA")
} else {
  ctrys <- specs$cset_list
}
vnms <- c("gdp", "cpi")
yrs_fcst <- 2013:2023
fsns <- c("F", "S")

# quantile grid
quantile_grid = seq(from = .01, to = .99, by = .01)
n_q <- length(quantile_grid)

full_dat <- data.table::fread(here("data", "weodat.csv")) |>
  .d(,target := ifelse(target == "pcpi_pch", "cpi", "gdp")) |>
  .d(, ind_rus := (target_year < 2000)) |>
  .d(, ind_bra := (target_year < 1998)) |>
  .d(, ind_rus := ifelse(ind_rus == 1 & country == "RUS" & target == "cpi", 1, 0))|>
  .d(, ind_bra := ifelse(ind_bra == 1 & country == "BRA" & target == "cpi", 1, 0)) |>
  .d(ind_rus == 0 & ind_bra == 0) |>
  .d(, ind_rus := NULL) |>
  .d(, ind_bra := NULL)

for(ctry in ctrys){
  for(fsn in fsns){
    df_all <- data.frame()
    for(tgt in vnms){
      for(yy in yrs_fcst){


        dat_y <- full_dat |>
          data.table::copy() |>
          .d(forecast_season == fsn) |>
          .d(country == ctry & target == tgt) |>
          .d(, c("country", "target", "target_year", "forecast_year", "tv_0.5", "tv_1", "horizon", "prediction")) |>
          .d(, ind := ifelse(fsn == "S" & target_year == yy, 1, 0)) |>
          .d(, tv_1 := ifelse(ind == 1, tv_0.5, tv_1)) #impute lastest tv_1 with tv_0 if in spring

        y_dat <- dat_y |>
          .d(target_year <= yy - 1) |> # only have access to last year's datapoint, of course
          .d(horizon %in% c(0, 0.5)) |>
          .d(, "tv_1") |>
          unname() |>
          unlist()
        Tl <- length(y_dat)
        #for dat_z, we have to select by forecast_year!
        dat_z <- full_dat |>
          .d(forecast_season == fsn) |>
          .d(country == ctry & target == tgt) |>
          .d(, c("country", "target", "forecast_year", "horizon", "prediction"))

        z_dat_c <- dat_z |>
          .d(forecast_year <= yy) |>
          .d(horizon %in% c(0, 0.5)) |>
          .d(, "prediction")|>
          unname() |>
          unlist()

        #exclude first forecast (no lagged value available yet)
        z_dat_c <- z_dat_c[2:length(z_dat_c)]

        z_dat_n <- dat_z |>
          .d(forecast_year <= yy) |>
          .d(horizon %in% c(1, 1.5)) |>
          .d(, "prediction")|>
          unname() |>
          unlist()


        quantile_grid = seq(from = .01, to = .99, by = .01)

        fit_current <- ar_x_fcst(y_dat, z_dat_c, lag = 1, max_h = 1)

        #fit_next1 <- ar_x_fcst_lag(y_dat, z_dat_n, lag = 2, max_h = 1)
        fit_next <- ar_x_fcst_lag_extra(dat = y_dat, dat_z_n = z_dat_n, dat_z_c = z_dat_c, lag = 2, max_h = 1)

        fc_current <- qnorm(p = quantile_grid,
                            mean = fit_current$fc_mean[1],
                            sd = sqrt(fit_current$fc_vcv[1,1]))
        #fc_next1 <- qnorm(p = quantile_grid,
        #                 mean = fit_next1$fc_mean[1],
        #                 sd = sqrt(fit_next1$fc_vcv[1,1]))
        fc_next <- qnorm(p = quantile_grid,
                          mean = fit_next$fc_mean[1],
                          sd = sqrt(fit_next$fc_vcv[1,1]))

        df_current <- data.table(var = rep(tgt, n_q),
                                 forecast_year = rep(yy, n_q),
                                 target_year = rep(yy, n_q),
                                 quantile_level = quantile_grid,
                                 value = fc_current)


        df_next <- data.table(var = rep(tgt, n_q),
                                 forecast_year = rep(yy, n_q),
                                 target_year = rep(yy+1, n_q),
                                 quantile_level = quantile_grid,
                                 value = fc_next)

        df_tmp <- rbind(df_current, df_next)

        df_all <- rbind(df_all, df_tmp)

      }
    }
    data.table::fwrite(df_all,
              file = here("benchmarks", "raw", "forecasts_2025", "fcsts_ar_2025",
                          paste0("fcst_", ctry, "_",
                            fsn,
                            "_annual_x.csv")),
              row.names = FALSE)
  }
}
