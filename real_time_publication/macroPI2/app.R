library(data.table)
library(ggplot2)
library(MetBrewer)


.d <- `[`
cmonth <- format(Sys.time(), "%m") |> as.numeric()
cyear <- format(Sys.time(), "%Y") |> as.numeric()

release_season <- ifelse(cmonth %in% 4:9, "Spring", "Fall")

#in the first few months of the year, last year's release is still the current one
release_year <- ifelse(cmonth < 4, cyear - 1, cyear)
release <- paste0(release_season, release_year)
release_title <- paste(release_season, release_year)


cval <- release_year + ifelse(release_season == "Spring", 0, 0.5)
sval <- 2023.5

allvals <- seq(sval, cval, by = 0.5)

qufcs <- vector(mode = "list", length = 100)
realized_vals <- vector(mode = "list", length = 100)
point_fcs <- vector(mode = "list", length = 100)

for (k in 1:length(allvals)){


  yr <- floor(allvals[k])
  seas <- ifelse(allvals[k] %% 1 == 0, "Spring", "Fall")

  rls <- paste0(seas, yr)

  qufcs[[k]] <- data.table::fread(paste0("https://raw.githubusercontent.com/MacroPrediction/MacroPI/main/forecasts/forecasts_", rls, ".csv")) |>
    setDT()
  realized_vals[[k]] <- data.table::fread(paste0("https://raw.githubusercontent.com/MacroPrediction/MacroPI/main/imf-data/historicvalues_", rls, ".csv"))|>
    setDT()
  point_fcs[[k]] <- data.table::fread(paste0("https://raw.githubusercontent.com/MacroPrediction/MacroPI/main/imf-data/pointforecasts_", rls, ".csv"))|>
    setDT()
}
qufcs <- rbindlist(qufcs) |> unique()
#realized_vals <- rbindlist(realized_vals, idcol = TRUE) |> unique()
realized_vals <- data.table::fread(paste0("https://raw.githubusercontent.com/MacroPrediction/MacroPI/main/imf-data/historicvalues_", "Fall2024", ".csv"))|>
  setDT()
point_fcs <- rbindlist(point_fcs) |> unique()

#Card Inflation
eval_plot <- function(qufcs,
                      realized_vals,
                      point_fcs,
                      tyear,
                      target_var,
                      cis = c(0.5, 0.8)){

  cis <- c(0.5, 0.8)
  colors <- met.brewer("Hokusai1", 7)
  names(colors) <- unique(qufcs$country)
  textsize_y <- 13

  qus_list <- qu_lvls(cis) |>
    lapply(as.list)

  qus_list[[1]][[4]] <- "80% Interval"
  qus_list[[2]][[4]] <- "50% Interval"

  all_hors <- data.table(
    target = target_var,
    forecast_year = rep(c(tyear, tyear-1), each = 2),
    forecast_season = rep(c("F", "S"), times = 2),
    horizon = c(0, 0.5, 1, 1.5),
    target_year = tyear
  )


  tyear_qufcs <- qufcs |>
    .d(target == target_var) |>
    .d(target_year == tyear) |>
    .d(,hhelper := ifelse(forecast_season == "S", 0.5, 0)) |>
    .d(, horizon := (target_year - forecast_year) + hhelper ) |>
    .d(, hhelper := NULL) |>
    .d()
  tyear_realvals <- realized_vals |>
    .d(target_year == tyear) |>
    .d(target == target_var) |>
    .d()


  missing_ents <- tyear_qufcs[all_hors, on = c("forecast_year", "forecast_season", "horizon")] |>
    .d(is.na(target)) |>
    .d(, c("forecast_year", "horizon")) |>
    .d(, target := target_var) |>
    .d(, missing := TRUE)


  labeldat <- missing_ents |>
    copy() |>
    .d(, country := "GBR") |>
    .d(, prediction :=  (min(tyear_qufcs$prediction, tyear_realvals$true_value, -4) + max(tyear_qufcs$prediction,tyear_realvals$true_value, 4))/2) |>
    .d(, label := paste0("For this horizon, no \n real-time forecast \n intervals are available.\n Note that the \n project started \n Fall 2023."))



  linerange_dat <- tyear_qufcs |>
    .d(,quantile := paste0("quantile", quantile)) |>
    .d(, .(country, target, target_year, horizon, quantile, prediction)) |>
    dcast(country + target + target_year + horizon ~ quantile, value.var = "prediction")

  tyear_missing_realvals <- expand.grid(
    country = unique(qufcs$country),
    target = target_var,
    horizon = c(0, 0.5, 1, 1.5),
    target_year = tyear
  ) |>
    setDT() |>
    .d(, forecast_year := ceiling(target_year - horizon))

  tyear_missing_realvals <- missing_ents[tyear_missing_realvals, on = c("forecast_year", "horizon", "target")]

  tyear_missing_realvals <- tyear_realvals[tyear_missing_realvals, on = c("target_year", "country", "target")] |>
    .d(, true_value := ifelse(!is.na(missing), NA, true_value))

  ggplot() +
    scale_color_met_d("Hokusai1") +
    lapply(qus_list, function(qupr){
      geom_linerange(
        aes(y = as.factor(country),
            xmin = get(paste0("quantile", qupr[[1]])),
            xmax = get(paste0("quantile", qupr[[2]])),
            alpha = qupr[[4]],
            color = as.factor(country)),
        data = linerange_dat,
        lwd = 3)

    }) +
    geom_point(aes(x = true_value, y = as.factor(country),
                   shape = "Realized Value"),
               data = tyear_missing_realvals,
               color = "grey30") +
    geom_label(aes(x = prediction, y = country, label = label), data = labeldat,
               color = "grey50",
               alpha = 0.45,
               size=2.35 ) +
    scale_alpha_manual(name = "",
                       breaks = c("50% Interval", "80% Interval"),
                       values = c("50% Interval" = 0.6, "80% Interval" = 0.4),
                       guide = guide_legend(override.aes = list(color = "grey30") )) +
    facet_grid(~horizon,
               labeller = as_labeller(plot_horizon_label(4))) +
    xlim(min(tyear_qufcs$prediction, tyear_realvals$true_value, -4), max(tyear_qufcs$prediction,tyear_realvals$true_value, 4)) +
    scale_y_discrete(breaks = unique(qufcs$country), labels = plot_country_label()) +
    theme_uqimf() %+replace%
    theme(
      legend.position = "right",
      #legend.text=element_text(size=12),
      #axis.text.x = element_text(size = textsize_y, angle = 90, hjust = .5, vjust = .5, face = "plain"),
      #strip.text = element_text(size = 8),
      axis.text.x = element_text(size = textsize_y),
      axis.text.y = element_text(size = textsize_y),
      axis.title.y = element_blank(),
      strip.text = element_text(size=textsize_y),
      legend.text=element_text(size=textsize_y),
      legend.title=element_blank(),
      plot.margin = margin(t=10,b=10,r=10,l=10, unit = "pt")) +
    guides(color = "none") +
    scale_shape_manual(name = "",
                       breaks = c("Realized Value"),
                       values = c("Realized Value" = 19),
                       guide = guide_legend(override.aes = list(color = "grey30") ))


}


eval_plot(qufcs, realized_vals, point_fcs, tyear = 2023, target_var = "gdp_growth")
