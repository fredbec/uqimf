shinyplot <- function(realized_series,
                      linerange_data_mod1,
                      linerange_data_mod2,
                      point_forecasts,
                      future_realized,
                      plot_country,
                      colorscale,
                      cis,
                      ylimmax = 9.5){

  .d <- `[`

  trgt <- unique(realized_series$target)
  qus_list <- qu_lvls(cis) |>
    lapply(as.list)

  qus_list[[1]][[4]] <- "80% Interval"
  qus_list[[2]][[4]] <- "50% Interval"

  minval <- min(realized_series$true_value) - 0.5

  ggplot() +
    geom_line(
      aes(x = target_year, y = true_value, linetype = "Realized Series"),
      color = colorscale[plot_country],
      data = realized_series |> .d(country == plot_country),
      lwd = 0.75,
      alpha = 0.5) +
    geom_point(
      aes(x = target_year, y = true_value),
      color = colorscale[plot_country],
      data = realized_series |> .d(country == plot_country),
      pch = 3,
      size = 0.95) +
    ggtitle(paste0("Actual Series, with forecast for year ", release_year)) +
    ylab(plot_target_label()[trgt]) +
    ylim(minval, ylimmax) +
    xlab("") +
    ggtitle(plot_country_label()[plot_country]) +
    scale_color_met_d("Hokusai1") +
    lapply(qus_list, function(qupr){
      geom_linerange(
        aes(x = target_year,
            ymin = get(paste0("quantile", qupr[[1]])),
            ymax = get(paste0("quantile", qupr[[2]])),
            alpha = qupr[[4]]),
        color = colorscale[plot_country],
        data = linerange_data_mod1 |> .d(country == plot_country),
        lwd = 3)

    }) +
    lapply(qus_list, function(qupr){
      geom_linerange(
        aes(x = target_year,
            ymin = get(paste0("quantile", qupr[[1]])),
            ymax = get(paste0("quantile", qupr[[2]])),
            alpha = qupr[[4]]),
        color = colorscale[plot_country],
        data = linerange_data_mod2 |> .d(country == plot_country),
        lwd = 3)

    }) +

    geom_point(
      aes(x = target_year, y = prediction, shape = "IMF Point Forecast"),
      color = colorscale[plot_country],
      data = point_forecasts |> .d(country == plot_country)
    ) +

    geom_line(
      aes(x = target_year, y = prediction, linetype = "Projection"),
      color = colorscale[plot_country],
      data = future_realized |> .d(country == plot_country)
    ) +
    theme_uqimf() %+replace%
    theme(
      legend.position = "right",
      legend.text=element_text(size=12),
      plot.title = element_text(hjust = 0.5,
                                vjust = 3,
                                colour = colorscale[plot_country],
                                size = 18,
                                face = "bold")) +
    scale_alpha_manual(name = "",
                       breaks = c("50% Interval", "80% Interval"),
                       values = c("50% Interval" = 0.6, "80% Interval" = 0.4),
                       guide = guide_legend(override.aes = list(color = "grey30") )) +

    scale_shape_manual(name = "",
                       breaks = c("IMF Point Forecast"),
                       values = c("IMF Point Forecast" = 19),
                       guide = guide_legend(override.aes = list(color = "grey30") )) +
    scale_linetype_manual(name = "LEGEND",
                          breaks = c("Realized Series", "Projection"),
                          values = c("Realized Series" = "solid", "Projection" = "dashed"),
                          guide = guide_legend(override.aes = list(color = "grey30") ))

}
