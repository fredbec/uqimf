library(here)
library(data.table)
library(ggplot2)
library(MetBrewer)
library(patchwork)
library(ggpubr)
library(bslib)

#source(here("test-macropi", "shiny-plot-deploy-function.R"))

.d <- `[`
cmonth <- format(Sys.time(), "%m") |> as.numeric()
cyear <- format(Sys.time(), "%Y") |> as.numeric()

release_season <- ifelse(cmonth %in% 4:9, "Spring", "Fall")

#in the first few months of the year, last year's release is still the current one
release_year <- ifelse(cmonth < 4, cyear - 1, cyear)
release <- paste0(release_season, release_year)
release_title <- paste(release_season, release_year)

localrun <- FALSE


stepback <- tryCatch({
  # Attempt to read the file from the URL
  if(localrun){
    data.table::fread(here("real_time_publication", "forecasts", paste0("forecasts_", release, ".csv")))
  }

  else {
    read.csv(paste0("https://raw.githubusercontent.com/MacroPrediction/MacroPI/main/forecasts/forecasts_", release, ".csv"))
  }

  FALSE

}, error = function(e) {
  # If an error occurs (e.g., file not found), set month to 9
  #message("Current release not yet out, default to previous season")

  return(TRUE)
})

if(stepback){

  release_year <- ifelse(release_season == "Spring", release_year - 1, release_year)
  release_season <- ifelse(release_season == "Spring", "Fall", "Spring")
  release <- paste0(release_season, release_year)
  release_title <- paste(release_season, release_year)
}

ui <- page_sidebar(
  title = "Simple Macroeconomic Forecast Distributions for the G7 Economies",
  sidebar = sidebar("These panels contain forecast intervals for growth and inflation
                    in the G7 countries and current and next
                    year's target.", tags$br(), tags$br(),
                    "Our forecast intervals are constructed around the current IMF
                    point forecasts and the methodology to create them is based on past
                    IMF point forecast errors.",tags$br(), tags$br(),
                    "All forecast intervals are publicly available via Github
                    (repo name: KITmetricslab/MacroPI)",tags$br(), tags$br(),
                    "Please note that this project is not affiliated with or endorsed
                    by the IMF."),
  card(
    card_header(paste0("GDP Growth Forecasts - ", release_title)),
    plotOutput(outputId = "allcplot_gdp",
               height = 1000, width = 1400)
  ),
  card(
    card_header(paste0("Inflation Forecasts - ", release_title)),
    plotOutput(outputId = "allcplot_inflation",
               height = 1000, width = 1400)
  )
)



server <- function(input, output) {
  .d <- `[`


  #######################################FUNS#####################################
  qu_lvls <- function(cis){

    qus <- ci_to_quantiles(cis, "directional") #always directional

    n_qus <- length(qus)

    no_pairs <- floor(n_qus/2)


    alpha_vals <- seq(0.5, 0.75, length.out = no_pairs)

    #inner and outer quantile levels
    lapply(seq_along(1:no_pairs),
           function(ind) c(qus[ind], qus[(n_qus)-(ind-1)], alpha_vals[ind]))


  }

  ci_to_quantiles <- function(ci_levels,
                              error_method){

    if(error_method == "directional"){

      qu_down <- 0.5 - ci_levels / 2
      qu_up <- 0.5 + ci_levels / 2

      qus <- c(qu_down, qu_up) |>
        unique() |> #in case of median
        sort()

    } else if (error_method == "absolute"){

      qus <- ci_levels
    }

    return(qus)
  }


  plot_target_label <- function(){

    ptl <- c(`inflation` = "Inflation rate (in %)",
             `gdp_growth` = "Real GDP growth rate (in %)")

    return(ptl)
  }


  plot_country_label <- function(){

    pcl <- c(`CAN` = "Canada",
             `DEU` = "Germany",
             `FRA` = "France",
             `GBR` = "United Kingdom",
             `ITA` = "Italy",
             `JPN` = "Japan",
             `USA` = "United States")

    return(pcl)
  }

  theme_uqimf <- function() {
    theme_minimal() %+replace%
      theme(axis.line = element_line(colour = "grey80"),
            axis.ticks = element_line(colour = "grey80"),
            panel.border = element_blank(),
            panel.background = element_blank(),
            legend.position = "bottom",
            legend.title=element_blank())
  }

  shinyplot <- function(realized_series,
                        linerange_data,
                        point_forecasts,
                        future_realized,
                        labeldat_list,
                        plot_country,
                        colorscale,
                        cis,
                        ylimmax = 9.5){

    .d <- `[`

    textsize_y <- 12

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
        lwd = 0.75) +
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
      scale_x_continuous(breaks=seq(cyear-7, cyear+1, 1)) +
      ggtitle(plot_country_label()[plot_country]) +
      scale_color_met_d("Hokusai1") +
      lapply(qus_list, function(qupr){
        geom_linerange(
          aes(x = target_year,
              ymin = get(paste0("quantile", qupr[[1]])),
              ymax = get(paste0("quantile", qupr[[2]])),
              alpha = qupr[[4]]),
          color = colorscale[plot_country],
          data = linerange_data |> .d(country == plot_country),
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
      lapply(labeldat_list, function(labeldat){
        geom_label(data = labeldat |> .d(country == plot_country),
                   aes(x = x, y = y, label = label),
                   color = "grey50",
                   alpha = 0.45,
                   size=4.35 ,
                   #angle=45,
                   fontface="bold")
      }) +
      theme_uqimf() %+replace%
      theme(
        legend.position = "right",
        #legend.text=element_text(size=12),
        plot.title = element_text(hjust = 0.5,
                                  vjust = 3,
                                  colour = colorscale[plot_country],
                                  size = 18,
                                  face = "bold"),
        #axis.text.x = element_text(size = textsize_y, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        #strip.text = element_text(size = 8),
        axis.text.x = element_text(size = textsize_y),
        axis.text.y = element_text(size = textsize_y),
        axis.title.y = element_text(size = textsize_y, angle = 90, vjust = 2),
        strip.text = element_text(size=textsize_y),
        legend.text=element_text(size=textsize_y),
        legend.title=element_blank(),
        plot.margin = margin(t=10,b=10,r=10,l=10, unit = "pt")) +
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

  #######################################Global Settings####################################

  patchwork_layout <-
    "112333
    445566
    778899"
  patchwork_layout2 <-
    "11223344
    55667788"

  displaytext <-  function(targetlabel){
    return(paste(paste0("This panel contains distributional forecasts for ", targetlabel, " in the"),
                 "format of prediction intervals, for G7 countries and current and next",
                 "year's target",
                 "",
                 "Our distributional forecasts are constructed around the current IMF",
                 "point forecasts and the methodology to create them is based on past",
                 "IMF point forecast errors.",
                 "",
                 "Our forecasts are publicly available and documented in the following",
                 "Github repository: https://github.com/KITmetricslab/MacroPI/ ",
                 "",
                 "Please note that this project is not affiliated with or endorsed",
                 "by the IMF.",
                 sep = "\n"))
  }

  if(localrun){

    qufcs <- data.table::fread(here("real_time_publication", "forecasts", paste0("forecasts_", release, ".csv"))) |>
      setDT()
    realized_vals <- data.table::fread(here("real_time_publication", "imf-data", paste0("historicvalues_", release, ".csv"))) |>
      setDT()
    point_fcs <- data.table::fread(  here("real_time_publication", "imf-data", paste0("pointforecasts_", release, ".csv"))) |>
      setDT()
  } else {
    qufcs <- read.csv(paste0("https://raw.githubusercontent.com/MacroPrediction/MacroPI/main/forecasts/forecasts_", release, ".csv")) |>
      setDT()
    realized_vals <- read.csv(paste0("https://raw.githubusercontent.com/MacroPrediction/MacroPI/main/imf-data/historicvalues_", release, ".csv"))|>
      setDT()
    point_fcs <- read.csv(paste0("https://raw.githubusercontent.com/MacroPrediction/MacroPI/main/imf-data/pointforecasts_", release, ".csv"))|>
      setDT()
  }



  cis <- c(0.5, 0.8)
  qus <- c(0.1, 0.25, 0.75, 0.9)
  cols <- paste0("quantile", qus)


  colors <- met.brewer("Hokusai1", 7)
  names(colors) <- unique(qufcs$country)

  #######################################Inflation#####################################
  infl <- reactive({
    ylimmax <- 11
    trgt <- "inflation"
    ylabel <- "Inflation rate (in %)"
    textlabel <- "CPI inflation"

    linerange_dat <- qufcs |>
      .d(target == trgt) |>
      .d(,quantile := paste0("quantile", quantile)) |>
      .d(,season_helper := ifelse(forecast_season == "S", 0.5, 0)) |>
      .d(,horizon := (target_year - forecast_year) + season_helper) |>
      .d(, .(country, target, target_year, horizon, quantile, prediction)) |>
      dcast(country + target + target_year + horizon ~ quantile, value.var = "prediction")

    realized_vals_infl <- realized_vals |>
      .d(target == trgt)
    point_fcs_infl <- point_fcs |>
      .d(target == trgt)

    dashed_line <- rbind(point_fcs_infl,
                         realized_vals_infl |>
                           copy() |>
                           setnames("true_value", "prediction") |>
                           .d(target_year >= release_year - 1)
    )

    labeldat_80_cy <- linerange_dat |>
      copy() |>
      .d(, (cols) := lapply(.SD, function(val) as.character(round(val, 1))), .SDcols = cols) |>
      .d(, (cols) := lapply(.SD, function(val) ifelse(grepl("[.]", val), val, paste0(val, ".0"))), .SDcols = cols) |>
      .d(, .(country, target, target_year, quantile0.1, quantile0.9)) |>
      dcast(country + target ~ target_year, value.var = c("quantile0.1", "quantile0.9")) |>
      .d(, x :=  release_year - 5.1) |>
      .d(, y := ylimmax - 1.75) |>
      .d(, label := paste0("80% Forecast Intervals:", "\n", release_year, ": ", get(paste0("quantile0.1_", release_year)), "% to ", get(paste0("quantile0.9_", release_year)) , "%", "\n",
                           release_year+1, ": ", get(paste0("quantile0.1_", release_year+1)), "% to ", get(paste0("quantile0.9_", release_year+1)) , "%"))
    ###########################################################################

    plotlist <- lapply(as.list(unique(qufcs$country)),
                       function(pltc) shinyplot(realized_vals_infl, linerange_dat, point_fcs_infl, dashed_line, list(labeldat_80_cy), pltc, colors, cis, ylimmax = ylimmax))


    text2 <- get_legend(plotlist[[1]])
    text3 <- grid::textGrob(label = displaytext(textlabel),
                            x = unit(0.10, "npc"),
                            hjust = 0)

   # text2 + plotlist[[2]] + plotlist[[1]] + plotlist[[3]] + plotlist[[4]] + plotlist[[5]] + plotlist[[6]] + plotlist[[7]]+
    #plot_layout(guides = "collect",
    #              design = patchwork_layout2) &
    #  theme(legend.position='none')
    plotlist[[2]] + plotlist[[1]] + plotlist[[3]] + plotlist[[4]] + plotlist[[5]] + plotlist[[6]] + plotlist[[7]]+ text2 +
      plot_layout(guides = "collect",
                  design = patchwork_layout2) &
      theme(legend.position='none')
  })

  output$allcplot_inflation <- renderPlot(

    infl()

  )

  ########################################GDP#####################################
  gdp <-  reactive({
    ylimmax <- 14.5
    trgt <- "gdp_growth"
    textlabel <- "GDP Growth"

    linerange_dat <- qufcs |>
      .d(target == trgt) |>
      .d(,quantile := paste0("quantile", quantile)) |>
      .d(,season_helper := ifelse(forecast_season == "S", 0.5, 0)) |>
      .d(,horizon := (target_year - forecast_year) + season_helper) |>
      .d(, .(country, target, target_year, horizon, quantile, prediction)) |>
      dcast(country + target + target_year + horizon ~ quantile, value.var = "prediction")

    realized_vals_infl <- realized_vals |>
      .d(target == trgt)
    point_fcs_infl <- point_fcs |>
      .d(target == trgt)

    dashed_line <- rbind(point_fcs_infl,
                         realized_vals_infl |>
                           copy() |>
                           setnames("true_value", "prediction") |>
                           .d(target_year >= release_year - 1)
    )

    labeldat_80_cy <- linerange_dat |>
      copy() |>
      .d(, (cols) := lapply(.SD, function(val) as.character(round(val, 1))), .SDcols = cols) |>
      .d(, (cols) := lapply(.SD, function(val) ifelse(grepl("[.]", val), val, paste0(val, ".0"))), .SDcols = cols) |>
      .d(, .(country, target, target_year, quantile0.1, quantile0.9)) |>
      dcast(country + target ~ target_year, value.var = c("quantile0.1", "quantile0.9")) |>
      .d(, x :=  release_year - 5.0) |>
      .d(, y := ylimmax - 3.25) |>
      .d(, label := paste0("80% Forecast Intervals:", "\n", release_year, ": ", get(paste0("quantile0.1_", release_year)), "% to ", get(paste0("quantile0.9_", release_year)) , "%", "\n",
                           release_year+1, ": ", get(paste0("quantile0.1_", release_year+1)), "% to ", get(paste0("quantile0.9_", release_year+1)) , "%"))
    ###########################################################################

    plotlist <- lapply(as.list(unique(qufcs$country)),
                       function(pltc) shinyplot(realized_vals_infl, linerange_dat, point_fcs_infl, dashed_line, list(labeldat_80_cy), pltc, colors, cis, ylimmax = ylimmax))

    #print(realized_vals_infl)
    #shinyplot(realized_vals_infl, linerange_dat, point_fcs_infl, dashed_line, list(labeldat_80_cy), "USA", colors, cis)

    text2 <- get_legend(plotlist[[1]])

    plotlist[[2]] + plotlist[[1]] + plotlist[[3]] + plotlist[[4]] + plotlist[[5]] + plotlist[[6]] + plotlist[[7]]+ text2 +
      plot_layout(guides = "collect",
                  design = patchwork_layout2) &
      theme(legend.position='none')
  })


  output$allcplot_gdp<- renderPlot(

    gdp()

  )
}

shinyApp(ui = ui, server = server)
