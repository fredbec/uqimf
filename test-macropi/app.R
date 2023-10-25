library(data.table)
library(ggplot2)
library(MetBrewer)
library(patchwork)

#source(here("test-macropi", "shiny-plot-deploy-function.R"))

.d <- `[`



# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel("IMF Empirical Quantile Forecasts"),

  # Sidebar layout with input and output definitions ----

  fluidRow(
    column(12,

           h2("Visualisation of quantile forecasts"),

           # Output: Tabset w/ plot, summary, and table ----
           tabsetPanel(type = "tabs",
                       tabPanel("Spring 2022 Forecasts - Inflation", plotOutput(outputId = "allcplot_inflation",
                                                   height = 800, width = 800)),
                       tabPanel("Spring 2022 Forecasts - GDP Growth", plotOutput(outputId = "allcplot_gdp",
                                                      height = 800, width = 800))
           )
    )
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

    ptl <- c(`pcpi_pch` = "Inflation",
             `ngdp_rpch` = "Real GDP Growth")

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
                        labels_currentyear,
                        labels_nextyear,
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
        aes(x = target_year, y = true_value),
        color = colorscale[plot_country],
        data = realized_series |> .d(country == plot_country),
        lwd = 0.75) +
      geom_point(
        aes(x = target_year, y = true_value),
        color = colorscale[plot_country],
        data = realized_series |> .d(country == plot_country),
        size = 0.95) +
      ggtitle(paste0("Actual Series, with forecast for year ", 2023)) +
      ylab(plot_target_label()[trgt]) +
      ylim(minval, ylimmax) +
      xlab("Target Year") +
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
          lwd = 2.15)

      }) +

      geom_point(
        aes(x = target_year, y = prediction),
        color = colorscale[plot_country],,
        data = point_forecasts |> .d(country == plot_country),
        size = 2.5
      ) +

      geom_line(
        aes(x = target_year, y = prediction),
        color = colorscale[plot_country],
        data = future_realized |> .d(country == plot_country),
        linetype = "dashed"
      ) +
      geom_label(data=labels_currentyear |> .d(country == plot_country), aes(x=x, y=y, label=label),
                 color = colorscale[plot_country],
                 size=3.75 , angle=45, fontface="bold") +
      geom_label(data=labels_nextyear |> .d(country == plot_country), aes(x=x, y=y, label=label),
                 color = colorscale[plot_country],
                 size=3.75 , angle=45, fontface="bold") +
      theme_uqimf() %+replace%
      theme(
        plot.title = element_text(hjust = 0.5,
                                  vjust = 3)) +
      scale_alpha_manual(name = "",
                         breaks = c("50% Interval", "80% Interval"),
                         values = c("50% Interval" = 0.6, "80% Interval" = 0.4),
                         guide = guide_legend(override.aes = list(color = "black") ))

  }

  #######################################Global Settings####################################

  design <- "122
               345
               678"


  #######################################Inflation#####################################
  infl <- reactive({
    cis <- c(0.5, 0.8)
    qus <- c(0.1, 0.25, 0.75, 0.9)
    ylimmax <- 9.5


    qufcs <- read.csv("https://raw.githubusercontent.com/MacroPrediction/test-macropi/main/forecasts/forecasts_Spring2022.csv")

    linerange_dat <- qufcs |>
      setDT() |>
      .d(target == "inflation") |>
      .d(,quantile := paste0("quantile", quantile)) |>
      .d(,season_helper := ifelse(forecast_season == "S", 0.5, 0)) |>
      .d(,horizon := (target_year - forecast_year) + season_helper) |>
      .d(, .(country, target, target_year, horizon, quantile, prediction)) |>
      dcast(country + target + target_year + horizon ~ quantile, value.var = "prediction")

    realized_vals <- read.csv("https://raw.githubusercontent.com/MacroPrediction/test-macropi/main/extra-data/historicvalues_Spring2022.csv")|>
      setDT() |>
      .d(target == "inflation")
    point_fcs <- read.csv("https://raw.githubusercontent.com/MacroPrediction/test-macropi/main/extra-data/pointforecasts_Spring2022.csv") |>
      setDT() |>
      .d(target == "inflation")

    dashed_line <- rbind(point_fcs, realized_vals |> copy() |> setnames("true_value", "prediction") |> .d(target_year > 2020))


    ###########################################################################
    cols <- paste0("quantile", qus)

    labeldat_2022 <- linerange_dat |>
      .d(target_year == 2022) |>
      .d(, x := 2016.25) |>
      .d(, y := ylimmax - 1.3) |>
      .d(, (cols) := lapply(.SD, function(val) as.character(round(val, 1))), .SDcols = cols) |>
      .d(, (cols) := lapply(.SD, function(val) ifelse(grepl("[.]", val), val, paste0(val, ".0"))), .SDcols = cols) |>
      .d(, label := paste0("2022\n", "50% PI: ", quantile0.25, " - ", quantile0.75, "\n",
                           "80% PI: ", quantile0.1, " - ", quantile0.9))

    labeldat_2023 <- linerange_dat |>
      .d(target_year == 2023) |>
      .d(, x := 2016.75) |>
      .d(, y := 5.0) |>
      .d(, (cols) := lapply(.SD, function(val) as.character(round(val, 1))), .SDcols = cols) |>
      .d(, (cols) := lapply(.SD, function(val) ifelse(grepl("[.]", val), val, paste0(val, ".0"))), .SDcols = cols) |>
      .d(, label := paste0("2023\n", "50% PI: ", quantile0.25, " - ", quantile0.75, "\n",
                           "80% PI: ", quantile0.1, " - ", quantile0.9))
    ###########################################################################
    colors <- met.brewer("Hokusai1", 7)
    names(colors) <- unique(qufcs$country)

    plotlist <- lapply(as.list(unique(qufcs$country)),
                       function(pltc) shinyplot(realized_vals, linerange_dat, point_fcs, dashed_line, labeldat_2022, labeldat_2023, pltc, colors, cis) + ylab("inflation"))

    text2 <- wrap_elements(grid::textGrob("Here we'll place some explanatory text and possibly also the legend"))

    plotlist[[1]] + text2 + plotlist[[2]] + plotlist[[3]] + plotlist[[4]] + plotlist[[5]] + plotlist[[6]] + plotlist[[7]]+
      plot_layout(guides = "collect",
                  design = design) &
      theme(legend.position='bottom')
  })

  output$allcplot_inflation <- renderPlot(

    infl()

  )

  #  #######################################GDP#####################################
  gdp <- reactive({
    cis <- c(0.5, 0.8)
    qus <- c(0.1, 0.25, 0.75, 0.9)
    ylimmax <- 14.5


    qufcs <- read.csv("https://raw.githubusercontent.com/MacroPrediction/test-macropi/main/forecasts/forecasts_Spring2022.csv")

    linerange_dat <- qufcs |>
      setDT() |>
      .d(target == "gdp_growth") |>
      .d(,quantile := paste0("quantile", quantile)) |>
      .d(,season_helper := ifelse(forecast_season == "S", 0.5, 0)) |>
      .d(,horizon := (target_year - forecast_year) + season_helper) |>
      .d(, .(country, target, target_year, horizon, quantile, prediction)) |>
      dcast(country + target + target_year + horizon ~ quantile, value.var = "prediction")

    realized_vals <- read.csv("https://raw.githubusercontent.com/MacroPrediction/test-macropi/main/extra-data/historicvalues_Spring2022.csv")|>
      setDT() |>
      .d(target == "gdp_growth")
    point_fcs <- read.csv("https://raw.githubusercontent.com/MacroPrediction/test-macropi/main/extra-data/pointforecasts_Spring2022.csv") |>
      setDT() |>
      .d(target == "gdp_growth")

    dashed_line <- rbind(point_fcs, realized_vals |> copy() |> setnames("true_value", "prediction") |> .d(target_year > 2020))


    ###########################################################################
    cols <- paste0("quantile", qus)

    labeldat_2022_gdp <- linerange_dat |>
      .d(target_year == 2022) |>
      .d(, x := 2016.25) |>
      .d(, y := ylimmax - 1.85) |>
      .d(, (cols) := lapply(.SD, function(val) as.character(round(val, 1))), .SDcols = cols) |>
      .d(, (cols) := lapply(.SD, function(val) ifelse(grepl("[.]", val), val, paste0(val, ".0"))), .SDcols = cols) |>
      .d(, label := paste0("2022\n", "50% PI: ", quantile0.25, " - ", quantile0.75, "\n",
                           "80% PI: ", quantile0.1, " - ", quantile0.9))

    labeldat_2023_gdp <- linerange_dat |>
      .d(target_year == 2023) |>
      .d(, x := 2016.75) |>
      .d(, y := 6.0) |>
      .d(, (cols) := lapply(.SD, function(val) as.character(round(val, 1))), .SDcols = cols) |>
      .d(, (cols) := lapply(.SD, function(val) ifelse(grepl("[.]", val), val, paste0(val, ".0"))), .SDcols = cols) |>
      .d(, label := paste0("2023\n", "50% PI: ", quantile0.25, " - ", quantile0.75, "\n",
                           "80% PI: ", quantile0.1, " - ", quantile0.9))
    ###########################################################################
    colors <- met.brewer("Hokusai1", 7)
    names(colors) <- unique(qufcs$country)

    plotlist <- lapply(as.list(unique(qufcs$country)),
                       function(pltc) shinyplot(realized_vals, linerange_dat, point_fcs, dashed_line, labeldat_2022_gdp, labeldat_2023_gdp, pltc, colors, cis, ylimmax = ylimmax) + ylab("gdp_growth"))


    text2 <- wrap_elements(grid::textGrob("Here we'll place some explanatory text and possibly also the legend"))

    plotlist[[1]] + text2 + plotlist[[2]] + plotlist[[3]] + plotlist[[4]] + plotlist[[5]] + plotlist[[6]] + plotlist[[7]]+
      plot_layout(guides = "collect",
                  design = design) &
      theme(legend.position='bottom')
  })

  output$allcplot_gdp<- renderPlot(

    gdp()

  )
}

shinyApp(ui = ui, server = server)
