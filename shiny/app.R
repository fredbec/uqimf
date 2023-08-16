library(shiny)
library(ggplot2)
library(here)
library(data.table)
library(patchwork)
library(MetBrewer)

devtools::load_all()

.d <- `[`

# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel("IMF Empirical Quantile Forecasts"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    position = "left",

    # Sidebar panel for inputs ----
    sidebarPanel(


      # Input: Slider for the number of bins ----
      radioButtons(inputId = "target",
                   choices = setNames(c("ngdp_rpch", "pcpi_pch"),
                                      c("GDP Growth", "Inflation")),
                   selected = "pcpi_pch",
                   label = "Forecast Target"),

      radioButtons(inputId = "error_method",
                   choices = c("directional", "absolute"),
                   selected = "absolute",
                   label = "Error Method"),

      radioButtons(inputId = "method",
                   label = "Estimation Method",
                   choices = c("leave-one-out", "rolling window", "expanding window"),
                   selected = "rolling window"),

      numericInput(inputId = "window",
                   label = "Window length (for rolling window)",
                   value = 5,
                   min = 4,
                   max = 10),

      numericInput(inputId = "target_year",
                   label = "Target Year",
                   value = 2000,
                   min = 1990,
                   max = 2009),

      checkboxGroupInput("ci_lvls", label = "Prediction Intervals",
                         choices = list("50%" = 0.5, "80%" = 0.8, "90%" = 0.9),
                         selected = c(0.5,0.8)),

      selectInput(inputId = "country",
                  label = "Country to highlight",
                  choices = c("Canada", "France", "Germany", "Italy",
                              "Japan", "United Kingdom", "United States")),

      radioButtons(inputId = "zoomin",
                   label = "Zoom in?",
                   choices = c("TRUE", "FALSE"),
                   selected = "TRUE"),


      radioButtons(inputId = "colorscale",
                   choices = setNames(c("OKeeffe1", "Hokusai1", "VanGogh1", "Monet"),
                                      c("Georgia O'Keeffe", "Katsushika Hokusai", "Vincent van Gogh", "Claude Monet")),
                   selected = "Hokusai1",
                   label = "Your favorite artist?"),

    ),

    # Main panel for displaying outputs ----
    mainPanel(
      h2("Visualisation of quantile forecasts"),

      plotOutput(outputId = "allcplot"),

      plotOutput(outputId = "series_visual"),

      #textOutput(outputId = "hey"),
      h2("Scores of quantile forecasts")
    )
  )
)


server <- function(input, output) {
  #########PARAMS TO MOVE TO INPUT##################
  tv_release <- 0.5

  qu_lvls <- reactive({

    qus <- ci_to_quantiles(as.numeric(input$ci_lvls), "directional") #always directional

    n_qus <- length(qus)

    no_pairs <- floor(n_qus/2)


    alpha_vals <- seq(0.5, 0.75, length.out = no_pairs)

    #inner and outer quantile levels
    lapply(seq_along(1:no_pairs),
           function(ind) c(qus[ind], qus[(n_qus)-(ind-1)], alpha_vals[ind]))


  })



  # some non-reactive stuff
  plot_horizon_label <- c(`0.5` = "Spring forecast, same year",
                          `0` = "Fall forecast, same year")
  plot_country_label <- c("Germany" = "DE",
                          "France" = "FR",
                          "Japan" = "JP",
                          "Canada" = "CA",
                          "United States" = "USA",
                          "United Kingdom" = "UK",
                          "Italy" = "IT")



  #create subset of weodat
  #countries, target, horizon, calculate error
  sub_weodat <- reactive({

    data.table::fread(here("WEOforecasts_tidy.csv")) |>
      .d(g7 == 1,) |>
      .d(target == input$target) |>
      .d(horizon <=0.5) |> ######################this shall be extended sometime##############
      .d(, error := prediction - get(paste0("tv_", tv_release))) |>
      .d(target_year < 2010) |>
      .d(, .(country, prediction, target, error,
             horizon, target_year, forecast_year, tv_0.5))
  })

  #years that enter calculation
  yearset <- reactive({
    years <- year_set(input$target_year,
                      unique(sub_weodat()$target_year),
                      input$method,
                      window_length = input$window)

    #check if there is a split
    splitind <- which(diff(years) > 1)

    if(length(splitind) > 0){

      years <- list(years[1:(splitind)],
                    years[(splitind+1):length(years)])

    } else {

      years <- list(years)
    }

    return(years)
  })


  min_viable_year <- reactive({

    if(input$method %in% c("leave-one-out")){
      return(min(sub_weodat()$target_year))
    } else {

      return(min(sub_weodat()$target_year) + input$window)
    }
  })


  #create forecasts
  weodat_qu <- reactive({
    sub_weodat() |>
    empFC(target_years = min_viable_year():max(sub_weodat()$target_year),
          tv_release = tv_release,
          error_method = input$error_method,
          method = input$method,
          window_length = input$window,
          ci_levels = as.numeric(input$ci_lvls))
  })



  #make wide quantile data for displaying CI's in plot
  linerange_dat <- reactive({


    pldat <- weodat_qu() |>
      .d(,.(country, target, horizon, quantile,prediction, target_year)) |>
      .d(, quantile := paste0("quant", quantile)) |>
      data.table::dcast(country + target + horizon + target_year ~ quantile,
                        value.var = "prediction")

    return(pldat)
  })

  ##############################################################################
  #plot showing the errors series
  ##############################################################################
  errorplot <- reactive({

    ggplot() +
      geom_line(
        aes(x = target_year, y = error,
            group = country, color = country),
        data = sub_weodat() |>
          .d(country == input$country)) +
      ggtitle("Forecast Errors") +
      ylab("Error") +
      ylim(max(abs(sub_weodat()$error)), - max(abs(sub_weodat()$error))) +

      geom_hline(yintercept = 0, linetype = "dashed") +

      #Highlight years depending on method and window
      lapply(yearset(), function(yrs) {
        list(annotate("rect", xmin = min(yrs)-0.5,
                      xmax = max(yrs)+0.5, ymin = -Inf, ymax = Inf,
                      alpha = .2))
        }
      ) +


      scale_color_met_d(input$colorscale) +

      facet_wrap(~horizon,
                 labeller = as_labeller(plot_horizon_label)) +

      xlab("Target Year") +

      theme_uqimf()
  })


  ##############################################################################
  #plot showing the realized series
  ##############################################################################
  seriesplot <- reactive({

    fctodis <- weodat_qu() |>
      .d(target_year == input$target_year) |>
      .d(country == input$country)

    if(input$zoomin){
      year_range <- (input$target_year-5):input$target_year
    } else {
      year_range <- unique(sub_weodat()$target_year)
    }

    ggplot() +
      geom_line(
        aes(x = target_year, y = get(paste0("tv_", tv_release)),
            group = country, color = country),
        data = sub_weodat() |>
          .d(country == input$country) |>
          .d(target_year %in% year_range)) +
      ggtitle(paste0("Actual Series, with forecast for year ", input$target_year)) +
      ylab("True value") +

      lapply(qu_lvls(), function(qupr){
        geom_linerange(
          aes(x = target_year,
              ymin = get(paste0("quant", qupr[1])),
              ymax = get(paste0("quant", qupr[2])),
              color = country),
          data = linerange_dat() |>
            .d(country == input$country) |>
            .d(target_year == input$target_year),
          lwd = 1.5, alpha = qupr[3], show.legend = TRUE)
      }) +

      geom_point(
        aes(x = target_year, y = imf_pp, color = country),
        data = fctodis
      ) +

      scale_color_met_d(input$colorscale) +

      facet_wrap(~horizon,
                 labeller = as_labeller(plot_horizon_label)) +

      xlab("Target Year") +

      theme_uqimf()
  })


  ##############################################################################
  #Single Country Zoom-in plot
  ##############################################################################
  overallplot <- reactive({
    (errorplot()) /
      (seriesplot()) +
      plot_layout(guides = "collect",
                  heights = c(1, 1)) &
      plot_annotation(tag_levels = 'I')  &
      theme(legend.position = 'bottom',
            legend.box="vertical", legend.margin=margin())

  })

  ##############################################################################
  #plot showing all series
  ##############################################################################

  allcplot <- reactive({

    ggplot() +
      geom_line(
        aes(x = target_year, y = get(paste0("tv_", tv_release))),
        data = sub_weodat(),
        color = "gray") +
      ylab("Value") +

      geom_point(
        aes(x = target_year, y = imf_pp, color = country),
        data = weodat_qu(),
        shape = 4
      ) +

      #geom_line(
      #  aes(x = target_year, y = imf_pp, color = country),
      #  data = weodat_qu(),
      #  linetype = "dashed"

      #) +
      lapply(qu_lvls(), function(qupr){
        geom_linerange(
          aes(x = target_year,
              ymin = get(paste0("quant", qupr[1])),
              ymax = get(paste0("quant", qupr[2])),
              color = country),
          data = linerange_dat() ,
          lwd = 1.5, alpha = qupr[3], show.legend = TRUE)
      }) +

      scale_color_met_d(input$colorscale) +

      facet_grid(horizon ~ country,
                 labeller = as_labeller(c(plot_horizon_label,
                                          plot_country_label))) +

      xlab("Target Year") +

      theme_uqimf()

  })


  output$allcplot <- renderPlot(

    allcplot()
  )

  output$series_visual <- renderPlot(

    overallplot()
 )




}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
