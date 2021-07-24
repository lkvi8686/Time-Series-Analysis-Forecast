# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#

library(shiny)
library(shinydashboard)
library(scales)
library(tseries)
library(tidyverse)
library(plotly)
library(lubridate)
library(forecast)
library(zoo)
library(DT)
# library(feasts)
library(fpp3)
library(naniar)
library(imputeTS)

# Set current path as working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

tsdf <- read_csv("TS_RawData.csv")
tsdf$Date <- mdy(tsdf$Date)



na_modls_df <- data.frame(fnctn = c('na_interpolation','na_interpolation','na_interpolation',
                                    'na_kalman','na_kalman','na_locf','na_locf','na_ma','na_ma','na_ma','na_mean','na_mean','na_mean'),
                          optn = c('linear','spline','stine','StructTS','auto.arima','locf','nocb','simple','linear','exponential','mean',
                                   'median','mode'))

# Define UI for application that draws a histogram
ui <- fluidPage(
    dashboardPage(
        dashboardHeader(title = "TS Dashboard"),
        dashboardSidebar(
            sidebarMenu(
                menuItem("RawData", tabName = "raw", icon = icon('table')),
                menuItem("TS Impute", tabName = "tsimpute", icon = icon('table')),
                menuItem("EDA", tabName = "eda", icon = icon('dashboard')),
                menuItem("Correlation", tabName = "correl", icon = icon('table')),
                menuItem("Decomp Analysis", tabName = "decomp", icon = icon('chart-line')),
                menuItem("Basic Models", tabName = "basic", icon = icon('chart-line')),
                menuItem("ARIMA", tabName = "arima", icon = icon('chart-line')),
                #menuItem("Regression for TS", tabName = "regression", icon = icon('table')),
                menuItem("Readme", tabName = "readme", icon = icon('info')),
                menuItem(selectInput('ySel', 'Analyse', colnames(Filter(is.numeric, tsdf)))),
                menuItem(dateRangeInput('dateRange', label = "Model Input Date Range", format = "M-yyyy",
                                        start = min(tsdf$Date),
                                        end = max(tsdf$Date),
                                        startview = "year"))
                #tags$img(src = "OSG_Logo.jpg")
            )
        ),
        # Dashboard Pages defined
        dashboardBody(
            tabItems(
                tabItem(tabName = "raw", h1('Raw Data'),
                        fluidRow(verbatimTextOutput("value")),
                        fluidRow(verbatimTextOutput('smry')),
                        fluidRow(verbatimTextOutput('info'))
                        ),
                tabItem(tabName = 'tsimpute',
                        fluidRow(column(6,(plotOutput('ggmisvar'))),
                                 column(6,plotOutput('vismiss'))),
                        #fluidRow(verbatimTextOutput('nastats')),
                        fluidRow(column(4,selectInput(inputId = 'na_model_sel', 
                                                      label = 'Select Imputation Model', 
                                             choices = list('Interpolation'= 'na_interpolation',
                                                            'Kalman Smoothing' = 'na_kalman', 
                                                            'Carry Forward or Backward' = 'na_locf', 
                                                            'Moving Average' = 'na_ma',
                                                            'Mean Imputation' = 'na_mean'),
                                             selected = 'na_kalman')),
                                 column(2,uiOutput('na_sel_optn')),
                                 conditionalPanel(condition = "input.na_model_sel == 'na_ma'",
                                 column(2, numericInput(inputId = 'kval', label = 'k (no of pd)', value = 2))),
                                 column(2,br(), downloadButton('download_TSimp'))),
                        fluidRow(
                            column(6, plotOutput('na_dist_plot')),
                            column(6,
                            conditionalPanel(
                                condition = "input.na_model_sel == 'na_kalman' && input.na_model_optn_sel == 'StructTS'",
                                plotOutput('kalman_struct_plot')),
                            conditionalPanel(
                                condition = "input.na_model_sel == 'na_kalman' && input.na_model_optn_sel == 'auto.arima'",
                                plotOutput('kalman_autoarima_plot')
                            ),
                            conditionalPanel(
                                condition = "input.na_model_sel == 'na_interpolation' && input.na_model_optn_sel == 'linear'",
                                plotOutput('lin_interpol')
                            ),
                            conditionalPanel(
                                condition = "input.na_model_sel == 'na_interpolation' && input.na_model_optn_sel == 'spline'",
                                plotOutput('spline_interpol')
                            ),
                            conditionalPanel(
                                condition = "input.na_model_sel == 'na_interpolation' && input.na_model_optn_sel == 'stine'",
                                plotOutput('stine_interpol')
                            ),
                            conditionalPanel(
                                condition = "input.na_model_sel == 'na_locf' && input.na_model_optn_sel == 'locf'",
                                plotOutput('locf_plot')
                            ),
                            conditionalPanel(
                                condition = "input.na_model_sel == 'na_locf' && input.na_model_optn_sel == 'nocb'",
                                plotOutput('nocb_plot')
                            ),
                            conditionalPanel(
                                condition = "input.na_model_sel == 'na_mean' && input.na_model_optn_sel == 'mean'",
                                plotOutput('mean_plot')
                            ),
                            conditionalPanel(
                                condition = "input.na_model_sel == 'na_mean' && input.na_model_optn_sel == 'median'",
                                plotOutput('median_plot')
                            ),
                            conditionalPanel(
                                condition = "input.na_model_sel == 'na_mean' && input.na_model_optn_sel == 'mode'",
                                plotOutput('mode_plot')
                            ),
                            conditionalPanel(
                                condition = "input.na_model_sel == 'na_ma' && input.na_model_optn_sel == 'simple'",
                                plotOutput('sma')
                            ),
                            conditionalPanel(
                                condition = "input.na_model_sel == 'na_ma' && input.na_model_optn_sel == 'linear'",
                                plotOutput('lwma')
                            ),
                            conditionalPanel(
                                condition = "input.na_model_sel == 'na_ma' && input.na_model_optn_sel == 'exponential'",
                                plotOutput('ewma')
                            )
                            
                        ))
                        ),
                tabItem(tabName = "eda",
                        fluidRow(plotlyOutput('Report1')),
                        fluidRow(plotlyOutput('Report2')),
                        fluidRow(uiOutput('eda_comp')),
                        fluidRow(plotlyOutput('compChart'))
                            #box(plotlyOutput('Report3')))
                        ),
                tabItem(tabName = 'correl', 
                        p('This page may take a min to load'),
                        #verbatimTextOutput('seasonal_r1')),
                        column(10,plotOutput('relPlot')),
                        column(2,uiOutput('checkbox1')),
                        fluidRow(plotOutput('corplot'))),
                tabItem(tabName = 'decomp',
                        # fluidRow(column(4, selectInput('d_type', 'Decomp Type', 
                        #                                choices = list('additive','multiplicative'),
                        #                                selected = 'additive'))),
                        fluidRow(box(plotlyOutput('decomp_r0')),
                                 box(plotlyOutput('decomp_r3'))),
                        fluidRow(box(plotlyOutput('decomp_r2')),
                                 box(plotlyOutput('decomp_r4')))),
                tabItem(tabName = 'basic',
                        tabsetPanel(
                            tabPanel("Simple Exponential Smoothing", 
                                     fluidRow(column(8,(plotlyOutput("ses"))),
                                              column(2,
                                                     numericInput('alpha',label = 'Alpha (0.1-0.8)', value = 0.2,step =0.1, min = 0.1, max = 1.0),
                                                     numericInput('ses_f_pd',label = 'Forecast Periods', value = 1),
                                                     downloadButton('downloadses','Download')
                                                     )),
                                     fluidRow(column(8, verbatimTextOutput('SES_Accuracy'))),
                                     fluidRow(
                                         column(8,h3('Simple Exponential Smoothing Explained:'),
                                         p('One of the drawbacks of simple moving average technique is that it gives equal weight to all the previous  observations used in forecasting the future value. This can be overcome by assigning differential weights  to the past observations. 
                                         One easier way to assign differential weight is achieved by using  simple exponential smoothing (SES) technique. Just like  the moving average, SES assumes a fairly steady time-series data with no significant trend, seasonal or  cyclical component.',
                                           strong('Here, the weights assigned to past data decline exponentially with the most recent observations assigned higher weights.'
                                                  ))))),
                            tabPanel("Holt Method",
                                     fluidRow(column(8, plotlyOutput("holt")),
                                              column(2,
                                                     numericInput('holt_alpha', label = 'Alpha (0.1-0.8)', value = 0.2,step =0.1, min = 0.1, max = 1.0),
                                                     numericInput('holt_beta', label = 'Beta (0.1-0.8)', value = 0.2,step =0.1, min = 0.1, max = 1.0),
                                                     numericInput('holt_h', label = 'Forecast Periods', value = 10),
                                                     downloadButton('downloadholt')
                                                     )),
                                     fluidRow(column(8,verbatimTextOutput('holt_accuracy'))),
                                     fluidRow(column(8,h3("Double Exponential Smoothing - Holt's Method"),
                                              p("One of the drawbacks of single exponential smoothing is that the model does not do well in the presence of trend.
                                                This can be improved by introducing an additional equation for capturing the trend in the  time-series data.
                                                Double exponential smoothing uses two equations to forecast the future values of the  time series, 
                                                one for forecasting the level (short term average value) and another for capturing the trend."),
                                              p(strong("alpha and beta"), "are the smoothing constants for",strong("level and trend"))))
                                     ),
                            tabPanel("Holt Winters",
                                     fluidRow(column(8, plotlyOutput("holtwin")),
                                              column(2,
                                                     selectInput('hw_seasonal', "Model Type", choices = list("additive", "multiplicative"), selected ="additive" ),
                                                     numericInput('hw_alpha', label = 'Alpha (0.1-0.8)', value = 0.1,step =0.1, min = 0.1, max = 1.0),
                                                     numericInput('hw_beta', label = 'Beta (0.1-0.8)', value = 0.1,step =0.1, min = 0.1, max = 1.0),
                                                     numericInput('hw_gamma', label = 'Gamma (0.1-0.8)', value = 0.1,step =0.1, min = 0.1, max = 1.0),
                                                     numericInput('hw_h', label = 'Forecast Periods', value = 12),
                                                     downloadButton('downloadHW')
                                              )),
                                     fluidRow(column(8,verbatimTextOutput('holtwin_accuracy'))),
                                     fluidRow(
                                         h3("Triple Exponential Smoothing (Holt-Winter Model)"),
                                         p("Triple exponential smoothing is used when the data has trend as well as seasonality."),
                                         p("Alpha is the Level or Intercept - smoothing constant"),
                                         p("Beta is the Trend - smoothing constant"),
                                         p("Gamma is the Seasonal - smoothing constant")
                                         )
                                     
                                     )
                            #tabPanel("Naive"),
                            #tabPanel("Moving Average")
                            )),
                tabItem(tabName = 'arima',
                        tabsetPanel(
                            tabPanel("SARIMA",
                                     fluidRow(column(8,plotlyOutput("sarima_plot")),
                                              column(2, 
                                                     numericInput('p', label = 'p (AR)', value = 0,step =1, min = 0, max = 10),
                                                     numericInput('d', label = 'd (I)',  value = 0,step =1, min = 0, max = 10),
                                                     numericInput('q', label = 'q (MA)', value = 0,step =1, min = 0, max = 10),
                                                     numericInput('sarima_h', label = 'Forecast Periods', value = 12),
                                                     downloadButton('downloadsarima',"Downlaod")
                                                     ),
                                              column(2, 
                                                     numericInput('sp', label = 'p (AR) - Seasonal', value = 0,step =1, min = 0, max = 10),
                                                     numericInput('sd', label = 'd (I) - Seasonal',  value = 0,step =1, min = 0, max = 10),
                                                     numericInput('sq', label = 'q (MA) - Seasonal', value = 0,step =1, min = 0, max = 10))),
                                     fluidRow(column(8,verbatimTextOutput('sarima_accuracy'))),
                                     fluidRow(column(6,plotOutput('sarima_acf')),
                                              column(6,plotOutput('sarima_pacf'))),
                                     fluidRow(column(8,
                                                     h2('Seasonality (S)'),
                                                     p('Seasonality makes it so that the mean of the observations is not constant, but instead evolves according to a cyclical pattern we say that the series has seasonality of periods'),
                                                     h2('Autoregressive  (AR) (order p)'),
                                                     p('A type of a stochastic model which depends on its time-lagged forecasts of the series is named autoregressive (AR). Essentially, AR is a regressive model. The generalized form of an AR model is of order p'),
                                                     h2('Integrated (I) (order d)'),
                                                     p("Represents the differencing of raw observations to allow for the time series to become stationary, i.e., data values are replaced by the difference between the data values and the previous values."),
                                                     p("The lag operator shifts the data and the errors either forward (when k < 0) or backward lags"),
                                                     h2("Moving average (MA) (order q) "),
                                                     p("Incorporates the dependency between an observation and a residual error from a moving average model applied to lagged observations"),
                                                     p("Another type of time series model which regresses against the past errors of the series is also available. This model is called Moving Average (MA) and, similarly to AR, it is a type of stochastic process. Its generalized form of order is q"),
                                                     #h2("Exogenous input (X)"),
                                                     #p("Integrates an ordinary regression model that uses external variables into the SARIMA model")
                                                     ))
                                     ),
                            tabPanel("ARIMA",
                                     fluidRow(column(8,plotlyOutput("arima_plot")),
                                              column(2, 
                                                     numericInput('ap', label = 'p (AR)', value = 0,step =1, min = 0, max = 10),
                                                     numericInput('ad', label = 'd (I)',  value = 0,step =1, min = 0, max = 10),
                                                     numericInput('aq', label = 'q (MA)', value = 0,step =1, min = 0, max = 10),
                                                     numericInput('arima_h', label = 'Forecast Periods', value = 12),
                                                     downloadButton('downloadarima',"Downlaod")
                                              )),
                                     fluidRow(column(8,verbatimTextOutput('arima_accuracy'))),
                                     fluidRow(column(6,plotOutput('arima_acf')),
                                              column(6,plotOutput('arima_pacf'))),
                                     fluidRow(column(8,
                                                     h2('Seasonality (S)'),
                                                     p('Seasonality makes it so that the mean of the observations is not constant, but instead evolves according to a cyclical pattern we say that the series has seasonality of periods'),
                                                     h2('Autoregressive  (AR)'),
                                                     p('A type of a stochastic model which depends on its time-lagged forecasts of the series is named autoregressive (AR). Essentially, AR is a regressive model. The generalized form of an AR model is of order p'),
                                                     h2('Integrated (I)'),
                                                     p("Represents the differencing of raw observations to allow for the time series to become stationary, i.e., data values are replaced by the difference between the data values and the previous values."),
                                                     p("The lag operator shifts the data and the errors either forward (when k < 0) or backward lags"),
                                                     h2("Moving average (MA)"),
                                                     p("Incorporates the dependency between an observation and a residual error from a moving average model applied to lagged observations"),
                                                     p("Another type of time series model which regresses against the past errors of the series is also available. This model is called Moving Average (MA) and, similarly to AR, it is a type of stochastic process. Its generalized form of order is q"),
                                                     ))
                                     ),
                            tabPanel("SARIMAX",
                                     column(8,plotlyOutput('sarimaX_plot')),
                                     column(2, 
                                            numericInput('px', label = 'p (AR)', value = 0,step =1, min = 0, max = 10),
                                            numericInput('dx', label = 'd (I)',  value = 0,step =1, min = 0, max = 10),
                                            numericInput('qx', label = 'q (MA)', value = 0,step =1, min = 0, max = 10),
                                            #numericInput('arima_hX', label = 'Forecast Periods', value = 12),
                                            downloadButton('downloadsarimaX',"Downlaod")),
                                     column(2,
                                            numericInput('spx', label = 'p (AR) - Seasonal', value = 0,step =1, min = 0, max = 10),
                                            numericInput('sdx', label = 'd (I) - Seasonal',  value = 0,step =1, min = 0, max = 10),
                                            numericInput('sqx', label = 'q (MA) - Seasonal', value = 0,step =1, min = 0, max = 10),
                                            uiOutput('checkbox3')),
                                     fluidRow(column(8, verbatimTextOutput('sarimaX_accuracy'))),
                                     fluidRow(column(6, plotOutput('sarimaX_acf')),
                                              column(6, plotOutput('sarimaX_Pacf'))),
                                     fluidRow(column(8,
                                                     h3('Instructions for TS Models with Exogenous Variables/External Regressors'),
                                                     p('- Input File should have forecasted values for the Exogenous Variables'),
                                                     p('- Select the Time Period as Actual Period - since additional rows with forecast are present in the input file'),
                                                     p('- Select the Exogenous variables to be used in checkbox, ensure you do not check the Y variable')))
                                     ),
                            tabPanel("ARIMAX", 
                                     column(8,plotlyOutput('arimaX_plot')),
                                     column(2, 
                                            numericInput('apx', label = 'p (AR)', value = 0,step =1, min = 0, max = 10),
                                            numericInput('adx', label = 'd (I)',  value = 0,step =1, min = 0, max = 10),
                                            numericInput('aqx', label = 'q (MA)', value = 0,step =1, min = 0, max = 10),
                                            numericInput('arima_hX', label = 'Forecast Periods', value = 12),
                                            downloadButton('downloadarimaX',"Downlaod")),
                                     column(2,uiOutput('checkbox2')),
                                     fluidRow(column(8, verbatimTextOutput('arimaX_accuracy'))),
                                     fluidRow(column(6, plotOutput('arimaX_acf')),
                                               column(6, plotOutput('arimaX_Pacf'))),
                                     fluidRow(column(8,
                                                     h3('Instructions for TS Models with Exogenous Variables/External Regressors'),
                                                     p('- Input File should have forecasted values for the Exogenous Variables'),
                                                     p('- Select the Time Period as Actual Period - since additional rows with forecast are present in the input file'),
                                                     p('- Select the Exogenous variables to be used in checkbox, ensure you do not check the Y variable')))

                                     )
                        )
                        
                        ),
                tabItem(tabName = 'readme',
                        h1('Information Page'),
                        h2('Data Format'),
                        p("- First column must be Date, with Column Name ", strong("'Date'")," & must be ",strong("mm/dd/yyyy")," format"),
                        p("- The rest of the columns represent dependent & independent TS variables"),
                        h2('Glossary'),
                        h3("Mean Absolute Percentage Error (MAPE)"),
                        p('Mean absolute percentage error (MAPE) is the average of absolute percentage error. 
                           Assume that the  validation data has n observations and the forecasting is carried out on these n observations.'),
                        h3("Root Mean Square Error (RMSE)"),
                        p('Root mean square error (RMSE) is the square root of mean square error'),
                        h2("References"),
                        tags$cite("Forecasting: Principles and Practice, Rob J Hyndman and George Athanasopoulos,",
                        a("https://otexts.com/fpp2", href="https://otexts.com/fpp2")), p(),
                        tags$cite("Kumar, U. Dinesh. Business Analytics: The Science of Data-Driven Decision Making. Wiley India, 2017"), p(),
                        tags$cite("Steffen Moritz and Thomas Bartz-Beielstein,imputeTS: Time Series Missing Value Imputation in R")
                        )
                    )
                )
            )
)

#Define Server logic required to draw a histogram
server <- function(input, output) {
    output$info <- renderPrint({
        str(tsdf)
        #input$checkbox2_arimax
        #input$arima_hX
        #str(values$df_data)
        #tsdf()[1]
        #colnames(tsdf)[-1]
        #input$xcol
        #tsdf()[input$tscol]
        #x
        #input$na_model_sel
        #input$na_model_optn_sel

    })

    output$smry <- renderPrint({
        summary(tsdf)
    })
    
    output$value <- renderPrint({tsdf})
    

    f <- list(
        family = "Courier New, monospace",
        size = 16,
        color = "#7f7f7f"
    )
    
    output$Report1 <- renderPlotly({
        plot_ly ( tsdf %>% select(Date, selc = input$ySel) %>% 
                      filter(Date >= input$dateRange[1], Date <= input$dateRange[2]),
                 x = ~Date,
                 y = ~selc,
                 type = 'scatter',
                 mode = 'lines'
        ) %>%
            layout(title = list(text = "Trend",
                                #text = paste0(input$ySel," Trends"),
                                font = f),
                   yaxis = list(title = input$ySel, font = f, hovermode = 'compare'),
                   xaxis = list(title = ''))
    })
    
    output$Report2 <- renderPlotly({
        m <- tsdf %>% select(Date, var = (input$ySel)) %>% 
            filter(Date >= input$dateRange[1], Date <= input$dateRange[2])
        m$pd <- as.character(year(m$Date))
        plot_ly(m, y = ~var, color = ~pd, type = "box") %>%
            layout(title = list(text = "Variation over time",
                                #text = paste0(input$ySel," - Variation over time"),
                                font = f),showlegend = FALSE,
                   yaxis = list(title = input$ySel, font = f,hovermode = 'compare'))
    })
    
    output$decomp_r1 <- renderPlotly({
        m <- ts(tsdf %>% select(Date, var = (input$ySel)) %>% 
                    filter(Date >= input$dateRange[1], Date <= input$dateRange[2]),
                start = c(year(input$dateRange[1]), month(input$dateRange[1])),
                end = c(year(input$dateRange[2]), month(input$dateRange[2])),
                frequency = 12
                )
        d <- decompose(m, type = input$d_type)
        sdf <- as.data.frame(d$seasonal)
        colnames(sdf) <- 'Seasonality'
        dt_seq <- as.data.frame(seq(as.Date(input$dateRange[1]), as.Date(input$dateRange[2]), "months"))
        colnames(dt_seq) <- 'Date'
        sdf <- sdf[1:dim(dt_seq)[1],'Seasonality']
        sq1 <- cbind(dt_seq, sdf)
        plot_ly ( sq1,
                  x = ~Date,
                  y = ~sdf,
                  type = 'scatter',
                  mode = 'lines'
        ) %>%
            layout(title = list(text = "Seasonality",
                                #text = paste0(input$ySel," Trends"),
                                font = f),
                   yaxis = list(title = input$ySel, font = f, hovermode = 'compare'),
                   xaxis = list(title = ''))
        
    })
    
    output$decomp_r0 <- renderPlotly({
        plot_ly ( tsdf %>% select(Date, selc = input$ySel) %>% 
                      filter(Date >= input$dateRange[1], Date <= input$dateRange[2]),
                  x = ~Date,
                  y = ~selc,
                  type = 'scatter',
                  mode = 'lines'
        ) %>%
            layout(title = list(text = "Observed",
                                #text = paste0(input$ySel," Trends"),
                                font = f),
                   yaxis = list(title = input$ySel, font = f, hovermode = 'compare'),
                   xaxis = list(title = ''))
    })
    
    output$decomp_r2 <- renderPlotly({
        dffil <- tsdf %>% filter(Date >= input$dateRange[1], Date <= input$dateRange[2])
        m <- ts(dffil[[input$ySel]],
                start = c(year(input$dateRange[1]), month(input$dateRange[1])),
                end = c(year(input$dateRange[2]), month(input$dateRange[2])),
                frequency = 12)
        d <- stl(m, s.window = "per")
        sdf <- as.data.frame(d$time.series)
        dt_seq <- as.data.frame(seq(as.Date(input$dateRange[1]), as.Date(input$dateRange[2]), "months"))
        colnames(dt_seq) <- 'Date'
        sdf1 <- cbind(dt_seq, sdf)
        plot_ly (sdf1,
                  x = ~Date,
                  y = ~seasonal,
                  type = 'scatter',
                  mode = 'lines'
        ) %>%
            layout(title = list(text = "Seasonality",
                                #text = paste0(input$ySel," Trends"),
                                font = f),
                   yaxis = list(title = input$ySel, font = f, hovermode = 'compare'),
                   xaxis = list(title = ''))

    })
    output$decomp_r3 <- renderPlotly({
        dffil <- tsdf %>% filter(Date >= input$dateRange[1], Date <= input$dateRange[2])
        m <- ts(dffil[[input$ySel]],
                start = c(year(input$dateRange[1]), month(input$dateRange[1])),
                end = c(year(input$dateRange[2]), month(input$dateRange[2])),
                frequency = 12
        )
        d <- stl(m, s.window = "per")
        sdf <- as.data.frame(d$time.series)
        dt_seq <- as.data.frame(seq(as.Date(input$dateRange[1]), as.Date(input$dateRange[2]), "months"))
        colnames(dt_seq) <- 'Date'
        sdf1 <- cbind(dt_seq, sdf)
        plot_ly (sdf1,
                 x = ~Date,
                 y = ~trend,
                 type = 'scatter',
                 mode = 'lines'
        ) %>%
            layout(title = list(text = "Trend Component",
                                #text = paste0(input$ySel," Trends"),
                                font = f),
                   yaxis = list(title = input$ySel, font = f, hovermode = 'compare'),
                   xaxis = list(title = ''))
        
    })
    output$decomp_r4 <- renderPlotly({
        dffil <- tsdf %>% filter(Date >= input$dateRange[1], Date <= input$dateRange[2])
        m <- ts(dffil[[input$ySel]],
                start = c(year(input$dateRange[1]), month(input$dateRange[1])),
                end = c(year(input$dateRange[2]), month(input$dateRange[2])),
                frequency = 12
        )
        d <- stl(m, s.window = "per")
        sdf <- as.data.frame(d$time.series)
        dt_seq <- as.data.frame(seq(as.Date(input$dateRange[1]), as.Date(input$dateRange[2]), "months"))
        colnames(dt_seq) <- 'Date'
        sdf1 <- cbind(dt_seq, sdf)
        plot_ly (sdf1,
                 x = ~Date,
                 y = ~remainder,
                 type = 'scatter',
                 mode = 'lines'
        ) %>%
            layout(title = list(text = "Residual",
                                #text = paste0(input$ySel," Trends"),
                                font = f),
                   yaxis = list(title = input$ySel, font = f, hovermode = 'compare'),
                   xaxis = list(title = ''))
        
    })
    output$seasonal_r1 <- renderPrint({
        z <- tsdf %>% select(Date, var = (input$ySel)) %>% 
            mutate(Date = yearmonth(Date)) %>%
            as_tsibble(index = Date) %>%
            fill_gaps()
        #m <- as_tsibble(tsdf)
        z %>% gg_subseries(Sales)
            
    })
    
    output$ses <- renderPlotly({
        dffil <- tsdf %>% filter(Date >= input$dateRange[1], Date <= input$dateRange[2]) %>%
            select(Date, input$ySel)
        m <- ts(dffil[[input$ySel]],
                start = c(year(input$dateRange[1]), month(input$dateRange[1])),
                end = c(year(input$dateRange[2]), month(input$dateRange[2])),
                frequency = 12)
        f_cast_mod <- ses(m, h = input$ses_f_pd, alpha = input$alpha)
        accuracy(f_cast_mod)
        mod_fit <- as.data.frame(fitted(f_cast_mod))
        dffil['Forecast'] <- as.numeric(mod_fit$x)
        f_cast <- as.data.frame(f_cast_mod)
        f_cast$Date <- row.names(f_cast)
        f_cast$Date <- as.Date(as.yearmon(f_cast$Date))
        colnames(f_cast)[colnames(f_cast)=='Point Forecast'] = 'Forecast'
        f_cast <- f_cast[,c('Date','Forecast')]
        f_cast[[input$ySel]] <- NA
        rownames(f_cast) <- NULL
        co_df<- rbind(dffil, f_cast)
        plot_ly(co_df %>% select(Date, selc = input$ySel, Forecast), x = ~Date) %>%
            add_trace(y = ~selc, name = "Observed", type = 'scatter', mode = 'lines') %>%
            add_trace(y = ~Forecast, name = "Forecast", type = 'scatter', mode = 'lines') %>%
            layout(title = list(text = ""), font = f,
                   hovermode = 'compare',xaxis    = list(title = ''))
    })
    
    output$SES_Accuracy <- renderPrint({
        dffil <- tsdf %>% filter(Date >= input$dateRange[1], Date <= input$dateRange[2]) %>%
            select(Date, input$ySel)
        m <- ts(dffil[[input$ySel]],
                start = c(year(input$dateRange[1]), month(input$dateRange[1])),
                end = c(year(input$dateRange[2]), month(input$dateRange[2])),
                frequency = 12)
        f_cast_mod <- ses(m, h = input$ses_f_pd, alpha = input$alpha)
        accuracy(f_cast_mod)
    })
    
    output$holt <- renderPlotly({
        dffil <- tsdf %>% filter(Date >= input$dateRange[1], Date <= input$dateRange[2]) %>%
            select(Date, input$ySel)
        m <- ts(dffil[[input$ySel]],
                start = c(year(input$dateRange[1]), month(input$dateRange[1])),
                end = c(year(input$dateRange[2]), month(input$dateRange[2])),
                frequency = 12)
        f_cast_mod <- holt(m, h = input$holt_h, alpha = input$holt_alpha, beta = input$holt_beta)
        accuracy(f_cast_mod)
        mod_fit <- as.data.frame(fitted(f_cast_mod))
        dffil['Forecast'] <- as.numeric(mod_fit$x)
        f_cast <- as.data.frame(f_cast_mod)
        f_cast$Date <- row.names(f_cast)
        f_cast$Date <- as.Date(as.yearmon(f_cast$Date))
        colnames(f_cast)[colnames(f_cast)=='Point Forecast'] = 'Forecast'
        f_cast <- f_cast[,c('Date','Forecast')]
        f_cast[[input$ySel]] <- NA
        rownames(f_cast) <- NULL
        co_df<- rbind(dffil, f_cast)
        plot_ly(co_df %>% select(Date, selc = input$ySel, Forecast), x = ~Date) %>%
            add_trace(y = ~selc, name = "Observed", type = 'scatter', mode = 'lines') %>%
            add_trace(y = ~Forecast, name = "Forecast", type = 'scatter', mode = 'lines') %>%
            layout(title = list(text = ""), font = f,
                   hovermode = 'compare',xaxis    = list(title = ''))
    })
    
    output$holt_accuracy <- renderPrint({
        dffil <- tsdf %>% filter(Date >= input$dateRange[1], Date <= input$dateRange[2]) %>%
            select(Date, input$ySel)
        m <- ts(dffil[[input$ySel]],
                start = c(year(input$dateRange[1]), month(input$dateRange[1])),
                end = c(year(input$dateRange[2]), month(input$dateRange[2])),
                frequency = 12)
        f_cast_mod <- holt(m, h = input$holt_h, alpha = input$holt_alpha, beta = input$holt_beta)
        accuracy(f_cast_mod)
           })
    
    output$val1 <- renderPrint({
        tsdf %>% select(Date, var = (input$ySel)) %>% as_tsibble(index = Date) %>%
            fill_gaps(.full = T)
    })
    
    output$holtwin <- renderPlotly({
        dffil <- tsdf %>% filter(Date >= input$dateRange[1], Date <= input$dateRange[2]) %>%
            select(Date, input$ySel)
        m <- ts(dffil[[input$ySel]],
                start = c(year(input$dateRange[1]), month(input$dateRange[1])),
                end = c(year(input$dateRange[2]), month(input$dateRange[2])),
                frequency = 12)
        f_cast_mod <- hw(m, h = input$hw_h, seasonal = input$hw_seasonal, 
                         alpha = input$hw_alpha,
                         beta = input$hw_beta,
                         gamma = input$hw_gamma)
        accuracy(f_cast_mod)
        mod_fit <- as.data.frame(fitted(f_cast_mod))
        dffil['Forecast'] <- as.numeric(mod_fit$x)
        f_cast <- as.data.frame(f_cast_mod)
        f_cast$Date <- row.names(f_cast)
        f_cast$Date <- as.Date(as.yearmon(f_cast$Date))
        colnames(f_cast)[colnames(f_cast)=='Point Forecast'] = 'Forecast'
        f_cast <- f_cast[,c('Date','Forecast')]
        f_cast[[input$ySel]] <- NA
        rownames(f_cast) <- NULL
        co_df<- rbind(dffil, f_cast)
        plot_ly(co_df %>% select(Date, selc = input$ySel, Forecast), x = ~Date) %>%
            add_trace(y = ~selc, name = "Observed", type = 'scatter', mode = 'lines') %>%
            add_trace(y = ~Forecast, name = "Forecast", type = 'scatter', mode = 'lines') %>%
            layout(title = list(text = ""), font = f,
                   hovermode = 'compare',xaxis    = list(title = ''))
    })
    
    output$holtwin_accuracy <- renderPrint({
        dffil <- tsdf %>% filter(Date >= input$dateRange[1], Date <= input$dateRange[2]) %>%
            select(Date, input$ySel)
        m <- ts(dffil[[input$ySel]],
                start = c(year(input$dateRange[1]), month(input$dateRange[1])),
                end = c(year(input$dateRange[2]), month(input$dateRange[2])),
                frequency = 12)
        f_cast_mod <- hw(m, h = input$hw_h, seasonal = input$hw_seasonal, 
                         alpha = input$hw_alpha,
                         beta = input$hw_beta,
                         gamma = input$hw_gamma)
        accuracy(f_cast_mod)
    })
    
    output$relPlot <- renderPlot({
        tsdf %>% as_tibble() %>% select(input$corr_list) %>% GGally::ggpairs()
    })
    
    output$checkbox1 <- renderUI({
        checkboxGroupInput('corr_list',"Choose Variables", choices = colnames(tsdf)[-1], selected = colnames(tsdf)[-1] )
    })
    

    
    output$eda_comp <- renderUI({
        selectInput('sel_comp',"Choose Variables to compare", choices = colnames(tsdf)[-1])
    })
    
    output$compChart <- renderPlotly({
        m1 <- tsdf %>% select(Date, input$ySel, input$sel_comp) %>% 
            filter(Date>= input$dateRange[1], Date <= input$dateRange[2])
        ay <- list(
            tickfont = list(color = 'rgb(165, 15, 21)'),
            overlaying = "y",
            side = "right",
            title = input$sel_comp,
            font = f)
        plot_ly(m1 %>% select(Date, var1 = (input$ySel), var2 = (input$sel_comp))) %>%
            add_lines(x  = ~Date, y = ~var1, name = input$ySel) %>%
            add_lines(x  = ~Date, y = ~var2, name = input$sel_comp, yaxis = "y2", line = list(color = 'rgb(165, 15, 21)')) %>%
            layout(title = list(text = "Variable Relationship", font = f),
                   legend   = list(orientation = 'h'),  hovermode = 'compare',
                   yaxis2   = ay,
                   yaxis    = list(title = input$ySel, font = f, color = 'rgb(22, 96, 167)'),
                   xaxis    = list(title = ""),
                   margin   = list(l=50, r=50)
            )        
        })
    
    output$sarima_plot <- renderPlotly({
        dffil <- tsdf %>% filter(Date >= input$dateRange[1], Date <= input$dateRange[2]) %>%
            select(Date, input$ySel)
        m <- ts(dffil[[input$ySel]],
                start = c(year(input$dateRange[1]), month(input$dateRange[1])),
                end = c(year(input$dateRange[2]), month(input$dateRange[2])),
                frequency = 12)
        f_cast_mod <- arima(m, order = c(input$p,input$d,input$q), seasonal = c(input$sp,input$sd,input$sq))
        accuracy(forecast(f_cast_mod, frequency(m)))
        mod_fit <- as.data.frame(fitted(f_cast_mod))
        dffil['Forecast'] <- as.numeric(mod_fit$x)
        f_cast <- as.data.frame(forecast(f_cast_mod, input$sarima_h))
        f_cast$Date <- row.names(f_cast)
        f_cast$Date <- as.Date(as.yearmon(f_cast$Date))
        colnames(f_cast)[colnames(f_cast)=='Point Forecast'] = 'Forecast'
        f_cast <- f_cast[,c('Date','Forecast')]
        f_cast[[input$ySel]] <- NA
        rownames(f_cast) <- NULL
        co_df<- rbind(dffil, f_cast)
        plot_ly(co_df %>% select(Date, selc = input$ySel, Forecast), x = ~Date) %>%
            add_trace(y = ~selc, name = "Observed", type = 'scatter', mode = 'lines') %>%
            add_trace(y = ~Forecast, name = "Forecast", type = 'scatter', mode = 'lines') %>%
            layout(title = list(text = ""), font = f,
                   hovermode = 'compare',xaxis    = list(title = ''))
    })
    
    output$sarima_accuracy <- renderPrint({
        dffil <- tsdf %>% filter(Date >= input$dateRange[1], Date <= input$dateRange[2]) %>%
            select(Date, input$ySel)
        m <- ts(dffil[[input$ySel]],
                start = c(year(input$dateRange[1]), month(input$dateRange[1])),
                end = c(year(input$dateRange[2]), month(input$dateRange[2])),
                frequency = 12)
        f_cast_mod <- arima(m, order = c(input$p,input$d,input$q), seasonal = c(input$sp,input$sd,input$sq))
        accuracy(forecast(f_cast_mod, frequency(m)))
    })
    output$sarima_acf <- renderPlot({
        dffil <- tsdf %>% filter(Date >= input$dateRange[1], Date <= input$dateRange[2]) %>%
            select(Date, input$ySel)
        m <- ts(dffil[[input$ySel]],
                start = c(year(input$dateRange[1]), month(input$dateRange[1])),
                end = c(year(input$dateRange[2]), month(input$dateRange[2])),
                frequency = 12)
        f_cast_mod <- arima(m, order = c(input$p,input$d,input$q), seasonal = c(input$sp,input$sd,input$sq))
        Acf(resid(f_cast_mod))
    })
    output$sarima_pacf <- renderPlot({
        dffil <- tsdf %>% filter(Date >= input$dateRange[1], Date <= input$dateRange[2]) %>%
            select(Date, input$ySel)
        m <- ts(dffil[[input$ySel]],
                start = c(year(input$dateRange[1]), month(input$dateRange[1])),
                end = c(year(input$dateRange[2]), month(input$dateRange[2])),
                frequency = 12)
        f_cast_mod <- arima(m, order = c(input$p,input$d,input$q), seasonal = c(input$sp,input$sd,input$sq))
        Pacf(resid(f_cast_mod))
    })
    
    output$arima_plot <- renderPlotly({
        dffil <- tsdf %>% filter(Date >= input$dateRange[1], Date <= input$dateRange[2]) %>%
            select(Date, input$ySel)
        m <- ts(dffil[[input$ySel]],
                start = c(year(input$dateRange[1]), month(input$dateRange[1])),
                end = c(year(input$dateRange[2]), month(input$dateRange[2])),
                frequency = 12)
        f_cast_mod <- arima(m, order = c(input$ap,input$ad,input$aq))
        accuracy(forecast(f_cast_mod, frequency(m)))
        mod_fit <- as.data.frame(fitted(f_cast_mod))
        dffil['Forecast'] <- as.numeric(mod_fit$x)
        f_cast <- as.data.frame(forecast(f_cast_mod, input$arima_h))
        f_cast$Date <- row.names(f_cast)
        f_cast$Date <- as.Date(as.yearmon(f_cast$Date))
        colnames(f_cast)[colnames(f_cast)=='Point Forecast'] = 'Forecast'
        f_cast <- f_cast[,c('Date','Forecast')]
        f_cast[[input$ySel]] <- NA
        rownames(f_cast) <- NULL
        co_df<- rbind(dffil, f_cast)
        plot_ly(co_df %>% select(Date, selc = input$ySel, Forecast), x = ~Date) %>%
            add_trace(y = ~selc, name = "Observed", type = 'scatter', mode = 'lines') %>%
            add_trace(y = ~Forecast, name = "Forecast", type = 'scatter', mode = 'lines') %>%
            layout(title = list(text = ""), font = f,
                   hovermode = 'compare',xaxis    = list(title = ''))
    })
   
    
    output$arima_accuracy <- renderPrint({
        dffil <- tsdf %>% filter(Date >= input$dateRange[1], Date <= input$dateRange[2]) %>%
            select(Date, input$ySel)
        m <- ts(dffil[[input$ySel]],
                start = c(year(input$dateRange[1]), month(input$dateRange[1])),
                end = c(year(input$dateRange[2]), month(input$dateRange[2])),
                frequency = 12)
        f_cast_mod <- arima(m, order = c(input$ap,input$ad,input$aq))
        accuracy(forecast(f_cast_mod, frequency(m)))
    })
    
    output$arima_acf <- renderPlot({
        dffil <- tsdf %>% filter(Date >= input$dateRange[1], Date <= input$dateRange[2]) %>%
            select(Date, input$ySel)
        m <- ts(dffil[[input$ySel]],
                start = c(year(input$dateRange[1]), month(input$dateRange[1])),
                end = c(year(input$dateRange[2]), month(input$dateRange[2])),
                frequency = 12)
        f_cast_mod <- arima(m, order = c(input$ap,input$ad,input$aq))
        Acf(resid(f_cast_mod))
    })
    
    output$arima_pacf <- renderPlot({
        dffil <- tsdf %>% filter(Date >= input$dateRange[1], Date <= input$dateRange[2]) %>%
            select(Date, input$ySel)
        m <- ts(dffil[[input$ySel]],
                start = c(year(input$dateRange[1]), month(input$dateRange[1])),
                end = c(year(input$dateRange[2]), month(input$dateRange[2])),
                frequency = 12)
        f_cast_mod <- arima(m, order = c(input$ap,input$ad,input$aq))
        Pacf(resid(f_cast_mod))
    })
    
    output$test1 <- renderPrint({
        xo <- tsdf %>% filter(Date >= input$dateRange[1], Date <= input$dateRange[2]) %>%
            select(input$checkbox2_arimax)
        (xo)
    })
    
    output$arimaX_plot <- renderPlotly({
        dffil <- tsdf %>% filter(Date >= input$dateRange[1], Date <= input$dateRange[2]) %>%
            select(Date, yvar= input$ySel)
        m <- ts(dffil$yvar,
                start = c(year(input$dateRange[1]), month(input$dateRange[1])),
                end = c(year(input$dateRange[2]), month(input$dateRange[2])),
                frequency = 12)
        xo <- tsdf %>% filter(Date >= input$dateRange[1], Date <= input$dateRange[2]) %>%
            select(input$checkbox2_arimax)
        xo <- as.matrix(xo)
        xo1 <- tsdf %>% filter(Date > input$dateRange[2]) %>%
            select(input$checkbox2_arimax)
        xo1 <- as.matrix(xo1)
        f_cast_mod <- Arima(m, order = c(input$apx,input$adx,input$aqx), xreg = xo)
        #accuracy(f_cast_mod)
        mod_fit <- as.data.frame(fitted(f_cast_mod))
        dffil['Forecast'] <- as.numeric(mod_fit$x)
        f_cast <- as.data.frame(predict(f_cast_mod, newxreg = xo1)$pred) #input$arima_hX
        f_cast$Date <- seq(as.Date(ymd(input$dateRange[2]) %m+% months(1)), by ="months", length = dim(xo1)[1])
        
        f_cast['Act'] <- NA
        colnames(f_cast)[colnames(f_cast)=='x'] = 'Forecast'
        f_cast <- f_cast[,c('Date','Act','Forecast')]
        colnames(f_cast) <- colnames(dffil)
        co_df<- rbind(dffil, f_cast)
        plot_ly(co_df %>% select(Date, yvar, Forecast), x = ~Date) %>%
            add_trace(y = ~yvar, name = "Observed", type = 'scatter', mode = 'lines') %>%
            add_trace(y = ~Forecast, name = "Forecast", type = 'scatter', mode = 'lines') %>%
            layout(title = list(text = ""), font = f,
                   hovermode = 'compare', xaxis = list(title = ''))
    })
    
    output$arimaX_accuracy <- renderPrint({
        dffil <- tsdf %>% filter(Date >= input$dateRange[1], Date <= input$dateRange[2]) %>%
            select(Date, yvar= input$ySel)
        m <- ts(dffil$yvar,
                start = c(year(input$dateRange[1]), month(input$dateRange[1])),
                end = c(year(input$dateRange[2]), month(input$dateRange[2])),
                frequency = 12)
        xo <- tsdf %>% filter(Date >= input$dateRange[1], Date <= input$dateRange[2]) %>%
            select(input$checkbox2_arimax)
        xo <- as.matrix(xo)
        xo1 <- tsdf %>% filter(Date > input$dateRange[2]) %>%
            select(input$checkbox2_arimax)
        xo1 <- as.matrix(xo1)
        f_cast_mod <- Arima(m, order = c(input$apx,input$adx,input$aqx), xreg = xo)
        accuracy(forecast(f_cast_mod, h = dim(xo1)[1], xreg = xo1))
    })
    
    output$arimaX_acf <- renderPlot({
        dffil <- tsdf %>% filter(Date >= input$dateRange[1], Date <= input$dateRange[2]) %>%
            select(Date, yvar= input$ySel)
        m <- ts(dffil$yvar,
                start = c(year(input$dateRange[1]), month(input$dateRange[1])),
                end = c(year(input$dateRange[2]), month(input$dateRange[2])),
                frequency = 12)
        xo <- tsdf %>% filter(Date >= input$dateRange[1], Date <= input$dateRange[2]) %>%
            select(input$checkbox2_arimax)
        xo <- as.matrix(xo)
        xo1 <- tsdf %>% filter(Date > input$dateRange[2]) %>%
            select(input$checkbox2_arimax)
        xo1 <- as.matrix(xo1)
        f_cast_mod <- Arima(m, order = c(input$apx,input$adx,input$aqx), xreg = xo)
        Acf(resid(f_cast_mod))
    })

    output$arimaX_Pacf <- renderPlot({
        dffil <- tsdf %>% filter(Date >= input$dateRange[1], Date <= input$dateRange[2]) %>%
            select(Date, yvar= input$ySel)
        m <- ts(dffil$yvar,
                start = c(year(input$dateRange[1]), month(input$dateRange[1])),
                end = c(year(input$dateRange[2]), month(input$dateRange[2])),
                frequency = 12)
        xo <- tsdf %>% filter(Date >= input$dateRange[1], Date <= input$dateRange[2]) %>%
            select(input$checkbox2_arimax)
        xo <- as.matrix(xo)
        xo1 <- tsdf %>% filter(Date > input$dateRange[2]) %>%
            select(input$checkbox2_arimax)
        xo1 <- as.matrix(xo1)
        f_cast_mod <- Arima(m, order = c(input$apx,input$adx,input$aqx), xreg = xo)
        Pacf(resid(f_cast_mod))
    })
    
    output$sarimaX_plot <- renderPlotly({
        dffil <- tsdf %>% filter(Date >= input$dateRange[1], Date <= input$dateRange[2]) %>%
            select(Date, yvar= input$ySel)
        m <- ts(dffil$yvar,
                start = c(year(input$dateRange[1]), month(input$dateRange[1])),
                end = c(year(input$dateRange[2]), month(input$dateRange[2])),
                frequency = 12)
        xo <- tsdf %>% filter(Date >= input$dateRange[1], Date <= input$dateRange[2]) %>%
            select(input$checkbox2_sarimax)
        xo <- as.matrix(xo)
        xo1 <- tsdf %>% filter(Date > input$dateRange[2]) %>%
            select(input$checkbox2_sarimax)
        xo1 <- as.matrix(xo1)
        f_cast_mod <- Arima(m, order = c(input$px,input$dx,input$qx),seasonal = c(input$spx,input$sdx,input$sqx), xreg = xo)
        #accuracy(f_cast_mod)
        mod_fit <- as.data.frame(fitted(f_cast_mod))
        dffil['Forecast'] <- as.numeric(mod_fit$x)
        f_cast <- as.data.frame(predict(f_cast_mod, newxreg = xo1)$pred) #input$arima_hX
        f_cast$Date <- seq(as.Date(ymd(input$dateRange[2]) %m+% months(1)), by ="months", length = dim(xo1)[1])
        
        f_cast['Act'] <- NA
        colnames(f_cast)[colnames(f_cast)=='x'] = 'Forecast'
        f_cast <- f_cast[,c('Date','Act','Forecast')]
        colnames(f_cast) <- colnames(dffil)
        co_df<- rbind(dffil, f_cast)
        plot_ly(co_df %>% select(Date, yvar, Forecast), x = ~Date) %>%
            add_trace(y = ~yvar, name = "Observed", type = 'scatter', mode = 'lines') %>%
            add_trace(y = ~Forecast, name = "Forecast", type = 'scatter', mode = 'lines') %>%
            layout(title = list(text = ""), font = f,
                   hovermode = 'compare', xaxis = list(title = ''))
    })
    
    output$sarimaX_accuracy <- renderPrint({
        dffil <- tsdf %>% filter(Date >= input$dateRange[1], Date <= input$dateRange[2]) %>%
            select(Date, yvar= input$ySel)
        m <- ts(dffil$yvar,
                start = c(year(input$dateRange[1]), month(input$dateRange[1])),
                end = c(year(input$dateRange[2]), month(input$dateRange[2])),
                frequency = 12)
        xo <- tsdf %>% filter(Date >= input$dateRange[1], Date <= input$dateRange[2]) %>%
            select(input$checkbox2_sarimax)
        xo <- as.matrix(xo)
        xo1 <- tsdf %>% filter(Date > input$dateRange[2]) %>%
            select(input$checkbox2_sarimax)
        xo1 <- as.matrix(xo1)
        f_cast_mod <- Arima(m, order = c(input$px,input$dx,input$qx),seasonal = c(input$spx,input$sdx,input$sqx), xreg = xo)
        accuracy(forecast(f_cast_mod, h = dim(xo1)[1], xreg = xo1))
    })
    
    output$sarimaX_acf <- renderPlot({
        dffil <- tsdf %>% filter(Date >= input$dateRange[1], Date <= input$dateRange[2]) %>%
            select(Date, yvar= input$ySel)
        m <- ts(dffil$yvar,
                start = c(year(input$dateRange[1]), month(input$dateRange[1])),
                end = c(year(input$dateRange[2]), month(input$dateRange[2])),
                frequency = 12)
        xo <- tsdf %>% filter(Date >= input$dateRange[1], Date <= input$dateRange[2]) %>%
            select(input$checkbox2_sarimax)
        xo <- as.matrix(xo)
        xo1 <- tsdf %>% filter(Date > input$dateRange[2]) %>%
            select(input$checkbox2_sarimax)
        xo1 <- as.matrix(xo1)
        f_cast_mod <- Arima(m, order = c(input$px,input$dx,input$qx),seasonal = c(input$spx,input$sdx,input$sqx), xreg = xo)
        Acf(resid(f_cast_mod))
    })
    
    output$sarimaX_Pacf <- renderPlot({
        dffil <- tsdf %>% filter(Date >= input$dateRange[1], Date <= input$dateRange[2]) %>%
            select(Date, yvar= input$ySel)
        m <- ts(dffil$yvar,
                start = c(year(input$dateRange[1]), month(input$dateRange[1])),
                end = c(year(input$dateRange[2]), month(input$dateRange[2])),
                frequency = 12)
        xo <- tsdf %>% filter(Date >= input$dateRange[1], Date <= input$dateRange[2]) %>%
            select(input$checkbox2_sarimax)
        xo <- as.matrix(xo)
        xo1 <- tsdf %>% filter(Date > input$dateRange[2]) %>%
            select(input$checkbox2_sarimax)
        xo1 <- as.matrix(xo1)
        f_cast_mod <- Arima(m, order = c(input$px,input$dx,input$qx),seasonal = c(input$spx,input$sdx,input$sqx), xreg = xo)
        Pacf(resid(f_cast_mod))
    })
    
    
    output$checkbox2 <- renderUI({
        checkboxGroupInput('checkbox2_arimax',"Choose Variables", choices = colnames(tsdf)[-1])
    })
    
    output$checkbox3 <- renderUI({
        checkboxGroupInput('checkbox2_sarimax',"Choose Variables", choices = colnames(tsdf)[-1])
    })
    
    
    
    output$downloadarima <- downloadHandler(
        
        filename = function() { 
            paste("ARIMA_",input$ySel,"_", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            dffil <- tsdf %>% filter(Date >= input$dateRange[1], Date <= input$dateRange[2]) %>%
                select(Date, input$ySel)
            m <- ts(dffil[[input$ySel]],
                    start = c(year(input$dateRange[1]), month(input$dateRange[1])),
                    end = c(year(input$dateRange[2]), month(input$dateRange[2])),
                    frequency = 12)
            f_cast_mod <- arima(m, order = c(input$ap,input$ad,input$aq))
            accuracy(forecast(f_cast_mod, frequency(m)))
            mod_fit <- as.data.frame(fitted(f_cast_mod))
            dffil['Forecast'] <- as.numeric(mod_fit$x)
            f_cast <- as.data.frame(forecast(f_cast_mod, input$arima_h))
            f_cast$Date <- row.names(f_cast)
            f_cast$Date <- as.Date(as.yearmon(f_cast$Date))
            colnames(f_cast)[colnames(f_cast)=='Point Forecast'] = 'Forecast'
            f_cast <- f_cast[,c('Date','Forecast')]
            f_cast[[input$ySel]] <- NA
            rownames(f_cast) <- NULL
            co_df<- rbind(dffil, f_cast)
            write_csv(co_df, file)
        })
    
    output$downloadsarima <- downloadHandler(
        
        filename = function() { 
            paste("SARIMA_",input$ySel,"_" ,Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            dffil <- tsdf %>% filter(Date >= input$dateRange[1], Date <= input$dateRange[2]) %>%
                select(Date, input$ySel)
            m <- ts(dffil[[input$ySel]],
                    start = c(year(input$dateRange[1]), month(input$dateRange[1])),
                    end = c(year(input$dateRange[2]), month(input$dateRange[2])),
                    frequency = 12)
            f_cast_mod <- arima(m, order = c(input$p,input$d,input$q), seasonal = c(input$sp,input$sd,input$sq))
            mod_fit <- as.data.frame(fitted(f_cast_mod))
            dffil['Forecast'] <- as.numeric(mod_fit$x)
            f_cast <- as.data.frame(forecast(f_cast_mod, input$sarima_h))
            f_cast$Date <- row.names(f_cast)
            f_cast$Date <- as.Date(as.yearmon(f_cast$Date))
            colnames(f_cast)[colnames(f_cast)=='Point Forecast'] = 'Forecast'
            f_cast <- f_cast[,c('Date','Forecast')]
            f_cast[[input$ySel]] <- NA
            rownames(f_cast) <- NULL
            co_df<- rbind(dffil, f_cast)
            
            write_csv(co_df, file)
        })
    
    output$downloadarimaX <- downloadHandler(
        
        filename = function() { 
            paste("ARIMAX_",input$ySel,"_", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            dffil <- tsdf %>% filter(Date >= input$dateRange[1], Date <= input$dateRange[2]) %>%
                select(Date, yvar= input$ySel)
            m <- ts(dffil$yvar,
                    start = c(year(input$dateRange[1]), month(input$dateRange[1])),
                    end = c(year(input$dateRange[2]), month(input$dateRange[2])),
                    frequency = 12)
            xo <- tsdf %>% filter(Date >= input$dateRange[1], Date <= input$dateRange[2]) %>%
                select(input$checkbox2_arimax)
            xo <- as.matrix(xo)
            xo1 <- tsdf %>% filter(Date > input$dateRange[2]) %>%
                select(input$checkbox2_arimax)
            xo1 <- as.matrix(xo1)
            f_cast_mod <- Arima(m, order = c(input$apx,input$adx,input$aqx), xreg = xo)
            #accuracy(f_cast_mod)
            mod_fit <- as.data.frame(fitted(f_cast_mod))
            dffil['Forecast'] <- as.numeric(mod_fit$x)
            f_cast <- as.data.frame(predict(f_cast_mod, newxreg = xo1)$pred) #input$arima_hX
            f_cast$Date <- seq(as.Date(ymd(input$dateRange[2]) %m+% months(1)), by ="months", length = dim(xo1)[1])
            f_cast['Act'] <- NA
            colnames(f_cast)[colnames(f_cast)=='x'] = 'Forecast'
            f_cast <- f_cast[,c('Date','Act','Forecast')]
            colnames(f_cast) <- colnames(dffil)
            co_df<- rbind(dffil, f_cast)            
            write_csv(co_df, file)
        })
    
    output$downloadsarimaX <- downloadHandler(
        
        filename = function() { 
            paste("SARIMAX_",input$ySel,"_", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            dffil <- tsdf %>% filter(Date >= input$dateRange[1], Date <= input$dateRange[2]) %>%
                select(Date, yvar= input$ySel)
            m <- ts(dffil$yvar,
                    start = c(year(input$dateRange[1]), month(input$dateRange[1])),
                    end = c(year(input$dateRange[2]), month(input$dateRange[2])),
                    frequency = 12)
            xo <- tsdf %>% filter(Date >= input$dateRange[1], Date <= input$dateRange[2]) %>%
                select(input$checkbox2_sarimax)
            xo <- as.matrix(xo)
            xo1 <- tsdf %>% filter(Date > input$dateRange[2]) %>%
                select(input$checkbox2_sarimax)
            xo1 <- as.matrix(xo1)
            f_cast_mod <- Arima(m, order = c(input$px,input$dx,input$qx),seasonal = c(input$spx,input$sdx,input$sqx), xreg = xo)
            #accuracy(f_cast_mod)
            mod_fit <- as.data.frame(fitted(f_cast_mod))
            dffil['Forecast'] <- as.numeric(mod_fit$x)
            f_cast <- as.data.frame(predict(f_cast_mod, newxreg = xo1)$pred) #input$arima_hX
            f_cast$Date <- seq(as.Date(ymd(input$dateRange[2]) %m+% months(1)), by ="months", length = dim(xo1)[1])
            
            f_cast['Act'] <- NA
            colnames(f_cast)[colnames(f_cast)=='x'] = 'Forecast'
            f_cast <- f_cast[,c('Date','Act','Forecast')]
            colnames(f_cast) <- colnames(dffil)
            co_df<- rbind(dffil, f_cast)
            write_csv(co_df, file)
        })
    
    output$downloadses <- downloadHandler(
        
        filename = function() { 
            paste("SES_",input$ySel,"_" ,Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            dffil <- tsdf %>% filter(Date >= input$dateRange[1], Date <= input$dateRange[2]) %>%
                select(Date, input$ySel)
            m <- ts(dffil[[input$ySel]],
                    start = c(year(input$dateRange[1]), month(input$dateRange[1])),
                    end = c(year(input$dateRange[2]), month(input$dateRange[2])),
                    frequency = 12)
            f_cast_mod <- ses(m, h = input$ses_f_pd, alpha = input$alpha)
            accuracy(f_cast_mod)
            mod_fit <- as.data.frame(fitted(f_cast_mod))
            dffil['Forecast'] <- as.numeric(mod_fit$x)
            f_cast <- as.data.frame(f_cast_mod)
            f_cast$Date <- row.names(f_cast)
            f_cast$Date <- as.Date(as.yearmon(f_cast$Date))
            colnames(f_cast)[colnames(f_cast)=='Point Forecast'] = 'Forecast'
            f_cast <- f_cast[,c('Date','Forecast')]
            f_cast[[input$ySel]] <- NA
            rownames(f_cast) <- NULL
            co_df<- rbind(dffil, f_cast)
            

            write_csv(co_df, file)
        })
    
    output$downloadholt <- downloadHandler(
        
        filename = function() { 
            paste("Holt_",input$ySel,"_" ,Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            dffil <- tsdf %>% filter(Date >= input$dateRange[1], Date <= input$dateRange[2]) %>%
                select(Date, input$ySel)
            m <- ts(dffil[[input$ySel]],
                    start = c(year(input$dateRange[1]), month(input$dateRange[1])),
                    end = c(year(input$dateRange[2]), month(input$dateRange[2])),
                    frequency = 12)
            f_cast_mod <- holt(m, h = input$holt_h, alpha = input$holt_alpha, beta = input$holt_beta)
            accuracy(f_cast_mod)
            mod_fit <- as.data.frame(fitted(f_cast_mod))
            dffil['Forecast'] <- as.numeric(mod_fit$x)
            f_cast <- as.data.frame(f_cast_mod)
            f_cast$Date <- row.names(f_cast)
            f_cast$Date <- as.Date(as.yearmon(f_cast$Date))
            colnames(f_cast)[colnames(f_cast)=='Point Forecast'] = 'Forecast'
            f_cast <- f_cast[,c('Date','Forecast')]
            f_cast[[input$ySel]] <- NA
            rownames(f_cast) <- NULL
            co_df<- rbind(dffil, f_cast)
            
            
            write_csv(co_df, file)
        })
    
    output$downloadHW<- downloadHandler(
        
        filename = function() { 
            paste("Holt Winters_",input$ySel,"_" ,Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            dffil <- tsdf %>% filter(Date >= input$dateRange[1], Date <= input$dateRange[2]) %>%
                select(Date, input$ySel)
            m <- ts(dffil[[input$ySel]],
                    start = c(year(input$dateRange[1]), month(input$dateRange[1])),
                    end = c(year(input$dateRange[2]), month(input$dateRange[2])),
                    frequency = 12)
            f_cast_mod <- hw(m, h = input$hw_h, seasonal = input$hw_seasonal, 
                             alpha = input$hw_alpha,
                             beta = input$hw_beta,
                             gamma = input$hw_gamma)
            accuracy(f_cast_mod)
            mod_fit <- as.data.frame(fitted(f_cast_mod))
            dffil['Forecast'] <- as.numeric(mod_fit$x)
            f_cast <- as.data.frame(f_cast_mod)
            f_cast$Date <- row.names(f_cast)
            f_cast$Date <- as.Date(as.yearmon(f_cast$Date))
            colnames(f_cast)[colnames(f_cast)=='Point Forecast'] = 'Forecast'
            f_cast <- f_cast[,c('Date','Forecast')]
            f_cast[[input$ySel]] <- NA
            rownames(f_cast) <- NULL
            co_df<- rbind(dffil, f_cast)
            
            
            write_csv(co_df, file)
        })
    
    output$ggmisvar = renderPlot({
        gg_miss_var(tsdf)
    })
    
    output$vismiss = renderPlot({
        vis_miss(tsdf)
    })
    
    output$nastats = renderPrint({
        statsNA(tsdf[[input$ySel]])
        
    })
    

    
    output$na_sel_optn <- renderUI({
        fltr = na_modls_df %>% filter(fnctn == input$na_model_sel) %>% select(optn)
        selectInput('na_model_optn_sel',"Model Options", choices =fltr, selected = 'StructTS')
    })
    
    output$na_dist_plot <- renderPlot({
        plotNA.distribution(tsdf[[input$ySel]])
    })
    
    output$kalman_struct_plot = renderPlot({
        plotNA.imputations(tsdf[[input$ySel]],na_kalman(tsdf[[input$ySel]], model = 'StructTS'), colWithImputations = 'red')
    })
    
    output$kalman_autoarima_plot = renderPlot({
        plotNA.imputations(tsdf[[input$ySel]],na_kalman(tsdf[[input$ySel]], model = 'auto.arima'), colWithImputations = 'red')
    })
    
    output$locf_plot = renderPlot({
        plotNA.imputations(tsdf[[input$ySel]],na_locf(tsdf[[input$ySel]], option = 'locf'), colWithImputations = 'red')
    })
    
    output$nocb_plot = renderPlot({
        plotNA.imputations(tsdf[[input$ySel]],na_locf(tsdf[[input$ySel]], option = 'nocb'), colWithImputations = 'red')
    })
    
    output$mean_plot = renderPlot({
        plotNA.imputations(tsdf[[input$ySel]],na_mean(tsdf[[input$ySel]], option = 'mean'), colWithImputations = 'red')
    })
    
    output$median_plot = renderPlot({
        plotNA.imputations(tsdf[[input$ySel]],na_mean(tsdf[[input$ySel]], option = 'median'), colWithImputations = 'red')
    })
    
    output$mode_plot = renderPlot({
        plotNA.imputations(tsdf[[input$ySel]],na_mean(tsdf[[input$ySel]], option = 'mode'), colWithImputations = 'red')
    })
    
    output$lin_interpol = renderPlot({
        plotNA.imputations(tsdf[[input$ySel]],na_interpolation(tsdf[[input$ySel]], option = 'linear'), colWithImputations = 'red')
    })
    
    output$spline_interpol = renderPlot({
        plotNA.imputations(tsdf[[input$ySel]],na_interpolation(tsdf[[input$ySel]], option = 'spline'), colWithImputations = 'red')
    })
    
    
    output$stine_interpol = renderPlot({
        plotNA.imputations(tsdf[[input$ySel]],na_interpolation(tsdf[[input$ySel]], option = 'stine'), colWithImputations = 'red')
    })
    
    output$sma = renderPlot({
        plotNA.imputations(tsdf[[input$ySel]],na_ma(tsdf[[input$ySel]], k = input$kval, weighting = 'simple'), colWithImputations = 'red')
    })
    
    output$lwma = renderPlot({
        plotNA.imputations(tsdf[[input$ySel]],na_ma(tsdf[[input$ySel]], k = input$kval, weighting = 'linear'), colWithImputations = 'red')
    })
    
    output$ewma = renderPlot({
        plotNA.imputations(tsdf[[input$ySel]],na_ma(tsdf[[input$ySel]], k = input$kval, weighting = 'exponential'), colWithImputations = 'red')
    })

    output$download_TSimp <- downloadHandler(
        
        filename = function() { 
            paste("Imputed_Data",Sys.Date(),".csv")
        },
        content = function(file) {
            if (input$na_model_sel == 'na_kalman' && input$na_model_optn_sel == 'StructTS') {
                na_df <- na_kalman(tsdf, model = 'StructTS')
            } 
            else if (input$na_model_sel == 'na_kalman' && input$na_model_optn_sel == 'auto.arima') {
                na_df <- na_kalman(tsdf, model = 'auto.arima')
            }
            else if(input$na_model_sel == 'na_interpolation' && input$na_model_optn_sel == 'linear'){
                na_df <- na_interpolation(tsdf, option = 'linear')
            }
            else if(input$na_model_sel == 'na_interpolation' && input$na_model_optn_sel == 'spline'){
                na_df <- na_interpolation(tsdf, option = 'spline')
            }
            else if(input$na_model_sel == 'na_interpolation' && input$na_model_optn_sel == 'stine'){
                na_df <- na_interpolation(tsdf, option = 'stine')
            }            
            else if(input$na_model_sel == 'na_locf' && input$na_model_optn_sel == 'locf'){
                na_df <- na_locf(tsdf, option = 'locf')
            }
            else if(input$na_model_sel == 'na_locf' && input$na_model_optn_sel == 'nocb'){
                na_df <- na_locf(tsdf, option = 'nocb')
            }
            else if(input$na_model_sel == 'na_mean' && input$na_model_optn_sel == 'mean'){
                na_df <- na_mean(tsdf, option = 'mean')
            }
            else if(input$na_model_sel == 'na_mean' && input$na_model_optn_sel == 'median'){
                na_df <- na_mean(tsdf, option = 'median')
            }
            else if(input$na_model_sel == 'na_mean' && input$na_model_optn_sel == 'mode'){
                na_df <- na_mean(tsdf, option = 'mode')
            }
            else if(input$na_model_sel == 'na_ma' && input$na_model_optn_sel == 'simple'){
                na_df <- na_ma(tsdf, weighting = 'mode', k = input$kval)
            }
            else if(input$na_model_sel == 'na_ma' && input$na_model_optn_sel == 'linear'){
                na_df <- na_ma(tsdf, weighting = 'mode', k = input$kval)
            }
            else if(input$na_model_sel == 'na_ma' && input$na_model_optn_sel == 'exponential'){
                na_df <- na_ma(tsdf, weighting = 'mode', k = input$kval)
            }
            write_csv(na_df, file)
        })
    
    output$corplot <- renderPlot({
        corrplot::corrplot(round(cor(select_if(tsdf, is.numeric)),2), method = 'square',
                           use = 'complete.obs', type = 'lower')
        })
    

}

# Run the application 
shinyApp(ui = ui, server = server)

