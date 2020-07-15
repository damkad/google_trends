library(rsconnect)
library(rmarkdown)
library(tidyverse)
library(timetk)
library(gtrendsR)
library(modeltime)
library(lubridate)
library(randomForest)
library(parsnip)
library(rsample)
library(kernlab)
library(reactable)
library(plotly)
library(countrycode)
library(shiny)
library(shinydashboard)
library(shinycssloaders)


countries <- countrycode::codelist %>% as.data.frame() %>% select(un.name.en, iso2c) %>% drop_na()
countries_list <- c(countries$iso2c)
names(countries_list) = countries$un.name.en


#functions
data_google <- function(keyword, geo, from_date, to_date, all_time){
  data = NULL
  
  timeFrame = ifelse(all_time == TRUE, "all", 
                     ifelse(as.numeric(from_date) >= as.numeric(to_date), "all",  
                            paste(from_date, to_date, sep = " ")))
  
  
  gt = gtrendsR::gtrends(keyword = keyword, geo = geo, time = timeFrame)
  data <- gt$interest_over_time %>% select(date, hits)
  data$date <- ymd(data$date)
  data$hits <- as.numeric(data$hits)
  data <- drop_na(data)
  
  
  return(data)  
}

calibrate <- function(data){
  data_split <- time_series_split(data, assess = "1 year", cumulative = T)
  #arima
  model_arima <- arima_reg() %>% 
    set_engine("auto_arima") %>% 
    fit(hits~date, data = training(data_split))
  
  #arima boost
  model_arima_boost <- arima_boost(mode = "regression") %>% 
    set_engine("auto_arima_xgboost") %>% 
    fit(hits~date, data = training(data_split))
  
  #linear regression with trend
  model_lr <- linear_reg() %>% 
    set_engine("lm") %>% fit(hits ~ as.numeric(date) + month(date, label = T), data = training(data_split))
  
  #linear regression without trend
  model_lr_detrend <- linear_reg() %>% 
    set_engine("lm") %>% fit(hits ~ date + month(date, label = T), data = training(data_split))
  
  
  #random forest 
  model_r_for <- rand_forest(mode = "regression") %>%
    set_engine("randomForest") %>% fit(hits ~ as.numeric(date) + month(date, label = T), data = training(data_split))
  
  
  #prophet
  model_prophet <- prophet_reg() %>%
    set_engine("prophet") %>% fit(hits ~ date, training(data_split))
  
  #prophet boost 
  model_prophet_boost <- prophet_boost(mode = "regression") %>%
    set_engine("prophet_xgboost") %>% fit(hits ~ date, training(data_split))
  
  
  #xg boost
  model_xgboost <- boost_tree(mode = "regression")  %>%
    set_engine("xgboost") %>% fit(hits ~ as.numeric(date) + month(date, label = T), data = training(data_split))
  
  #svm_poly
  model_svm_poly <- svm_poly(mode = "regression")  %>%
    set_engine("kernlab") %>% fit(hits ~ as.numeric(date) + month(date, label = T), data = training(data_split))
  
  
  #svm_rbf
  model_svm_rbf <- svm_rbf(mode = "regression")  %>%
    set_engine("kernlab") %>% fit(hits ~ as.numeric(date) + month(date, label = T), data = training(data_split))
  
  
  models_tbl <- modeltime_table(model_arima, model_arima_boost, model_lr, model_lr_detrend, model_r_for, model_prophet, model_prophet_boost,
                                model_xgboost, model_svm_poly, model_svm_rbf)
  
  #calibrate
  calibrate_tbl <- models_tbl %>% modeltime_calibrate(testing(data_split))
  
  return(calibrate_tbl)
}
ui <- dashboardPage(
  dashboardHeader(title = "Google Forecast"),
  dashboardSidebar(
   
    sidebarMenu(
      textInput("key", "key word", value = ""),
      selectInput("geo", "Country", 
                  choices =  countries_list),
      dateRangeInput("dateRange", "time interval"),
      checkboxInput("dateAll", "All date"),
      br(),
      actionButton(inputId = "search", "Search")
      
    )
    
  ),
  dashboardBody(
    fluidRow((box(withSpinner(plotlyOutput("plot_data")) , title = "Time Series Plot", solidHeader = TRUE, status = "primary", width = 12))),
    fluidRow(box(withSpinner(plotlyOutput("plot_test")), title = "Train & Test Plot", solidHeader = T, status = "primary", width = 12)),
    fluidRow(box(withSpinner(reactableOutput("show_accuracy")), title = "Model Accuracy", status = "primary", solidHeader = T, width = 12)),
    fluidRow(box(withSpinner(plotlyOutput("plot_forecast")), title = "Forecast Flot", status = "primary", solidHeader = T, width = 12)),
    fluidRow(box(withSpinner(plotlyOutput("plot_avg_forecast")), title = "Average Forecast Plot", status = "primary", solidHeader = T, width = 12))
  )
)

server <- function(input, output, session) {
  
  data <- eventReactive(input$search, {
    data_ <- data_google(input$key, input$geo, input$dateRange[1], input$dateRange[2], input$dateAll)
    if (exists("data_") == T){
      return(data_)
    }
  })
  
  cal_helper = reactive({
    helper = calibrate(data())
    return(helper)
  })
  
  output$plot_data <- renderPlotly({
    tryCatch({
      data() %>% timetk::plot_time_series(date, hits, .interactive = T, .title = FALSE)
    }, error = function(e) {
      
    })
    
  })
  
  output$plot_test <- renderPlotly({
   
    tryCatch({
      plt_test <- cal_helper() %>%  modeltime_forecast(new_data =  testing(time_series_split(data(), assess = "1 year", cumulative = T)), 
                                                       actual_data = data(), conf_interval = 0.8) %>%
        plot_modeltime_forecast(.legend_show = T, .legend_max_width = 25, .title = FALSE)   
      plt_test
    }, error = function(e) {
      
    })
 
  })
  
  
  output$show_accuracy <- renderReactable({
    tryCatch({
      plt_s_a <-  cal_helper()  %>% modeltime_accuracy() %>% table_modeltime_accuracy(resizable = T, bordered = T)
      plt_s_a
    }, error = function(e) {
    })
    
  })
  

  output$plot_forecast <- renderPlotly({
    tryCatch({
      plt_for <-  cal_helper() %>% modeltime_refit(data()) %>% 
        modeltime_forecast(actual_data = data(), h = "1 year", conf_interval = 0.8) 
      
      plt_for  %>% plot_modeltime_forecast(.interactive = T, .title = FALSE)
    }, error = function(e) {
      
    })
  })
  
  output$plot_avg_forecast <- renderPlotly({

    tryCatch({
      forecast_tbl <-  cal_helper() %>% modeltime_refit(data()) %>% 
        modeltime_forecast(actual_data = data(), h = "1 year", conf_interval = 0.8)
      
      mean_forecast_table <- forecast_tbl %>% filter(.key != "actual") %>% group_by(.key, .index) %>% 
        summarise(across(.value:.conf_hi, mean)) %>% mutate(.model_id = 10, .model_desc = "AVERAGE OF MODELS")
      
        forecast_tbl %>% filter(.key == "actual") %>% bind_rows(mean_forecast_table) %>% plot_modeltime_forecast(.title = FALSE)
      
    }, error = function(e) {
    })
    
  })
  
}



shinyApp(ui, server)

