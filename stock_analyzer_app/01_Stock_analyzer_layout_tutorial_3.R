# Business Analytics with Data Science and Machine Learning ----
# Building Business Data Products ----
# STOCK ANALYZER APP - LAYOUT -----

# APPLICATION DESCRIPTION ----
# - Create a basic layout in shiny showing the stock dropdown, interactive plot and commentary


# LIBRARIES ----
library(shiny)
library(shinyWidgets)

library(plotly)
library(tidyverse)

library(rvest)
library(glue)

#source(file = "00_scripts/stock_analysis_functions.R")
source(file = "stock_analysis_functions.R")
stock_list_tbl <- get_stock_list("SP500")
#stock_list_tbl%label

#stock_data_tbl <- get_stock_data("AAPL", from = "2020-06-01", to = "2021-01-12", mavg_short = 5, mavg_long = 8)

# UI -----

# 1.0 HEADER ----
ui <- fluidPage(
  title = "Stock Analyzer",
  
  # 0.0 Test ----
  #"Learning Shiny... I'm building my first App"
  
  div(
    h1("Stock Analzer"),
    p("paragraph writing goes here"), 
    
    
    
# 2.0 APPLICATION UI -----    

    #First column
    column(width = 4, "Selection",
           wellPanel(
             pickerInput(inputId = "stock_selection",
                         #choices = 1:10)
                         choices = stock_list_tbl$label,
                         multiple = FALSE,
                         selected = NULL,
                         options = pickerOptions(
                           actionsBox = TRUE,
                           liveSearch = TRUE,
                           size = 10)
             ),
             br(),
             #Button for frist column
             actionButton(inputId = "analyze", label = "Analyze", icon = icon("download")),
             
             # For Testing observeEvent()
             verbatimTextOutput("eventOutput")
           )
           
           
    ),
    
    #br(),
    #Button for frist column
    #actionButton(inputId = "analyze", label = "Analyze", icon = icon("download")),
    
    
    
    #2nd column
    column(width = 8, "Plot",
           # For stock name
           verbatimTextOutput("selected_symbol_output"),
           # For stock data table
           #verbatimTextOutput(outputId = "stock_data"),
           div(
             style = "height: 200px; overflow: scroll;",
             #tableOutput("tableOutput"),
             verbatimTextOutput("tableOutput"),
           ),
           
           
           div(
             h4("Stock Analysis"),
             div(id = "stock_plot",
                 #plotlyOutput(outputId = "time_series_plot"),
                 plotlyOutput(outputId = "plotly_plot")
             )
             
           )
    )
),

# 3.0 ANALYST COMMENTARY ----
  fluidRow(
    column(width = 12,
           div(
             h4("Analyst Commentary"),
             textOutput("analyst_commentary"),
             #generate_commentary(stock_data_tbl, user_input = "Placeholder")
             #generate_commentary(stock_data_tbl, user_input = stock_selection)
             )
           )
    )
)



# SERVER ----
server <- function(input, output, session) {
  
  # Test using observeEvent(), printing to console
  observeEvent(input$analyze, {
    selected_stock_name <- input$stock_selection
    print(selected_stock_name)
  })
  
  observeEvent(input$analyze, {
    output$eventOutput <- renderText({
      "Testing observeEvent Triggered"
    })
  })

  #Plotting-----------------------
  # Store the stock data
  stock_data <- reactive({
    stock_symbol <- selected_symbol()
    #get_stock_data(stock_symbol)
    stock_symbol() %>% get_symbol_from_user_input() %>% get_stock_data()
  })
  
  # Render the time series plot
  output$time_series_plot <- renderPlotly({
    plot_stock_data(stock_data())
  })
  
  # Analyst commentary -----------------------------
  # Render the analyst commentary
  output$analyst_commentary <- renderText({
    generate_commentary(stock_data(), user_input = selected_symbol())
  })
  
  # Reactive symbol extraction
  selected_symbol <- eventReactive(input$analyze, {
    input$stock_selection
  })
  
  # Render the selected symbol
  output$selected_symbol_text <- renderText({
    paste("Selected symbol:", selected_symbol())
  })
  
  
  # Stock Symbol name ------------------------
  stock_symbol <- eventReactive(input$analyze, ignoreNULL = FALSE, {
    input$stock_selection
  })
  
  output$selected_symbol_output <- renderText({stock_symbol()
    })
  
  #output$plot_header <- renderText(stock_symbol())
  
  # extract / get stock data ------------------------------------
  #output$tableOutput <- renderTable({
  output$tableOutput <- renderPrint({
    if (input$analyze > 0) {
       stock_symbol() %>% get_symbol_from_user_input() %>%
        get_stock_data(from = today() - days(180), 
                       to   = today(),
                       mavg_short = 20,
                       mavg_long  = 50)
    }
  })
  
  # Plotting number 2 ----------------------------------------------
  output$plotly_plot <- renderPlotly({
    #if (input$analyze > 0){
      #stock_data_2 <- selected_symbol() %>% get_symbol_from_user_input() %>% get_stock_data() %>% plot_stock_data()
      #plot_stock_data(stock_data())
      #plot_stock_data(stock_data_2)
      #print(selected_symbol() %>% get_symbol_from_user_input() %>% get_stock_data())
      #print(input$stock_selection)
      #stock_data_2 <- selected_symbol() %>% get_symbol_from_user_input() %>% get_stock_data()
      
      #input$stock_selection %>% get_stock_data() %>% plot_stock_data()
      #stock_data_2 %>% plot_stock_data()
      #print(stock_data_2)
      #print(selected_symbol())
      stock_data_tbl <- stock_symbol() %>% get_symbol_from_user_input() %>%
        get_stock_data(from = today() - days(180), 
                       to   = today(),
                       mavg_short = 20,
                       mavg_long  = 50)
      
      plot_stock_data(stock_data_tbl)
      
      
    #}
    
  })
  

}

# RUN APP ----
shinyApp(ui = ui, server = server)