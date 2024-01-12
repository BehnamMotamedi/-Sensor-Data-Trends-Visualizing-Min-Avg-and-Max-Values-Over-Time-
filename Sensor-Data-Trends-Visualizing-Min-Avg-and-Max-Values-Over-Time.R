# Load the required libraries
library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)

# Define the UI for the Shiny app
ui <- fluidPage(
  titlePanel("Sensor Data Over Time"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("fileInput", "Choose Excel Files",
                accept = c(".xlsx", ".xls"), multiple = TRUE),
      dateRangeInput("dateRange", "Select Date Range:",
                     format = "yyyy-mm-dd",
                     separator = " - "),
      br(),
      actionButton("updateBtn", "Update Plots")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("JVP116", plotlyOutput("plot_JVP116")),
        tabPanel("JVP153", plotlyOutput("plot_JVP153")),
        tabPanel("JVP113", plotlyOutput("plot_JVP113")),
        tabPanel("JVP111", plotlyOutput("plot_JVP111")),
        tabPanel("GW_Level", plotlyOutput("plot_GW_Level"))
      )
    )
  )
)

server <- function(input, output) {
  
  # Initialize data
  data <- reactiveVal(NULL)
  
  # Reactive function to read data from the selected Excel file
  observeEvent(input$fileInput, {
    req(input$fileInput)
    data_list <- lapply(input$fileInput$datapath, read_excel)
    data_combined <- bind_rows(data_list)
    data(data_combined)
  })
  
  # Reactive function to filter data based on the selected date range
  filtered_data <- reactive({
    req(data())
    start_date <- as.POSIXct(input$dateRange[1])
    end_date <- as.POSIXct(input$dateRange[2])
    filtered <- filter(data(), Time >= start_date & Time <= end_date)
    
    # Extract Hour from the Time column
    filtered <- mutate(filtered, Hour = format(Time, "%H"))
    
    filtered
  })
  
  # Reactive function to calculate summary statistics for each sensor
  summary_stats <- reactive({
    filtered_data() %>%
      group_by(Hour) %>%
      summarize(
        JVP116_Avr = mean(JVP116, na.rm = TRUE),
        JVP116_Max = ifelse(all(is.na(JVP116)), NA, max(JVP116, na.rm = TRUE)),
        JVP116_Min = ifelse(all(is.na(JVP116)), NA, min(JVP116, na.rm = TRUE)),
        JVP153_Avr = mean(JVP153, na.rm = TRUE),
        JVP153_Max = ifelse(all(is.na(JVP153)), NA, max(JVP153, na.rm = TRUE)),
        JVP153_Min = ifelse(all(is.na(JVP153)), NA, min(JVP153, na.rm = TRUE)),
        JVP113_Avr = mean(JVP113, na.rm = TRUE),
        JVP113_Max = ifelse(all(is.na(JVP113)), NA, max(JVP113, na.rm = TRUE)),
        JVP113_Min = ifelse(all(is.na(JVP113)), NA, min(JVP113, na.rm = TRUE)),
        JVP111_Avr = mean(JVP111, na.rm = TRUE),
        JVP111_Max = ifelse(all(is.na(JVP111)), NA, max(JVP111, na.rm = TRUE)),
        JVP111_Min = ifelse(all(is.na(JVP111)), NA, min(JVP111, na.rm = TRUE)),
        GW_Level_Avr = mean(GW_Level, na.rm = TRUE),
        GW_Level_Max = ifelse(all(is.na(GW_Level)), NA, max(GW_Level, na.rm = TRUE)),
        GW_Level_Min = ifelse(all(is.na(GW_Level)), NA, min(GW_Level, na.rm = TRUE))
      )
  })
  
  # Reactive function to reshape the data for plotly for each sensor
  summary_stats_long <- reactive({
    gather(summary_stats(), key = "Stat", value = "Value", -Hour)
  })
  
  # Reactive function to update the JVP116 plot
  output$plot_JVP116 <- renderPlotly({
    plot_ly(subset(summary_stats_long(), grepl("JVP116", Stat)),
            x = ~Hour, y = ~Value, color = ~Stat, type = "scatter", mode = "lines+markers") %>%
      layout(title = "JVP116 Over Time",
             xaxis = list(title = "Hour of the Day"),
             yaxis = list(title = "Sensor Value"),
             legend = list(title = "Statistic"),
             colorway = c("gray", "blue", "red"))
  })
  
  # Reactive function to update the JVP153 plot
  output$plot_JVP153 <- renderPlotly({
    plot_ly(subset(summary_stats_long(), grepl("JVP153", Stat)),
            x = ~Hour, y = ~Value, color = ~Stat, type = "scatter", mode = "lines+markers") %>%
      layout(title = "JVP153 Over Time",
             xaxis = list(title = "Hour of the Day"),
             yaxis = list(title = "Sensor Value"),
             legend = list(title = "Statistic"),
             colorway = c("gray", "blue", "red"))
  })
  
  # Reactive function to update the JVP113 plot
  output$plot_JVP113 <- renderPlotly({
    plot_ly(subset(summary_stats_long(), grepl("JVP113", Stat)),
            x = ~Hour, y = ~Value, color = ~Stat, type = "scatter", mode = "lines+markers") %>%
      layout(title = "JVP113 Over Time",
             xaxis = list(title = "Hour of the Day"),
             yaxis = list(title = "Sensor Value"),
             legend = list(title = "Statistic"),
             colorway = c("gray", "blue", "red"))
  })
  
  # Reactive function to update the JVP111 plot
  output$plot_JVP111 <- renderPlotly({
    plot_ly(subset(summary_stats_long(), grepl("JVP111", Stat)),
            x = ~Hour, y = ~Value, color = ~Stat, type = "scatter", mode = "lines+markers") %>%
      layout(title = "JVP111 Over Time",
             xaxis = list(title = "Hour of the Day"),
             yaxis = list(title = "Sensor Value"),
             legend = list(title = "Statistic"),
             colorway = c("gray", "blue", "red"))
  })
  
  # Reactive function to update the GW_Level plot
  output$plot_GW_Level <- renderPlotly({
    plot_ly(subset(summary_stats_long(), grepl("GW_Level", Stat)),
            x = ~Hour, y = ~Value, color = ~Stat, type = "scatter", mode = "lines
+ markers") %>%
      layout(title = "GW_Level Over Time",
             xaxis = list(title = "Hour of the Day"),
             yaxis = list(title = "Sensor Value"),
             legend = list(title = "Statistic"),
             colorway= c("gray", "blue", "red"))
  })
}

# Run the Shiny app
shinyApp(ui, server)
#Designer: Behnam Motamedi
#Doctoral Researcher
#Water, Energy, and Environmental Engineering
#Faculty of Technology
#University of Oulu, Finland 
#Room no: SÄ333
#Phone: 358 40 859 8551
#E-mail: behnam.motamedi@oulu.fi
