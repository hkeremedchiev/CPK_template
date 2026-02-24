library(shiny)
library(DT)

ui <- fluidPage(
  titlePanel("CSV File Viewer"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose CSV File",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv"))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Table", DTOutput("table")),
        tabPanel("Summary", verbatimTextOutput("summary"))
      )
    )
  )
)

server <- function(input, output) {
  
  data <- reactive({
    req(input$file)
    
    read.csv(input$file$datapath,
             header = FALSE,
             sep = ";")
  })
  
  data_with_avg <- reactive({
    df <- data()
    
    # Skip first 4 rows for calculations
    data_for_calc <- df[5:nrow(df), ]
    
    # Calculate standard deviation for each column (5 decimal places)
    # Skip first two columns (TimeStamp and Serial Num)
    stdev_row <- data.frame(
      V1 = NA,  # TimeStamp
      V2 = NA,  # Serial Num
      lapply(data_for_calc[, 3:ncol(data_for_calc)], function(col) {
        round(sd(as.numeric(col), na.rm = TRUE), 5)
      })
    )
    
    # Add row names
    row.names(stdev_row) <- "St Dev"
    
    # Calculate averages for each column (5 decimal places)
    # Skip first two columns (TimeStamp and Serial Num)
    avg_row <- data.frame(
      V1 = NA,  # TimeStamp
      V2 = NA,  # Serial Num
      lapply(data_for_calc[, 3:ncol(data_for_calc)], function(col) {
        round(mean(as.numeric(col), na.rm = TRUE), 5)
      })
    )
    
    # Add row names
    row.names(avg_row) <- "Average"
    
    # Combine both calculations with original data
    rbind(stdev_row, avg_row, df)
  })
  
  output$table <- renderDT({
    datatable(data_with_avg(),
              options = list(pageLength = 10,
                             scrollX = TRUE),
              rownames = TRUE)
  })
  
  output$summary <- renderPrint({
    summary(data())
  })
}

shinyApp(ui, server)