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
    
    # Calculate CPK low: (Average - LowLimit) / (St Dev * 3)
    # LowLimit is in row 2 (df[2, ])
    cpk_low_row <- data.frame(
      V1 = NA,  # TimeStamp
      V2 = NA,  # Serial Num
      mapply(function(low_limit, avg_val, stdev_val) {
        if (is.na(avg_val) || is.na(stdev_val) || is.na(low_limit)) {
          return(NA)
        }
        low_limit_num <- as.numeric(low_limit)
        if (is.na(low_limit_num) || stdev_val == 0) {
          return(NA)
        }
        round((avg_val - low_limit_num) / (stdev_val * 3), 5)
      },
      as.list(df[2, 3:ncol(df)]),
      avg_row[, 3:ncol(avg_row)],
      stdev_row[, 3:ncol(stdev_row)],
      SIMPLIFY = FALSE)
    )
    
    # Add row names
    row.names(cpk_low_row) <- "CPK low"
    
    # Calculate CPK high: (Highlimit - Average ) / (St Dev * 3)
    # HighLimit is in row 1(df[1, ])
    cpk_high_row <- data.frame(
      V1 = NA,  # TimeStamp
      V2 = NA,  # Serial Num
      mapply(function(high_limit, avg_val, stdev_val) {
        if (is.na(avg_val) || is.na(stdev_val) || is.na(high_limit)) {
          return(NA)
        }
        high_limit_num <- as.numeric(high_limit)
        if (is.na(high_limit_num) || stdev_val == 0) {
          return(NA)
        }
        round(( high_limit_num - avg_val) / (stdev_val * 3), 5)
      },
      as.list(df[1, 3:ncol(df)]),
      avg_row[, 3:ncol(avg_row)],
      stdev_row[, 3:ncol(stdev_row)],
      SIMPLIFY = FALSE)
    )
    
    # Add row names
    row.names(cpk_high_row) <- "CPK high"
    
    # Calculate CPK : min (cpk_low , cpk_high)
   
    cpk_row <- data.frame(
      V1 = NA,  # TimeStamp
      V2 = NA,  # Serial Num
      mapply(function(cpk_low_val, cpk_high_val) {
        if (is.na(cpk_low_val) || is.na(cpk_high_val) ) {
          return(NA)
        }
       
        round(min(cpk_low_val, cpk_high_val), 5)
      },
     
      cpk_low_row[, 3:ncol(cpk_low_row)],
      cpk_high_row[, 3:ncol(cpk_high_row)],
      SIMPLIFY = FALSE)
    )
    
    # Add row names
    row.names(cpk_row) <- "CPK"
    
    
    # Combine all calculations with original data
    rbind(cpk_row, cpk_high_row, cpk_low_row, stdev_row, avg_row, df)
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