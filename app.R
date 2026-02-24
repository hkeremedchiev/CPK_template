library(shiny)
library(DT)

#TODO: fix the Excel download button
#TODO: add functionality to limit number of measurements to 50 for Gm
#TODO: add plots for each parameter on demand with button?
#TODO: delete colomn 87 at the end //visual bug?//
#TODO: make the app stand alone

ui <- fluidPage(
  titlePanel("CSV File Viewer - Frozen Columns & Custom Headers"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose CSV File",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      helpText("Note: This app expects row 4 to be the header and data to start on row 5.")
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
  
  # 1. Raw Data Input
  data <- reactive({
    req(input$file)
    # Using stringsAsFactors = FALSE to keep text data clean
    read.csv(input$file$datapath,
             header = FALSE,
             sep = ";",
             stringsAsFactors = FALSE)
  })
  
  # 2. Processing and Calculations
  data_with_avg <- reactive({
    df <- data()
    
    # --- HEADER PREPARATION ---
    # Extract Row 4 for headers
    header_names <- as.character(unlist(df[4, ]))
    
    # Fix: Replace NA or Empty strings in headers to prevent DT errors
    header_names[is.na(header_names) | header_names == ""] <- paste0("Column_", which(is.na(header_names) | header_names == ""))
    
    # --- CALCULATION LOGIC ---
    # Rows 5 onwards are actual numeric data
    data_for_calc <- df[5:nrow(df), ]
    
    # Standard Deviation
    stdev_row <- data.frame(
      V1 = NA, V2 = NA,
      lapply(data_for_calc[, 3:ncol(data_for_calc)], function(col) {
        round(sd(as.numeric(col), na.rm = TRUE), 5)
      })
    )
    row.names(stdev_row) <- "St Dev"
    
    # Average
    avg_row <- data.frame(
      V1 = NA, V2 = NA,
      lapply(data_for_calc[, 3:ncol(data_for_calc)], function(col) {
        round(mean(as.numeric(col), na.rm = TRUE), 5)
      })
    )
    row.names(avg_row) <- "Average"
    
    # CPK Low
    cpk_low_row <- data.frame(
      V1 = NA, V2 = NA,
      mapply(function(low_limit, avg_val, stdev_val) {
        low_limit_num <- as.numeric(low_limit)
        if (is.na(avg_val) || is.na(stdev_val) || is.na(low_limit_num) || stdev_val == 0) return(NA)
        round((avg_val - low_limit_num) / (stdev_val * 3), 5)
      }, as.list(df[2, 3:ncol(df)]), avg_row[, 3:ncol(avg_row)], stdev_row[, 3:ncol(stdev_row)], SIMPLIFY = FALSE)
    )
    row.names(cpk_low_row) <- "CPK low"
    
    # CPK High
    cpk_high_row <- data.frame(
      V1 = NA, V2 = NA,
      mapply(function(high_limit, avg_val, stdev_val) {
        high_limit_num <- as.numeric(high_limit)
        if (is.na(avg_val) || is.na(stdev_val) || is.na(high_limit_num) || stdev_val == 0) return(NA)
        round((high_limit_num - avg_val) / (stdev_val * 3), 5)
      }, as.list(df[1, 3:ncol(df)]), avg_row[, 3:ncol(avg_row)], stdev_row[, 3:ncol(stdev_row)], SIMPLIFY = FALSE)
    )
    row.names(cpk_high_row) <- "CPK high"
    
    # CPK (Min of High and Low)
    cpk_row <- data.frame(
      V1 = NA, V2 = NA,
      mapply(function(low, high) {
        if (is.na(low) || is.na(high)) return(NA)
        min(as.numeric(low), as.numeric(high))
      }, cpk_low_row[, 3:ncol(cpk_low_row)], cpk_high_row[, 3:ncol(cpk_high_row)], SIMPLIFY = FALSE)
    )
    row.names(cpk_row) <- "CPK"
    
    # --- COMBINE EVERYTHING ---
    final_table <- rbind(cpk_row, cpk_high_row, cpk_low_row, stdev_row, avg_row, df)
    
    # Apply the cleaned headers
    colnames(final_table) <- header_names
    
    return(final_table)
  })
  
  # 3. Render Table with Frozen Column
  output$table <- renderDT({
    df_raw <- data_with_avg()
    
    datatable(
      df_raw,
      extensions = c('FixedColumns', 'Buttons'), # 1. Added 'Buttons' here
      rownames = TRUE,
      options = list(
        dom = 'Bfrtip', # 2. 'B' stands for Buttons - this shows the button on the UI
        buttons = list(
          list(
            extend = 'excel',
            text = 'Download Excel', # You can name the button whatever you like
            filename = 'Processed_Data_Report',
            exportOptions = list(modifier = list(page = 'all')) # Exports all pages, not just the visible 10
          )
        ),
        pageLength = 100,
        scrollX = TRUE,
        scrollY = "600px",
        scrollCollapse = TRUE,
        fixedColumns = list(leftColumns = 1),
        # Your working JS RowCallback for CPK coloring
        rowCallback = JS(
          "function(row, data, index) {",
          "  if (data[0] === 'CPK') {",
          "    for (var i = 3; i < data.length; i++) {",
          "      var val = parseFloat(data[i]);",
          "      if (val < 1.0) {",
          "        $('td:eq(' + i + ')', row).css('background-color', '#ff7f7f');",
          "      } else if (val >= 1.0 && val <= 1.33) {",
          "        $('td:eq(' + i + ')', row).css('background-color', '#ffeb9c');",
          "      } else if (val > 1.33) {",
          "        $('td:eq(' + i + ')', row).css('background-color', '#c6efce');",
          "      }",
          "      $('td:eq(' + i + ')', row).css('font-weight', 'bold');",
          "    }",
          "  }",
          "}"
        )
      )
    )
  })
  
  # 4. Summary Output
  output$summary <- renderPrint({
    req(data())
    summary(data())
  })
}

shinyApp(ui, server)