# ==============================================================================
# LIBRARIES
# ==============================================================================
library(shiny)
library(DT)        
library(ggplot2)   
library(plotly) 

# ==============================================================================
# USER INTERFACE (UI)
# ==============================================================================
ui <- fluidPage(
  tags$head(tags$style(HTML("
    .dataTables_wrapper { margin-top: 20px; }
    .modal-lg { width: 900px; }
  "))),
  
  titlePanel("Process Capability Analyzer (CSV)"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose CSV File", accept = c(".csv")),
      
      selectizeInput("selected_cols", "Columns to Analyze:", choices = NULL, multiple = TRUE),
      actionButton("select_all", "Select All"),
      actionButton("clear_all", "Clear All"),
      
      hr(),
      # NEW: Row Removal Option
      numericInput("remove_row", "Exclude Specific Data Row (Enter Row #):", value = NULL, min = 5),
      helpText("Note: Row numbering follows the original CSV (Data starts at Row 5)"),
      
      hr(),
      checkboxInput("highlight_outliers", "Highlight Outliers", FALSE),
      checkboxInput("gauge_limit", "Limit to 50 Rows", FALSE),
      
      helpText("Instructions: Rows 1-2: Limits | Row 4: Headers | Row 5+: Data")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Table View", DTOutput("table")),
        tabPanel("Data Summary", verbatimTextOutput("summary"))
      )
    )
  )
)

# ==============================================================================
# SERVER LOGIC
# ==============================================================================
server <- function(input, output, session) {
  
  # 1. RAW DATA LOADING
  raw_data <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath, header = FALSE, sep = ";", stringsAsFactors = FALSE)
    
    # Safety Check: Only remove row if input is NOT empty and NOT NA
    if (!is.null(input$remove_row) && !is.na(input$remove_row)) {
      if (input$remove_row >= 1 && input$remove_row <= nrow(df)) {
        df <- df[-input$remove_row, ]
      }
    }
    return(df)
  })
  
  # 2. FILTERING ENGINE
  data_input <- reactive({
    req(raw_data())
    df <- raw_data()
    if (!is.null(input$selected_cols) && length(input$selected_cols) > 0) {
      # Ensure we always keep TimeStamp/Serial columns
      df <- df[, c(names(df)[1:2], input$selected_cols), drop = FALSE]
    }
    return(df)
  })
  
  # 3. DYNAMIC PICKER UPDATES
  observeEvent(input$file, {
    df <- raw_data()
    # Pull names from Row 4 of the CSV
    col_names <- as.character(df[4, ])
    data_cols <- names(df)[-(1:2)]
    names(data_cols) <- col_names[-(1:2)] # Show friendly names in picker
    updateSelectizeInput(session, "selected_cols", choices = data_cols, selected = data_cols)
  })
  
  observeEvent(input$select_all, {
    df <- raw_data(); data_cols <- names(df)[-(1:2)]
    updateSelectizeInput(session, "selected_cols", selected = data_cols)
  })
  
  observeEvent(input$clear_all, {
    updateSelectizeInput(session, "selected_cols", selected = character(0))
  })
  
  # 4. CALCULATION & TABLE ASSEMBLY
  processed_info <- reactive({
    req(data_input())
    df <- data_input()
    
    # Define Headers from Row 4
    header_names <- as.character(df[4, ])
    
    # Isolate Measurement Data (Row 5+)
    raw_measurements <- df[5:nrow(df), ]
    if (input$gauge_limit) { raw_measurements <- head(raw_measurements, 50) }
    
    stats_list <- list()
    for(i in 3:ncol(df)) {
      col_vals <- suppressWarnings(as.numeric(raw_measurements[, i]))
      active_vals <- col_vals[!is.na(col_vals)]
      
      if(length(active_vals) > 0) {
        avg_v <- mean(active_vals)
        sd_v  <- sd(active_vals)
        h_lim <- suppressWarnings(as.numeric(df[1, i])) 
        l_lim <- suppressWarnings(as.numeric(df[2, i])) 
        
        cpk_l <- if(!is.na(l_lim) && !is.na(sd_v) && sd_v != 0) (avg_v - l_lim) / (sd_v * 3) else NA
        cpk_h <- if(!is.na(h_lim) && !is.na(sd_v) && sd_v != 0) (h_lim - avg_v) / (sd_v * 3) else NA
        cpk_v <- if(!is.na(cpk_l) || !is.na(cpk_h)) min(cpk_l, cpk_h, na.rm = TRUE) else NA
        
        stats_list[[i-2]] <- list(avg=avg_v, sd=sd_v, cpk=cpk_v, raw=active_vals, h=h_lim, l=l_lim)
      } else {
        stats_list[[i-2]] <- list(avg=NA, sd=NA, cpk=NA, raw=numeric(0), h=NA, l=NA)
      }
    }
    
    # Create Summary Rows
    fmt <- function(x) if(is.na(x)) "" else round(x, 4)
    sum_rows <- rbind(
      c("SUMMARY", "CPK", sapply(stats_list, function(x) fmt(x$cpk))),
      c("SUMMARY", "St Dev", sapply(stats_list, function(x) fmt(x$sd))),
      c("SUMMARY", "Average", sapply(stats_list, function(x) fmt(x$avg)))
    )
    
    # Stack: Summary + Limits (Rows 1-2) + Header (Row 4) + Data (Row 5+)
    # We skip Row 3 (often blank/metadata) for a cleaner view
    final_tab <- rbind(sum_rows, df[1:2, ], df[4, ], raw_measurements)
    colnames(final_tab) <- header_names
    
    return(list(table = final_tab, stats = stats_list, headers = header_names))
  })
  
  # 5. RESTORED PLOTTING LOGIC
  observeEvent(input$table_cell_clicked, {
    info <- input$table_cell_clicked
    if (is.null(info$value) || info$col < 2) return()
    
    res <- processed_info()
    col_idx <- info$col - 1 # Offset for stats list
    col_data <- res$stats[[col_idx]]
    
    if (length(col_data$raw) > 1) {
      showModal(modalDialog(
        title = paste("Trend Analysis:", res$headers[info$col + 1]),
        fluidRow(
          column(6, renderPlotly({
            p1 <- ggplot(data.frame(x=col_data$raw), aes(x=x)) +
              geom_histogram(fill="steelblue", color="white") + theme_minimal() +
              geom_vline(xintercept=c(col_data$l, col_data$h), color="red", linetype="dashed")
            ggplotly(p1)
          })),
          column(6, renderPlotly({
            p2 <- ggplot(data.frame(s=1:length(col_data$raw), v=col_data$raw), aes(x=s, y=v)) +
              geom_line() + geom_point() + theme_minimal() +
              geom_hline(yintercept=c(col_data$l, col_data$h), color="red", linetype="dashed")
            ggplotly(p2)
          }))
        ),
        footer = modalButton("Close"), size = "l"
      ))
    }
  })
  
  # 6. RENDER OUTPUTS
  output$table <- renderDT({
    req(processed_info())
    datatable(processed_info()$table, selection = "single", 
              options = list(scrollX = TRUE, pageLength = 50))
  })
  
  output$summary <- renderPrint({ req(data_input()); summary(data_input()) })
}

shinyApp(ui, server)