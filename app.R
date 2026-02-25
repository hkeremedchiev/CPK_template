# ==============================================================================
# LIBRARIES
# ==============================================================================
library(shiny)
library(DT)       # For interactive tables
library(ggplot2)  # For parameter plotting
library(plotly) # New library for interactivity

# ==============================================================================
# USER INTERFACE (UI)
# ==============================================================================
ui <- fluidPage(
  # Custom CSS to manage scrollbar behavior and layout padding
  tags$head(
    tags$style(HTML("
      .dataTables_wrapper { margin-top: 20px; }
      .dataTables_scrollBody { overflow-x: auto !important; }
      .modal-lg { width: 900px; } /* Expand the plot window size */
    "))
  ),
  
  titlePanel("Process Capability Analyzer (CSV)"),
  
  sidebarLayout(
    sidebarPanel(
      # 1. File Upload
      fileInput("file", "Choose CSV File",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      
      # 2. Analysis Toggles
      checkboxInput("highlight_outliers", "Highlight Outliers (Closest to Limit)", FALSE),
      checkboxInput("gauge_limit", "Gauge Measurement (Limit to 50 Rows)", FALSE),
      
      hr(),
      helpText("Instructions:"),
      tags$ul(
        tags$li("Rows 1-2: High/Low Limits"),
        tags$li("Row 4: Headers"),
        tags$li("Row 5+: Measurement Data"),
        tags$li("Click any cell to view Parameter Plots.")
      )
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
server <- function(input, output) {
  
  # --- STEP 1: LOAD RAW DATA ---
  # Only triggers when a new file is uploaded.
  data_input <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath, header = FALSE, sep = ";", stringsAsFactors = FALSE)
    
    # Visual Bug Fix: Remove specific column 87 if it exists in the raw CSV
    if (ncol(df) >= 87) { df <- df[, -87] }
    return(df)
  })
  
  
  # --- STEP 2: CORE CALCULATION ENGINE ---
  # Processes limits, CPK, and prepares the final display table.
  processed_info <- reactive({
    df <- data_input()
    
    # Extract headers and handle empty names
    header_names <- as.character(unlist(df[4, ]))
    header_names[is.na(header_names) | header_names == ""] <- paste0("Col_", which(is.na(header_names) | header_names == ""))
    
    # Separate data from limits/metadata
    raw_measurements <- df[5:nrow(df), ]
    
    # OPTIONAL: Truncate rows if Gauge Mode is active
    if (input$gauge_limit) { raw_measurements <- head(raw_measurements, 50) }
    
    stats_list <- list()
    outlier_map <- list()
    
    # Iterate through data columns (starting from Column 3)
    for(i in 3:ncol(df)) {
      col_vals <- suppressWarnings(as.numeric(raw_measurements[, i]))
      active_vals <- col_vals[!is.na(col_vals)]
      
      if(length(active_vals) > 0) {
        # Calculate Stats
        avg_v <- mean(active_vals, na.rm = TRUE)
        sd_v  <- sd(active_vals, na.rm = TRUE)
        h_lim <- suppressWarnings(as.numeric(df[1, i])) # High Limit Row
        l_lim <- suppressWarnings(as.numeric(df[2, i])) # Low Limit Row
        
        # CPK Math
        cpk_l <- if(!is.na(l_lim) && !is.na(sd_v) && sd_v != 0) (avg_v - l_lim) / (sd_v * 3) else NA
        cpk_h <- if(!is.na(h_lim) && !is.na(sd_v) && sd_v != 0) (h_lim - avg_v) / (sd_v * 3) else NA
        cpk_v <- if(!is.na(cpk_l) || !is.na(cpk_h)) min(cpk_l, cpk_h, na.rm = TRUE) else NA
        
        # Store for plotting later
        stats_list[[i-2]] <- list(avg=avg_v, sd=sd_v, cpkl=cpk_l, cpkh=cpk_h, cpk=cpk_v, 
                                  raw = active_vals, h = h_lim, l = l_lim)
        
        # MAP OUTLIERS: Identify row index for JavaScript highlighting
        if(!is.na(cpk_l) && !is.na(cpk_h)) {
          target <- if(cpk_l < cpk_h) min(active_vals, na.rm=TRUE) else max(active_vals, na.rm=TRUE)
          match_row <- which(suppressWarnings(as.numeric(raw_measurements[, i])) == target)[1]
          if(!is.na(match_row)) {
            # Offset = match_row + 5 (stats) + 4 (meta) - 1 (JS index 0-based) = +8
            js_row <- match_row + 8 
            outlier_map[[paste0(js_row, "-", i)]] <- TRUE
          }
        }
      } else {
        stats_list[[i-2]] <- list(avg=NA, sd=NA, cpkl=NA, cpkh=NA, cpk=NA, raw=numeric(0), h=NA, l=NA)
      }
    }
    
    # Build the 5 calculated summary rows
    fmt <- function(x) if(is.na(x) || is.infinite(x)) "" else round(x, 4)
    cpk_row  <- c("CPK", "", sapply(stats_list, function(x) fmt(x$cpk)))
    cpkh_row <- c("CPK High", "", sapply(stats_list, function(x) fmt(x$cpkh)))
    cpkl_row <- c("CPK Low", "", sapply(stats_list, function(x) fmt(x$cpkl)))
    sd_row   <- c("St Dev", "", sapply(stats_list, function(x) fmt(x$sd)))
    avg_row  <- c("Average", "", sapply(stats_list, function(x) fmt(x$avg)))
    
    # Reassemble final table: Stats -> File Metadata -> Measurements
    summary_table <- rbind(cpk_row, cpkh_row, cpkl_row, sd_row, avg_row)
    final_tab <- rbind(summary_table, df[1:4, ], raw_measurements)
    colnames(final_tab) <- header_names
    
    return(list(table = final_tab, outliers = outlier_map, stats = stats_list, headers = header_names))
  })
  
  
  # --- STEP 3: INTERACTIVE MODAL PLOTTING (PLOTLY) ---
  observeEvent(input$table_cell_clicked, {
    info <- input$table_cell_clicked
    if (is.null(info$value) || info$col < 2) return()
    
    res <- processed_info()
    col_idx_stats <- info$col - 1
    if(col_idx_stats > length(res$stats)) return()
    
    col_data <- res$stats[[col_idx_stats]]
    param_name <- res$headers[info$col + 1]
    
    if (length(col_data$raw) > 1) {
      showModal(modalDialog(
        title = paste("Interactive Analysis:", param_name),
        fluidRow(
          column(6, plotlyOutput("histPlot")), # Changed to plotlyOutput
          column(6, plotlyOutput("trendPlot"))  # Changed to plotlyOutput
        ),
        hr(),
        div(style="text-align: center; font-weight: bold; font-size: 16px;",
            renderText({ paste0("CPK: ", round(col_data$cpk, 4), " | Avg: ", round(col_data$avg, 4)) })),
        footer = modalButton("Close"),
        size = "l"
      ))
      
      # Plot 1: Histogram (Converted to Plotly)
      output$histPlot <- renderPlotly({
        p1 <- ggplot(data.frame(x=col_data$raw), aes(x=x)) +
          geom_histogram(fill="steelblue", color="white", bins=15) +
          theme_minimal() + 
          labs(title="Distribution", x="Value", y="Frequency") +
          geom_vline(xintercept=c(col_data$l, col_data$h), color="red", linetype="dashed")
        
        ggplotly(p1) %>% config(displayModeBar = FALSE) # Clean view, no toolbar until hover
      })
      
      # Plot 2: Trend (Converted to Plotly)
      output$trendPlot <- renderPlotly({
        p2 <- ggplot(data.frame(s=1:length(col_data$raw), v=col_data$raw), aes(x=s, y=v)) +
          geom_line(color="grey") + 
          geom_point(color="steelblue") +
          theme_minimal() + 
          labs(title="Trend Over Time", x="Sample Number", y="Value") +
          geom_hline(yintercept=c(col_data$l, col_data$h), color="red", linetype="dashed")
        
        ggplotly(p2)
      })
    }
  })
  
  # --- STEP 4: RENDER TABLE ---
  output$table <- renderDT({
    res <- processed_info()
    
    # JS Callback handles color logic on the browser side for performance
    my_callback <- JS(sprintf(
      "function(row, data, index) {
        var outlierEnabled = %s;
        var outlierMap = %s;
        
        // 1. Color CPK Summary Row (Row 0)
        if (index === 0) {
          for (var i = 2; i < data.length; i++) {
            var val = parseFloat(data[i]);
            if (!isNaN(val)) {
              if (val < 1.0) { $('td:eq('+i+')', row).css('background-color', '#ff7f7f'); }
              else if (val <= 1.33) { $('td:eq('+i+')', row).css('background-color', '#ffeb9c'); }
              else { $('td:eq('+i+')', row).css('background-color', '#c6efce'); }
            }
          }
        }
        
        // 2. Color Outlier Cells (Data Rows index >= 9)
        if (outlierEnabled && index >= 9) {
          for (var i = 2; i < data.length; i++) {
            if (outlierMap[index + '-' + i]) {
              $('td:eq(' + i + ')', row).css({'background-color': '#ffa500', 'color': 'white'});
            }
          }
        }
      }", tolower(input$highlight_outliers), jsonlite::toJSON(res$outliers, auto_unbox = TRUE)
    ))
    
    datatable(
      res$table,
      selection = "single",
      extensions = c('FixedColumns', 'Buttons'),
      options = list(
        dom = 'Bfrtip',
        buttons = list(list(extend = 'excel', text = 'Download Excel')),
        pageLength = 100,
        scrollX = TRUE,
        scrollY = "600px",
        scrollCollapse = TRUE,
        fixedColumns = list(leftColumns = 2),
        rowCallback = my_callback
      )
    )
  }, server = FALSE)
  
  output$summary <- renderPrint({ req(data_input()); summary(data_input()) })
}

# ==============================================================================
# RUN APP
# ==============================================================================
shinyApp(ui, server)