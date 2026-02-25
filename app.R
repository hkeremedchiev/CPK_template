library(shiny)
library(DT)
library(ggplot2)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .dataTables_wrapper { margin-top: 20px; }
      .dataTables_scrollBody { overflow-x: auto !important; }
    "))
  ),
  
  titlePanel("CSV Analyzer - Gauge & Parameter Plotting"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose CSV File",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      
      checkboxInput("highlight_outliers", "Highlight Outliers (Closest to Limit)", FALSE),
      checkboxInput("gauge_limit", "Gauge Measurement (Limit to 50 Rows)", FALSE),
      
      hr(),
      helpText("Click on any CPK or Data cell to view the Parameter Histogram.")
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
  
  data_input <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath, header = FALSE, sep = ";", stringsAsFactors = FALSE)
    if (ncol(df) >= 87) { df <- df[, -87] }
    return(df)
  })
  
  processed_info <- reactive({
    df <- data_input()
    header_names <- as.character(unlist(df[4, ]))
    header_names[is.na(header_names) | header_names == ""] <- paste0("Col_", which(is.na(header_names) | header_names == ""))
    
    raw_measurements <- df[5:nrow(df), ]
    if (input$gauge_limit) { raw_measurements <- head(raw_measurements, 50) }
    
    stats_list <- list()
    outlier_map <- list()
    
    for(i in 3:ncol(df)) {
      col_vals <- suppressWarnings(as.numeric(raw_measurements[, i]))
      active_vals <- col_vals[!is.na(col_vals)]
      
      if(length(active_vals) > 0) {
        avg_v <- mean(active_vals, na.rm = TRUE)
        sd_v  <- sd(active_vals, na.rm = TRUE)
        h_lim <- suppressWarnings(as.numeric(df[1, i]))
        l_lim <- suppressWarnings(as.numeric(df[2, i]))
        
        cpk_l <- if(!is.na(l_lim) && !is.na(sd_v) && sd_v != 0) (avg_v - l_lim) / (sd_v * 3) else NA
        cpk_h <- if(!is.na(h_lim) && !is.na(sd_v) && sd_v != 0) (h_lim - avg_v) / (sd_v * 3) else NA
        cpk_v <- if(!is.na(cpk_l) || !is.na(cpk_h)) min(cpk_l, cpk_h, na.rm = TRUE) else NA
        
        stats_list[[i-2]] <- list(avg=avg_v, sd=sd_v, cpkl=cpk_l, cpkh=cpk_h, cpk=cpk_v, raw = active_vals, h = h_lim, l = l_lim)
        
        if(!is.na(cpk_l) && !is.na(cpk_h)) {
          target <- if(cpk_l < cpk_h) min(active_vals, na.rm=TRUE) else max(active_vals, na.rm=TRUE)
          match_row <- which(suppressWarnings(as.numeric(raw_measurements[, i])) == target)[1]
          if(!is.na(match_row)) {
            js_row <- match_row + 8 
            outlier_map[[paste0(js_row, "-", i)]] <- TRUE
          }
        }
      } else {
        stats_list[[i-2]] <- list(avg=NA, sd=NA, cpkl=NA, cpkh=NA, cpk=NA, raw = numeric(0), h=NA, l=NA)
      }
    }
    
    fmt <- function(x) if(is.na(x) || is.infinite(x)) "" else round(x, 4)
    cpk_row  <- c("CPK", "", sapply(stats_list, function(x) fmt(x$cpk)))
    cpkh_row <- c("CPK High", "", sapply(stats_list, function(x) fmt(x$cpkh)))
    cpkl_row <- c("CPK Low", "", sapply(stats_list, function(x) fmt(x$cpkl)))
    sd_row   <- c("St Dev", "", sapply(stats_list, function(x) fmt(x$sd)))
    avg_row  <- c("Average", "", sapply(stats_list, function(x) fmt(x$avg)))
    
    summary_table <- rbind(cpk_row, cpkh_row, cpkl_row, sd_row, avg_row)
    final_tab <- rbind(summary_table, df[1:4, ], raw_measurements)
    colnames(final_tab) <- header_names
    
    return(list(table = final_tab, outliers = outlier_map, stats = stats_list, headers = header_names))
  })
  
  # --- IMPROVED PLOTTING LOGIC ---
  observeEvent(input$table_cell_clicked, {
    info <- input$table_cell_clicked
    # Only trigger if a data column is clicked
    if (is.null(info$value) || info$col < 2) return()
    
    res <- processed_info()
    col_idx_stats <- info$col - 1
    
    # Safety check for index
    if(col_idx_stats > length(res$stats)) return()
    
    col_data <- res$stats[[col_idx_stats]]
    param_name <- res$headers[info$col + 1]
    
    if (length(col_data$raw) > 1) {
      showModal(modalDialog(
        title = paste("Detailed Analysis:", param_name),
        fluidRow(
          column(6, plotOutput("histPlot")),
          column(6, plotOutput("trendPlot"))
        ),
        hr(),
        # Display key stats in the footer area for quick reference
        renderText({
          paste0("Avg: ", round(col_data$avg, 4), " | SD: ", round(col_data$sd, 4), 
                 " | CPK: ", round(col_data$cpk, 4))
        }),
        footer = modalButton("Close"),
        size = "l"
      ))
      
      # 1. Improved Histogram with Dynamic Bins
      output$histPlot <- renderPlot({
        df_plot <- data.frame(val = col_data$raw)
        # Sturges' Rule for bins: log2(n) + 1
        n_bins <- ceiling(log2(length(col_data$raw)) + 1)
        
        p1 <- ggplot(df_plot, aes(x = val)) +
          geom_histogram(aes(y = ..density..), bins = n_bins, fill = "steelblue", color = "white", alpha = 0.6) +
          geom_density(color = "red", size = 1) +
          theme_minimal() +
          labs(title = "Distribution (Histogram)", x = "Value", y = "Density")
        
        # Add Limit Lines
        if(!is.na(col_data$l)) p1 <- p1 + geom_vline(xintercept = col_data$l, color = "darkred", linetype = "dashed", size = 1)
        if(!is.na(col_data$h)) p1 <- p1 + geom_vline(xintercept = col_data$h, color = "darkred", linetype = "dashed", size = 1)
        p1
      })
      
      # 2. Trend Plot (Values over time/sequence)
      output$trendPlot <- renderPlot({
        df_trend <- data.frame(seq = 1:length(col_data$raw), val = col_data$raw)
        
        p2 <- ggplot(df_trend, aes(x = seq, y = val)) +
          geom_line(color = "grey70") +
          geom_point(color = "steelblue", size = 2) +
          theme_minimal() +
          labs(title = "Trend (Sequence)", x = "Sample Number", y = "Measured Value")
        
        if(!is.na(col_data$l)) p2 <- p2 + geom_hline(yintercept = col_data$l, color = "darkred", linetype = "dashed")
        if(!is.na(col_data$h)) p2 <- p2 + geom_hline(yintercept = col_data$h, color = "darkred", linetype = "dashed")
        p2
      })
    }
  })
  
  output$table <- renderDT({
    res <- processed_info()
    my_callback <- JS(sprintf(
      "function(row, data, index) {
        var outlierEnabled = %s;
        var outlierMap = %s;
        if (index === 0) {
          for (var i = 2; i < data.length; i++) {
            var val = parseFloat(data[i]);
            if (!isNaN(val)) {
              if (val < 1.0) { $('td:eq('+i+')', row).css('background-color', '#ff7f7f'); }
              else if (val <= 1.33) { $('td:eq('+i+')', row).css('background-color', '#ffeb9c'); }
              else if (val > 1.33) { $('td:eq('+i+')', row).css('background-color', '#c6efce'); }
              $('td:eq('+i+')', row).css('font-weight', 'bold');
            }
          }
        }
        if (outlierEnabled && index >= 9) {
          for (var i = 2; i < data.length; i++) {
            var key = index + '-' + i;
            if (outlierMap[key]) {
              $('td:eq(' + i + ')', row).css({'background-color': '#ffa500', 'color': 'white', 'font-weight': 'bold'});
            }
          }
        }
      }", tolower(input$highlight_outliers), jsonlite::toJSON(res$outliers, auto_unbox = TRUE)
    ))
    
    datatable(
      res$table,
      selection = "single", # Ensure we can click cells
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

shinyApp(ui, server)