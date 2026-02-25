library(shiny)
library(DT)

ui <- fluidPage(
  # Custom CSS to force the horizontal scrollbar to appear at the top and bottom
  tags$head(
    tags$style(HTML("
      .dataTables_wrapper { overflow-x: auto; }
      .dataTables_scrollBody { overflow-x: auto !important; }
      /* Stick the header so you don't lose track of columns */
      table.dataTable thead th { position: sticky; top: 0; background: white; z-index: 10; }
    "))
  ),
  
  titlePanel("CSV Analyzer - Optimized Navigation"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose CSV File",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      
      checkboxInput("highlight_outliers", "Highlight Outliers (Closest to Limit)", FALSE),
      checkboxInput("gauge_limit", "Gauge Measurement (Limit Gm to 50)", FALSE),
      
      hr(),
      helpText("Scrollbar is now available at the top and bottom of the table container.")
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
    
    stats_list <- list()
    outlier_map <- list()
    
    for(i in 3:ncol(df)) {
      # Use suppressed warnings to stop the 'NAs introduced by coercion' spam
      col_vals <- suppressWarnings(as.numeric(raw_measurements[, i]))
      col_name <- header_names[i]
      
      if (input$gauge_limit && grepl("Gm", col_name, ignore.case = TRUE)) {
        active_vals <- head(col_vals[!is.na(col_vals)], 50)
      } else {
        active_vals <- col_vals[!is.na(col_vals)]
      }
      
      # Safety check: only calculate if we have numeric data
      if(length(active_vals) > 0 && !all(is.na(active_vals))) {
        avg_v <- mean(active_vals, na.rm = TRUE)
        sd_v  <- sd(active_vals, na.rm = TRUE)
        h_lim <- suppressWarnings(as.numeric(df[1, i]))
        l_lim <- suppressWarnings(as.numeric(df[2, i]))
        
        cpk_l <- if(!is.na(l_lim) && !is.na(sd_v) && sd_v != 0) (avg_v - l_lim) / (sd_v * 3) else NA
        cpk_h <- if(!is.na(h_lim) && !is.na(sd_v) && sd_v != 0) (h_lim - avg_v) / (sd_v * 3) else NA
        
        # Fixed the 'min' warning by checking for NAs first
        cpk_v <- NA
        if(!is.na(cpk_l) || !is.na(cpk_h)) {
          cpk_v <- min(cpk_l, cpk_h, na.rm = TRUE)
        }
        
        stats_list[[i-2]] <- list(avg=avg_v, sd=sd_v, cpkl=cpk_l, cpkh=cpk_h, cpk=cpk_v)
        
        if(!is.na(cpk_l) && !is.na(cpk_h) && length(active_vals) > 0) {
          target <- if(cpk_l < cpk_h) min(active_vals, na.rm=TRUE) else max(active_vals, na.rm=TRUE)
          match_row <- which(suppressWarnings(as.numeric(raw_measurements[, i])) == target)[1]
          if(!is.na(match_row)) {
            js_row <- match_row + 8 
            outlier_map[[paste0(js_row, "-", i)]] <- TRUE
          }
        }
      } else {
        stats_list[[i-2]] <- list(avg=NA, sd=NA, cpkl=NA, cpkh=NA, cpk=NA)
      }
    }
    
    # Round results for the table display
    fmt <- function(x) if(is.na(x) || is.infinite(x)) "" else round(x, 4)
    
    cpk_row  <- c("CPK", "", sapply(stats_list, function(x) fmt(x$cpk)))
    cpkh_row <- c("CPK High", "", sapply(stats_list, function(x) fmt(x$cpkh)))
    cpkl_row <- c("CPK Low", "", sapply(stats_list, function(x) fmt(x$cpkl)))
    sd_row   <- c("St Dev", "", sapply(stats_list, function(x) fmt(x$sd)))
    avg_row  <- c("Average", "", sapply(stats_list, function(x) fmt(x$avg)))
    
    summary_table <- rbind(cpk_row, cpkh_row, cpkl_row, sd_row, avg_row)
    colnames(summary_table) <- colnames(df)
    final_tab <- rbind(summary_table, df)
    colnames(final_tab) <- header_names
    
    return(list(table = final_tab, outliers = outlier_map))
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
      extensions = c('FixedColumns', 'Buttons'),
      options = list(
        dom = 'Bfrtip',
        buttons = list(list(extend = 'excel', text = 'Download Excel')),
        pageLength = 100, # Show 100 rows as requested
        scrollX = TRUE,
        fixedColumns = list(leftColumns = 2),
        rowCallback = my_callback
      )
    )
  }, server = FALSE)
  
  output$summary <- renderPrint({ req(data_input()); summary(data_input()) })
}

shinyApp(ui, server)