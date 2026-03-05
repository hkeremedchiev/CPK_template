# ==============================================================================
# LIBRARIES
# ==============================================================================
library(shiny)
library(DT)           
library(ggplot2)    
library(plotly)
library(colorspace) 
library(munsell)    
library(jsonlite)   
library(openxlsx)   

source("modals.R")

# ==============================================================================
# UI DEFINITION
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
      selectizeInput("selected_cols", "Columns to Analyze:", choices = NULL, multiple = TRUE, options = list(plugins = list('remove_button'))),
      actionButton("select_all", "Select All"),
      actionButton("clear_all", "Clear All"),
      hr(),
      textInput("remove_row_list", "Exclude Rows (e.g., 5, 12, 18):", value = ""),
      hr(),
      h4("Value-Based Filtering"),
      selectizeInput("filter_col", "Filter by Column:", choices = NULL),
      textInput("exclude_values", "Exclude specific values (comma separated):", ""),
      hr(),
      checkboxInput("highlight_outliers", "Highlight Outliers", FALSE),
      checkboxInput("gauge_limit", "Limit to 50 Rows", FALSE),
      downloadButton("download_excel", "Export to Excel (.xlsx)", class = "btn-success")
    ),
    
    mainPanel(
      tabsetPanel(
        id = "main_tabs",
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
  
  # 1. Create a reactive value to track the user's choice
  current_view_length <- reactiveVal(100)
  
  # 2. Update that value whenever the user changes the 'Show X entries' dropdown
  # 'table_state' is a special input created by DT when stateSave = TRUE
  observe({
    state <- input$table_state
    if (!is.null(state$length)) {
      current_view_length(state$length)
    }
  })
  
  raw_data <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath, header = FALSE, sep = ";", stringsAsFactors = FALSE)
    df <- cbind(Row_ID = 1:nrow(df), df)
    
    exclude_text <- input$remove_row_list
    if (!is.null(exclude_text) && exclude_text != "") {
      remove_vector <- as.numeric(unlist(regmatches(exclude_text, gregexpr("[0-9]+", exclude_text))))
      if (length(remove_vector) > 0) {
        df <- df[!(df$Row_ID %in% remove_vector), ]
      }
    }
    
    if (!is.null(input$exclude_values) && input$exclude_values != "" && !is.null(input$filter_col)) {
      vals_to_remove <- trimws(unlist(strsplit(input$exclude_values, ",")))
      target_col <- input$filter_col
      keep_rows <- !(as.character(df[[target_col]]) %in% vals_to_remove)
      if (nrow(df) >= 4) { keep_rows[1:4] <- TRUE }
      df <- df[keep_rows, ]
    }
    return(df)
  })
  
  observeEvent(input$file, {
    df <- raw_data()
    all_ids <- names(df)
    all_labels <- as.character(df[4, ])
    names(all_ids) <- all_labels
    analysis_cols <- all_ids[-(1:3)]
    updateSelectizeInput(session, "selected_cols", choices = analysis_cols, selected = analysis_cols)
    updateSelectizeInput(session, "filter_col", choices = all_ids, selected = NULL)
  })
  
  observeEvent(input$select_all, {
    df <- raw_data()
    all_ids <- names(df)[-(1:3)]
    all_labels <- as.character(df[4, -(1:3)])
    names(all_ids) <- all_labels
    updateSelectizeInput(session, "selected_cols", selected = all_ids)
  })
  
  observeEvent(input$clear_all, {
    updateSelectizeInput(session, "selected_cols", selected = character(0))
  })
  
  processed_info <- reactive({
    req(raw_data())
    full_df <- raw_data()
    meta_cols <- names(full_df)[1:3]
    selected_ids <- input$selected_cols
    all_possible_ids <- names(full_df)
    ordered_selection <- all_possible_ids[all_possible_ids %in% selected_ids]
    current_cols <- c(meta_cols, ordered_selection)
    df <- full_df[, current_cols, drop = FALSE]
    header_names <- as.character(df[4, ]) 
    
    if (ncol(df) <= 3) return(list(table = df, stats = list(), headers = header_names, outliers = list()))
    
    raw_measurements <- df[5:nrow(df), ]
    if (input$gauge_limit) { raw_measurements <- head(raw_measurements, 50) }
    
    stats_list <- list()
    outlier_map <- list()
    
    for(i in 4:ncol(df)) {
      col_vals <- suppressWarnings(as.numeric(raw_measurements[, i]))
      active_vals <- col_vals[!is.na(col_vals)]
      
      if(length(active_vals) > 0) {
        med_v <- median(active_vals)
        mad_v <- mad(active_vals, na.rm = TRUE)
        
        # We use a 0.05 floor and 10x multiplier to ignore the 'tight pack' noise
        threshold_spread <- max(mad_v, 0.05)
        
        # Stats for the summary rows
        avg_v <- mean(active_vals)
        sd_v  <- sd(active_vals)
        h_lim <- suppressWarnings(as.numeric(df[1, i]))
        l_lim <- suppressWarnings(as.numeric(df[2, i]))
        cpk_l <- if(!is.na(l_lim) && !is.na(sd_v) && sd_v != 0) (avg_v - l_lim) / (sd_v * 3) else NA
        cpk_h <- if(!is.na(h_lim) && !is.na(sd_v) && sd_v != 0) (h_lim - avg_v) / (sd_v * 3) else NA
        cpk_v <- if(!is.na(cpk_l) || !is.na(cpk_h)) min(cpk_l, cpk_h, na.rm = TRUE) else NA
        stats_list[[i-3]] <- list(avg=avg_v, sd=sd_v, cpk=cpk_v, raw=active_vals, h=h_lim, l=l_lim)
        
        # --- THE FIX ---
        is_outlier <- !is.na(col_vals) & (abs(col_vals - med_v) > (10 * threshold_spread))
        # --- THE CORRECTED MAPPING ---
        is_outlier <- !is.na(col_vals) & (abs(col_vals - med_v) > (10 * threshold_spread))
        
        if(any(is_outlier)) {
          out_rows <- which(is_outlier)
          for(r in out_rows) {
            # JS row = (index in raw_measurements - 1) + 6 offset for summary/header rows
            js_row <- (r - 1) + 6 
            
            # THE CRITICAL ADJUSTMENT:
            # If i-1 was still too far to the right, i-2 will snap it 
            # to the left, onto the Crystal Frequency column.
            col_target <- i - 2
            
            outlier_map[[paste0(js_row, "-", col_target)]] <- TRUE
          }
        }
      } else { 
        stats_list[[i-3]] <- list(avg=NA, sd=NA, cpk=NA, raw=numeric(0), h=NA, l=NA) 
      }
    }
    
    fmt <- function(x) if(is.na(x)) "" else round(x, 4)
    sum_rows <- rbind(
      c("---", "SUMMARY", "CPK", sapply(stats_list, function(x) fmt(x$cpk))),
      c("---", "SUMMARY", "St Dev", sapply(stats_list, function(x) fmt(x$sd))),
      c("---", "SUMMARY", "Average", sapply(stats_list, function(x) fmt(x$avg)))
    )
    
    final_tab <- rbind(
      unname(as.matrix(sum_rows)),
      unname(as.matrix(df[1:2, ])),
      unname(as.matrix(df[4, , drop=FALSE])),
      unname(as.matrix(raw_measurements))
    )
    colnames(final_tab) <- header_names 
    
    return(list(table = final_tab, stats = stats_list, headers = header_names, outliers = outlier_map))
  })
  
  output$download_excel <- downloadHandler(
    filename = function() { paste0("Cpk_Report_", Sys.Date(), ".xlsx") },
    content = function(file) {
      res <- processed_info()
      export_df <- as.data.frame(res$table)
      colnames(export_df) <- res$headers
      wb <- createWorkbook()
      addWorksheet(wb, "Cpk Analysis")
      writeData(wb, "Cpk Analysis", export_df)
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  observeEvent(input$table_cell_clicked, {
    click <- input$table_cell_clicked
    req(click, !is.null(click$col)) 
    if (click$col + 1 >= 4) {
      show_analysis_modal(click, processed_info(), output)
    }
  })
  
  output$table <- renderDT({
    req(processed_info())
    res <- processed_info()
    datatable(
      res$table, 
      selection = "single", 
      extensions = 'FixedColumns',
      options = list(
        scrollX = TRUE, 
        lengthMenu = list(c(10, 25, 50, 100, 200, 500, -1), c('10', '25', '50', '100', '200', '500', 'All')),
        # 3. Use the reactive value here
        pageLength = current_view_length(), 
        stateSave = TRUE,
        stateDuration = -1,
        fixedColumns = list(leftColumns = 3),
        rowCallback = JS(sprintf(
          "function(row, data, index) {
            var outlierEnabled = %s; var outlierMap = %s;
            if (index === 0) {
              for (var i = 3; i < data.length; i++) {
                var val = parseFloat(data[i]);
                if (!isNaN(val)) {
                  if (val < 1.0) { $('td:eq('+i+')', row).css('background-color', '#ff7f7f'); }
                  else if (val <= 1.33) { $('td:eq('+i+')', row).css('background-color', '#ffeb9c'); }
                  else { $('td:eq('+i+')', row).css('background-color', '#c6efce'); }
                }
              }
            }
            if (outlierEnabled && index >= 6) {
              for (var i = 3; i < data.length; i++) {
                if (outlierMap[index + '-' + (i-1)]) {
                  $('td:eq(' + i + ')', row).css({'background-color': '#ffa500', 'color': 'white'});
                }
              }
            }
          }", tolower(input$highlight_outliers), jsonlite::toJSON(res$outliers, auto_unbox = TRUE)
        ))
      )
    )
  }, server = FALSE)
  
  output$summary <- renderPrint({ req(raw_data()); summary(raw_data()) })
}

shinyApp(ui, server)