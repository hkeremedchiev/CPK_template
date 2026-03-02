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

# ==============================================================================
# UI DEFINITION
# ==============================================================================
##' @title Process Capability Analyzer UI
##' @description Defines the layout, sidebar controls, and CSS styling for the Shiny application.
ui <- fluidPage(
  tags$head(tags$style(HTML("
    .dataTables_wrapper { margin-top: 20px; }
    .modal-lg { width: 900px; }
  "))),
  
  titlePanel("Process Capability Analyzer (CSV)"),
  
  sidebarLayout(
    sidebarPanel(
      ##' @section Input Controls
      fileInput("file", "Choose CSV File", accept = c(".csv")),
      
      selectizeInput(
        "selected_cols", "Columns to Analyze:", 
        choices = NULL, multiple = TRUE,
        options = list(plugins = list('remove_button'))
      ),
      
      actionButton("select_all", "Select All"),
      actionButton("clear_all", "Clear All"),
      
      hr(),
      textInput("remove_row_list", "Exclude Rows (e.g., 5, 12, 18):", value = ""),
      helpText("Note: Row numbering follows the 'Row' column in the table."),
      
      hr(),
      checkboxInput("highlight_outliers", "Highlight Outliers", FALSE),
      checkboxInput("gauge_limit", "Limit to 50 Rows", FALSE)
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
##' @title Process Capability Analyzer Server
##' @param input Shiny input object containing UI values.
##' @param output Shiny output object for rendering.
##' @param session Shiny session object.
server <- function(input, output, session) {
  
  # ----------------------------------------------------------------------------
  ##' @name raw_data
  ##' @description Reactive expression to ingest CSV and apply row exclusions.
  ##' @return A dataframe with an added 'Row_ID' column and filtered rows.
  # ----------------------------------------------------------------------------
  raw_data <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath, header = FALSE, sep = ";", stringsAsFactors = FALSE)
    
    # Prepend Row_ID as Column 1 to preserve original record index
    df <- cbind(Row_ID = 1:nrow(df), df)
    
    exclude_text <- input$remove_row_list
    if (!is.null(exclude_text) && exclude_text != "") {
      # Use Regex to extract numeric digits regardless of separators used
      remove_vector <- as.numeric(unlist(regmatches(exclude_text, gregexpr("[0-9]+", exclude_text))))
      if (length(remove_vector) > 0) {
        df <- df[!(df$Row_ID %in% remove_vector), ]
      }
    }
    return(df)
  })
  
  # ----------------------------------------------------------------------------
  ##' @name UI_Observer
  ##' @description Updates the selectizeInput choices based on uploaded file content.
  ##' @details Skips first 3 columns (RowID, Time, Serial) to find measurement data.
  # ----------------------------------------------------------------------------
  observeEvent(input$file, {
    df <- raw_data()
    col_names <- as.character(df[4, ])
    data_cols <- names(df)[-(1:3)] 
    names(data_cols) <- col_names[-(1:3)]
    updateSelectizeInput(session, "selected_cols", choices = data_cols, selected = data_cols)
  })
  
  # ----------------------------------------------------------------------------
  ##' @name data_input
  ##' @description Synchronizes selected columns with their respective labels.
  ##' @return A list containing the filtered dataframe and corresponding labels.
  # ----------------------------------------------------------------------------
  data_input <- reactive({
    req(raw_data())
    df <- raw_data()
    # Labels must include the manually added 'Row Index' to match Column 1
    full_labels <- c("Row Index", as.character(df[4, -1])) 
    
    if (!is.null(input$selected_cols) && length(input$selected_cols) > 0) {
      selected_indices <- sort(match(input$selected_cols, names(df)))
      return(list(
        data = df[, c(1, 2, 3, selected_indices), drop = FALSE],
        labels = full_labels[c(1, 2, 3, selected_indices)]
      ))
    } else {
      return(list(data = df[, 1:3, drop = FALSE], labels = full_labels[1:3]))
    }
  })
  
  # ----------------------------------------------------------------------------
  ##' @name processed_info
  ##' @description Core engine for statistical calculations and table reassembly.
  ##' @details Calculates Mean, SD, and Cpk. Handles the 'dimnames' alignment 
  ##' to prevent re-indexing crashes.
  ##' @return A list containing the final table matrix, stats, and outlier map.
  # ----------------------------------------------------------------------------
  processed_info <- reactive({
    req(data_input())
    bundle <- data_input()
    df <- bundle$data
    header_names <- bundle$labels 
    
    # If no measurements are selected, return metadata only
    if (ncol(df) <= 3) return(list(table = df, stats = list(), headers = header_names, outliers = list()))
    
    raw_measurements <- df[5:nrow(df), ]
    if (input$gauge_limit) { raw_measurements <- head(raw_measurements, 50) }
    
    stats_list <- list()
    outlier_map <- list()
    
    # Loop through measurement columns (Starting at index 4)
    for(i in 4:ncol(df)) {
      col_vals <- suppressWarnings(as.numeric(raw_measurements[, i]))
      active_vals <- col_vals[!is.na(col_vals)]
      
      if(length(active_vals) > 0) {
        avg_v <- mean(active_vals); sd_v <- sd(active_vals)
        h_lim <- suppressWarnings(as.numeric(df[1, i])); l_lim <- suppressWarnings(as.numeric(df[2, i]))
        
        # Cpk Formula: min((USL-avg)/3sd, (avg-LSL)/3sd)
        cpk_l <- if(!is.na(l_lim) && !is.na(sd_v) && sd_v != 0) (avg_v - l_lim) / (sd_v * 3) else NA
        cpk_h <- if(!is.na(h_lim) && !is.na(sd_v) && sd_v != 0) (h_lim - avg_v) / (sd_v * 3) else NA
        cpk_v <- if(!is.na(cpk_l) || !is.na(cpk_h)) min(cpk_l, cpk_h, na.rm = TRUE) else NA
        
        stats_list[[i-3]] <- list(avg=avg_v, sd=sd_v, cpk=cpk_v, raw=active_vals, h=h_lim, l=l_lim)
        
        # Identify critical outlier for highlighting (Value closest to failing limit)
        if(!is.na(cpk_v)) {
          target <- if(is.na(cpk_l) || cpk_l < cpk_h) min(active_vals) else max(active_vals)
          match_idx <- which(suppressWarnings(as.numeric(raw_measurements[, i])) == target)[1]
          if(!is.na(match_idx)) {
            # Offset mapping for JavaScript: 3 summary rows + 2 limits + 1 header
            js_row <- (match_idx - 1) + 6 
            outlier_map[[paste0(js_row, "-", (i-1))]] <- TRUE
          }
        }
      } else { stats_list[[i-3]] <- list(avg=NA, sd=NA, cpk=NA, raw=numeric(0), h=NA, l=NA) }
    }
    
    fmt <- function(x) if(is.na(x)) "" else round(x, 4)
    
    # Generate Summary Rows with length-3 prefix to match metadata width
    sum_rows <- rbind(
      c("---", "SUMMARY", "CPK", sapply(stats_list, function(x) fmt(x$cpk))),
      c("---", "SUMMARY", "St Dev", sapply(stats_list, function(x) fmt(x$sd))),
      c("---", "SUMMARY", "Average", sapply(stats_list, function(x) fmt(x$avg)))
    )
    
    # Assemble final display matrix using unname() to strip coordinate names
    final_tab <- rbind(
      unname(as.matrix(sum_rows)),
      unname(as.matrix(df[1:2, ])),
      unname(as.matrix(df[4, , drop=FALSE])),
      unname(as.matrix(raw_measurements))
    )
    
    colnames(final_tab) <- header_names 
    return(list(table = final_tab, stats = stats_list, headers = header_names, outliers = outlier_map))
  })
  
  # ----------------------------------------------------------------------------
  ##' @name table_output
  ##' @description Renders the DT datatable with custom JavaScript callbacks.
  ##' @details Implements FixedColumns (left 3) and conditional formatting 
  ##' for CPK scores and Outliers.
  # ----------------------------------------------------------------------------
  output$table <- renderDT({
    req(processed_info())
    res <- processed_info()
    datatable(
      res$table, 
      selection = "single", 
      extensions = 'FixedColumns',
      options = list(
        scrollX = TRUE, pageLength = 50, fixedColumns = list(leftColumns = 3),
        autoWidth = FALSE,
        destroy = TRUE,
        rowCallback = JS(sprintf(
          "function(row, data, index) {
            var outlierEnabled = %s;
            var outlierMap = %s;
            // Highlighting for Summary CPK row (Row Index 0)
            if (index === 0) {
              for (var i = 3; i < data.length; i++) {
                var val = parseFloat(data[i]);
                if (!isNaN(val)) {
                  if (val < 1.0) { $('td:eq('+i+')', row).css('background-color', '#ff7f7f'); }
                  else if (val <= 1.33) { $('td:eq('+i+')', row