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
      checkboxInput("highlight_outliers", "Show Variability Contributor", value = FALSE),
      helpText("Identifies the single measurement in each column that has the greatest negative impact on Cpk."),
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
    req(raw_data())
    updateSelectizeInput(session, "selected_cols", 
                         choices = names(raw_data()), 
                         selected = names(raw_data()))
  })
  
  raw_data <- reactive({
    req(input$file)
    # Read the raw CSV
    df <- read.csv(input$file$datapath, header = FALSE, sep = ";", stringsAsFactors = FALSE)
    
    # --- FIX 1: PROTECT COLUMN NAMES ---
    # Assign names from Row 4 immediately so the UI 'Columns to Analyze' stays stable
    if (nrow(df) >= 4) {
      colnames(df) <- as.character(df[4, ])
    }
    
    # --- FIX 2: CORRECT ROW DELETION ---
    exclude_text <- input$remove_row_list
    if (!is.null(exclude_text) && exclude_text != "") {
      remove_vector <- as.numeric(unlist(regmatches(exclude_text, gregexpr("[0-9]+", exclude_text))))
      if (length(remove_vector) > 0) {
        # Offset of 4: User types '1' (Measurement 1) -> refers to Physical Row 5
        # User types '2' (Measurement 2) -> refers to Physical Row 6
        physical_remove_vector <- remove_vector + 4
        
        # Security: Don't let the user delete the first 4 rows (Limits/Headers)
        physical_remove_vector <- physical_remove_vector[physical_remove_vector > 4]
        
        if (length(physical_remove_vector) > 0) {
          df <- df[!(seq_len(nrow(df)) %in% physical_remove_vector), ]
        }
      }
    }
    
    # --- VALUE-BASED FILTERING ---
    if (!is.null(input$exclude_values) && input$exclude_values != "" && !is.null(input$filter_col)) {
      vals_to_remove <- trimws(unlist(strsplit(input$exclude_values, ",")))
      target_col <- input$filter_col
      
      # Now that we fixed the names above, this should work reliably
      keep_rows <- !(as.character(df[[target_col]]) %in% vals_to_remove)
      # Always keep the first 4 rows (Limits/Header)
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
    
    # --- 1. COLUMN SELECTION ---
    selected_ids <- input$selected_cols
    all_possible_ids <- names(full_df)
    
    # If nothing is selected, show everything. Otherwise, use selection.
    if (is.null(selected_ids) || length(selected_ids) == 0) {
      df <- full_df
    } else {
      # Keep the original CSV order for the selected columns
      ordered_selection <- all_possible_ids[all_possible_ids %in% selected_ids]
      df <- full_df[, ordered_selection, drop = FALSE]
    }
    
    # Row 4 is the visual header
    header_names <- as.character(df[4, ]) 
    raw_measurements <- df[5:nrow(df), ]
    if (input$gauge_limit) { raw_measurements <- head(raw_measurements, 50) }
    
    stats_list <- list()
    outlier_map <- list()
    
    # --- 2. THE MATH LOOP (1-to-1 Mapping) ---
    for(i in 1:ncol(df)) {
      col_vals <- suppressWarnings(as.numeric(raw_measurements[, i]))
      active_vals <- col_vals[!is.na(col_vals)]
      
      # Check if the column has numeric data for CPK
      if(length(active_vals) > 1) {
        avg_v <- mean(active_vals); sd_v <- sd(active_vals)
        h_lim <- suppressWarnings(as.numeric(df[1, i]))
        l_lim <- suppressWarnings(as.numeric(df[2, i]))
        
        cpk_l <- if(!is.na(l_lim) && sd_v > 0) (avg_v - l_lim) / (sd_v * 3) else NA
        cpk_h <- if(!is.na(h_lim) && sd_v > 0) (h_lim - avg_v) / (sd_v * 3) else NA
        current_cpk <- if(!is.na(cpk_l) || !is.na(cpk_h)) min(cpk_l, cpk_h, na.rm = TRUE) else NA
        
        stats_list[[i]] <- list(avg=avg_v, sd=sd_v, cpk=current_cpk)
        
        # --- CPK DRIVER HIGHLIGHT ---
        if(!is.na(current_cpk)) {
          is_lower_critical <- (!is.na(cpk_l) && (is.na(cpk_h) || cpk_l <= cpk_h))
          target_idx <- if(is_lower_critical) which.min(col_vals) else which.max(col_vals)
          
          if(length(target_idx) > 0) {
            # Row offset: 6 (Summary/Limits/Header)
            # Column offset: i (Row_ID is index 0 in DT)
            outlier_map[[paste0((target_idx[1] - 1) + 6, "-", i)]] <- TRUE
          }
        }
      } else {
        stats_list[[i]] <- list(avg=NA, sd=NA, cpk=NA)
      }
    }

    # --- 3. CLEANED SUMMARY ROWS & LIMIT LABELS ---
    fmt <- function(x) if(is.na(x)) "" else round(x, 4)
    
    # Build stats matrix directly
    sum_rows <- rbind(
      sapply(stats_list, function(x) fmt(x$cpk)),
      sapply(stats_list, function(x) fmt(x$sd)),
      sapply(stats_list, function(x) fmt(x$avg))
    )
    
    # Overwrite the first column of the summary with our clear labels
    sum_rows[, 1] <- c("CPK", "St Dev", "Average")
    
    # Prepare Limit rows and explicitly label them
    limit_rows <- unname(as.matrix(df[1:2, ]))
    limit_rows[1, 1] <- "USL"
    limit_rows[2, 1] <- "LSL"
    
    # --- 4. TABLE ASSEMBLY ---
    meta_row_count <- 6 # 3 Stats + 2 Limits + 1 Header
    data_row_count <- nrow(raw_measurements)
    visual_ids <- c(rep("", meta_row_count), 1:data_row_count)
    
    final_tab <- rbind(
      unname(as.matrix(sum_rows)),            # Rows 0-2 in JS
      limit_rows,                             # Rows 3-4 in JS
      unname(as.matrix(df[4, , drop=FALSE])), # Row 5 (Header)
      unname(as.matrix(raw_measurements))     # Row 6+ (Data)
    )
    
    final_tab <- cbind(Row_ID = visual_ids, final_tab)
    colnames(final_tab) <- c("Row_ID", header_names)
    
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
    var outlierEnabled = %s; 
    var outlierMap = %s;

    // 1. BOLDING & LABEL STYLING
    // Bold the entire top 3 Summary rows (CPK, St Dev, Average)
    if (index < 3) {
      $(row).css('font-weight', 'bold');
    }
    // Bold just the Labels for USL/LSL (Index 3 and 4, first data column td:eq(1))
    if (index === 3 || index === 4) {
      $('td:eq(1)', row).css('font-weight', 'bold');
    }

    // 2. CPK COLOR CODING (Existing Logic)
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

    // 3. OUTLIER / CPK DRIVER HIGHLIGHTING (Existing Logic)
    if (outlierEnabled && index >= 6) {
      for (var i = 3; i < data.length; i++) {
        // Note: keeping your i-1 mapping as it currently works for your data structure
        if (outlierMap[index + '-' + (i-1)]) {
          $('td:eq(' + i + ')', row).css({'background-color': '#ffa500', 'color': 'white', 'font-weight': 'bold'});
        }
      }
    }
  }", 
          tolower(input$highlight_outliers), 
          jsonlite::toJSON(res$outliers, auto_unbox = TRUE)
        ))
      )
    )
  }, server = FALSE)
  
  output$summary <- renderPrint({ req(raw_data()); summary(raw_data()) })
}

shinyApp(ui, server)