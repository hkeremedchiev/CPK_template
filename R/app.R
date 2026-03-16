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

# THE FIX: Smart sourcing that works regardless of Working Directory
source(if(file.exists("modals.R")) "modals.R" else "R/modals.R")
source(if(file.exists("doc_helper.R")) "doc_helper.R" else "R/doc_helper.R")

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
      selectizeInput("selected_cols", "Columns to Analyze:", choices = NULL, multiple = TRUE, 
                     options = list(plugins = list('remove_button'))),
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
      checkboxInput("gauge_mode", "Gauge measurement coloring", value = FALSE),
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
  
  # 1. Load and Filter Raw Data
  raw_data <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath, header = FALSE, sep = ";", stringsAsFactors = FALSE)
    
    if (nrow(df) >= 4) { colnames(df) <- as.character(df[4, ]) }
    
    # Row Deletion Logic
    exclude_text <- input$remove_row_list
    if (!is.null(exclude_text) && exclude_text != "") {
      nums <- as.numeric(unlist(regmatches(exclude_text, gregexpr("[0-9]+", exclude_text))))
      if (length(nums) > 0) {
        p_rows <- nums + 4
        df <- df[!(seq_len(nrow(df)) %in% p_rows[p_rows > 4]), ]
      }
    }
    
    # Value-Based Filtering Logic
    if (!is.null(input$exclude_values) && input$exclude_values != "" && !is.null(input$filter_col)) {
      vals <- trimws(unlist(strsplit(input$exclude_values, ",")))
      if (input$filter_col %in% names(df)) {
        keep <- !(as.character(df[[input$filter_col]]) %in% vals)
        if (nrow(df) >= 4) { keep[1:4] <- TRUE }
        df <- df[keep, ]
      }
    }
    return(df)
  })
  
  # 2. UI Updates (The "Happy Bunny" Sidebar Fix)
  # --- 1. SELECT ALL BUTTON (Strictly Measurements Only) ---
  observeEvent(input$select_all, {
    req(raw_data()) # Ensure a file exists first
    df <- raw_data()
    all_names <- names(df)
    
    if(length(all_names) >= 4) {
      meas_only <- all_names[4:length(all_names)]
      # This forces the sidebar to grab only the measurements
      updateSelectizeInput(session, "selected_cols", selected = meas_only)
    }
  })
  
  observeEvent(input$clear_all, {
    updateSelectizeInput(session, "selected_cols", selected = character(0))
  })
  
  # --- CLEAR ALL BUTTON ---
  observeEvent(input$clear_all, {
    updateSelectizeInput(session, "selected_cols", selected = character(0))
  })
  
  # 3. Process Table Data & Math
  # --- 2. THE TABLE FIREWALL (Prevents Reappearance & Red Error) ---
  # --- 3. THE TABLE FIREWALL (Fixes Ghosts & Red Error) ---
  processed_info <- reactive({
    req(raw_data())
    full_df <- raw_data()
    selected_ids <- input$selected_cols
    
    # SHIELD: If sidebar is empty, return NULL to stop the math loop safely
    if (is.null(selected_ids) || length(selected_ids) == 0) {
      return(NULL) 
    }
    
    # FIREWALL: Strictly only include columns currently tagged in the sidebar
    df <- full_df[, names(full_df) %in% selected_ids, drop = FALSE]
    
    # Safety exit if subsetting resulted in 0 columns
    if (ncol(df) == 0) return(NULL)
    
    header_names <- as.character(df[4, ])
    raw_m <- df[5:nrow(df), ]
    if (input$gauge_limit) { raw_m <- head(raw_m, 50) }
    
    stats_list <- list(); outlier_map <- list()
    
    for(i in 1:ncol(df)) {
      v <- suppressWarnings(as.numeric(raw_m[, i]))
      act <- v[!is.na(v)]
      if(length(act) > 1) {
        avg_v <- mean(act); sd_v <- sd(act)
        h_l <- suppressWarnings(as.numeric(df[1, i])); l_l <- suppressWarnings(as.numeric(df[2, i]))
        cpk_l <- if(!is.na(l_l) && sd_v > 0) (avg_v - l_l) / (sd_v * 3) else NA
        cpk_h <- if(!is.na(h_l) && sd_v > 0) (h_l - avg_v) / (sd_v * 3) else NA
        c_cpk <- if(!is.na(cpk_l) || !is.na(cpk_h)) min(cpk_l, cpk_h, na.rm = TRUE) else NA
        stats_list[[i]] <- list(avg=avg_v, sd=sd_v, cpk=c_cpk)
        if(!is.na(c_cpk) && input$highlight_outliers) {
          low_c <- (!is.na(cpk_l) && (is.na(cpk_h) || cpk_l <= cpk_h))
          t_idx <- if(low_c) which.min(v) else which.max(v)
          outlier_map[[paste0((t_idx[1] - 1) + 6, "-", i)]] <- TRUE
        }
      } else { stats_list[[i]] <- list(avg=NA, sd=NA, cpk=NA) }
    }
    
    # 1. Prepare Summary Rows (CPK, St Dev, Average)
    fmt <- function(x) if(is.na(x)) "" else round(x, 4)
    sum_rows <- rbind(sapply(stats_list, function(x) fmt(x$cpk)),
                      sapply(stats_list, function(x) fmt(x$sd)),
                      sapply(stats_list, function(x) fmt(x$avg)))
    
    # 2. Prepare Limit Rows (USL, LSL)
    lim_rows <- unname(as.matrix(df[1:2, , drop = FALSE]))
    
    # 3. Combine them without labels yet
    math_block <- rbind(sum_rows, lim_rows)
    
    # 4. Create the Row_ID column values
    # These labels go in the VERY FIRST column (Row_ID)
    v_ids <- c("CPK", "St Dev", "Average", "USL", "LSL", "Header", 1:nrow(raw_m))
    
    # 5. Build the final table: Row_ID labels + Data columns
    # We rbind the math, the header (row 4), and the raw measurements
    data_block <- rbind(math_block, unname(as.matrix(df[4, , drop=F])), unname(as.matrix(raw_m)))
    final_tab <- cbind(Row_ID = v_ids, data_block)
    
    colnames(final_tab) <- c("Row_ID", header_names)
    
    return(list(table = final_tab, stats = stats_list, headers = header_names, outliers = outlier_map))
  })
  
  # --- 1. POPULATE ON UPLOAD ---
  observeEvent(input$file, {
    df <- raw_data()
    all_names <- names(df)
    
    if (length(all_names) >= 4) {
      # Measurements start at Index 4
      meas_cols <- all_names[4:length(all_names)]
      
      # FIX 1: Populate the main 'Columns to Analyze' picker
      updateSelectizeInput(session, "selected_cols", 
                           choices = all_names, 
                           selected = meas_cols)
      
      # FIX 2: This line brings back your "Filter by Column" dropdown!
      updateSelectizeInput(session, "filter_col", choices = all_names)
    }
  })
  
  # 4. Render the Data Table
  output$table <- renderDT({
    res <- processed_info()
    if (is.null(res)) {
      return(datatable(matrix(nrow = 0, ncol = 0), 
                       caption = "Please select columns from the sidebar to begin analysis."))
    }
    
    datatable(res$table, selection = "single", extensions = 'FixedColumns',
              options = list(
                scrollX = TRUE, 
                stateSave = TRUE,
                pageLength = 100, 
                lengthMenu = list(c(10, 25, 50, 100, 500, -1), 
                                  c('10', '25', '50', '100', '500', 'All')),
                fixedColumns = list(leftColumns = 1),
                rowCallback = JS(sprintf("function(row, data, index) { 
                   var outE = %s; var outM = %s; var gaugeMode = %s;
                   
                   if (index < 5) { $(row).css('font-weight', 'bold'); }
                   
                   // CPK COLORING LOGIC (Row Index 0)
                   if (index === 0) { 
                     for (var i = 1; i < data.length; i++) {
                       var v = parseFloat(data[i]);
                       if (!isNaN(v)) {
                         var low = gaugeMode ? 5.0 : 1.0;
                         var high = gaugeMode ? 8.0 : 1.33;
                         
                         if (v < low) { $('td:eq('+i+')', row).css('background-color', '#ff7f7f'); }
                         else if (v <= high) { $('td:eq('+i+')', row).css('background-color', '#ffeb9c'); }
                         else { $('td:eq('+i+')', row).css('background-color', '#c6efce'); }
                       }
                     }
                   }
                   
                   // OUTLIER HIGHLIGHTING
                   if (outE && index >= 6) {
                     for (var i = 1; i < data.length; i++) {
                       if (outM[(index) + '-' + (i-1)]) {
                         $('td:eq(' + i + ')', row).css({'background-color': '#ffa500', 'color': 'white', 'font-weight': 'bold'});
                       }
                     }
                   }
                }", 
                                         tolower(input$highlight_outliers), 
                                         jsonlite::toJSON(res$outliers, auto_unbox = TRUE),
                                         tolower(input$gauge_mode))) # Pass the new checkbox value here
              ))
  }, server = FALSE)
  output$summary <- renderPrint({ req(raw_data()); summary(raw_data()) })
  
  observeEvent(input$table_cell_clicked, {
    click <- input$table_cell_clicked
    req(click, !is.null(click$col)) 
    if (click$col >= 1) { show_analysis_modal(click, processed_info(), output) }
  })
  # --- 5. EXPORT TO EXCEL (Clean Labels & Traffic Lights) ---
  output$download_excel <- downloadHandler(
    filename = function() {
      paste0("CPK_Analysis_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      res <- processed_info()
      if (is.null(res) || is.null(res$table)) {
        showNotification("No data selected! Please pick columns before exporting.", type = "error")
        return()
      }
      
      # 1. Prepare Data Frame
      export_df <- as.data.frame(res$table, stringsAsFactors = FALSE)
      
      # FIX: Remove the "Original_CSV_Row" text from the first header cell
      if (ncol(export_df) > 0) { 
        colnames(export_df)[1] <- "" 
      }
      
      # 2. Create Workbook and add Data
      wb <- createWorkbook()
      addWorksheet(wb, "CPK Analysis")
      writeData(wb, "CPK Analysis", export_df)
      
      # 3. Traffic Light Logic for CPK (Excel Row 2)
      last_col <- ncol(export_df)
      if (last_col > 1) {
        cpk_range <- 2:last_col
        
        # Red < 1.0
        negStyle <- createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
        conditionalFormatting(wb, "CPK Analysis", cols = cpk_range, rows = 2, 
                              rule = "< 1.0", style = negStyle)
        
        # Yellow 1.0 to 1.33
        warnStyle <- createStyle(fontColour = "#9C6500", bgFill = "#FFEB9C")
        conditionalFormatting(wb, "CPK Analysis", cols = cpk_range, rows = 2, 
                              rule = "between", type = "expression",
                              style = warnStyle, expression = c(">= 1.0", "<= 1.33"))
        
        # Green > 1.33
        posStyle <- createStyle(fontColour = "#006100", bgFill = "#C6EFCE")
        conditionalFormatting(wb, "CPK Analysis", cols = cpk_range, rows = 2, 
                              rule = "> 1.33", style = posStyle)
      }
      
      # 4. Bold the Math/Spec labels and headers (Rows 1-7)
      headerStyle <- createStyle(textDecoration = "bold")
      addStyle(wb, "CPK Analysis", style = headerStyle, rows = 1:7, cols = 1:last_col, gridExpand = TRUE)
      
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}

shinyApp(ui, server)