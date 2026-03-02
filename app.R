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
library(writexl)
library (openssl)

# ==============================================================================
# UI DEFINITION
# ==============================================================================
##' @title Process Capability Analyzer UI
##' @description Defines the layout, sidebar controls, and CSS styling.
ui <- fluidPage(
  tags$head(tags$style(HTML("
    .dataTables_wrapper { margin-top: 20px; }
    .modal-lg { width: 900px; }
  "))),
  
  titlePanel("Process Capability Analyzer (CSV)"),
  
  sidebarLayout(
    sidebarPanel(
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
      checkboxInput("gauge_limit", "Limit to 50 Rows", FALSE),
      downloadButton("download_excel", "Export to Excel (.xlsx)", class = "btn-success")
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
server <- function(input, output, session) {
  
  # ----------------------------------------------------------------------------
  ##' @name raw_data
  ##' @description Ingests CSV and applies numeric row exclusions via Regex.
  # ----------------------------------------------------------------------------
  raw_data <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath, header = FALSE, sep = ";", stringsAsFactors = FALSE)
    
    # Prepend Row_ID as Column 1
    df <- cbind(Row_ID = 1:nrow(df), df)
    
    exclude_text <- input$remove_row_list
    if (!is.null(exclude_text) && exclude_text != "") {
      # Extract digits regardless of separators (comma, space, etc.)
      remove_vector <- as.numeric(unlist(regmatches(exclude_text, gregexpr("[0-9]+", exclude_text))))
      if (length(remove_vector) > 0) {
        df <- df[!(df$Row_ID %in% remove_vector), ]
      }
    }
    return(df)
  })
  
  # ----------------------------------------------------------------------------
  ##' @name UI_Observer
  ##' @description Updates column selection choices after file upload.
  # ----------------------------------------------------------------------------
  observeEvent(input$file, {
    df <- raw_data()
    col_names <- as.character(df[4, ])
    data_cols <- names(df)[-(1:3)] # Skip RowID, Time, Serial
    names(data_cols) <- col_names[-(1:3)]
    updateSelectizeInput(session, "selected_cols", choices = data_cols, selected = data_cols)
  })
  
  # ----------------------------------------------------------------------------
  ##' @name data_input
  ##' @description Pairs selected columns with labels; ensures a 3-column metadata anchor.
  # ----------------------------------------------------------------------------
  data_input <- reactive({
    req(raw_data())
    df <- raw_data()
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
  ##' @description Calculates Cpk and assembles the final table matrix.
  # ----------------------------------------------------------------------------
  processed_info <- reactive({
    req(data_input())
    bundle <- data_input()
    df <- bundle$data
    header_names <- bundle$labels 
    
    if (ncol(df) <= 3) return(list(table = df, stats = list(), headers = header_names, outliers = list()))
    
    raw_measurements <- df[5:nrow(df), ]
    if (input$gauge_limit) { raw_measurements <- head(raw_measurements, 50) }
    
    stats_list <- list()
    outlier_map <- list()
    
    for(i in 4:ncol(df)) {
      col_vals <- suppressWarnings(as.numeric(raw_measurements[, i]))
      active_vals <- col_vals[!is.na(col_vals)]
      
      if(length(active_vals) > 0) {
        avg_v <- mean(active_vals); sd_v <- sd(active_vals)
        h_lim <- suppressWarnings(as.numeric(df[1, i])); l_lim <- suppressWarnings(as.numeric(df[2, i]))
        
        cpk_l <- if(!is.na(l_lim) && !is.na(sd_v) && sd_v != 0) (avg_v - l_lim) / (sd_v * 3) else NA
        cpk_h <- if(!is.na(h_lim) && !is.na(sd_v) && sd_v != 0) (h_lim - avg_v) / (sd_v * 3) else NA
        cpk_v <- if(!is.na(cpk_l) || !is.na(cpk_h)) min(cpk_l, cpk_h, na.rm = TRUE) else NA
        
        stats_list[[i-3]] <- list(avg=avg_v, sd=sd_v, cpk=cpk_v, raw=active_vals, h=h_lim, l=l_lim)
        
        if(!is.na(cpk_v)) {
          target <- if(is.na(cpk_l) || cpk_l < cpk_h) min(active_vals) else max(active_vals)
          match_idx <- which(suppressWarnings(as.numeric(raw_measurements[, i])) == target)[1]
          if(!is.na(match_idx)) {
            js_row <- (match_idx - 1) + 6 
            outlier_map[[paste0(js_row, "-", (i-1))]] <- TRUE
          }
        }
      } else { stats_list[[i-3]] <- list(avg=NA, sd=NA, cpk=NA, raw=numeric(0), h=NA, l=NA) }
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
  
  # ----------------------------------------------------------------------------
  ##' @name excel_export
  ##' @description Exports processed data to Excel with CPK-based color coding.
  ##' @details Production version: removed debug prints for clean console logs.
  # ----------------------------------------------------------------------------
  output$download_excel <- downloadHandler(
    filename = function() {
      paste0("Cpk_Report_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      tryCatch({
        req(processed_info())
        res <- processed_info()
        
        # 1. Prepare Data structure
        export_df <- as.data.frame(res$table)
        colnames(export_df) <- res$headers
        
        # 2. Initialize Workbook
        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "Cpk Analysis")
        openxlsx::writeData(wb, "Cpk Analysis", export_df)
        
        # 3. Define Styles for CPK Row
        style_red    <- openxlsx::createStyle(fgFill = "#ff7f7f")
        style_yellow <- openxlsx::createStyle(fgFill = "#ffeb9c")
        style_green  <- openxlsx::createStyle(fgFill = "#c6efce")
        
        # 4. Apply Conditional Formatting to Summary CPK (Row 2)
        if (ncol(export_df) >= 4) {
          for (col_idx in 4:ncol(export_df)) {
            val <- as.numeric(export_df[1, col_idx])
            if (!is.na(val)) {
              target_style <- if (val < 1.0) style_red else if (val <= 1.33) style_yellow else style_green
              openxlsx::addStyle(wb, "Cpk Analysis", style = target_style, rows = 2, cols = col_idx)
            }
          }
        }
        
        # 5. UI Comfort: Freeze Metadata Columns and Auto-adjust width
        openxlsx::freezePane(wb, "Cpk Analysis", firstActiveRow = 2, firstActiveCol = 4)
        openxlsx::setColWidths(wb, "Cpk Analysis", cols = 1:ncol(export_df), widths = "auto")
        
        # 6. Final Write
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
        
      }, error = function(e) {
        # Silent error in console, but stops the download process gracefully
        stop(safeError(e))
      })
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
  
  # ----------------------------------------------------------------------------
  ##' @name table_render
  ##' @description Renders DT with FixedColumns and JavaScript highlighting.
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
        autoWidth = FALSE, destroy = TRUE,
        rowCallback = JS(sprintf(
          "function(row, data, index) {
            var outlierEnabled = %s;
            var outlierMap = %s;
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

# ==============================================================================
# EXECUTION
# ==============================================================================
shinyApp(ui, server)