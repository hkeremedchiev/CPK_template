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

# ==============================================================================
# UI DEFINITION
# ==============================================================================
##' @title Process Capability Analyzer UI
##' @description Defines the layout, sidebar controls, and CSS styling.
##' @export
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
##' @name server
##' @description Handles the backend logic for data processing and export.
##' @export
server <- function(input, output, session) {
  
  # ----------------------------------------------------------------------------
  ##' @section Data Ingestion:
  # ----------------------------------------------------------------------------
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
    return(df)
  })
  
  # ----------------------------------------------------------------------------
  ##' @section Selection Observers:
  # ----------------------------------------------------------------------------
  observeEvent(input$file, {
    df <- raw_data()
    all_ids <- names(df)[-(1:3)]
    all_labels <- as.character(df[4, -(1:3)])
    names(all_ids) <- all_labels
    updateSelectizeInput(session, "selected_cols", choices = all_ids, selected = all_ids)
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
  
  # ----------------------------------------------------------------------------
  ##' @section Processing Logic:
  # ----------------------------------------------------------------------------
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
    
    if (ncol(df) <= 3) {
      return(list(table = df, stats = list(), headers = header_names, outliers = list()))
    }
    
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
  
  # ----------------------------------------------------------------------------
  ##' @section Excel Export:
  # ----------------------------------------------------------------------------
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
  
  # ----------------------------------------------------------------------------
  ##' @section Table Rendering:
  # ----------------------------------------------------------------------------
  output$table <- renderDT({
    req(processed_info())
    res <- processed_info()
    datatable(
      res$table, 
      selection = "single", 
      extensions = 'FixedColumns',
      options = list(
        scrollX = TRUE, 
        pageLength = 50, 
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

# ==============================================================================
# EXECUTION
# ==============================================================================
shinyApp(ui, server)