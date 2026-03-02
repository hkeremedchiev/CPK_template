# ==============================================================================
# LIBRARIES
# ==============================================================================
library(shiny)
library(DT)        
library(ggplot2)   
library(plotly)
library(colorspace) # Required for ggplot2 scales in deployment
library(munsell)    # Required for ggplot2 color transformations
library(jsonlite)   # Required for passing outlier maps to JavaScript

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
      
      # Selectize input with 'remove_button' plugin for full functionality
      selectizeInput(
        "selected_cols", "Columns to Analyze:", 
        choices = NULL, multiple = TRUE,
        options = list(plugins = list('remove_button'))
      ),
      
      actionButton("select_all", "Select All"),
      actionButton("clear_all", "Clear All"),
      
      hr(),
      numericInput("remove_row", "Exclude Specific Data Row:", value = NA, min = 5),
      helpText("Note: Row numbering follows original CSV (Data starts at Row 5)"),
      
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
  
  # 1. LOAD RAW DATA
  raw_data <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath, header = FALSE, sep = ";", stringsAsFactors = FALSE)
    
    # Handle user-defined row exclusions
    if (!is.null(input$remove_row) && !is.na(input$remove_row)) {
      if (input$remove_row >= 1 && input$remove_row <= nrow(df)) { 
        df <- df[-input$remove_row, ] 
      }
    }
    return(df)
  })
  
  # 2. UI INITIALIZATION & UPDATES
  observeEvent(input$file, {
    df <- raw_data()
    # Map friendly names from Row 4 to internal column indices (V1, V2...)
    col_names <- as.character(df[4, ])
    data_cols <- names(df)[-(1:2)] # Exclude Timestamp and Serial columns
    names(data_cols) <- col_names[-(1:2)]
    
    updateSelectizeInput(session, "selected_cols", choices = data_cols, selected = data_cols)
  })
  
  observeEvent(input$select_all, {
    df <- raw_data()
    data_cols <- names(df)[-(1:2)]
    updateSelectizeInput(session, "selected_cols", selected = data_cols)
  })
  
  observeEvent(input$clear_all, {
    updateSelectizeInput(session, "selected_cols", selected = character(0))
  })
  
  # 3. SYNCHRONIZED FILTERING
  # Bundles data and labels to ensure they are always processed as a pair
  data_input <- reactive({
    req(raw_data())
    df <- raw_data()
    full_labels <- as.character(df[4, ])
    
    if (!is.null(input$selected_cols) && length(input$selected_cols) > 0) {
      # Find indices and sort to maintain CSV sequence order
      selected_indices <- sort(match(input$selected_cols, names(df)))
      
      return(list(
        data = df[, c(1, 2, selected_indices), drop = FALSE],
        labels = full_labels[c(1, 2, selected_indices)]
      ))
    } else {
      # Default view when nothing is selected
      return(list(data = df[, 1:2, drop = FALSE], labels = full_labels[1:2]))
    }
  })
  
  # 4. CALCULATION & REASSEMBLY ENGINE
  processed_info <- reactive({
    req(data_input())
    bundle <- data_input()
    df <- bundle$data
    header_names <- bundle$labels
    
    # Return early if no data columns are selected
    if (ncol(df) <= 2) return(list(table = df, stats = list(), headers = header_names, outliers = list()))
    
    raw_measurements <- df[5:nrow(df), ]
    if (input$gauge_limit) { raw_measurements <- head(raw_measurements, 50) }
    
    stats_list <- list()
    outlier_map <- list()
    
    # Process each selected measurement column
    for(i in 3:ncol(df)) {
      col_vals <- suppressWarnings(as.numeric(raw_measurements[, i]))
      active_vals <- col_vals[!is.na(col_vals)]
      
      if(length(active_vals) > 0) {
        avg_v <- mean(active_vals); sd_v <- sd(active_vals)
        h_lim <- suppressWarnings(as.numeric(df[1, i])); l_lim <- suppressWarnings(as.numeric(df[2, i]))
        
        # Capability (Cpk) Calculations
        cpk_l <- if(!is.na(l_lim) && !is.na(sd_v) && sd_v != 0) (avg_v - l_lim) / (sd_v * 3) else NA
        cpk_h <- if(!is.na(h_lim) && !is.na(sd_v) && sd_v != 0) (h_lim - avg_v) / (sd_v * 3) else NA
        cpk_v <- if(!is.na(cpk_l) || !is.na(cpk_h)) min(cpk_l, cpk_h, na.rm = TRUE) else NA
        
        stats_list[[i-2]] <- list(avg=avg_v, sd=sd_v, cpk=cpk_v, raw=active_vals, h=h_lim, l=l_lim)
        
        # Identify worst-performing data point for outlier highlighting
        if(!is.na(cpk_v)) {
          target <- if(is.na(cpk_l) || cpk_l < cpk_h) min(active_vals) else max(active_vals)
          match_idx <- which(suppressWarnings(as.numeric(raw_measurements[, i])) == target)[1]
          if(!is.na(match_idx)) {
            # Offset accounts for 3 Summary rows + 2 Limit rows + 1 Header row
            js_row <- (match_idx - 1) + 6 
            outlier_map[[paste0(js_row, "-", (i-1))]] <- TRUE
          }
        }
      } else { stats_list[[i-2]] <- list(avg=NA, sd=NA, cpk=NA, raw=numeric(0), h=NA, l=NA) }
    }
    
    # Format summary row statistics
    fmt <- function(x) if(is.na(x)) "" else round(x, 4)
    sum_rows <- rbind(
      c("SUMMARY", "CPK", sapply(stats_list, function(x) fmt(x$cpk))),
      c("SUMMARY", "St Dev", sapply(stats_list, function(x) fmt(x$sd))),
      c("SUMMARY", "Average", sapply(stats_list, function(x) fmt(x$avg)))
    )
    
    # REASSEMBLY: unname(as.matrix()) prevents 'names do not match' crash during column re-indexing
    final_tab <- rbind(
      unname(as.matrix(sum_rows)),
      unname(as.matrix(df[1:2, ])),
      unname(as.matrix(df[4, , drop=FALSE])),
      unname(as.matrix(raw_measurements))
    )
    
    colnames(final_tab) <- header_names 
    return(list(table = final_tab, stats = stats_list, headers = header_names, outliers = outlier_map))
  })
  
  # 5. DATA TABLE RENDERING
  output$table <- renderDT({
    req(processed_info())
    res <- processed_info()
    
    datatable(
      res$table, 
      selection = "single", 
      extensions = 'FixedColumns',
      options = list(
        scrollX = TRUE, pageLength = 50, fixedColumns = list(leftColumns = 2),
        autoWidth = FALSE,
        destroy = TRUE, # Forces table re-initialization on column change
        rowCallback = JS(sprintf(
          "function(row, data, index) {
            var outlierEnabled = %s;
            var outlierMap = %s;
            // Color CPK row based on quality thresholds
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
            // Highlight specific points contributing to low Cpk
            if (outlierEnabled && index >= 6) {
              for (var i = 2; i < data.length; i++) {
                if (outlierMap[index + '-' + (i-1)]) {
                  $('td:eq(' + i + ')', row).css({'background-color': '#ffa500', 'color': 'white'});
                }
              }
            }
          }", tolower(input$highlight_outliers), jsonlite::toJSON(res$outliers, auto_unbox = TRUE)
        ))
      )
    )
  }, server = FALSE) # Required for the destroy and name-sync logic
  
  # 6. PLOTS (MODAL VIEW)
  observeEvent(input$table_cell_clicked, {
    info <- input$table_cell_clicked
    if (is.null(info$value) || info$col < 2) return()
    res <- processed_info()
    col_idx <- info$col - 1
    if(col_idx > length(res$stats)) return()
    col_data <- res$stats[[col_idx]]
    
    if (length(col_data$raw) > 1) {
      showModal(modalDialog(
        title = paste("Analysis:", res$headers[info$col + 1]),
        fluidRow(
          column(6, renderPlotly({
            p1 <- ggplot(data.frame(x=col_data$raw), aes(x=x)) + 
              geom_histogram(fill="steelblue", color="white") + 
              geom_vline(xintercept=c(col_data$l, col_data$h), color="red", linetype="dashed") + 
              theme_minimal()
            ggplotly(p1)
          })),
          column(6, renderPlotly({
            p2 <- ggplot(data.frame(s=1:length(col_data$raw), v=col_data$raw), aes(x=s, y=v)) + 
              geom_line() + geom_point() +
              geom_hline(yintercept=c(col_data$l, col_data$h), color="red", linetype="dashed") + 
              theme_minimal()
            ggplotly(p2)
          }))
        ), size = "l", easyClose = TRUE
      ))
    }
  })
  
  output$summary <- renderPrint({ req(raw_data()); summary(raw_data()) })
}

shinyApp(ui, server)