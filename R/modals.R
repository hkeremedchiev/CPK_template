
#' Show Detailed Process Analysis Modal
#' 
#' Triggers a popup containing a trend plot with specification limits and 
#' a distribution histogram based on a table cell click.
#'
#' @param click The \code{input$table_cell_clicked} object from DT.
#' @param res The \code{processed_info()} reactive list containing the data matrix.
#' @param output The Shiny output object to render the plots.
#' 
#' @export
show_analysis_modal <- function(click, res, output) {
  # 1. Map Column (DT is 0-indexed)
  col_idx <- click$col + 1 
  
  # 2. Extract Labels and Limits from the fixed row structure
  plot_label <- colnames(res$table)[col_idx]
  h_lim <- suppressWarnings(as.numeric(res$table[4, col_idx])) # HighLimit Row
  l_lim <- suppressWarnings(as.numeric(res$table[5, col_idx])) # LowLimit Row
  
  # 3. Extract Measurement Data (Starting Row 7)
  raw_data <- res$table[7:nrow(res$table), col_idx]
  clean_vals <- as.numeric(gsub("[^0-9.-]", "", raw_data))
  final_vals <- clean_vals[!is.na(clean_vals)]
  
  # Guard against non-numeric columns
  if(length(final_vals) == 0) return(NULL)
  
  # 4. Trigger the UI Overlay
  showModal(modalDialog(
    title = paste("Detailed Analysis:", plot_label),
    size = "l", 
    easyClose = TRUE,
    fluidRow(
      column(8, plotlyOutput("modal_trend", height = "400px")),
      column(4, plotOutput("modal_hist", height = "400px"))
    ),
    footer = modalButton("Close")
  ))
  
  # 5. Render Trend with Limits
  output$modal_trend <- renderPlotly({
    p <- ggplot(data.frame(x = 1:length(final_vals), y = final_vals), aes(x, y)) +
      geom_line(color = "#2c3e50", linewidth = 0.5) +
      geom_point(color = "#3498db", size = 1) +
      theme_minimal() +
      labs(title = "Trend & Specs", x = "Sample Index", y = "Value")
    
    if(!is.na(h_lim)) p <- p + geom_hline(yintercept = h_lim, linetype = "dashed", color = "red", alpha = 0.8)
    if(!is.na(l_lim)) p <- p + geom_hline(yintercept = l_lim, linetype = "dashed", color = "red", alpha = 0.8)
    ggplotly(p)
  })
  
  # 6. Render Histogram with Limits
  output$modal_hist <- renderPlot({
    hist(final_vals, breaks = 15, col = "#3498db", border = "white",
         main = "Distribution", xlab = "Measurement", ylab = "Frequency")
    if(!is.na(h_lim)) abline(v = h_lim, col = "red", lwd = 2, lty = 2)
    if(!is.na(l_lim)) abline(v = l_lim, col = "red", lwd = 2, lty = 2)
  })
}