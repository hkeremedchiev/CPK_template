library(shiny)
library(DT)

ui <- fluidPage(
  titlePanel("CSV File Viewer"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose CSV File",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      hr(),
      checkboxInput("header", "Header", TRUE),
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ",")
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
  
  data <- reactive({
    req(input$file)
    
    read.csv(input$file$datapath,
             header = input$header,
             sep = input$sep)
  })
  
  output$table <- renderDT({
    datatable(data(),
              options = list(pageLength = 10,
                             scrollX = TRUE))
  })
  
  output$summary <- renderPrint({
    summary(data())
  })
}

shinyApp(ui, server)