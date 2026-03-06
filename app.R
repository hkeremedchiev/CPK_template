library(pkgload)
load_all(".")
shinyApp(ui = ui, server = server)