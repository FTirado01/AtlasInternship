library(shiny)
library(dplyr)
library(ggplot2)
#library(DT)

movieTitles = c("Kabhi Khushi Kabhie Gham 2001","Mission Impossible 1996","Sholay 1975","Shree 420 1955")
movieTitles = sort(movieTitles)

shinyApp(
  ui = fluidPage(
    selectInput("variable", "Variable:",
                sort(c("Kabhi Khushi Kabhie Gham 2001","Mission Impossible 1996","Sholay 1975","Shree 420 1955"))),
    #tableOutput("data")
  ),
  server = function(input, output) {
    output$data <- renderTable({
      mtcars[, c("mpg", input$variable), drop = FALSE]
    }, rownames = TRUE)
  }
)
