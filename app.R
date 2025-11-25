#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
dig_dataset <- read.csv("DIG.csv")
ui <- fluidPage(
  titlePanel("Digitalis Investigation Group test"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "TRTMT", label = "Select Treatment type", choices = c("Digoxin", "Placebo"), multiple = FALSE),
      selectInput(inputId = "Sex", label = "Select patient sex", choices = c("Male", "Female"), multiple = FALSE)
    ),
    mainPanel(
    )
  )
)


server <- function(input, output) {
  
}

shinyApp(ui = ui, server = server)

#connected successfully 