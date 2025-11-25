#installing libraries 
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(survival)
library(survminer)

#load dataset
dig_dataset <- read.csv("DIG.csv")

#age definition
dig_dataset$age_group <- cut(
  dig_dataset$AGE,
  breaks = c(18,30,40,50,60,70,80,200),
  labels = c("19-30","31-40","41-50","51-60","61-70","71-80","80+"),
  include.lowest = TRUE
)

#converting Char to fact
dig_dataset <- dig_dataset %>%
  mutate(across(where(is.character), as.factor))

#ui part
ui <- fluidPage(
 titlePanel("Digitalis Investigation Group test"),
  sidebarLayout (
    sidebarPanel(
      selectInput(inputId = "TRTMT", label = "Select Treatment type", choices = c("Digoxin", "Placebo"), multiple = FALSE),
      selectInput(inputId = "Sex", label = "Select patient sex", choices = c("Male", "Female"), multiple = FALSE),
      selectInput(inputId = "age",label = "Select age group",choices = c("19-30", "31-40", "41-50", "51-60", "61-70", "71-80", "80+"),
                  multiple = FALSE)
      ),
    
    mainPanel(
  
    )
  )
)

#server part
server <- function(input, output) {

}

shinyApp(ui = ui, server = server)

#connected successfully
