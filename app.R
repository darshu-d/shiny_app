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
  include.lowest = TRUE)

#converting Char to fact
dig_dataset <- dig_dataset %>%
  mutate(across(where(is.character), as.factor))

#ui part
ui <- fluidPage(
 titlePanel("Digitalis Investigation Group test"),
  sidebarLayout (
    sidebarPanel(
      width = 2,
      selectInput(inputId = "TRTMT", label = "Select Treatment type", choices = c("Digoxin", "Placebo"), multiple = FALSE),
      selectInput(inputId = "Sex", label = "Select patient sex", choices = c("Male", "Female"), multiple = FALSE),
      selectInput(inputId = "age",label = "Select age group",choices = c("19-30", "31-40", "41-50", "51-60", "61-70", "71-80", "80+"),
                  multiple = FALSE)
      ),
    
    mainPanel(
      
  #VALUE BOXES
      fluidRow(
        
        # Total Patients
        column(width = 4,
          div(style="background:#6A5ACD; padding:20px; border-radius:10px; color:white; position:relative; overflow:hidden;",
              div(style="font-size:32px; font-weight:bold;", textOutput("totalPatients")),
              div(style="font-size:14px; margin-top:5px;", "Total Patients"),
              div(style="position:absolute; right:10px; top:10px; font-size:60px; opacity:0.3;","👥")
          )
        ),
        
        # Alive Patients
        column(width = 4,
          div(style="background:#019875; padding:20px; border-radius:10px; color:white; position:relative; overflow:hidden;",
              div(style="font-size:32px; font-weight:bold;", textOutput("alivePatients")),
              div(style="font-size:14px; margin-top:5px;", "Alive Patients"),
              div(style="position:absolute; right:10px; top:10px; font-size:60px; opacity:0.3;","❤️")
          )
        ),
        
        # Deaths
        column(width = 4,
          div(style="background:#F39C12; padding:20px; border-radius:10px; color:white; position:relative;overflow:hidden;",
              div(style="font-size:32px; font-weight:bold;", textOutput("deadPatients")),
              div(style="font-size:14px; margin-top:5px;", "Deaths"),
              div(style="position:absolute;right:10px; top:10px; font-size:60px; opacity:0.3;", "⚰️")
          )
        )
      )
    )
  ) 
)


#server part
server <- function(input, output) {

  #VALUE BOX NUMBERS
  output$totalPatients <- renderText({ nrow(dig_dataset) })
  output$alivePatients <- renderText({ sum(dig_dataset$DEATH == 0, na.rm = TRUE) })
  output$deadPatients  <- renderText({ sum(dig_dataset$DEATH == 1, na.rm = TRUE) })
}

shinyApp(ui = ui, server = server)

#connected successfully
