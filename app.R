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
  # TITLE
  div(style="background:#2C3E50; padding:18px; color:white; 
             font-size:28px; font-weight:bold; text-align:center;
             border-radius:6px; margin-bottom:15px;",
      "Digitalis Investigation Group – Exploratory Dashboard"
  ),
  sidebarLayout (
    sidebarPanel(
      width = 2,h4("Filters"),tags$hr(),
      selectInput("TRTMT", "Treatment", choices = c("All", "Digoxin", "Placebo"), selected = "All"),
      selectInput("Sex", "Sex", choices = c("All", "Male", "Female"), selected = "All"),
      selectInput("Age", "Age Group",choices = c("All", "19-30", "31-40", "41-50", "51-60", "61-70", "71-80", "80+"), selected = "All")
      ),

#main panel
    mainPanel(
      tabsetPanel(
       tabPanel("Overview",
                  br(),
                  
                  fluidRow(
        
        # Total Patients
        column(width = 4,
          div(style="background:#6A5ACD; padding:20px; border-radius:10px; color:white;",
              div(style="font-size:34px; font-weight:bold;", textOutput("totalPatients")),
              div(style="font-size:16px; margin-top:6px;", "Total Patients")
          )
        ),
        
        # Alive Patients
        column(width = 4,
          div(style="background:#019875; padding:20px; border-radius:10px; color:white;",
              div(style="font-size:34px; font-weight:bold;", textOutput("alivePatients")),
              div(style="font-size:16px; margin-top:6px;", "Alive Patients"),
          )
        ),
        
        # Deaths
        column(width = 4,
          div(style="background:#F39C12; padding:20px; border-radius:10px; color:white;",
              div(style="font-size:34px; font-weight:bold;", textOutput("deadPatients")),
              div(style="font-size:16px; margin-top:6px;", "Deaths")
          )
        )
    
    ),
    
    br(),
    h4("Age Group Distribution"),
    plotOutput("agePlot", height = "350px")
    
      ),
    
#2nd tab
    tabPanel("Patient Data",
             br(),
             DTOutput("patientTable")
     )
    )
   )
 )
)
#server part
server <- function(input, output) {
  
  filtered_data <- reactive({
    data <- dig_dataset
    
    # Treatment filter
    if (input$TRTMT != "All") {
      data <- data %>% 
        filter(TRTMT == ifelse(input$TRTMT=="Digoxin", 1, 0))
    }
    
    # Sex filter
    if (input$Sex != "All") {
      data <- data %>% 
        filter(SEX == ifelse(input$Sex=="Male", 1, 2))
    }
    
    # Age group filter
    if (input$Age != "All") {
      data <- data %>% filter(age_group == input$Age)
    }
    
    return(data)
  })

#VALUE BOX NUMBERS
  output$totalPatients <- renderText({ nrow(filtered_data())})
  output$alivePatients <- renderText({ sum(filtered_data()$DEATH == 0, na.rm = TRUE)})
  output$deadPatients  <- renderText({ sum(filtered_data()$DEATH == 1, na.rm = TRUE)})

#DISTRIBUTION PLOT
output$agePlot <- renderPlot({
  filtered_data() %>%
    ggplot(aes(x = age_group, fill = as.factor(TRTMT))) +
    geom_bar(position = "dodge") +
    scale_fill_manual(values = c("#6A5ACD", "#F39C12"),
                      labels = c("Placebo", "Digoxin")) +
    labs(x = "Age Group", y = "Count", fill = "Treatment",
         title = "Age Group Distribution by Treatment") +
    theme_minimal(base_size = 14)
})


#DATA TABLE
output$patientTable <- renderDT({
  datatable(filtered_data(), 
            options = list(pageLength = 10, scrollX = TRUE))
 })
}

shinyApp(ui = ui, server = server)

#connected successfully
