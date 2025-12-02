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
  mutate(across(where(is.character), as.factor))%>%
  mutate(TRTMT = factor(TRTMT, levels = c(0,1), labels = c("Placebo", "Digoxin")),
         SEX = factor(SEX, levels =c(1,2), labels = c("Male", "Female")))

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
              div(style="font-size:16px; margin-top:6px;", "Alive Patients")
          )
        ),
        
        # Dead patients
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
    
#2nd tab - Patient Data
    tabPanel("Patient Data",
             br(),
             DTOutput("patientTable"),
),
             
#3rd tab - Survival analysis
    tabPanel("Survival Analysis",
             br(),
             fluidRow(
               column(8, plotlyOutput("kmPlot", height = "400px")),
               column(4, downloadButton("downloadKM", "Download KM Plot", class = "btn-primary btn-block"),
                      br(), br(),
                      verbatimTextOutput("Survival Summary")
                )
             )
          ),
#4th Tab - Clinical outcomes
    tabPanel("Clinical Outcomes",
             br(),
             fluidRow(
               column(6, plotOutput("bpPlot", height = "350px")),
               column(6, plotOutput("biomarkerPlot", height = "350px"))
             )
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
        filter(TRTMT == input$TRTMT)
    }
    
    # Sex filter
    if (input$Sex != "All") {
      data <- data %>% 
        filter(SEX == input$Sex)
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

#AGE DISTRIBUTION PLOT
output$agePlot <- renderPlot({
  df <- filtered_data()
  ggplot(df, aes(x = age_group, fill = TRTMT)) +
    geom_bar(position = "dodge") +
    scale_fill_manual(
      values = c("Placebo" = "#6A5ACD", "Digoxin" = "#F39C12"),
                      drop = TRUE
  ) +
  labs(x = "Age Group", y = "Count", fill = "Treatment",
         title = "Age Group Distribution by Treatment") +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
})


#PATIENT TABLE
output$patientTable <- renderDT({
  datatable(filtered_data() %>%
              select(ID, TRTMT, AGE, SEX, BMI, KLEVEL, CREAT, DEATH, DEATHDAY),
            options = list(pageLength = 10, scrollX = TRUE),
            rownames = FALSE)
 })

#Kaplan-Meier survival plot
km_object <- reactive({
  fit <- survfit(Surv(DEATHDAY, DEATH) ~ TRTMT, data = filtered_data())

  ggsurvplot(fit,
             data = filtered_data(),
             pval = TRUE,
             conf.int = TRUE,
             risk.table = FALSE,
             legend.labs = c("Placebo", "Digoxin"),   
             legend.title = "Treatment",   
             ggtheme = theme_minimal(),
             palette = c("#6A5ACD", "#F39C12")
  )

})

output$kmPlot <- renderPlotly({
  
  ggplotly(km_object()$plot) %>%   
    layout(
      hovermode = "closest",
      xaxis = list(title = "Time (Days)"),
      yaxis = list(title = "Survival Probability"),
      showlegend = TRUE
    ) %>%
    config(displaylogo = FALSE,
           modeBarButtonsToRemove = c(
             "zoom2d",
             "pan2d",
             "lasso2d",
             "select2d",
             "hoverClosestCartesian",   
             "hoverCompareCartesian", 
             "toggleSpikelines"
             )
    )
})

# Download option
output$downloadKM <- downloadHandler(
  filename = function() {
    "KM_plot.png"
  },
  
  content = function(file) {
    ggsave(file, plot = km_object()$plot, width = 8, height = 6, dpi = 300)  # ✔ Correct reactive call
  }
)
}

shinyApp(ui = ui, server = server)

#connected successfully
