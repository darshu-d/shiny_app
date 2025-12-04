#installing libraries 
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(survival)
library(survminer)
library(bslib)
library(shinyWidgets)
library(shinycssloaders)
library(shinythemes)
library(tidyr)
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
  theme = bs_theme(bootswatch = "minty"),
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
    plotlyOutput("agePlot", height = "350px")
),
    
#2nd tab - Patient Data
    tabPanel("Patient Data",
             br(),
             DTOutput("patientTable") %>% withSpinner(type = 7)
),
             
#3rd tab - Survival analysis
    tabPanel("Survival Analysis",
              br(),
              fluidRow(
              column(8, plotlyOutput("kmPlot", height = "400px")),
              column(4, 
                    downloadButton("downloadKM", "Download KM Plot", class = "btn-primary btn-block"),
                    DTOutput("coxTable"),
                    br(), br(),
                    verbatimTextOutput("Survival Summary")
              )
            ),
            br(),
            h4("Risk Table"),
            DTOutput("riskTable")
    ),
#4th Tab - Clinical outcomes
    tabPanel("Clinical Outcomes",
             br(),
             fluidRow(
               column(6, plotlyOutput("biomarkerBoxPlot", height = "350px")),
               column(6, plotlyOutput("creatKScatter", height = "350px"))
             ),
             br(),
             selectInput(
               "outcomeVar", "Outcome",
               choices = c("Hospitalization (any)" = "HOSP",
                           "Cardiovascular death" = "CVD",
                           "Worsening HF"        = "WHF",
                           "All-cause death"     = "DEATH"),
               selected = "DEATH"
             ),
             plotlyOutput("outcomeBar", height = "350px"),
             h4("Clinical Outcomes Summary"),
             DTOutput("outcomeSummary"),
             
             br(),
             h4("Risk Profile Interpretation"),
             verbatimTextOutput("clinicalNote")
      ),
#5th Tab - Risk factors
    tabPanel("Risk Factors",
              br(),
              fluidRow(
                column(6, plotlyOutput("comorbidityPlot", height = "350px")),
                column(6, plotlyOutput("vitalsPlot", height = "350px"))
                ),
            br(),
            h4("Risk Profile Summary"),
            DTOutput("riskFactorsTable")
    )

    )
  )
))

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
output$agePlot <- renderPlotly({
  df <- filtered_data()
  plot <- ggplot(df, aes(x = age_group, fill = TRTMT)) +
    geom_bar(position = "dodge") +
    scale_fill_manual(
      values = c("Placebo" = "#6A5ACD", "Digoxin" = "#F39C12"),
                      drop = TRUE
  ) +
  labs(x = "Age Group", y = "Count", fill = "Treatment",
         title = "Age Group Distribution by Treatment") +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ggplotly(plot)
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
  req(filtered_data())
  
  p <- ggplotly(km_object()$plot, dynamicTicks = TRUE) %>% plotly::plotly_build()
  
  p$x$data <- lapply(p$x$data, function(tr) {
    
    if (!is.null(tr$name)) {
      
      #TRTMT = 0 → Placebo
      if (grepl("Placebo", tr$name, ignore.case = TRUE) ||
          grepl("0", tr$name, ignore.case = TRUE)) {
        tr$name <- "Placebo"
        tr$legendgroup <- "Placebo"
      }
      
      #TRTMT = 1 → Digoxin
      if (grepl("Digoxin", tr$name, ignore.case = TRUE) ||
          grepl("1", tr$name, ignore.case = TRUE)) {
        tr$name <- "Digoxin"
        tr$legendgroup <- "Digoxin"
      }
    }
    
    tr
  })
    p %>%
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
    ggsave(file, plot = km_object()$plot, width = 8, height = 6, dpi = 300) 
  }
)

#COX table

output$coxTable <- renderDT({
  
  df <- filtered_data()
  
  #Ensuring 2 groups
  if (length(unique(df$TRTMT)) < 2) {
    return(datatable(
      data.frame(Message = "Need at least two treatment groups."),
      rownames = FALSE,
      options = list(dom = 't')
    ))
  }
  
  # Cox proportional hazards model
  model <- coxph(Surv(DEATHDAY, DEATH) ~ TRTMT + AGE + SEX, data = df)
  sum_model <- summary(model)
  
  tbl <- as.data.frame(sum_model$coefficients)
  
  tbl$HR        <- round(exp(tbl$coef), 3)
  tbl$Lower_CI  <- round(exp(tbl$coef - 1.96 * tbl$`se(coef)`), 3)
  tbl$Upper_CI  <- round(exp(tbl$coef + 1.96 * tbl$`se(coef)`), 3)
  
  result <- tbl[, c("HR", "Lower_CI", "Upper_CI", "Pr(>|z|)")]
  
  datatable(
    result,
    options = list(
      pageLength = 5,
      autoWidth = TRUE,
      dom = 't'
    ),
    rownames = TRUE
  )
})

#Risk table
output$riskTable <- renderDT({
  
  req(filtered_data())
  
  df <- filtered_data()
  
  risk_df <- df %>%
    group_by(TRTMT) %>%
    summarise(
      N = n(),
      Events = sum(DEATH == 1, na.rm = TRUE),
      Censored = sum(DEATH == 0, na.rm = TRUE),
      Median_Survival = median(DEATHDAY[DEATH == 1], na.rm = TRUE)
    ) %>%
    mutate(
      TRTMT = ifelse(TRTMT == "Placebo", "Placebo", "Digoxin")
    )
  
  datatable(
    risk_df,
    options = list(
      dom = 't',    
      paging = FALSE,   
      ordering = TRUE,  
      autoWidth = TRUE
    ),
    rownames = FALSE
  )
})

#box plot
output$biomarkerBoxPlot <- renderPlotly({
  
  df <- filtered_data()
  
  df_long <- df %>%
    tidyr::pivot_longer(cols = c(CREAT, KLEVEL),
                        names_to = "Biomarker",
                        values_to = "Value")
  
  p <- ggplot(df_long, aes(x = TRTMT, y = Value, fill = TRTMT)) +
    geom_boxplot(alpha = 0.7) +
    scale_fill_manual(values = c("Placebo" = "#6A5ACD",
                                 "Digoxin" = "#F39C12")) +
    facet_wrap(~Biomarker, scales = "free_y") +
    labs(
      x = "Treatment",
      y = "Value",
      fill = "Treatment",
      title = "Creatinine & Potassium by Treatment"
    ) +
    theme_minimal()
  
  ggplotly(p) %>%
    config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = c(
        "zoom2d","pan2d","lasso2d","select2d",
        "hoverClosestCartesian","hoverCompareCartesian",
        "toggleSpikelines"
      ),
      modeBarPosition = "bottom"
    )
})

#scatter plot
output$creatKScatter <- renderPlotly({
  
  df <- filtered_data()
  
  p <- ggplot(df, aes(x = CREAT, y = KLEVEL, color = TRTMT)) +
    geom_point(alpha = 0.6, size = 2) +
    geom_smooth(method = "lm", se = FALSE) +
    scale_color_manual(values = c("Placebo" = "#6A5ACD",
                                  "Digoxin" = "#F39C12")) +
    labs(
      x = "Creatinine",
      y = "Potassium Level",
      color = "Treatment",
      title = "Creatinine vs Potassium (Risk Profile)"
    ) +
    theme_minimal()
  
  ggplotly(p, tooltip = c("x", "y", "color")) %>%
    config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = c(
        "zoom2d",
        "pan2d",
        "lasso2d",
        "select2d",
        "hoverClosestCartesian",
        "hoverCompareCartesian",
        "toggleSpikelines"
      ),
      modeBarPosition = "bottom"  
    )
})

#simple clinical notes
output$clinicalNote <- renderText({
  
  df <- filtered_data()
  
  if(nrow(df) == 0){
    return("No data available for this selection.")
  }
  
  mean_creat_placebo <- mean(df$CREAT[df$TRTMT == "Placebo"], na.rm = TRUE)
  mean_creat_digoxin <- mean(df$CREAT[df$TRTMT == "Digoxin"], na.rm = TRUE)
  
  mean_k_placebo <- mean(df$KLEVEL[df$TRTMT == "Placebo"], na.rm = TRUE)
  mean_k_digoxin <- mean(df$KLEVEL[df$TRTMT == "Digoxin"], na.rm = TRUE)
  
  note <- "• Creatinine and potassium levels appear broadly similar across treatment groups.\n"
  
  if(!is.na(mean_creat_digoxin) && !is.na(mean_creat_placebo)){
    if(mean_creat_digoxin > mean_creat_placebo + 0.1){
      note <- paste0(note, "• Digoxin group shows slightly higher creatinine, indicating possible reduced kidney clearance.\n")
    }
  }
  
  if(!is.na(mean_k_digoxin) && !is.na(mean_k_placebo)){
    if(mean_k_digoxin > mean_k_placebo + 0.1){
      note <- paste0(note, "• Potassium levels are slightly higher in the Digoxin group.\n")
    }
  }
  
  cor_val <- suppressWarnings(cor(df$CREAT, df$KLEVEL, use = "complete.obs"))
  
  if(!is.na(cor_val) && cor_val > 0.3){
    note <- paste0(note, "• Creatinine and potassium show a positive relationship, which may indicate increased clinical risk.\n")
  }
  
  if(nrow(df) < 1000){
    note <- paste0(note, "• Interpretation is based on a filtered subset of patients.\n")
  }
  
  return(note)
})

#Risk factors
output$comorbidityPlot <- renderPlotly({
  df <- filtered_data()
  
  df$TRTMT <- factor(df$TRTMT, levels = c("Placebo", "Digoxin"))
  df$hypertension <- factor(df$HYPERTEN, levels = c(0,1), labels = c("No HTN", "Hypertension"))
  
  p <- ggplot(df, aes(x = hypertension, fill = TRTMT)) +
    geom_bar(position = "dodge", alpha = 0.8) +
    scale_fill_manual(values = c("Placebo" = "#6A5ACD", "Digoxin" = "#F39C12")) +
    labs(title = "Hypertension Prevalence by Treatment", 
         x = "Hypertension Status", y = "Count") +
    theme_minimal(base_size = 12)
  
  ggplotly(p, height = 350) %>% 
    config(displayModeBar = FALSE, displaylogo = FALSE)
})

output$vitalsPlot <- renderPlotly({
  df <- filtered_data()
  
  df$TRTMT <- factor(df$TRTMT, levels = c("Placebo", "Digoxin"))
  
  df_long <- df %>%
    select(TRTMT,SYSBP, DIABP, BMI) %>%
    tidyr::pivot_longer(
      cols = c(SYSBP, DIABP, BMI), 
      names_to = "Vital", 
      values_to = "Value"
    ) %>%
    filter(!is.na(Value))
  
  p <- ggplot(df_long, aes(x = TRTMT, y = Value, fill = TRTMT)) +
    geom_boxplot(alpha = 0.7) +
    facet_wrap(~Vital, scales = "free_y") +
    scale_fill_manual(values = c("Placebo" = "#6A5ACD", "Digoxin" = "#F39C12")) +
    labs(title = "Vital Signs by Treatment", x = "Treatment", y = "Value") +
    theme_minimal(base_size = 12)
  
  ggplotly(p, height = 350) %>% 
    config(displayModeBar = FALSE, displaylogo = FALSE)
})

output$riskFactorsTable <- renderDT({
  df <- filtered_data()
  
  df$TRTMT <- factor(df$TRTMT, levels = c("Placebo", "Digoxin"))
  
  risk_summary <- df %>%
    group_by(TRTMT) %>%
    summarise(
      N = n(),
      `HTN %` = round(mean(HYPERTEN, na.rm = TRUE) * 100, 1),
      `Mean Age` = round(mean(AGE, na.rm = TRUE), 1),
      `Mean BMI` = round(mean(BMI, na.rm = TRUE), 1),
      `Mean SYSBP` = round(mean(SYSBP, na.rm = TRUE), 1),
      `Mean DIABP` = round(mean(DIABP, na.rm = TRUE), 1),
      `Mean Creat` = round(mean(CREAT, na.rm = TRUE), 2),
      `Hosp %` = round(mean(HOSP > 0, na.rm = TRUE) * 100, 1),
      .groups = 'drop'
    ) %>%
    mutate(TRTMT = ifelse(TRTMT == "Placebo", "Placebo", "Digoxin"))
  
  datatable(
    risk_summary,
    options = list(dom = "t", pageLength = 10, scrollX = TRUE),
    rownames = FALSE
  ) %>%
    DT::formatPercentage(2, 1) %>%   
    DT::formatPercentage(9, 1)
})


}

shinyApp(ui = ui, server = server)

#testing

#connected successfully
