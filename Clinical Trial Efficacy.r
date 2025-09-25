
#load packages
library(readr)
library(dplyr)
library(ggplot2)
library(shiny)
library(shinythemes)
library(stringr)

#Read data
df<-read_csv("efficacy_df.csv")
glimpse(df)

#List of colnames
colnames(df)

print(unique(df$p_value))

# Check for missing values in key columns
summary(df$trial_phase)
summary(df$enrollment_num)
summary(df$completion_year)
summary(df$outcome_type)
summary(df$study_type)

table(df$outcome_type, useNA = "ifany")
head(df$outcome_title, 10)


# Define UI
ui <- fluidPage(
  theme = shinytheme("journal"),
  
  tags$style(HTML("
    body {
      background-color: lightgrey;
    }
  ")),
  
  titlePanel("Clinical Trial Efficacy Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_type", "Select Outcome Type:",
                  choices = na.omit(unique(df$outcome_type)),
                  selected = na.omit(unique(df$outcome_type))[1]),
      
      selectInput("selected_phase", "Select Trial Phase:",
                  choices = na.omit(unique(df$trial_phase)),
                  selected = na.omit(unique(df$trial_phase))[1]),
      br(),
      tags$div(style = "display: flex; justify-content: center;",
               tableOutput("summaryStats") )
      
               ),
    
    mainPanel(
      plotOutput("barPlot", height="600px"),
      br(),
      plotOutput("enrollmentHist"),
      br(),
    
      tableOutput("outcomeExamples")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  filtered_data <- reactive({
    df %>% filter(outcome_type == input$selected_type,
                  trial_phase == input$selected_phase
                  )
  })
  
  output$barPlot <- renderPlot({
    top_outcomes <- filtered_data() %>%
      count(outcome_title, sort = TRUE) %>%
      mutate(outcome_title = str_wrap(outcome_title, width = 40)) %>%
      head(10)
      
      n_outcomes <-nrow(top_outcomes)
      
      ggplot(top_outcomes, aes(x = reorder(outcome_title, n), y = n)) +
      geom_bar(stat = "identity", fill = "deeppink4") +
      coord_flip() +
        labs(
          title = paste0("Top ", n_outcomes, " ", input$selected_type, 
                         " Outcome", ifelse(n_outcomes > 1, "s", "")),
          x = "Outcome Title",
          y = "Count"
          ) +
        theme_minimal() +
        theme(             # additional custom tweaks
        plot.margin = margin(15, 15, 15, 15),
        #panel.border = element_rect(color = "black", fill = NA, size = 1),
        axis.text.y = element_text(size = 10), 
        )
      
  })
  
  # Histogram of enrollment numbers
  output$enrollmentHist <- renderPlot({
    ggplot(filtered_data(), aes(x = enrollment_num)) +
      geom_histogram(bins = 100, fill = "#E69F00") +
      labs(title = "Enrollment Distribution",
           x = "Enrollment Number", y = "Count") +
      theme_minimal()
  })
  
  
  output$summaryStats <- renderTable({
    d <- filtered_data()
    
    data.frame(
      Metric = c("Total Trials", "Avg Enrollment", "Min Enrollment", "Max Enrollment"),
      Value = c(
        nrow(d),
        as.integer(mean(d$enrollment_num, na.rm = TRUE)),
        as.integer(min(d$enrollment_num, na.rm = TRUE)),
        as.integer(max(d$enrollment_num, na.rm = TRUE))
      )
    )
  })
  
  
  
  output$outcomeExamples <- renderTable({
    d <- filtered_data()
    data.frame(Example_Outcomes = head(d$outcome_title, 5))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

