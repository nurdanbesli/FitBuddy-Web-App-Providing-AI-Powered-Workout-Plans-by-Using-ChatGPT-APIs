#install.packages(c("shiny", "RMySQL", "DBI", "dplyr", "ggplot2", "shinydashboard", "plotly"))
#install.packages("purrr")

library(shiny)
library(RMySQL)
library(DBI)
library(dplyr)
library(ggplot2)
library(shinydashboard)
library(plotly)

# Database connection
con <- dbConnect(RMySQL::MySQL(), 
                 dbname = "fitBuddy", 
                 host = "127.0.0.1", 
                 user = "root", 
                 password = "")

data <- dbReadTable(con, "user_data") 

# Shiny app 
shinyApp(
  ui = dashboardPage(
    dashboardHeader(title = "FitBuddy Dashboard"),
    dashboardSidebar(
      
      selectInput("gender", "Gender:",
                  choices = c("All", unique(data$gender))),
      selectInput("age_range", "Age Range:",
                  choices = c("All", "0-18", "19-25", "25-35", "35-45", "45-55", "60-65", "65-100"),
                  multiple = TRUE),
      selectInput("weight_status", "Weight Status:",
                  choices = c("All", unique(data$weight_status))),
      selectInput("fitness_goal", "Fitness Goal:",
                  choices = c("All", unique(data$fitness_goal))),
      selectInput("frequency_preference", "Workout Frequency:",
                  choices = c("All", unique(data$frequency_preference))),
      selectInput("difficulty_level", "Difficulty Level:",
                  choices = c("All", unique(data$difficulty_level))),
      selectInput("body_part_selection", "Body Part Selection:",
                  choices = c("All", unique(data$body_part_selection))),
      selectInput("physical_problems", "Physical Problems:",
                  choices = c("All", unique(data$physical_problems)))
    ),
    dashboardBody(
      fluidRow(
        box(plotlyOutput("gender_pie_chart"), width = 4),
        box(plotlyOutput("age_histogram"), width = 4),
        box(plotlyOutput("bmi_box_plot"), width = 4)
      ),
      fluidRow(
        box(plotlyOutput("weight_status_histogram"), width = 4),
        box(plotlyOutput("fitness_goal_histogram"), width = 4),
        box(plotlyOutput("frequency_histogram"), width = 4)
      ),
      fluidRow(
        box(plotlyOutput("difficulty_level_histogram"), width = 4),
        box(plotlyOutput("body_part_histogram"), width = 4),
        box(plotlyOutput("physical_problem_pie_chart"), width = 4)
      )
    )
  ),
  
  server = function(input, output) {
    
    filtered_data <- reactive({
      data_temp <- data
      
      if(input$weight_status != "All") {
        data_temp <- data_temp %>% filter(weight_status == input$weight_status)
      }
      
      if(input$gender != "All") {
        data_temp <- data_temp %>% filter(gender == input$gender)
      }
      
      if(!is.null(input$age_range) && "All" %in% input$age_range) {
        input$age_range <- input$age_range[input$age_range != "All"]
      }
      if(!is.null(input$age_range)) {
        data_temp <- data_temp %>% filter(age %in% input$age_range)
      }
      
      if(input$fitness_goal != "All") {
        data_temp <- data_temp %>% filter(fitness_goal == input$fitness_goal)
      }
      
      if(input$frequency_preference != "All") {
        data_temp <- data_temp %>% filter(frequency_preference == input$frequency_preference)
        
      }
      
      if(input$difficulty_level != "All") {
        data_temp <- data_temp %>% filter(difficulty_level == input$difficulty_level)
      }
      
      if(input$body_part_selection != "All") {
        data_temp <- data_temp %>% filter(body_part_selection == input$body_part_selection)
      }
      
      if(input$physical_problems != "All") {
        data_temp <- data_temp %>% filter(physical_problems == input$physical_problems)
      }
      
      data_temp
    })
    
    
    output$gender_pie_chart <- renderPlotly({
      filtered_data() %>%
        count(gender) %>%
        plot_ly(labels = ~gender, values = ~n, type = 'pie') %>%
        layout(title = "Gender")
    })
    
    output$age_histogram <- renderPlotly({
      filtered_data() %>%
        mutate(age_range = cut(age, breaks = c(0, 18, 25, 35, 45, 55, 60, 100))) %>%
        count(age_range) %>%
        mutate(ratio = n / sum(n)) %>%
        plot_ly(x = ~age_range, y = ~n, type = "bar", text = ~paste0(round(ratio * 100, 1), "%")) %>%
        layout(title = "Age Range", xaxis = list(title = ""), yaxis = list(title = ""))
    })
    
    output$bmi_box_plot <- renderPlotly({
      filtered_data() %>%
        filter(bmi_score > quantile(bmi_score, 0.01) & bmi_score < quantile(bmi_score, 0.99)) %>%
        plot_ly(y = ~bmi_score, type = "box") %>%
        layout(title = "BMI Score", xaxis = list(title = ""), yaxis = list(title = ""),
               annotations = list(text = paste("Mean: <b>", round(mean(filtered_data()$bmi_score), 2), "</b>"),
                                  x = 1, y = mean(filtered_data()$bmi_score),
                                  showarrow = FALSE, font = list(weight = "bold")),
               list(text = paste("Median: <b>", median(filtered_data()$bmi_score), "</b>"),
                    x = 1, y = median(filtered_data()$bmi_score),
                    showarrow = FALSE, font = list(weight = "bold")))
    })
    
    output$fitness_goal_histogram <- renderPlotly({
      filtered_data() %>%
        count(fitness_goal) %>%
        mutate(ratio = n / sum(n)) %>%
        plot_ly(x = ~fitness_goal, y = ~n, type = 'bar', orientation = 'v', text = ~paste0(round(ratio * 100, 1), "%")) %>%
        layout(title = "Fitness Goal", xaxis = list(title = ""), yaxis = list(title = ""),
               bargap = 0.1, marker = list(line = list(color = "rgba(0, 0, 0, 1)", width = 1)))
    })
    
    output$frequency_histogram <- renderPlotly({
      filtered_data() %>%
        count(frequency_preference) %>%
        mutate(ratio = n / sum(n)) %>%
        plot_ly(x = ~frequency_preference, y = ~n, type = 'bar', orientation = 'v', text = ~paste0(round(ratio * 100, 1), "%")) %>%
        layout(title = "Workout Frequency", xaxis = list(title = ""), yaxis = list(title = ""),
               bargap = 0.1, marker = list(line = list(color = "rgba(0, 0, 0, 1)", width = 1)))
    })
    
    output$difficulty_level_histogram <- renderPlotly({
      filtered_data() %>%
        count(difficulty_level) %>%
        mutate(ratio = n / sum(n)) %>%
        plot_ly(x = ~difficulty_level, y = ~n, type = 'bar', orientation = 'v', text = ~paste0(round(ratio * 100, 1), "%")) %>%
        layout(title = "Difficulty Level", xaxis = list(title = ""), yaxis = list(title = ""),
               bargap = 0.1, marker = list(line = list(color = "rgba(0, 0, 0, 1)", width = 1)))
    })
    
    output$body_part_histogram <- renderPlotly({
      filtered_data() %>%
        count(body_part_selection) %>%
        mutate(ratio = n / sum(n)) %>%
        plot_ly(x = ~body_part_selection, y = ~n, type = 'bar', text = ~paste0(round(ratio * 100, 1), "%")) %>%
        layout(title = "Body Part Selection", xaxis = list(title = ""), yaxis = list(title = ""))
    })
    
    output$physical_problem_pie_chart <- renderPlotly({
      filtered_data() %>%
        count(physical_problems) %>%
        plot_ly(labels = ~physical_problems, values = ~n, type = 'pie') %>%
        layout(title = "Physical Problems")
    })
    
    output$weight_status_histogram <- renderPlotly({
      filtered_data() %>%
        count(weight_status) %>%
        mutate(ratio = n / sum(n)) %>%
        plot_ly(x = ~weight_status, y = ~n, type = 'bar', text = ~paste0(round(ratio * 100, 1), "%")) %>%
        layout(title = "Weight Status", xaxis = list(title = ""), yaxis = list(title = ""))
    })
  }
)
