library(shiny)
library(httr)
library(jsonlite)
library(shinyBS)
library(RMySQL)
Rcpp::sourceCpp("bmi_bmr_calculations.cpp")

Sys.setenv(OPENAI_API_KEY = "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")

setClass(
  "UserInput",
  slots = list(
    height = "numeric",
    weight = "numeric",
    gender = "character",
    age = "numeric"
  )
)


setMethod(
  "calculate_bmi",
  signature = "UserInput",
  function(object) {
    return(calculate_bmi_cpp(object@weight, object@height))
  }
)

setMethod(
  "calculate_bmr",
  signature = "UserInput",
  function(object) {
    return(calculate_bmr_cpp(object@weight, object@height, object@age, object@gender))
  }
)

ui <- fluidPage(
  tags$head(
    tags$style(HTML(".header-img { width: 70%; margin-bottom:30px; } "))
  ),
  headerPanel(
    img(src = "logo.png", class = "header-img")
  ),
  sidebarLayout(
    sidebarPanel(
      numericInput("height", "Height (in cm):", value = ""),
      numericInput("weight", "Weight (in kg):", value = ""),
      selectInput("gender", "Gender:", choices = c("", "Male", "Female")),
      numericInput("age", "Age (in years):", value = ""),
      actionButton("calculate", "Calculate"),
    ),
    mainPanel(
      style = "padding-right: 15px;",
      fluidRow(
        column(
          width = 6,
          conditionalPanel(
            condition = "!input.calculate",
            style = "background-color: #f3f3f3; border-radius: 10px; padding: 10px;",
            textOutput("text_placeholder"),
            br(),
            tags$div(
              style = "text-align: justify;",
              p(
                strong("What is BMI?"), br(),
                "The BMI (Body Mass Index) calculator gives everyone a quick and convenient way to calculate their own body mass index. How to calculate BMI? You must take into account height in centimeters and body weight in kilograms.", br(),
                br(),
                "Want to know if your weight is correct? The Body Mass Index will help you with this. Using the appropriate formula or using a calculator available, e.g., on our website you will receive a result that will indicate the correct weight, and in the case of incorrect results - thin or obese.", br(),
                br(),
                strong("How to calculate BMI?"), br(),
                "Calculating BMI requires four elements: weight, height, age, and gender. And do you know what your body mass index is? Enter the appropriate data in the BMI calculator, read the calculation result and decide to change your appearance for health reasons with the help of our diet and training plans."
              )
            )
          ),
          conditionalPanel(
            condition = "input.calculate",
            style = "background-color: #f3f3f3; border-radius: 10px; padding: 10px;",
            h3("Your Results:"),
            textOutput("result"),
            h3("Ideal Weight:"),
            textOutput("ideal_weight"),
            h3("BMI Score Table:"),
            tableOutput("bmi_table")
          )
        ),
        column(
          width = 6,
          conditionalPanel(
            condition = "input.calculate",
            style = "background-color: #f3f3f3; border-radius: 10px; padding: 10px;",
            h3("LET'S MAKE A FITNESS PLAN"),
            h4("Fitness Goal:"),
            selectInput("fitness_goal", "Select your fitness goal:", choices = c("Lose Weight", "Build Muscle", "Maintain Weight")),
            br(),
            h4("Frequency preference per week:"),
            selectInput("frequency_preference", "Select your frequency preference per week:", choices = c("1 workout", "2-3 workouts", "4-5 workouts", "6-7 workouts")),
            br(),
            h4("Difficulty Level:"),
            selectInput("difficulty_level", "Select your difficulty level:", choices = c("Beginner", "Normal", "Advanced")),
            br(),
            h4("Body Part Selection:"),
            selectInput("body_part_selection", "Select your body part:", choices = c("Abdomen (Belly)", "Hip", "Arm", "Whole body")),
            br(),
            h4("Do you have any physical problems?"),
            selectInput("physical_problems", "Select an option:", choices = c("Yes", "No")),
            br(),
            actionButton("send_button", "Send")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  user_bmi <- reactiveVal(NULL)  
  
  observeEvent(input$calculate, {
    if (input$height == "" || input$weight == "" || input$gender == "" || input$age == "") {
      output$result <- renderText({
        NULL
      })
      output$bmi_table <- renderTable({
        NULL
      })
      output$ideal_weight <- renderText({
        NULL
      })
    } else {
      my_input <- new("UserInput", height = input$height, weight = input$weight, gender = input$gender, age = input$age)
      my_bmi <- calculate_bmi(my_input)
      my_bmr <- calculate_bmr(my_input)
      
      user_bmi(my_bmi)  
      
      output$result <- renderText({
        paste("Your BMI is", my_bmi, "and your weight status is", get_weight_status(my_bmi), ".")
      })
      output$bmi_table <- renderTable({
        data.frame(
          BMI = c("<18.5", "18.5-24.9", "25-29.9", "30-34.9", "35-39.9", "≥40"),
          "Weight Status" = c("Underweight", "Normal weight", "Overweight", "Obesity class I", "Obesity class II", "Obesity class III")
        )
      })
      output$ideal_weight <- renderText({
        ideal_weight_range <- calculate_ideal_weight(my_input)
        paste("Your ideal weight is between", ideal_weight_range[1], "kg to", ideal_weight_range[2], "kg.")
      })
    }
  })
  
  # Dismiss button event handler
  observeEvent(input$dismiss_modal, {
    # Reset the input values
    updateNumericInput(session, "height", value = "")
    updateNumericInput(session, "weight", value = "")
    updateSelectInput(session, "gender", selected = "")
    updateNumericInput(session, "age", value = "")
    
    # Reload the page
    session$reload()
  })
  
  observeEvent(input$send_button, {
    # Handle the input from the fitness plan section
    bmi_score <- user_bmi()  # Now we can access the BMI here
    fitness_goal <- input$fitness_goal
    frequency_preference <- input$frequency_preference
    difficulty_level <- input$difficulty_level
    body_part_selection <- input$body_part_selection
    physical_problems <- input$physical_problems
    weight_status <- get_weight_status(bmi_score)
    
    # Insert the fitness plan, user input data, and additional fields into MySQL database
    user_input <- list(
      height = input$height,
      weight = input$weight,
      gender = input$gender,
      age = input$age
    )
    
    insert_user_data(user_input, bmi_score, fitness_goal, frequency_preference, difficulty_level, body_part_selection, physical_problems, weight_status)
    
    # Generate the fitness plan using ChatGPT API
    fitness_plan <- generate_fitness_plan(bmi_score, fitness_goal, frequency_preference, difficulty_level, body_part_selection, physical_problems)
    
    
    output$fitnessPlan <- renderUI({
      HTML(markdown::markdownToHTML(text = fitness_plan, fragment.only = TRUE))
    })
    
    showModal(
      modalDialog(
        withTags(
          div(
            style = "max-height: 80vh; overflow-y: auto;",
            h2("Generated Fitness Plan"),
            uiOutput("fitnessPlan")
          )
        ),
        easyClose = TRUE,
        footer = tagList(
          # Dismiss button in the modal footer
          actionButton("dismiss_modal", "Generate a new plan", class = "btn-success")
        )
      )
    )
  })
}


calculate_ideal_weight <- function(user_input) {
  ideal_bmi <- 22
  ideal_weight <- ideal_bmi * ((user_input@height / 100) ^ 2)
  ideal_weight_range <- c(round(0.9 * ideal_weight, 1), round(1.1 * ideal_weight, 1))
  return(ideal_weight_range)
}

get_weight_status <- function(bmi) {
  if (bmi < 18.5) {
    return("Underweight")
  } else if (bmi < 25) {
    return("Normal weight")
  } else if (bmi < 30) {
    return("Overweight")
  } else if (bmi < 35) {
    return("Obese class I")
  } else if (bmi < 40) {
    return("Obese class II")
  } else {
    return("Obese class III")
  }
}

# Function to insert user input data into MySQL database
insert_user_data <- function(user_input, bmi_score, fitness_goal, frequency_preference, difficulty_level, body_part_selection, physical_problems, weight_status) {
  # Connect to the MySQL database
  con <- DBI::dbConnect(RMySQL::MySQL(),
                        dbname = "fitBuddy",
                        host = "127.0.0.1",
                        port = 3306,
                        user = "root",
                        password = "")
  
  # Construct the SQL statement for inserting the user input data
  sql <- paste0("INSERT INTO user_data (height, weight, gender, age, bmi_score, fitness_goal, frequency_preference, difficulty_level, body_part_selection, physical_problems, weight_status) VALUES ('",
                user_input$height, "', '", user_input$weight, "', '", user_input$gender, "', '", user_input$age, "', '", bmi_score, "', '", fitness_goal, "', '",
                frequency_preference, "', '", difficulty_level, "', '", body_part_selection, "', '", physical_problems, "', '", weight_status, "')")
  
  # Execute the SQL statement
  DBI::dbExecute(con, statement = sql)
  
  # Close the database connection
  DBI::dbDisconnect(con)
}



generate_fitness_plan <- function(bmi_score, fitness_goal, frequency_preference, difficulty_level, body_part_selection, physical_problems) {
  prompt <- paste(
    "Given your BMI score of", bmi_score,
    ", your fitness goal is to", fitness_goal,
    ", your frequency preference per week is", frequency_preference,
    ", your difficulty level is", difficulty_level,
    ", your body part selection is", body_part_selection,
    ", and you have", physical_problems,
    "physical problems. Please generate a fitness plan for me."
  )
  
  # Prepare the request payload
  payload <- list(
    prompt = prompt,
    max_tokens = 4000L,
    n = 1L,
    temperature = 0.7
  )
  
  print("Sending request to OpenAI...")
  
  response <- tryCatch(
    {
      httr::POST(
        url = "https://api.openai.com/v1/engines/text-davinci-002/completions",
        httr::add_headers(Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY"))),
        httr::content_type("application/json"),
        body = jsonlite::toJSON(payload, auto_unbox = TRUE),
        encode = "json"
      )
    },
    error = function(e) {
      print("Error during the request:")
      print(e)
      return(NULL)
    }
  )
  
  print("Received response from OpenAI.")
  
  # Check if the request was successful
  if (!is.null(response)) {
    if (httr::status_code(response) == 200) {
      tryCatch({
        result <- jsonlite::fromJSON(httr::content(response, as = "text"), simplifyVector = FALSE)
        if(!is.null(result$choices) && length(result$choices) > 0) {
          fitness_plan <- result$choices[[1]]$text
          return(fitness_plan)
        } else {
          print("No choices found in the API response.")
          return(NULL)
        }
      },
      error = function(e) {
        print("Error parsing the API response:")
        print(e)
        print("API response:")
        print(httr::content(response, as = "text"))
        return(NULL)
      })
    } else {
      # Handle the error case
      print("Error from the API:")
      print(httr::content(response, as = "text"))
      return(NULL)
    }
  }
}



shinyApp(ui, server)
