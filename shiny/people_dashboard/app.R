#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("People Dashboard"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          radioButtons("outcome", "Outcome:", choices = c("Monthly Pay", "Turnover Status", "Overall Job Satisfaction"), selected = "Monthly Pay"),
          radioButtons("gender", "Gender:", choices = c("Male", "Female", "All"), selected = "All"),
          radioButtons("department", "Department:", choices = c("Sales", "Research & Development", "Human Resources", "All"), selected = "All"),
          radioButtons("education", "Field of Education:", choices = c("Life Sciences", "Medical", "Marketing", "Technical Degree", "Human Resources", "Other", "All"), selected = "All"),
          radioButtons("jobrole", "Job Role:", choices = c("Sales Executive", "Research Scientist", "Laboratory Technician", "Manufacturing Director", "Healthcare Representative", "Manager", "Sales Representative", "Research Director", "Human Resources", "All"), selected = "All")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("plot"),
           tableOutput("table")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  # This line reads in the data from where it was exported in the Rmd file. In this initial form, it is unfiltered, with all data points 
  finalshinydata_import_tbl <- readRDS(file = "./finalshinydata.rds")
  
  # This dataset goes through filtering as the user hits radio buttons
  finalshinydata_tbl <- finalshinydata_import_tbl
  
  
    output$plot <- renderPlot({
      # This if statement determines if data is filtered by participant gender. 
      # If users select 'All', no filtering happens - if they select 'Male' or 
      # 'Female', that input is used to filter the Gender column
      if (input$gender != "All") {
        finalshinydata_tbl <- finalshinydata_tbl %>% 
          filter(Gender == input$gender)
      }
      
      # This if statement determines if data is filtered by participant department. 
      # If users select 'All', no filtering happens - if they select another 
      # option, that input is used to filter the Department column
      if (input$department != "All") {
        finalshinydata_tbl <- finalshinydata_tbl %>% 
          filter(Department == input$department)
      }
      
      # This if statement determines if data is filtered by participant education field. 
      # If users select 'All', no filtering happens - if they select another 
      # option, that input is used to filter the EducationField column
      if (input$education != "All") {
        finalshinydata_tbl <- finalshinydata_tbl %>% 
          filter(EducationField == input$education)
      }
      
      # This if statement determines if data is filtered by participant job role. 
      # If users select 'All', no filtering happens - if they select another 
      # option, that input is used to filter the JobRole column
      if (input$jobrole != "All") {
        finalshinydata_tbl <- finalshinydata_tbl %>% 
          filter(JobRole == input$jobrole)
      }
      
      # This if else chain takes the chosen outcome names and puts them in terms
      # of the variable names in the dataset
      if (input$outcome == "Monthly Pay") {
        chosen_outcome <- "MonthlyIncome"
      } else if (input$outcome == "Turnover Status") {
        chosen_outcome <- "Attrition"
      } else if (input$outcome == "Overall Job Satisfaction") {
        chosen_outcome <- "JobSatisfaction"
      }
      
      ggplot(finalshinydata_tbl, aes(x = .data[[chosen_outcome]])) + 
        geom_histogram()
      
      
    })
    
    output$table <- renderTable({
      
      if(input$gender != "All") {
        gender_used <- "Gender"
      } else {
        gender_used <- NULL
      }
      
      if(input$department != "All") {
        department_used <- "Department"
      } else {
        department_used <- NULL
      }
      
      if(input$education != "All") {
        education_used <- "EducationField"
      } else {
        education_used <- NULL
      }
      
      if(input$jobrole != "All") {
        jobrole_used <- "JobRole"
      } else {
        jobrole_used <- NULL
      }
      
      filters_used <- c(gender_used, department_used, education_used, jobrole_used)
      
      # This if else chain takes the chosen outcome names and puts them in terms
      # of the variable names in the dataset
      if (input$outcome == "Monthly Pay") {
        chosen_outcome <- "MonthlyIncome"
      } else if (input$outcome == "Turnover Status") {
        chosen_outcome <- "Attrition"
      } else if (input$outcome == "Overall Job Satisfaction") {
        chosen_outcome <- "JobSatisfaction"
      }
      
      finalshinydata_import_tbl %>% 
        group_by(across(all_of(filters_used))) %>% 
        summarise("Mean" = mean(.data[[chosen_outcome]]), "Standard Deviation" = sd(.data[[chosen_outcome]]))
      
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
