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
          radioButtons("outcome", "Outcome:", choices = c("Monthly Pay", "Attrition", "Overall Job Satisfaction"), selected = "Monthly Pay"),
          radioButtons("gender", "Gender:", choices = c("Male", "Female", "All"), selected = "All"),
          radioButtons("department", "Department:", choices = c("Sales", "Research & Development", "Human Resources", "All"), selected = "All"),
          radioButtons("education", "Field of Education:", choices = c("Life Sciences", "Medical", "Marketing", "Technical Degree", "Human Resources", "Other", "All"), selected = "All"),
          radioButtons("jobrole", "Job Role:", choices = c("Sales Executive", "Research Scientist", "Laboratory Technician", "Manufacturing Director", "Healthcare Representative", "Manager", "Sales Representative", "Research Director", "Human Resources", "All"), selected = "All")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
