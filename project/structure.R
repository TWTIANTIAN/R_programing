library(shiny)
library(dplyr)
#install.packages('shinydashboard')
library(shinydashboard)
library(gridExtra)
library(grid)
library(ggplot2)
#data processing

#filter selection
gender_options <- c("No Filter", "Female", "Male")
senior_options <- c("No Filter", "is Senior Citizen", "is not Senior Citizen")
relationship_options <- c("No Filter", "has Partner", "Single")
dep_options <- c("No Filter", "has Dependents", "no Dependents")
serv_options <- c("No Filter", "Yes", "No")


ui <- dashboardPage(
  dashboardHeader(title = "Customer Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem("Customer Activity Analysis-HJ", tabName = "page1"),
      menuItem("Customer Order Analysis-HJ", tabName = "page2"),
      menuItem("Conversion Rate-YXC", tabName = "page3"),
      menuItem("Consumer Segmentation-YXC", tabName = "page4"),
      menuItem("Future Order Forecasting-WT", tabName = "page5"),
      menuItem("Customer Forecasting-WT", tabName = "page6")
    )
  ),
  dashboardBody(
    tabItems(
      # Page 1
      tabItem(
        tabName = "page1",
        sidebarLayout(
          sidebarPanel(
            # Add sidebar content for page 1 here
            selectInput("page1_filter", "Page 1 Filter", choices = c("Option 1", "Option 2"), selected = "Option 1")
          ),
          mainPanel(
            h2("Page 1"),
            # Add main panel content for page 1 here
            textOutput("page1_output")
          )
        )
      ),
      # Page 2
      tabItem(
        tabName = "page2",
        sidebarLayout(
          sidebarPanel(
            # Add sidebar content for page 2 here
            selectInput("page2_filter", "Page 2 Filter", choices = c("Option A", "Option B"), selected = "Option A")
          ),
          mainPanel(
            h2("Page 2"),
            # Add main panel content for page 2 here
            textOutput("page2_output")
          )
        )
      ),
      # Page 3
      tabItem(
        tabName = "page3",
        sidebarLayout(
          sidebarPanel(
            # Add sidebar content for page 1 here
            selectInput("page1_filter", "Page 1 Filter", choices = c("Option 1", "Option 2"), selected = "Option 1")
          ),
          mainPanel(
            h2("Page 3"),
            # Add main panel content for page 1 here
            textOutput("page1_output")
          )
        )
      ),
      # Page 4
      tabItem(
        tabName = "page4",
        sidebarLayout(
          sidebarPanel(
            # Add sidebar content for page 2 here
            selectInput("page2_filter", "Page 2 Filter", choices = c("Option A", "Option B"), selected = "Option A")
          ),
          mainPanel(
            h2("Page 4"),
            # Add main panel content for page 2 here
            textOutput("page2_output")
          )
        )
      ),
      # Page 5
      tabItem(
        tabName = "page5",
        sidebarLayout(
          sidebarPanel(
            # Add sidebar content for page 1 here
            selectInput("page1_filter", "Page 1 Filter", choices = c("Option 1", "Option 2"), selected = "Option 1")
          ),
          mainPanel(
            h2("Page 5"),
            # Add main panel content for page 1 here
            textOutput("page1_output")
          )
        )
      ),
      # Page 6
      tabItem(
        tabName = "page6",
        sidebarLayout(
          sidebarPanel(
            # Add sidebar content for page 2 here
            selectInput("page2_filter", "Page 2 Filter", choices = c("Option A", "Option B"), selected = "Option A")
          ),
          mainPanel(
            h2("Page 6"),
            # Add main panel content for page 2 here
            textOutput("page2_output")
          )
        )
      )
    )
  )
)

server <- function(input, output) {
  # Server logic goes here
  
  output$page1_output <- renderText({
    paste("Selected option in Page 1:", input$page1_filter)
  })
  
  output$page2_output <- renderText({
    paste("Selected option in Page 2:", input$page2_filter)
  })
  output$page3_output <- renderText({
    paste("Selected option in Page 3:", input$page1_filter)
  })
  
  output$page4_output <- renderText({
    paste("Selected option in Page 4:", input$page2_filter)
  })
  output$page5_output <- renderText({
    paste("Selected option in Page 5:", input$page1_filter)
  })
  
  output$page6_output <- renderText({
    paste("Selected option in Page 6:", input$page2_filter)
  })
}

shinyApp(ui, server)





