library(shiny)
library(dplyr)
library(shinydashboard)
library(gridExtra)
library(grid)
library(ggplot2)
library(flexdashboard)
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(tidyverse)
library(tidyquant)
library(plotly)
library(parsnip)
library(timetk)
library(xgboost)
library(ggthemes)
library(highcharter)
library(fmsb)
library(Rcpp)

library(PredictFutureOrder)
useShinyjs(rmd = TRUE)
source('function/xgboost_forcast_coustomer.R')

########################Data processing for page 2 ##########################
orders <- read.csv("data/orders.csv")
user_order <- function(data){
  user <-  data %>%
    group_by(date) %>%
    summarise(distinct_users = n_distinct(user_id),
              total_products = n(),
              total_price = sum(product_price))
  return(user)
}
User <- user_order(orders)
User$date <- as.Date.factor(User$date)
User$date1 <- User$date
min_date3 <- min(User$date1[!is.na(User$date)])
max_date4 <- max(User$date1[!is.na(User$date)])

############################## ui #########################
ui <- dashboardPage(
  dashboardHeader(title = "Customer Dashboard"),
  dashboardSidebar(
    sidebarMenu( width = 160,
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
      # Page 2
      tabItem(
        tabName = "page2",
        sidebarLayout(
          sidebarPanel(
            sliderInput("date1", HTML("Date Interval:"),
                        min = min_date3, 
                        max = max_date4, 
                        value = c(min_date3, max_date4))
            # Add sidebar content for page 2 here
            
          ),
          mainPanel(
            h2("Page 2"),
            plotOutput("plot_user_order"),
            plotOutput("plot_hot_product"),
            # Add main panel content for page 2 here
            textOutput("page2_output")
          )
        )
      )
    )
  )
)


######################### server #########################
server <- function(input, output) {
  # Server logic goes here

  output$page1_output <- renderText({
    paste("Selected option in Page 1:", input$page1_filter)
  })

  output$page2_output <- renderText({
    paste("Selected option in Page 2:", input$page2_filter)
  })
######## Reactive event for page1 #########################

######## Reactive event for page2 #########################
  data_filtered5 <- reactive({User %>%
      filter(
        between(date, input$date1[1], input$date1[2]))
  })

  plot_user_order <- function(data){
    D <- data_filtered5()
    if(colnames(D)[2] == "distinct_users"){
      par(mfrow = c(1,3))
      barplot(D$distinct_users,
              main = "Order-Buyers",
              names.arg = D$date,
              cex.names = 0.8,
              ylab = "Number",
              col = "orange",
              las = 2
      )
      legend("topright", "Buyers", cex = 0.8, fill = "orange")
    } else {
      "Please run user_order to generate distinct_users"
    }
    if(colnames(D)[3]== "total_products"){
      barplot(D$total_products,
              main = "Order-Orders",
              names.arg = D$date,
              cex.names = 0.8,
              col = "green",
              las = 2
      )
      legend("topright", "Orders", cex = 0.8, fill = "green")
    } else {
      "Please run user_order to generate total_products"
    }
    if(colnames(D)[4]== "total_price"){
      barplot(D$total_price,
              main = "Order-GMV",
              names.arg = D$date,
              cex.names = 0.8,
              col = "blue",
              las = 2
      )
      legend("topright", "GMV", cex = 0.8, fill = "blue")
    } else {
      "Please run user_order to generate total_price"
    }
  }
  output$plot_user_order <- renderPlot(plot_user_order(data_filtered5()))

  hot_product <- function(data){
    product <- data %>%
      group_by(product_name) %>%
      summarise(count_order = n(),
                sum_product_price = sum(product_price))

    return(product)
  }

  plot_hot_product <- function(data){
    E <- hot_product(data)
    if(colnames(E)[2]== "count_order"){
      par(mfrow = c(1,2))
      barplot(E$count_order,
              main = "Hot product-Sales Volume",
              names.arg = E$product_name,
              cex.names = 0.65,
              ylab = "Sales Money",
              col = "red",
              las = 2
      )
      legend("topleft", "Sales Volume", cex = 0.8, fill = "red")
    } else {
      "Please run hot_product to generate count_order"
    }
    if(colnames(E)[3]== "sum_product_price"){
      barplot(E$sum_product_price,
              main = "Hot product-Sales Money",
              names.arg = E$product_name,
              cex.names = 0.65,
              col = "orange",
              las = 2
      )
      legend("topleft", "Sales Money", cex = 0.8, fill = "orange")
    } else {
      "Please run hot_product to generate sum_product_price"
    }
  }
  output$plot_hot_product <- renderPlot(plot_hot_product(orders))
}

shinyApp(ui, server)
