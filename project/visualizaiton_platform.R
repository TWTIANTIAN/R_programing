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
########################Data processing for page 3 ##########################
user_behavior_data <- read_csv('data/user_behavior.csv') 
setClass("UserBehavior", slots = list(
  behavior_id = "numeric",
  user_id = "numeric",
  product_id = "numeric",
  categoryId = "numeric",
  behavior_type = "character",
  create_time = "numeric",
  date = "Date",
  year = "numeric",
  month = "numeric",
  day = "numeric",
  hour = "numeric"
))
pv_data <- user_behavior_data[user_behavior_data$behavior_type=='pv',]
pv_data_object <- new("UserBehavior",
                      behavior_id = pv_data$behavior_id,
                      user_id = pv_data$user_id,
                      product_id = pv_data$product_id,
                      categoryId = pv_data$categoryId,
                      behavior_type = pv_data$behavior_type,
                      create_time = pv_data$create_time,
                      date = pv_data$date,
                      year = pv_data$year,
                      month = pv_data$month,
                      day = pv_data$day,
                      hour = pv_data$hour
)
cf_data <- user_behavior_data[user_behavior_data$behavior_type=='cart'|user_behavior_data$behavior_type=='fav',]
cf_data_object <- new("UserBehavior",
                      behavior_id = cf_data$behavior_id,
                      user_id = cf_data$user_id,
                      product_id = cf_data$product_id,
                      categoryId = cf_data$categoryId,
                      behavior_type = cf_data$behavior_type,
                      create_time = cf_data$create_time,
                      date = cf_data$date,
                      year = cf_data$year,
                      month = cf_data$month,
                      day = cf_data$day,
                      hour = cf_data$hour
)
buy_data <- user_behavior_data[user_behavior_data$behavior_type=='buy',]
buy_data_object <- new("UserBehavior",
                       behavior_id = buy_data$behavior_id,
                       user_id = buy_data$user_id,
                       product_id = buy_data$product_id,
                       categoryId = buy_data$categoryId,
                       behavior_type = buy_data$behavior_type,
                       create_time = buy_data$create_time,
                       date = buy_data$date,
                       year = buy_data$year,
                       month = buy_data$month,
                       day = buy_data$day,
                       hour = buy_data$hour
)
merged_data <- rbind(pv_data, cf_data, buy_data)
########################Data processing for page 4 ##########################
order_data <- read_csv('data/orders.csv') 

new_order_data <- order_data %>%
  group_by(user_id) %>%
  summarize(min_day = min(20 - day),
            order_count = n(),
            total_price = sum(product_price))

# Create data frame
k_mean_data <- data.frame(user_id = new_order_data$user_id, recency = new_order_data$min_day, frequency = new_order_data$order_count, monetary = new_order_data$total_price)





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
      ),
    # Page 3
    tabItem(
      tabName = "page3",
      sidebarLayout(
        sidebarPanel(
          # Add sidebar content for page 1 here
          selectInput("page3_filter", "Page 3 Filter", choices = c("all", "individual"), selected = "all")
        ),
        mainPanel(
          h2("Conversion rate"),
          # # Add main panel content for page 1 here
          # highchartOutput("funnel_chart")
          # Conditional rendering of the funnel chart based on the selected option
          conditionalPanel(
            condition = "input.page3_filter == 'all'",
            highchartOutput("funnel_chart")
          ),
          conditionalPanel(
            condition = "input.page3_filter == 'individual'",
            highchartOutput("funnel_chart02")
          )
        )
      )
    ),
    ## tab 4
    tabItem(
      tabName = "page4",
      sidebarLayout(
        sidebarPanel(
          # Add sidebar content for page 1 here
          selectInput("page4_filter", "Graph type", choices = c("radar", "chart"), selected = "radar")
        ),
        mainPanel(
          h2("Rader-Buyer Group"),
          # # Add main panel content for page 1 here
          conditionalPanel(
            condition = "input.page4_filter == 'radar'",
            plotlyOutput("segmentation_plot")
            # ,
            # tableOutput("column_table") 
          ),
          conditionalPanel(
            condition = "input.page4_filter == 'chart'",
            plotlyOutput("column_chart")
            ,
            DT::dataTableOutput("column_table")
          )
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
  
  ######## Reactive event for page3 #########################
  output$funnel_chart <- renderHighchart({
    
    req(input$page3_filter)  # Ensure the input is available
    if (input$page3_filter == "all") {
      
      # pv_sum  <- nrow(pv_data)
      pv_sum  <- length(pv_data_object@behavior_id)
      cf_sum  <- length(cf_data_object@behavior_id)
      buy_sum  <- length(buy_data_object@behavior_id)
      merge_sum <- nrow(merged_data)
      
      sum
      # Calculating conversion rates
      convert_rate_action <- c(100, cf_sum[[1]] / pv_sum[[1]] * 100, buy_sum[[1]] / pv_sum[[1]] * 100)
      x_data <- c("Website visit", "Add to cart & Add to favorite", "Buy product")
      
      # Creating data for funnel chart
      data <- data.frame(name = x_data, y = convert_rate_action)
      
      # Creating funnel chart
      hc <- highchart() %>%
        hc_chart(type = "funnel") %>%
        hc_title(text = "conversion_rate_action",
                 subtitle = "Browse --> Purchase&Favorite --> Purchase") %>%
        hc_add_series(data = data,
                      type = "funnel",
                      name = "",
                      dataLabels = list(enabled = TRUE, inside = TRUE),
                      tooltip = list(pointFormat = "{point.name}: {point.y}%"),
                      borderColor = "#fff",
                      borderWidth = 1)
      
      hc
    }
  })
  
  
  output$funnel_chart02 <- renderHighchart({
    # pv_sum01<-distinct(pv_data, user_id)
    req(input$page3_filter)  # Ensure the input is available
    
    if (input$page3_filter == "individual") {
      
      pv_sum_distinct  <- nrow(distinct(pv_data,user_id))
      cf_sum_distinct  <- nrow(distinct(cf_data,user_id))
      buy_sum_distinct  <- nrow(distinct(buy_data,user_id))
      
      sum
      # Calculating conversion rates
      conversion_rate_individual <- c(100, cf_sum_distinct[[1]] / pv_sum_distinct[[1]] * 100, buy_sum_distinct[[1]] / pv_sum_distinct[[1]] * 100)
      x_data <- c("Website visit", "Add to cart & Add to favorite", "Buy product")
      
      # Creating data for funnel chart
      data <- data.frame(name = x_data, y = conversion_rate_individual)
      
      # Creating funnel chart
      hc <- highchart() %>%
        hc_chart(type = "funnel") %>%
        hc_title(text = "conversion_rate_individual",
                 subtitle = "Browse --> Purchase&Favorite --> Purchase") %>%
        hc_add_series(data = data,
                      type = "funnel",
                      name = "",
                      dataLabels = list(enabled = TRUE, inside = TRUE),
                      tooltip = list(pointFormat = "{point.name}: {point.y}%"),
                      borderColor = "#fff",
                      borderWidth = 1)
      hc
    }
  })
  
  ######## Reactive event for page4 #########################
  
  output$segmentation_plot <- renderPlotly({
    req(input$page4_filter)  # Ensure the input is available
    
    if (input$page4_filter == "radar") {
      scaled_data <- k_mean_data %>%
        select(-user_id) %>%
        scale()
      
      # Perform k-means clustering
      k <- 3  # Number of clusters
      kmeans_result <- kmeans(scaled_data, centers = k, nstart = 10)
      
      # Get cluster assignments and cluster centers
      clusters <- kmeans_result$cluster
      centroids <- kmeans_result$centers
      
      # Add cluster assignments to the dataframe
      k_mean_data$group <- clusters
      
      # Restore cluster center coordinates
      #This calculation is necessary when you want to interpret or visualize the 
      #cluster centers in the same scale as the original variables, 
      #allowing for a meaningful understanding of their positions in the original data space.
      centroids_restored <- centroids * attr(scaled_data, "scaled:scale") + attr(scaled_data, "scaled:center")
      
      # Plot the radar chart
      group1 <- as.list(centroids_restored[1, ])
      group2 <- as.list(centroids_restored[2, ])
      group3 <- as.list(centroids_restored[3, ])
      
      radar <- plot_ly(type = 'scatterpolar', mode = 'lines')
      
      radar <- add_trace(
        radar,
        r = c(group1$recency, group1$frequency, group1$monetary),
        theta = c('recency', 'frequency', 'monetary'),
        fill = 'toself',
        name = 'group1',
        line = list(color = '#f9713c')
      )
      
      radar <- add_trace(
        radar,
        r = c(group2$recency, group2$frequency, group2$monetary),
        theta = c('recency', 'frequency', 'monetary'),
        fill = 'toself',
        name = 'group2',
        line = list(color = '#b3e4a1')
      )
      
      radar <- add_trace(
        radar,
        r = c(group3$recency, group3$frequency, group3$monetary),
        theta = c('recency', 'frequency', 'monetary'),
        fill = 'toself',
        name = 'group3',
        line = list(color = '#5CACEE')
      )
      
      radar <- layout(
        radar,
        title = 'Radar-Buyer Group',
        showlegend = TRUE
      )
      
      radar
    }
  })
  
  output$column_chart <- renderPlotly({
    req(input$page4_filter)  # Ensure the input is available
    
    if (input$page4_filter == "chart") {
      
      # scaled_data <- k_mean_data %>%
      #   select(-user_id) %>%
      #   scale()
      # # Perform k-means clustering
      # k <- 3  # Number of clusters
      # kmeans_result <- kmeans(scaled_data, centers = k, nstart = 10)
      ## use c++ and bject-oriented programming
      cppFunction('
        #include <Rcpp.h>
using namespace Rcpp;

class KMeansClustering {
private:
  NumericMatrix data;
  int numClusters;

public:
  KMeansClustering(NumericMatrix inputData, int k) {
    data = inputData;
    numClusters = k;
  }

  List performClustering() {
    NumericMatrix scaledData = scaleData();
    return runKMeans(scaledData);
  }

private:
  NumericMatrix scaleData() {
    NumericMatrix scaledData = clone(data);
    int numRows = scaledData.nrow();
    int numCols = scaledData.ncol();

    for (int j = 0; j < numCols; j++) {
      double mean = 0.0;
      double sd = 0.0;

      for (int i = 0; i < numRows; i++) {
        mean += scaledData(i, j);
      }

      mean /= numRows;

      for (int i = 0; i < numRows; i++) {
        double diff = scaledData(i, j) - mean;
        sd += diff * diff;
      }

      sd /= numRows;
      sd = sqrt(sd);

      for (int i = 0; i < numRows; i++) {
        scaledData(i, j) = (scaledData(i, j) - mean) / sd;
      }
    }

    return scaledData;
  }

  List runKMeans(NumericMatrix scaledData) {
    Environment stats("package:stats");
    Function kmeans = stats["kmeans"];

    return kmeans(scaledData, Named("centers") = numClusters, Named("nstart") = 10);
  }
};

// [[Rcpp::export]]
List performKMeansClustering(NumericMatrix inputData, int k) {
  KMeansClustering kmeansClustering(inputData, k);
  return kmeansClustering.performClustering();
}

      
      
      
      ')
      k <- 3
      kmeans_result <- performKMeansClustering(as.matrix(k_mean_data), k)
      
      
      
      # Get cluster assignments and cluster centers
      clusters <- kmeans_result$cluster
      centroids <- kmeans_result$centers
      
      # Add cluster assignments to the dataframe
      k_mean_data$group <- clusters
      # Group the data by the 'group' column and calculate the count in each group
      group_counts <- k_mean_data %>% 
        group_by(group) %>% 
        summarize(count = n())
      
      # Create the column chart using ggplot2
      column_chart <- ggplot(group_counts, aes(x = group, y = count)) +
        geom_col(fill = "#5CACEE", width = 0.5) +  # Set the fill color and width of the columns
        labs(x = "Group", y = "Count") +  # Set the labels for the x and y axes
        ggtitle("Group Counts")  # Set the chart title
      
      # Return the column chart
      column_chart
    }
  })
  
  output$column_table <- DT::renderDataTable({
    
    scaled_data <- k_mean_data %>%
      select(-user_id) %>%
      scale()
    # Perform k-means clustering
    k <- 3  # Number of clusters
    kmeans_result <- kmeans(scaled_data, centers = k, nstart = 10)
    
    # Get cluster assignments and cluster centers
    clusters <- kmeans_result$cluster
    centroids <- kmeans_result$centers
    
    # Add cluster assignments to the dataframe
    k_mean_data$group <- clusters
    
    datatable(k_mean_data, options = list(dom = 't', paging = FALSE, searching = FALSE))
    
    
    
    
  })
  
}



shinyApp(ui, server)
