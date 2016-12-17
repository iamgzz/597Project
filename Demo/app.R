library(tidyverse)
library(gdata)
library(stringr)
library(shiny)
library(datasets)


tidy_all_long <- read.csv("Data/tidy_all_long")

# Define UI for dataset viewer application
ui <- fluidPage(
  titlePanel("Living Cost Comparison In Major International Cities"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("typeInput", "Choose a type:", choices = tidy_all_long$Type, selected = "Restaurants "),
      selectInput("itemInput", "Choose an item:", choices = tidy_all_long$Item, selected = "Meal, Inexpensive Restaurant "),
      selectInput("cityID", "City",  
                   choices = c("Shanghai","NYC","Tokyo","Paris", "London", "Sydney","Vancouver","Dubai"), multiple = TRUE,
                   selected = c("Shanghai","NYC","Tokyo","Paris", "London", "Sydney","Vancouver","Dubai")),
      radioButtons("highID", "Highest",  
                   choices = c("Shanghai","NYC","Tokyo","Paris", "London", "Sydney","Vancouver","Dubai","All"), 
                   selected = "All")
      ),
    mainPanel(
      plotOutput("main_plot"),
      plotOutput("second_plot"),
      plotOutput("third_plot"),
      tableOutput("results")
    )
  )
)


# Define server logic required to summarize and view the selected
# dataset
server <- function(input, output) {
  reduced_df1 <- reactive({

    filter(
      tidy_all_long, 
      Type == input$typeInput, 
      City %in% input$cityID
    )
  })
  reduced_df2 <- reactive({
    
    filter(
      tidy_all_long, 
      Item == input$itemInput, 
      City %in% input$cityID
    )
  })
  
  reduced_df3 <- reactive({
    
    high_vec <- switch(input$highID,
                       `Shanghai` = "Shanghai",
                       `NYC` = "NYC",
                       `Tokyo` = "Tokyo",
                       `Paris` = "Paris",
                       `London` = "London",
                       `Sydney` = "Sydney",
                       `Vancouver` = "Vancouver",
                       `Dubai` = "Dubai",
                       All = c("Shanghai","NYC","Tokyo","Paris", "London", "Sydney","Vancouver","Dubai")
    )
    
    filter(
      tidy_all_long, 
      #Item == input$itemInput, 
      Type == input$typeInput,
      City %in% input$cityID,
      Highest %in% high_vec 
    )
  })
  
  
  output$main_plot <- renderPlot({
    ggplot(filter(tidy_all_long, Type==input$typeInput & City %in% input$cityID),
           mapping = aes(x=reduced_df1()$Item, y = reduced_df1()$Value, fill=reduced_df1()$City)) + 
      geom_bar(stat = "identity", position = "dodge") +
      ggtitle("Price Comparison Among Cities On Certain Type Item") +
      xlab(input$typeInput) + ylab("Value") +
      theme(axis.text.x=element_text(size=8),legend.title=element_blank(),legend.position="bottom")
  })
  
  output$second_plot <- renderPlot({
    ggplot(filter(tidy_all_long, Item==input$itemInput & City %in% input$cityID),
           mapping = aes(x=reduced_df2()$City, y = reduced_df2()$Value, fill=reduced_df2()$City)) + 
      geom_bar(stat = "identity") +
      ggtitle("Closer Look At Prices Of Certain Item") +
      xlab(input$itemInput) + ylab("Value") +
      theme(axis.text.x=element_text(size=8),legend.title=element_blank(),legend.position="bottom")
  })
  
  output$third_plot <- renderPlot({
    ggplot(filter(tidy_all_long, Highest==input$highID & City %in% input$cityID & Type == input$typeInput),
           mapping = aes(x=reduced_df3()$Item, y = reduced_df3()$Value, fill=reduced_df3()$City)) + 
      geom_bar(stat = "identity",position = "dodge") +
      ggtitle("The City's Most Expensive Items") +
      xlab(input$typeInput) + ylab("Value") +
      theme(axis.text.x=element_text(size=8),legend.title=element_blank(),legend.position="bottom")
  })
  
  output$results <- renderTable({ 
    reduced_df3()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

