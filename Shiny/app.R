# 01-kmeans-app

palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
  "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

library(shiny)
library(dplyr)

ui <- fluidPage(
  headerPanel('Anomoly Detection Top 20 Lists'),
  sidebarPanel(
    selectInput('Factor', 'Z Score Factor', names(iris)),
    dateRangeInput(inputId = "ranger", label = "Select a Date", start = '1JAN2015', end = '1AUG2015')

  ),
  mainPanel(
    plotOutput('plot1')
  )
)

server <- function(input, output) {

  selectedData <- reactive({
    iris[, c(input$Factor)]
  })

  

  output$plot1 <- renderTable({
    
    
    Z_Score_Thres <- 2  #After testing make Z_Score_Thres and Sepal.Width an input
    results <- mutate(iris, SepalwZ = ((Sepal.Width - mean(Sepal.Width))/sd(Sepal.Width) )) %>% filter(SepalwZ > Z_Score_Thres)

  })

}

shinyApp(ui = ui, server = server)
