require(plyr)
require(dplyr)
require(randomForest)
require(lubridate)
require(CadeAllStar)

data(stats) #package assisted breakout of players
head(stats[, 1:3])

uni <- function(x, len = length(x)) {
  if ( is.integer(x) ) {
    sample(min(x):max(x), len, replace = TRUE)
  } else if ( is.numeric(x) ) {
    runif(len, min(x), max(x))
  } else if ( is.factor(x) ) {
    factor(sample(levels(x), len, replace = TRUE))
  } else {
    sample(unique(x), len, replace = TRUE)
  }
}

cade <- function(df, numTree = 500) {
  stopifnot(is.data.frame(df))
  
  # This is the data we will make the 'no' case
  real <- df
  
  # Create similar but uniform data
  fake <- as.data.frame(lapply(real, uni))
  
  real$y <- 0
  fake$y <- 1
  
  # Combine real and fake data
  data <- rbind(real, fake)
  
  # Build classifier
  tree <- randomForest(as.factor(y) ~ ., data = data, ntree = numTree)
  
  # The classifier probabilities
  df$prob <- predict(tree, newdata = df, type = 'prob')[, 2]
  df$prob <- df$prob / (1 - df$prob)
  
  df
}

# Run cade on data, this takes a minute, for analysis only use relevant fields.
stats$prob <- cade(subset(stats, select = -c(player, date, guid)))$prob

# Order by most likely to be an outlier.
stats <- stats[order(stats$prob, decreasing = TRUE), ]

# Do people appear in the top frequently.
rev(sort(table(stats$player[1:30])))

# Aggregate the per game score up to just the player
rank <- ddply(stats, .(player), summarise, score = sum(prob))

# Order largest sum
rank <- rank[order(rank$score, decreasing = TRUE), ]

# Rank each player by there sum total.
rank$rank <- 1:nrow(rank)
head(rank)

# Load all star game data
al2013 <- 'http://www.allstarnba.es/editions/2013.htm'
al2013 <- readHTMLTable(al2013)

# Join and clean these fields.
al2013 <- setdiff(c(al2013[[1]]$` EAST`, al2013[[2]]$` WEST`), 'TOTALS')

# Move all text to lower to be safe(er) in joining data.
al2013 <- data.frame(player = tolower(al2013))
rank$player <- tolower(rank$player)

# Join data
al2013 <- merge(al2013, rank)

# Order data by rank
al2013 <- al2013[order(al2013$rank), ]

# How far into the list do you need to go to capture the whole all star lineup
al2013$depth <- al2013$rank / nrow(rank)

al2013

ui <- pageWithSidebar(
  headerPanel('Anomaly Detection'),
  sidebarPanel(
    selectInput('Factor', 'Z Score Factor', names(stats[2:8])),
    sliderInput('endHour', 'Choose the anomoly detection threshold', min = 0, max = 23, value = 7)
  ),
  mainPanel(
    p('This shiny app will show you most anomolous players in the NBA.'),
    h4('The factor to detect outliers on:'),
    verbatimTextOutput('day'),
    h4('The single factor anomoly detection algorithm results'),
    fluidRow(DT::dataTableOutput('startHour')),
    h4('The anomoly detection threshold you chose:'),
    verbatimTextOutput('endHour'),
    h4('The Classifier Adjusted Density Estimation for Anomoly Detection'),
    dataTableOutput('CADETable')
  )
)

server <- 
  function(input, output) {
    output$day <- renderPrint({input$Factor})
    output$endHour <- renderPrint({input$endHour})
    output$CADETable <- renderDataTable({rank})
    

    

  }

#Need to somehow cast this in a reactive server call
Z_Score_Thres <- 2  #After testing make Z_Score_Thres and Sepal.Width an input
results <- mutate(stats, statsZ = ((input$Factor - mean(input$Factor))/sd(input$Factor) )) %>% filter(statsZ > Z_Score_Thres)
output$startHour <- renderDataTable({results})

results <- reactive({
  Z_Score_Thres <- 2 
  mutate(stats, statsZ = ((input$Factor - mean(input$Factor))/sd(input$Factor) )) %>% filter(statsZ > Z_Score_Thres)
})
output$startHour <- DT::renderDataTable({
  DT::datatable(results())
})

shinyApp(ui = ui, server = server)

