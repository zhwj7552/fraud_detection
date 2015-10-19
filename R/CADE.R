require(plyr)
require(XML)
require(devtools)
require(randomForest)
require(lubridate)
require(CadeAllStar)

# You can pull all data for the 2013 season via the following code.
season <- seasonify(2013)
head(season[, 1:3])

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

test <- rpois(10000, 35)
par(mfrow = c(2, 1))

hist(test)
hist(uni(test))

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

