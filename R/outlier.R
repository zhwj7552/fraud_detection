
#' The main CADE outlier function
#'
#' This function calls and runs CADE
#' @param df The data frame in question
#' @param prop The proportion of true to false cases
#' @param skip Fields that should not be included 
#' @param cutoff The probability above which to consider an outlier
#' @param ... Additional arguments to be passed on to randomForest
#' @keywords anonaly detection
#' @export
#' @examples
#' outlier(cars)
#' @references Friedland, L., Gentzel, A. & Jensen, D. (2014). Classifier-Adjusted Density Estimation for
#'    Anomaly Detection and One-Class Classification

outlier <- function(df, prop = 1, skip = c(), cutoff = .5, ...) {
  
  df <- df[complete.cases(df), ]
  
  ids <- df[, skip]
  real <- if (length(skip) > 0) df[, -(which(names(df) %in% skip))] else df
  
  # Create similar but uniform data
  fake <- as.data.frame(lapply(real, function(x) uniform(x, length(x) * prop)))
  
  # Label the data you started with as negative outcomes.
  real$y <- 0
  # Label the fake data as the positive outcomes.
  fake$y <- 1
  
  # Combine both the real and the fake data.
  data <- rbind(real, fake)
  
  # Build classifier
  tree <- randomForest::randomForest(as.factor(y) ~ ., data = data, 
                                     importance = TRUE)
  
  vars <- names(attr(tree$terms, 'dataClasses')[-1])
  prop <- tree$importance[, 1] / sum(tree$importance[, 1])
  
  # The classifier probabilities
  df$prob <- predict(tree, newdata = df, type = 'prob')[, 2]
  df$prob <- df$prob / (1 - df$prob)
  
  df$outlier <- ifelse(df$prob > cutoff, 1, 0)
  df[order(-df$prob), ]
}

