iris
mean(iris$Sepal.Width)
sd(iris$Sepal.Width)
iris <- iris

rm(SepalwZ)

library(dplyr)
Z_Score_Thres <- 2  
results <- mutate(iris, SepalwZ = ((Sepal.Width - mean(Sepal.Width))/sd(Sepal.Width) )) %>% filter(SepalwZ > Z_Score_Thres)
table(results)

library(ggplot2)
