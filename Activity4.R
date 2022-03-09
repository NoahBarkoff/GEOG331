#use built in iris dataset
#take a look at it 
head(iris)
#load in some tidyverse packages
library(dplyr)
library(ggplot2)

#####################################
##### Part 1: for loops         #####
#####################################

#Using only data for iris versicolor
#write a for loop
#that produces a regression table
iris.versicolor <- iris %>% filter(Species == "versicolor")

#for each of the following relationships
#1. iris  sepal length x width
#2. iris  petal length x width
#3. iris sepal length x petal length

Regressions.list <- list()

for(i in 2:ncol(iris.versicolor)) {                 
  
  predictors_i <- colnames(iris.versicolor)[2:i]    
  Regressions.list[[i - 1]] <- summary(    
    lm(iris.versicolor$width ~ ., iris.versicolor[ , c("width", predictors_i)]))
  
}
# hint: consider using a list, and also new vectors for regression variables



#####################################
##### Part 2: data in dplyr     #####
#####################################

#use dplyr to join data of maximum height
#to a new iris data frame
height <- data.frame(Species = c("virginica","setosa","versicolor"),
                     Height.cm = c(60,100,11.8))



#####################################
##### Part 3: plots in ggplot2  #####
#####################################

#look at base R scatter plot
plot(iris$Sepal.Length,iris$Sepal.Width)

#3a. now make the same plot in ggplot


#3b. make a scatter plot with ggplot and get rid of  busy grid lines


#3c. make a scatter plot with ggplot, remove grid lines, add a title and axis labels, 
#    show species by color, and make the point size proportional to petal length

#####################################
##### Question: how did         #####
##### arguments differ between  #####
##### plot and ggplot?          #####
#####################################		