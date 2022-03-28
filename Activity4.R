# Noah Barkoff
# 3/11/2022
# Activity 4



# Load in and observe the iris data frame
data(iris)

# Install tidyverse and library packages
install.packages("tidyverse")
library(dplyr)
library(ggplot2)

#####################################
##### Part 1: for loops         #####
#####################################

#Using only data for iris versicolor
#write a for loop
#that produces a regression table
#for each of the following relationships
#1. iris  sepal length x width
#2. iris  petal length x width
#3. iris sepal length x petal length


# Create data frame with variables only in the versicolor species
iris.versicolor <- iris %>% filter(Species == "versicolor")
flower <- datD[iris$Species == "versicolor",]
# Create linear models for each of the regressions to compare to for loop regressions for correctness
Sepal.LW <- lm(Sepal.Length ~ Sepal.Width, data = iris.versicolor)
Petal.LW <- lm(Petal.Length ~ Petal.Width, data = iris.versicolor)
SepPet.LL <- lm(Sepal.Length ~ Petal.Length, data = iris.versicolor)

# Assign object to the 3 linear models
Correct.Regressions.list <- list (Sepal.LW, Petal.LW, SepPet.LL)

# Create a list of the 3 dependent and independent variables 
Dependent.Variables <- list (iris.versicolor$Sepal.Length, iris.versicolor$Petal.Length,
               iris.versicolor$Sepal.Length)
Independent.Variables <- list (iris.versicolor$Sepal.Width, iris.versicolor$Petal.Width, 
                  iris.versicolor$Petal.Length)

# Create object for the for loop regressions to be assigned
Regressions.list <- list()

# Create for loop to make a linear regression for each of the 3 dependent and independent variables
for(i in 1:3) {                 
  
  Dependent.Regression = unlist(Dependent.Variables[i])
  Independent.Regression = unlist (Independent.Variables[i])
  Regressions.list[[i]] <- lm(Dependent.Regression ~ Independent.Regression)
  
}

# Compare correct regressions to for loop regressions to check for correctness
Correct.Regressions.list
Regressions.list


#####################################
##### Part 2: data in dplyr     #####
#####################################

#use dplyr to join data of maximum height
#to a new iris data frame
height <- data.frame(Species = c("virginica","setosa","versicolor"),
                     Height.cm = c(60,100,11.8))

# Create new data frame with height values for the 3 species in every row
Iris1 <- full_join (iris, height, by = "Species")

#####################################
##### Part 3: plots in ggplot2  #####
#####################################

#look at base R scatter plot
plot(iris$Sepal.Length,iris$Sepal.Width)

#3a. now make the same plot in ggplot
ggplot(data = iris, mapping = aes(x = Sepal.Length, y = Sepal.Width)) + geom_point()

#3b. make a scatter plot with ggplot and get rid of  busy grid lines
ggplot(data = iris, mapping = aes(x = Sepal.Length, y = Sepal.Width)) + geom_point() + theme_classic()

#3c. make a scatter plot with ggplot, remove grid lines, add a title and axis labels, 
#    show species by color, and make the point size proportional to petal length
ggplot(data = iris, mapping = aes(x = Sepal.Length, y = Sepal.Width, color= Species, size = Petal.Length)) + 
  geom_point() + theme_classic() + ggtitle("Sepal Length Versus Sepal Size by Species and Petal Length")
