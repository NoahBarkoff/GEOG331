# Noah Barkoff
# In class regression practice
# 2/28/2022

# Load the entire iris dataset
library(datasets)
data(iris)
summary(iris)

# Make a subset for iris virginica
flower <- iris[iris$Species=="virginica",]

# Make a linear model with the variables petal length as y values and sepal length as x values
# ~ indicates 2 seperate variables
fit <- lm(flower$Petal.Length~flower$Sepal.Length)

# View the linear model
summary(fit)

# Create a scatter plot of the 2 variables against each other
plot(flower$Petal.Length~flower$Sepal.Length,
    main = "Iris Virginica",
    xlab = "Sepal Length",
    ylab = "petal length",
    col = "purple", pch = 16)

# Create a scatter plot of the residuals against the independant variable, petal length
plot(flower$Petal.Length,summary(fit)$residuals,
        xlab = "Sepal Length",
        ylab = "petal length",
        col = "purple", pch = 16)

# Add a horizontal line to the scatter plot of residuals to distingush 0 residual value
abline(h=0,
       lty = "dashed")

# Create a histogram of the residuals
hist(summary(fit)$residuals,
     main = "regression Residuals",
     xlab = "Residual",
     col = "purple")

# Use Shapiro wilks test on the residuals
shapiro.test(summary(fit)$residuals)

# Create a qq plot
qqnorm(summary(fit)$residuals, pch = 16)

# Plot a qq line of the qnorm on the qq plot
qqline(summary(fit)$residuals, datax = FALSE, distribution = qnorm,
       probs = c(.25, .75), qtype = 7, pch = 16)
