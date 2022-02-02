# GEOG311 Activity 2 Script
# NB 1/31/2022

# Set working directory to my noaa data folder
setwd("Z:/students/nbarkoff/Data/noaa_weather")

# Make a vector of tree heights in meters
heights <- c(30,41,20,22)

# Convert heights to cm
heights_cm <- heights*100

# Show only first tree's height
heights[1]

# Get more info on the matrix function
help (matrix)

# Set up a matrix with 2 columns and fill in by rows
# First argument is the vector of numbers to fill in the matrix
Mat<-matrix(c(1,2,3,4,5,6), ncol=2, byrow=TRUE)

# Set up a matrix that fills in by columns
# First argument is the vector of numbers to fill in the matrix
Mat.bycol<-matrix(c(1,2,3,4,5,6), ncol=2, byrow=FALSE)

# Look at the number in the first row and second column
Mat.bycol[1,2]

# Look at all the numbers in the second column
Mat.bycol[,2]

# Read weather data
datw <- read.csv("2011124.csv")
