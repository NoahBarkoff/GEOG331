# GEOG311 Activity 2 Script
# NB 1/31/2022

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
