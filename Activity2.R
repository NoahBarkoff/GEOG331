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

# Specify a column with a proper date format
# Note the format here dataframe$column
datw$dateF <- as.Date(datw$DATE, "%Y-%m-%d")
# Google date formatting in r to find more options and learn more

# Create a date column by reformatting the date to only include years
# And indicating that it should be treated as numeric data
datw$year <- as.numeric(format(datw$dateF,"%Y"))

# Creating five different vectors with character, numeric, integer, and factor data

# Character vector
A <- c("B","C","D","E","F")

# Numeric vector
X <- c(1.64,2.18,6.92,1.97,5.51)

# Integer vector
Y <- c(1L,2L,3L,4L,5L)

# Factor vector
Z <- gl(5,1, labels = c(1,2,3,4,5))
print(Z)

# Determine all unique site names
unique(datw$NAME)

# Mean maximum temperature for Aberdeen
mean(datw$TMAX[datw$NAME == "ABERDEEN, WA US"])
# Now with na.rm so there is no NA
mean(datw$TMAX[datw$NAME == "ABERDEEN, WA US"], na.rm=TRUE)

# Calculate average daily temperature
datw$TAVE <- datw$TMIN + ((datw$TMAX-datw$TMIN)/2)

# Calculate mean temperature across all sites
averageTemp <- aggregate(datw$TAVE, by=list(datw$NAME), FUN="mean",na.rm=TRUE)

# Change names of columns in average temperature data
colnames(averageTemp) <- c("NAME","MAAT")

# Convert level to number for factor data type
datw$siteN <- as.numeric(datw$NAME)

# Make histogram for the first site in the levels
hist(datw$TAVE[datw$siteN == 1],
     freq=FALSE, 
     main = paste(levels(datw$NAME)[1]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")

# Add mean line with red (tomato3) color
abline(v = mean(datw$TAVE[datw$siteN == 1],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)

# Add standard deviation lines below and above the mean with red (tomato3) color
abline(v = mean(datw$TAVE[datw$siteN == 1],na.rm=TRUE) - sd(datw$TAVE[datw$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

abline(v = mean(datw$TAVE[datw$siteN == 1],na.rm=TRUE) + sd(datw$TAVE[datw$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)