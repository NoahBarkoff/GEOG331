# GEOG311 Activity 3 Script
# NB 2/21/2022

#----Create Function----

# Create a function with arguments in parentheses and if statement in brackets
assert <- function(statement,err.message){
  if(statement == FALSE){
    print(err.message)
  }
  
}

# Test the assert function wigth false and true statements
assert(1 == 2, "error: unequal values")
assert(2 == 2, "error: unequal values")

# Use assert function to test if vectors are the same length
a <- c(1,2,3,4)
b <- c(8,4,5)
assert(length(a) == length(b), "error: unequal length")

# Read Bewkes weather data file
datW <- read.csv("Z:/students/nbarkoff/Data/bewkes/bewkes_weather.csv",
                 na.strings=c("#N/A"), skip=3, header=FALSE)

# Get sensor information
sensorInfo <-   read.csv("Z:/students/nbarkoff/Data/bewkes/bewkes_weather.csv",
                         na.strings=c("#N/A"), nrows=2)

# Use same column names for datW as in sensorInfo
colnames(datW) <-   colnames(sensorInfo)

# Install lubridate
install.packages(c("lubridate"))
library(lubridate)

# Convert dates to standard month/day/year hour:minute format
dates <- mdy_hm(datW$timestamp, tz= "America/New_York")

# Calculate the day of the year
datW$doy <- yday(dates)

# Calculate hour in the day
datW$hour <- hour(dates) + (minute(dates)/60)

# Calculate decimal day of the year
datW$DD <- datW$doy + (datW$hour/24)

# Find all N/A values in the air.temperature, wind speed, precipitation, soil temp, and soil moisture columns column
length(which(is.na(datW$air.temperature)))
length(which(is.na(datW$wind.speed)))
length(which(is.na(datW$precipitation)))
length(which(is.na(datW$soil.moisture)))
length(which(is.na(datW$soil.temp)))

# Allow multiple plots to be shown at once
par(mfrow=c(2,2))

# Make a dot plot of soil moisture 
plot(datW$DD, datW$soil.moisture, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil moisture (cm3 water per cm3 soil)")

# Make a plot from properly taken data of ai temp
plot(datW$DD, datW$air.temperature, pch=19, type="b", xlab = "Day of Year",
     ylab="Air temperature (degrees C)")

# Set NA values in the air temp column for temp values under freezing during the summer
datW$air.tempQ1 <- ifelse(datW$air.temperature < 0, NA, datW$air.temperature)

# Observe values for median, minimum, maximum, etc data for air temp values in the summer
quantile(datW$air.tempQ1)

# Find days with an air temp under 8 degrees C
datW[datW$air.tempQ1 < 8,]  

# Find days with an air temp above 33 degrees C
datW[datW$air.tempQ1 > 33,]  

# Normalize the lightning strikes time to match the time scale of the precipitation
lightscale <- (max(datW$precipitation)/max(datW$lightning.acvitivy)) * datW$lightning.acvitivy

# Create the plot with axis labeled and no points
plot(datW$DD , datW$precipitation, xlab = "Day of Year", ylab = "Precipitation & lightning",
     type="n")

# Plot semi-transparent points for precipitation
points(datW$DD[datW$precipitation > 0], datW$precipitation[datW$precipitation > 0],
       col= rgb(95/255,158/255,160/255,.5), pch=15)

# Plot solid color points for lightning
points(datW$DD[lightscale > 0], lightscale[lightscale > 0],
       col= "tomato3", pch=19)

# Question 5
# Just creating an object with the same data already in the DatW dataset
assert(length(lightscale)==length(datW$wind.speed),
       "error: unequal lengths")
# ---------------


# Filter out the storms where there is some of rain and lightning
# As well as storms where there is lots of rain
# And create a new air temp column that excludes these values
datW$air.tempQ2 <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA,
                          ifelse(datW$precipitation > 5, NA, datW$air.tempQ1))

# Question 6
datW$wind.speedQ2 <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA,
                          ifelse(datW$precipitation > 5, NA, datW$wind.speed))


assert(length(which(is.na(datW$wind.speedQ2)))==length(which(is.na(datW$wind.speed))),
                          "error: unequal number of NA oberservations")

plot(datW$DD , datW$wind.speedQ2, xlab = "Day of Year", ylab = "Wind Speed",
     type="n")

points(datW$wind.speedQ2, col= "blue", pch=15)


#---------------

# Question 7
plot(datW$DD , datW$air.tempQ1, xlab = "Day of Year", ylab = "Soil & Air Temperature",
     type="n", xlim=c(185,200))

points(datW$soil.temp, col= "blue", pch=15)
points(datW$air.tempQ1, col= "red", pch=15)

####### Do I need to do this?
rainscale <- (max(datW$soil.moisture)/max(datW$precipitation)) * datW$precipitation

plot(datW$DD , datW$precipitation, xlab = "Day of Year", ylab = "Precipitation & Soil Moisture",
     type="n", ylim=c(0,.2), xlim=c(180,190))

####### Why are all the precipitation values 0?
points(datW$precipitation, col= "blue", pch=15)
points(datW$soil.moisture, col= "red", pch=15)

soil.tempQ1 <- na.omit(datW$soil.temp)

#-----------

# Question 8
# Create three vectors 
name <- c("al", "bea", "carol")
age <- c(6, 7, 4)
hair <- c("brown", "green", "blond")

# Create data frame 
#children <- data.frame(name, age, hair)
#children
#name age  hair
#1    al   6 brown
#2   bea   7 green
#3 carol   4 blond

#ave (x, FUN=mean, )
#help(NA)

Ave.AirTemp <- signif(mean (datW$air.temperature, na.rm = TRUE),3)
Ave.WindSpeed <- signif(mean (datW$wind.speedQ2, na.rm =TRUE),3)
Ave.SoilMoisture <- signif(mean (datW$soil.moisture, na.rm =TRUE),8)
Ave.Precipitation <- signif(mean (datW$precipitation, na.rm =TRUE),4)
length
Requested.Table <- data.frame (Ave.AirTemp,Ave.WindSpeed, Ave.SoilMoisture, Ave.Precipitation )

# ------------------
# Question 9
par(mfrow=c(2,2))

plot (datW$DD, datW$air.temperature, xlab = "Day of Year", 
      ylab = "Air Temperature", type="n", main="Air Temperature in the Summer")
points(datW$air.temperature, col="blue", pch=15)

plot (datW$DD, datW$soil.moisture, xlab = "Day of Year", 
      ylab = "Soil Moisture", type="n", main="Soil Moisture in the Summer")
points(datW$soil.moisture, col="blue", pch=15)

plot (datW$DD, datW$soil.temp, xlab = "Day of Year", 
      ylab = "Soil Temperature", type="n", main="Soil Temperature in the Summer")
points(datW$soil.temp, col="blue", pch=15)

plot (datW$DD, datW$precipitation, xlab = "Day of Year", 
      ylab = "Precipitation", type="n", ylim=c(0,-------), main="Precipitation in the Summer")
points(datW$precipitation > -------, col="blue", pch=15)










