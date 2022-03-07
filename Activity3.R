# GEOG311 Activity 3 Script
# NB 2/21/2022


###########################################
###########################################
#------FORMATING TO ANSWER QUESTIONS------#
###########################################
###########################################

# Create a function with arguments in parentheses and if statement in brackets
assert <- function(statement,err.message){
  if(statement == FALSE){
    print(err.message)
  }
  
}


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

# Allow for 4 plots to be displayed at the same time
par(mfrow=c(2,2))

#-----------------


# Question 4

# Set NA values in the air temp column for temp values under freezing during the summer
datW$air.tempQ1 <- ifelse(datW$air.temperature < 0, NA, datW$air.temperature)

# Find days with an air temp under 8 degrees C
datW[datW$air.tempQ1 < 8,]  

# Find days with an air temp above 33 degrees C
datW[datW$air.tempQ1 > 33,]  

#--------------


# Question 5

# Normalize the lightning strikes time to match the time scale of the precipitation
lightscale <- (max(datW$precipitation)/max(datW$lightning.acvitivy)) * datW$lightning.acvitivy

# Test that the lightscale object is the same data already in 
# the DatW dataset under datW$wind.speed
assert(length(lightscale)==length(datW$wind.speed),
       "error: unequal lengths")

# ---------------


# Question 6

# Filter out the wind speed values where datW$precipitation and 
# datW$lightning.acvitivy values lead to  unreliable wind speed measurements
datW$wind.speedQ2 <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA,
                          ifelse(datW$precipitation > 5, NA, datW$wind.speed))

# Test that the sum of the column labels that are NA values in datW$wind.speedQ2
# is equal to the sum of datW$precipitation and datW$lightning.acvitivy
# that lead to  unreliable wind speed measurements
assert(sum(which(is.na(datW$wind.speedQ2)))== sum(which(datW$precipitation  >= 2
         & datW$lightning.acvitivy >0 |  datW$precipitation > 5)),
                          "error")


# Make a line plot of wind speed values throughout the recording time period
plot(datW$DD , datW$wind.speedQ2, xlab = "Day of Year", ylab = "Wind Speed",
     type="l")

# Add points of wind speed to the line plot of wind speed
points(datW$wind.speedQ2, col= "blue", pch=15)


#---------------


# Question 7

# Make 4 separate line plots for air temperature, soil temperature, precipitation
# and soil moisture in the days before the soil sensor lost contact

plot(datW$DD , datW$air.tempQ1, xlab = "Day of Year", ylab = "Air Temperature",
     type="l", xlim=c(180,193))
plot(datW$DD , datW$soil.temp, xlab = "Day of Year", ylab = "Soil Temperature",
     type="l", xlim=c(180,193))
plot(datW$DD , datW$precipitation, xlab = "Day of Year", ylab = "Precipitation",
     type="l", xlim=c(180,193))
plot(datW$DD , datW$soil.moisture, xlab = "Day of Year", ylab = "Soil Moisture",
     type="l", xlim=c(180,193))

#-----------


# Question 8

# Create objects showing average air temperature, wind speed,
# soil moisture, and precipitation values
Ave.AirTemp <- signif(mean (datW$air.temperature, na.rm = TRUE),3)
Ave.WindSpeed <- signif(mean (datW$wind.speedQ2, na.rm =TRUE),3)
Ave.SoilMoisture <- signif(mean (datW$soil.moisture, na.rm =TRUE),8)
Ave.Precipitation <- signif(mean (datW$precipitation, na.rm =TRUE),4)
Ave.SoilTemp <-signif(mean (datW$soil.temp, na.rm =TRUE),3)

# Create the requested table with the requested average values
Requested.Table <- data.frame (Ave.AirTemp,Ave.WindSpeed, Ave.SoilMoisture, Ave.Precipitation, Ave.SoilTemp )

# Calculate number of observations in each object
length(datW$air.temperature[!is.na(datW$air.temperature)])
length(datW$wind.speedQ2[!is.na(datW$wind.speedQ2)])
length(datW$soil.moisture[!is.na(datW$soil.moisture)])
length(datW$precipitation[!is.na(datW$precipitation)])
length(datW$soil.temp[!is.na(datW$soil.temp)])

# Calculate time period for each observation
head(datW$timestamp)
tail(datW$timestamp)

# ------------------


# Question 9

# Create line plots of air temperature, wind speed, soil moisture,
# and precipitation values over the whole time period of the study
plot (datW$DD, datW$air.temperature, xlab = "Day of Year", 
      ylab = "Air Temperature", type="l", main="Air Temperature in the Summer")

plot (datW$DD, datW$soil.moisture, xlab = "Day of Year", 
      ylab = "Soil Moisture", type="l", main="Soil Moisture in the Summer")

plot (datW$DD, datW$soil.temp, xlab = "Day of Year", 
      ylab = "Soil Temperature", type="l", main="Soil Temperature in the Summer")

plot (datW$DD, datW$precipitation, xlab = "Day of Year", 
      ylab = "Precipitation", type="l", main="Precipitation in the Summer")

# ------------------








