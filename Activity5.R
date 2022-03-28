# Noah Barkoff
# Acivity 5
# 3/23/2022


# Library the lubridate package
library(lubridate)

# Create dataset for streamflow data
datH <- read.csv("Z:/data/streamflow/stream_flow_data.csv",
                 na.strings = c("Eqp"))

# Create dataset for precipitation data
datP <- read.csv("Z:/data/streamflow/2049867.csv")

# Create dataset with only reliable values
datD <- datH[datH$discharge.flag == "A",]

# Create new vector of data and time
datesD <- as.Date(datD$date, "%m/%d/%Y")

# Create the days of the years from datesD
datD$doy <- yday(datesD)

# Create the year from datesD
datD$year <- year(datesD)

# Create object defining time
timesD <- hm(datD$time)

# Define the time for precipitation data in new object  
dateP <- ymd_hm(datP$DATE)

# Get days of the year for precipitation data in new vector
datP$doy <- yday(dateP)

# Get the years for precipitation data in new vector
datP$year <- year(dateP)

# Convert dates to decimal format by first making the hours in decimal format
datD$hour <- hour(timesD ) + (minute(timesD )/60)

# Use decimal format in hours to get decimal format for days
datD$decDay <- datD$doy + (datD$hour/24)

# Use decimal format of years to get decimal format of years and account for leap years
datD$decYear <- ifelse(leap_year(datD$year),datD$year + (datD$decDay/366),
                       datD$year + (datD$decDay/365))

# Calculate decimal dates for precipitation data using same process                     
datP$hour <- hour(dateP ) + (minute(dateP )/60)
datP$decDay <- datP$doy + (datP$hour/24)
datP$decYear <- ifelse(leap_year(datP$year),datP$year + (datP$decDay/366),
                       datP$year + (datP$decDay/365))

# Plot discharge with decimal dates
plot(datD$decYear, datD$discharge, type="l", xlab="Year", ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))