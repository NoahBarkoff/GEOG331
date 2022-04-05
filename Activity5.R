# Noah Barkoff
# Acivity 5
# 3/23/2022


########### Load in data ###########

# Library the lubridate package
library(lubridate)

# Create dataset for streamflow data
datH <- read.csv("Z:/data/streamflow/stream_flow_data.csv",
                 na.strings = c("Eqp"))

# Create dataset for precipitation data
datP <- read.csv("Z:/data/streamflow/2049867.csv")

# Create dataset with only reliable values
datD <- datH[datH$discharge.flag == "A",]

########### Format dates ###########

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

########### Format plots ###########

# Calculate average and standard deviation for discharge data
aveF <- aggregate(datD$discharge, by=list(datD$doy), FUN="mean")
colnames(aveF) <- c("doy","dailyAve")
sdF <- aggregate(datD$discharge, by=list(datD$doy), FUN="sd")
colnames(sdF) <- c("doy","dailySD")

# Make all plots show up the same size
dev.new(width=8,height=8)

# Make plots include all text by standardizing margin size
par(mai=c(1,1,1,1))


#---------- Question 5 ----------#

# Create new dataset of only 2017 data
dat2017 <- datD[datD$year == "2017",]

# Plot discharge with standard deviation visible
# Add a line of 2017 discharge and add the line 
# to legend and adjust axis for months
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="End of month", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,200),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)       
axis(1, seq(0,360, by=30), #tick intervals
     lab=seq(0,12, by=1)) #tick labels
axis(2, seq(0,200, by=20),
     seq(0,200, by=20),
     las = 2)#show ticks at 90 degree angle
legend("topright", c("mean","1 standard deviation", "2017 discharge"), #legend items
       lwd=c(2,NA),#lines
       col=c("black",rgb(0.392, 0.584, 0.929,.2),"red"),#colors
       pch=c(NA,15),#symbols
       bty="n")#no legend border
lines(dat2017$doy, dat2017$discharge, col="red")


#---------- Question 6 ----------#

# Calculate mean and standard deviation of discharge data
mean(dat2017$discharge)
sd(dat2017$discharge)


#---------- Question 7 ----------#

# Add dplyr functionality
library("dplyr")

# Create days object to compile all data recorded into days
datP$days <-  paste(yday(dateP), year(dateP))

# Aggregate data into a data set with number of observations per day
observation.data <- aggregate(datP$days, by = list(datP$year, datP$doy), FUN = "length")

# Add column names to the observation.data
colnames(observation.data) <- c("year","day","observations")

# Add decimal year object to observation.data so it shares same time format as datD
observation.data$decYear <- ifelse(leap_year(observation.data$year),observation.data$year + (observation.data$day/366),
                            observation.data$year + (observation.data$day/365))

# Create full.data dat set with only days where there are no missing observations
full.data <- filter(observation.data, observations == 24, .preserve = FALSE)

# Fix plot format to include all text
par(mai=c(1,1,1,1))

# Plot discharge data
plot(datD$decYear, datD$discharge, type="l", xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")), 
     main = "Days With Full Discharge Observations")
legend("topright", c("Discharge Measurments", "Full Days"),
       lwd=c(2,2),
       col=c("black","red"),
       pch=c(NA,NA),
       bty="n")

# Add lines for full days on discharge data
abline(v = full.data$decYear, col="red")


#---------- Question 8 ----------#

# Display 2 graphs at once
par(mfrow=c(2,1))

# Format a hydrograph

#subsest discharge and precipitation within range of interest
hydroD <- datD[datD$doy >= 248 & datD$doy < 250 & datD$year == 2011,]
hydroP <- datP[datP$doy >= 248 & datP$doy < 250 & datP$year == 2011,]

#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl <- floor(min(hydroD$discharge))-1
#ceiling rounds up to the integer
yh <- ceiling(max(hydroD$discharge))+1
#minimum and maximum range of precipitation to plot
pl <- 0
pm <-  ceiling(max(hydroP$HPCP))+.5
#scale precipitation to fit on the 
hydroP$pscale <- (((yh-yl)/(pm-pl)) * hydroP$HPCP) + yl

# Make discharge hydrograph
par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroD$decDay,
     hydroD$discharge, 
     type="l", 
     ylim=c(yl,yh), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroP)){
  polygon(c(hydroP$decDay[i]-0.017,hydroP$decDay[i]-0.017,
            hydroP$decDay[i]+0.017,hydroP$decDay[i]+0.017),
          c(yl,hydroP$pscale[i],hydroP$pscale[i],yl),
          col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}

# Format a hydrograph on a different day with full measurments

# Subset discharge and precipitation data on Febuary 8
hydroD2 <- datD[datD$doy >= 39 & datD$doy < 40 & datD$year == 2011,]
hydroP2 <- datP[datP$doy >= 39 & datP$doy < 40 & datP$year == 2011,]

#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl2 <- floor(min(hydroD$discharge))-1
#ceiling rounds up to the integer
yh2 <- ceiling(max(hydroD$discharge))+1
#minimum and maximum range of precipitation to plot
pl2 <- 0
pm2 <-  ceiling(max(hydroP$HPCP))+.5
#scale precipitation to fit on the 
hydroP2$pscale <- (((yh-yl)/(pm-pl)) * hydroP$HPCP) + yl

# Make discharge hydrograph
plot(hydroD2$decDay,
     hydroD2$discharge, 
     type="l", 
     ylim=c(yl,yh), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroP2)){
  polygon(c(hydroP2$decDay[i]-0.017,hydroP2$decDay[i]-0.017,
            hydroP2$decDay[i]+0.017,hydroP2$decDay[i]+0.017),
          c(yl,hydroP2$pscale[i],hydroP2$pscale[i],yl),
          col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}


########### Format box and violin plots ###########

# Use ggplot2
library(ggplot2)

# Specify year as a factor
datD$yearPlot <- as.factor(datD$year)


#---------- Question 9 ----------#
# Create new data sets of only 2016 & 2017 data
dat2016 <- datD[datD$year == 2016,]
dat2017 <- datD[datD$year == 2017,]

# Classify the time periods for each season in 2016 and 2017
dat2016$season <- "Winter"
dat2016$season <- ifelse(dat2016$doy >= 61 & dat2016$doy <= 152, "Spring", dat2016$season)
dat2016$season <- ifelse(dat2016$doy >= 153 & dat2016$doy <= 244, "Summer", dat2016$season)
dat2016$season <- ifelse(dat2016$doy >= 245 & dat2016$doy <= 303, "Fall", dat2016$season)

dat2017$season <- "Winter"
dat2017$season <- ifelse(dat2017$doy >= 60 & dat2017$doy <= 151, "Spring", dat2017$season)
dat2017$season <- ifelse(dat2017$doy >= 152 & dat2017$doy <= 243, "Summer", dat2017$season)
dat2017$season <- ifelse(dat2017$doy >= 244 & dat2017$doy <= 302, "Fall", dat2017$season)

# Create violin plots for 2016 and 2017 of discharge per season
ggplot(data= dat2016, aes(season,discharge)) + 
  geom_violin()+
  ggtitle ("2016 seasonal discharge")+

ggplot(data= dat2017, aes(season,discharge)) + 
  geom_violin()+
  ggtitle ("2017 seasonal discharge")

