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
ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1"))
# Make plots include all text by standardizing margin size
par(mai=c(1,1,1,1))

# Plot discharge with standard deviation visible
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)       
axis(1, seq(0,360, by=40), #tick intervals
     lab=seq(0,360, by=40)) #tick labels
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle
legend("topright", c("mean","1 standard deviation"), #legend items
       lwd=c(2,NA),#lines
       col=c("black",rgb(0.392, 0.584, 0.929,.2)),#colors
       pch=c(NA,15),#symbols
       bty="n")#no legend border

#---------- Question 5 ----------#

dat2017 <- datD[datD$yearPlot == "2017",]

colnames(dat2017) <- c(" agency_cd", "site_no", "date", "time", "tz",
                       "discharge", "discharge.flag", "stage", "stage.flag",
                       "doy", "year", "hour", "decDay", "decYear", "yearPlot")

ave2017 <- aggregate(dat2017$discharge, by=list(dat2017$doy), FUN="mean")                       
colnames(ave2017) <- c("doy","dailyave")

lines(dat2017$discharge, type="l", col="red")

lines(ave2017$doy, ave2017$dailyAve, type="l", col="red")

########### Format a hydrograph ###########

# Subset discharge and precipitation data on September 5-6
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

#---------- Question 7 ----------#

library("dplyr")

datP$days <- paste(yday(dateP), year(dateP))

Days.observations <- summarise(group_by(datP, days), sum(hour))

colnames(Days.observations) <- c("UniqueDate", "SumHours")

datFP <- ifelse(Days.observations$SumHours == sum(c(0:23)), "full", "not full")
colnames(datFP) <- c("Fulldays", "NULL")
datFP




########### Format box and violin plots ###########

# Use ggplot2
library(ggplot2)

# Specify year as a factor
datD$yearPlot <- as.factor(datD$year)

#Make a boxplot
ggplot(data= datD, aes(yearPlot,discharge)) + 
  geom_boxplot()

# Make a violin plot
ggplot(data= datD, aes(yearPlot,discharge)) + 
  geom_violin()