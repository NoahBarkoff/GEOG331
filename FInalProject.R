# Noah Barkoff
# Final Project
# 4/8/2022

# Library dplyr
library("dplyr")
library("tidyverse")
library("ggplot2")
library("lubridate")

# Read in Lake Michigan (LM) basin precipitation data
PrecipLM <- read.csv("Z:/students/nbarkoff/Data/prc_mic_basn_mon.csv",
                   header = TRUE, skip = 5)

# Turn PrecipLM columns to rows
PrecipLM.COltoRow <- pivot_longer(PrecipLM, cols = (2:13))

# Change PrecipLM column names
colnames(PrecipLM.COltoRow) <- c("year","month", "precipitation")

# Move PrecipLM columns so each data frame has same format
datPrecipLM <- PrecipLM.COltoRow %>% select("month", "year", "precipitation")

# Read in all Great Lakes water level data
WaterLevels <- read.csv("Z:/students/nbarkoff/Data/GLHYD_data_metric.csv",
                        header = TRUE, skip = 12)

# Only include relevant waterLevel columns
datWaterLevelsLM <- WaterLevels[,c(1,2,4)]

# Change waterLevel column names
colnames(datWaterLevelsLM) <- c("month","year", "waterlevel")

# Include only shared years between waterLevels and precipitation data sets
datPrecipLMf <- datPrecipLM[-c(973:984), ]
datWaterLevelsLMf <- datWaterLevelsLM[c(265:1236),  ]

# Make new data frame with only the variables being plotted for a linear regression
datLinear.Model <- data.frame (datWaterLevelsLMf$`waterlevel`, datPrecipLMf$`precipitation`, datTemp$temperature)

# Name datLinear.Model column names
colnames(datLinear.Model) <- c("waterlevel", "precipitation", "temperature")

# Plot precipitation and water level along with a linear regression
plot(datLinear.Model$waterlevel, datLinear.Model$precipitation, pch = 16, cex = 1.3, col = "blue", xlab="Water Level (M)", 
     ylab= "Precipitation(mm)", 
     main = "Preciptation's influence on Water Level in Lake Michigan from 1940-2020")
legend("topright", c("Precipitation values","Linear Regression Model"),
       lwd=c(NA,2),
       col=c("blue","red"),
       pch=c(16,NA),
       bty="n")
abline(lm(precipitation~waterlevel, data = datLinear.Model), lwd = 2, col = "red")

# Create a plot and edit temperature dataset

Temperature <- read.csv("Z:/students/nbarkoff/Data/Chicago_Temp.csv",
                        header = TRUE, skip = 2)

Temp.COltoRow <- pivot_longer(Temperature, cols = (2:13))

colnames(Temp.COltoRow) <- c("year","N/A", "month", "temperature")

datTemp <- Temp.COltoRow %>% select("month", "year", "temperature")

datTemp <- datTemp[-c(973:1032), ]

# Temp with water level
plot(datLinear.Model$precipitation,datLinear.Model$temperature, pch = 16, cex = 1.3, col = "blue", xlab="precipitation (mm)", 
     ylab= "Temperature(F)", 
     main = "Temperature's correlation to rainfall in Chicago from 1940-2020")
legend("topright", c("Temperature values","Linear Regression Model"),
       lwd=c(NA,2),
       col=c("blue","red"),
       pch=c(16,NA),
       bty="n")
abline(lm(precipitation~temperature, data = datLinear.Model), lwd = 2, col = "red")


# Temp with water level
plot(datLinear.Model$waterlevel,datLinear.Model$temperature, pch = 16, cex = 1.3, col = "blue", xlab="Water Level (M)", 
     ylab= "Temperature(F)", 
     main = "Temperature's correlation to water level in Chicago from 1940-2020")
legend("topright", c("Temperature values","Linear Regression Model"),
       lwd=c(NA,2),
       col=c("blue","red"),
       pch=c(16,NA),
       bty="n")
abline(lm(temperature~waterlevel, data = datLinear.Model), lwd = 2, col = "red")


# All 3 variables 

ggplot(data = datLinear.Model, mapping = aes(x = waterlevel, y = as.numeric(precipitation))) +
  geom_point(aes(color = temperature), size = 2) +
  scale_color_gradient(low = "yellow", high = "red") +
  theme_classic()
                                                                  
# quantity of extreme temp variables over time                  
sd(datTemp$temperature) + mean(datTemp$temperature)

datTempExtreme <- datTemp[datTemp$temperature > 68.449,]

# data is only summer months. Need data to show all data above 1 sd for all months
# Need to make a histogram showing number of extreme months per year over time

TemperatureF <- Temperature[-c(82:86), ]

TempExtreme <- TemperatureF %>% summarise_if(is.numeric, mean) + TemperatureF %>% summarise_if(is.numeric, sd)

TempExtreme = subset(TempExtreme, select = -c(13) )

TempExtremeF <- cbind(TempExtreme, share = datTemp$year)

TempExtremeF = subset(TempExtremeF, select = -c(13) )

names(TempExtremeF)[12] <- 'year'

TempExtreme.ColtoRow <- pivot_longer(TempExtreme, cols = (1:12))

colnames(TempExtreme.ColtoRow) <- c("month","ExtremeTemp")

datTempExtreme <- merge(datTemp, TempExtreme.ColtoRow)

datTempExtremePlot <- data.frame (ifelse (datTempExtreme$temperature > datTempExtreme$ExtremeTemp, 1, 0))

colnames(datTempExtremePlot) <- "IsExtreme"

NewColumn <- datTempExtremePlot$IsExtreme

datTempExtreme$IsExtreme <- NewColumn

ExtremeYears <- data.frame (aggregate(datTempExtreme$IsExtreme, list(datTempExtreme$year), FUN=sum))

colnames(ExtremeYears) <- c("year","ExtremeValues")


# Plot Extreme Temperatures
ggplot(data= ExtremeYears, aes(x= year, y= ExtremeValues, group=1)) +
  geom_line()+
  geom_point()+
  geom_smooth(method=lm)+
  scale_x_discrete(breaks = seq(1940,2020, by = 5))

# Years with 5 or more extreme temperatures

ifelse(ExtremeYears$ExtremeValues > 4, ExtremeYears$year, NA)

# Plot water level and precipitation values per year 
# with points on the line for extreme years

YearlyPrecip <- data.frame (aggregate(datPrecipLMf$precipitation, list(datPrecipLMf$year), FUN=sum))