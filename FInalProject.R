# Noah Barkoff
# Final Project
# 4/8/2022

# Library necessary packages
library("dplyr")
library("tidyverse")
library("ggplot2")
library("lubridate")

# Read in Lake Michigan (LM) basin precipitation dataset
PrecipLM <- read.csv("Z:/students/nbarkoff/Data/prc_mic_basn_mon.csv",
                   header = TRUE, skip = 5)

# Turn PrecipLM columns to rows
PrecipLM.COltoRow <- pivot_longer(PrecipLM, cols = (2:13))

# Change PrecipLM column names
colnames(PrecipLM.COltoRow) <- c("year","month", "precipitation")

# Move PrecipLM columns so each data frame has same format
datPrecipLM <- PrecipLM.COltoRow %>% select("month", "year", "precipitation")

# Read in all Great Lakes water level dataset
WaterLevels <- read.csv("Z:/students/nbarkoff/Data/GLHYD_data_metric.csv",
                        header = TRUE, skip = 12)

# Only include relevant waterLevel columns
datWaterLevelsLM <- WaterLevels[,c(1,2,4)]

# Change waterLevel column names
colnames(datWaterLevelsLM) <- c("month","year", "waterlevel")

# Include only shared years between waterLevels and precipitation data sets
datPrecipLMf <- datPrecipLM[-c(973:984), ]
datWaterLevelsLMf <- datWaterLevelsLM[c(265:1236),  ]

# Read in all temperature data of Chicago
Temperature <- read.csv("Z:/students/nbarkoff/Data/Chicago_Temp.csv",
                        header = TRUE, skip = 2)

# Turn month columns into rows
Temp.COltoRow <- pivot_longer(Temperature, cols = (2:13))

# Change column names of termperature data frame
colnames(Temp.COltoRow) <- c("year","N/A", "month", "temperature")

# Make a temperature data frame with only relevant variables
datTemp <- Temp.COltoRow %>% select("month", "year", "temperature")

# Edit out unecessary rows from temperature data frame
datTemp <- datTemp[-c(973:1032), ]

# Make new data frame with only the variables being plotted for a linear regression
datLinear.Model <- data.frame (datWaterLevelsLMf$`waterlevel`, datPrecipLMf$`precipitation`, datTemp$temperature)

# Name datLinear.Model column names
colnames(datLinear.Model) <- c("waterlevel", "precipitation", "temperature")




########PLOTS########

# Plot monthly precipitation and water level along with a linear regression
plot(datLinear.Model$waterlevel, datLinear.Model$precipitation, pch = 16, cex = 1.3, col = "blue", xlab="Water Level (M)", 
     ylab= "Precipitation(mm)", 
     main = "Preciptation's Monthly Influence on Water Level in Lake Michigan from 1940-2020")
legend("topright", c("Precipitation values","Linear Regression Model"),
       lwd=c(NA,2),
       col=c("blue","red"),
       pch=c(16,NA),
       bty="n")
abline(lm(precipitation~waterlevel, data = datLinear.Model), lwd = 2, col = "red")

# Plot monthly temperature with precipitation along with a linear regression
plot(datLinear.Model$temperature,datLinear.Model$precipitation, pch = 16, cex = 1.3, col = "blue", xlab="Temperature(F)", 
     ylab= "precipitation (mm)", 
     main = "Temperature's Monthly Correlation to Precipitation in Chicago from 1940-2020")
legend("topright", c("Temperature values","Linear Regression Model"),
       lwd=c(NA,2),
       col=c("blue","red"),
       pch=c(16,NA),
       bty="n")
abline(lm(precipitation~temperature, data = datLinear.Model), lwd = 2, col = "red")

# Plot monthly temperature with water level along with a linear regression
plot(datLinear.Model$temperature,datLinear.Model$waterlevel, pch = 16, cex = 1.3, col = "blue", xlab="Temperature(F)", 
     ylab= "Water Level (M)", 
     main = "Temperature's Monthly Correlation to Water Level in Chicago from 1940-2020")
legend("topright", c("Temperature values","Linear Regression Model"),
       lwd=c(NA,2),
       col=c("blue","red"),
       pch=c(16,NA),
       bty="n")
abline(lm(waterlevel~temperature, data = datLinear.Model), lwd = 2, col = "red")

# Use ggplot to show monthly correlation between temperature, precipitation, and water level on the same graph
ggplot(data = datLinear.Model, mapping = aes(x = waterlevel, y = as.numeric(precipitation))) +
  geom_point(aes(color = temperature), size = 2) +
  scale_color_gradient(low = "yellow", high = "red") +
  theme_classic() + ggtitle("Precipitation's Monthly Impact on Water Level, Colored by Temperature") +
  xlab("Water Level (M)") + ylab("Precipitation(mm)")



########END-PLOTS########

# Make data frame showing months where the temperature was 1 standard deviation
# above the average temperature for that month between 1940-2020

# Include only relevant rows in temperature data frame
TemperatureF <- Temperature[-c(82:86), ]

# Make data frame of extreme temperature values for all months
TempExtreme <- TemperatureF %>% summarise_if(is.numeric, mean) + TemperatureF %>% summarise_if(is.numeric, sd)

# Remove annual extreme temperature value from TempExtreme
TempExtreme = subset(TempExtreme, select = -c(13) )

# Add year column to temperature data frame and give extreme values for each year
TempExtremeF <- cbind(TempExtreme, share = datTemp$year)

# Remove annual extreme temperature from TempExtremeF data frame
TempExtremeF = subset(TempExtremeF, select = -c(13) )

# Name last column year in TempExtremeF
names(TempExtremeF)[12] <- 'year'

# Make months in TempExtreme data frame rows
TempExtreme.ColtoRow <- pivot_longer(TempExtreme, cols = (1:12))

# change column names in new data frame
colnames(TempExtreme.ColtoRow) <- c("month","ExtremeTemp")

# Merge extreme termpatures for months to show with datTemp data
datTempExtreme <- merge(datTemp, TempExtreme.ColtoRow)

# Assign extreme months a value of 1 and non extreme months value of 0
datTempExtremePlot <- data.frame (ifelse (datTempExtreme$temperature > datTempExtreme$ExtremeTemp, 1, 0))

# Rename 1/0 extreme column
colnames(datTempExtremePlot) <- "IsExtreme"

# Name IsExtreme new column as a vector
NewColumn <- datTempExtremePlot$IsExtreme

# Add NewColumn data to IsExtreme column in datTempExtreme data frame
datTempExtreme$IsExtreme <- NewColumn

# Make new data frame that shows number of extremely hot months per year
ExtremeYears <- data.frame (aggregate(datTempExtreme$IsExtreme, list(datTempExtreme$year), FUN=sum))

# Change column names in ExtremeYears data frame
colnames(ExtremeYears) <- c("year","ExtremeValues")

# Plot number of extreme temperatures per year in ggplot
ggplot(data= ExtremeYears, aes(x= year, y= ExtremeValues, group=1)) +
  geom_line()+
  geom_point()+
  geom_smooth(method=lm)+
  scale_x_discrete(breaks = seq(1940,2020, by = 5))+
  xlab("Year") + ylab("Number of Extreme Months")+
  ggtitle("Number of Extremely High Temperature Months Per Year")
summary(lm(year ~ ExtremeValues, data = ExtremeYears))
  
# Find years with 5 or more extremely hot months
ifelse(ExtremeYears$ExtremeValues > 4, ExtremeYears$year, NA)

# Plot water level and precipitation values per year 
# with points on the line for extreme years

# Make precipitation data a numeric vector
NumericPrecipitation <- as.numeric(datLinear.Model$precipitation)

# Make a new data frame showing total amount of precipitation per year
YearlyPrecip <- data.frame (aggregate(NumericPrecipitation, list(datTempExtreme$year), FUN=sum))

# Change column names in YearlyPrecip data frame
colnames(YearlyPrecip) <- c("year","precipitation")

# Make water level data a numeric vector
NumericWaterLevel <- as.numeric(datLinear.Model$waterlevel)

# Make a new data frame showing avergae water level values per year
YearlyWaterLevel <- data.frame (aggregate(NumericWaterLevel/12, list(datTempExtreme$year), FUN=sum))

# Change column names in YearlyWaterLevel data frame
colnames(YearlyWaterLevel) <- c("year","waterlevel")

# Make data frame of precipitation values in only extreme years
YearlyPrecipExtreme <- data.frame (ifelse (YearlyPrecip$year == ifelse(ExtremeYears$ExtremeValues > 4,
                                              ExtremeYears$year, NA),YearlyPrecip$precipitation, NA ))

# Change column names in YearlyPrecipExtreme data frame
colnames(YearlyPrecipExtreme) <- c("ETPrecipitation")

# Make data frame of water level values in only extreme years
YearlyWaterLevelExtreme <- data.frame (ifelse (YearlyWaterLevel$year == ifelse(ExtremeYears$ExtremeValues > 4,
                                          ExtremeYears$year, NA),YearlyWaterLevel$waterlevel, NA ))

# Change column names in YearlyWaterLevelExtreme data frame
colnames(YearlyWaterLevelExtreme) <- c("ETWaterLevel")

# Make temperature data a numeric vector
NumericTemperature <- as.numeric(datLinear.Model$temperature)

# Make average yearly temperature data frame
YearlyTemperature <- data.frame (aggregate(NumericTemperature/12, list(datTempExtreme$year), FUN=sum))

# Change column names of YearlyTemperature data frame
colnames(YearlyTemperature) <- c("year", "temperature")



########PLOTS########

# Plot yearly precipitation values with precipitation values in extremely hot years highlighted
plot(YearlyPrecip$year, YearlyPrecip$precipitation, pch = 16, cex = 1.3, col = "blue", xlab="Year", 
          ylab= "Precipitation (mm)", 
          main = "Precipitation in Years with many Hot Months in Chicago from 1940-2020")
points(y = YearlyPrecipExtreme$ETPrecipitation, x = YearlyPrecip$year,
       pch = 16,
       col = "red",
       cex = 2)
legend("topright", c("Normal Temperature values","Extreme Temperature values"),
       lwd=c(NA,NA),
       col=c("blue","red"),
       pch=c(16,16),
       bty="n")

# Plot yearly water level values with water level values in extremely hot years highlighted
plot(YearlyWaterLevel$year, YearlyWaterLevel$waterlevel, pch = 16, cex = 1.3, col = "blue", xlab="Year", 
     ylab= "Water Level (M)", 
     main = "Water Level in Years with many Hot Months in Chicago from 1940-2020")
points(y = YearlyWaterLevelExtreme$ETWaterLevel, x = YearlyWaterLevel$year,
       pch = 16,
       col = "red",
       cex = 2)
legend("topright", c("Normal Temperature values","Extreme Temperature values"),
       lwd=c(NA,NA),
       col=c("blue","red"),
       pch=c(16,16),
       bty="n")

# Plot yearly precipitation and water level along with a linear regression
plot(YearlyWaterLevel$waterlevel, YearlyPrecip$precipitation, pch = 16, cex = 1.3, col = "blue", xlab="Water Level (M)", 
     ylab= "Precipitation(mm)", 
     main = "Preciptation's Yearly Influence on Water Level in Lake Michigan from 1940-2020")
legend("topright", c("Precipitation values","Linear Regression Model"),
       lwd=c(NA,2),
       col=c("blue","red"),
       pch=c(16,NA),
       bty="n")
abline(lm(YearlyPrecip$precipitation~YearlyWaterLevel$waterlevel), lwd = 2, col = "red")


# Plot yearly temperature with precipitation along with a linear regression
plot(YearlyTemperature$temperature, YearlyPrecip$precipitation, pch = 16, cex = 1.3, col = "blue", xlab="Temperature(F)", 
     ylab= "Precipitation(mm)", 
     main = "Temperature's Yearly Correlation to Precipitation in Chicago from 1940-2020")
legend("topright", c("Precipitation values","Linear Regression Model"),
       lwd=c(NA,2),
       col=c("blue","red"),
       pch=c(16,NA),
       bty="n")
abline(lm(YearlyPrecip$precipitation~YearlyTemperature$temperature), lwd = 2, col = "red")

# Plot yearly temperature with water level along with a linear regression
plot(YearlyTemperature$temperature, YearlyWaterLevel$waterlevel, pch = 16, cex = 1.3, col = "blue", xlab="Temperature(F)", 
     ylab= "Water Level (M)", 
     main = "Temperature's Yearly Correlation to Water Level in Chicago from 1940-2020")
legend("topright", c("Temperature values","Linear Regression Model"),
       lwd=c(NA,2),
       col=c("blue","red"),
       pch=c(16,NA),
       bty="n")
abline(lm(YearlyWaterLevel$waterlevel~YearlyTemperature$temperature), lwd = 2, col = "red")



########END-PLOTS########

# Make new data frame with showing yearly average of temperature, precipitation, and water level
Yearly3VariablePlot <- data.frame (YearlyWaterLevel$waterlevel, YearlyPrecip$precipitation, YearlyTemperature$temperature)

# Change column names in Yearly3Variableplot
colnames(Yearly3VariablePlot) <- c("waterlevel", "precipitation", "temperature")

# Use ggplot to show yearly correlation between temperature, precipitation, and water level on the same graph
ggplot(data = Yearly3VariablePlot, mapping = aes(x = waterlevel, y = as.numeric(precipitation))) +
  geom_point(aes(color = temperature), size = 2) +
  scale_color_gradient(low = "yellow", high = "red") +
  theme_classic() + ggtitle("Precipitation's Yearly Impact on Water Level, Colored by Temperature") +
  xlab("Water Level (M)") + ylab("Precipitation(mm)")

# Use new method to classify hot years
# Find years where the temperature across the entire year was 1 sd above the yearly average
ifelse(YearlyTemperature$temperature > mean(YearlyTemperature$temperature)+ sd(YearlyTemperature$temperature),
       YearlyTemperature$temperature, NA)

# Make new data frame with only temperature values from the 
# years where the entire year was 1 sd above the yearly average
HotYears <- data.frame (ifelse (YearlyTemperature$temperature == ifelse(YearlyTemperature$temperature > mean(YearlyTemperature$temperature)+
                                                                   sd(YearlyTemperature$temperature),
                                                                 YearlyTemperature$temperature, NA),
                                YearlyTemperature$temperature, NA))

# Change column name in HotYears
colnames(HotYears) <- c("ExtremeTemperature")

# Find precipitation and water level values during new classification of hot years
YearlyPrecipExtreme2 <- data.frame (ifelse (YearlyPrecip$year == ifelse(HotYears$ExtremeTemperature > 0,
                                                                       ExtremeYears$year, NA),YearlyPrecip$precipitation, NA ))

YearlyWaterLevelExtreme2 <- data.frame (ifelse (YearlyWaterLevel$year == ifelse(HotYears$ExtremeTemperature > 0,
                                                                               ExtremeYears$year, NA),YearlyWaterLevel$waterlevel, NA ))

# change column names of ETprecipitation and ETWaterLevel
colnames(YearlyPrecipExtreme2) <- c("ETPrecipitation")

colnames(YearlyWaterLevelExtreme2) <- c("ETWaterLevel")


# Plot Precipitation and Water Level with values during the new classification of extremely hot
#  years highlighted as well as a line showing average precipitation and water level values
plot(YearlyPrecip$year, YearlyPrecip$precipitation, pch = 16, cex = 1.3, col = "blue", xlab="Year", 
     ylab= "Precipitation (mm)", 
     main = "Precipitation in Hot Years in Chicago from 1940-2020")
points(y = YearlyPrecipExtreme2$ETPrecipitation, x = YearlyPrecip$year,
       pch = 16,
       col = "green",
       cex = 2)
legend("topright", c("Average Yearly Precipitation","Normal Temperature values","Extreme Temperature values"),
       lwd=c(2,NA,NA),
       col=c("red","blue","green"),
       pch=c(NA,16,16),
       bty="n")
abline(h= mean(YearlyPrecip$precipitation), col = "red")


plot(YearlyWaterLevel$year, YearlyWaterLevel$waterlevel, pch = 16, cex = 1.3, col = "blue", xlab="Year", 
     ylab= "Water Level (M)", 
     main = "Water Level in Hot Years in Chicago from 1940-2020")
points(y = YearlyWaterLevelExtreme2$ETWaterLevel, x = YearlyWaterLevel$year,
       pch = 16,
       col = "green",
       cex = 2)
legend("topright", c("Average Yearly Water Level","Normal Temperature values","Extreme Temperature values"),
       lwd=c(2,NA,NA),
       col=c("red","blue","green"),
       pch=c(NA,16,16),
       bty="n")
abline(h= mean(YearlyWaterLevel$waterlevel), col = "red")


