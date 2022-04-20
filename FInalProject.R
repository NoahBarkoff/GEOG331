# Noah Barkoff
# Final Project
# 4/8/2022

# Library dplyr
library("dplyr")
library("tidyverse")

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
datLinear.Model <- data.frame (datWaterLevelsLMf$`waterlevel`, datPrecipLMf$`precipitation`)

# Name datLinear.Model column names
colnames(datLinear.Model) <- c("waterlevel", "precipitation")

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

