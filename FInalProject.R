# Noah Barkoff
# Final Project
# 4/8/2022

# Library dplyr
library("dplyr")
library("tidyverse")
library("ggplot2")

# Read in data files

# Read in Lake Michigan (LM) basin precipitation data
PrecipLM <- read.csv("Z:/students/nbarkoff/Data/prc_mic_basn_mon.csv",
                   header = TRUE, skip = 5)
# USE PIVOT LONGER
# https://stackoverflow.com/questions/22286419/move-a-column-to-first-position-in-a-data-frame

# Turn columns to rows
PrecipLM.COltoRow <- pivot_longer(PrecipLM, cols = (2:13))

# Change column names
colnames(PrecipLM.COltoRow) <- c("year","month", "precipitation")

# Move columns so each data frame has same format
datPrecipLM <- PrecipLM.COltoRow %>% select("month", "year", "precipitation")


# Read in all Great Lakes water level data
WaterLevels <- read.csv("Z:/students/nbarkoff/Data/GLHYD_data_metric.csv",
                        header = TRUE, skip = 12)

# Only include relevant columns
datWaterLevelsLM <- WaterLevels[,c(1,2,4)]

# Change column names
colnames(datWaterLevelsLM) <- c("month","year", "waterlevel")

# Include only shared years between data sets

datPrecipLMf <- datPrecipLM[-c(973:984), ]
datWaterLevelsLMf <- datWaterLevelsLM[c(265:1236),  ]

# Plot overall trend in precipitation data and water level data on same graph

datLinear.Model <- data.frame (datWaterLevelsLMf$`waterlevel`, datPrecipLMf$`precipitation`)

# Name datLinear.Model column names
colnames(datLinear.Model) <- c("waterlevel", "precipitation")

datLinear.Model$precipitation = as.numeric(as.character(datLinear.Model$precipitation))

# https://www.statology.org/line-of-best-fit-in-r/


#Try using base plotting
plot(datLinear.Model$waterlevel, datLinear.Model$precipitation, pch = 16, cex = 1.3, col = "blue", xlab="Water Level (M)", 
     ylab= "Precipitation(mm)", 
     main = "Name")

# Try 1
abline (lm(waterlevel ~ precipitation, data = datLinear.Model))

# Try 2
abline(Liner.Model)
Linear.Model <- lm(waterlevel ~ precipitation, data = datLinear.Model)


# Try using ggplot2 - 3
ggplot(data = datLinear.Model, mapping = aes(x = waterlevel, y = precipitation)) + 
  geom_point() + theme_bw() + ggtitle("Name") +
  geom_smooth(method=lm, se=FALSE)


# https://stackoverflow.com/questions/12328056/how-do-i-delete-rows-in-a-data-frame