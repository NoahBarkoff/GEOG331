# Noah Barkoff
# Final Project
# 4/8/2022

# Library dplyr
library("dplyr")
library("tidyverse")

# Read in data files

# Read in Lake Michigan (LM) basin precipitation data
PrecipLM <- read.csv("Z:/students/nbarkoff/Data/prc_mic_basn_mon.csv",
                   header = TRUE, skip = 5)
# USE PIVOT LONGER
# https://stackoverflow.com/questions/22286419/move-a-column-to-first-position-in-a-data-frame

# Turn columns to rows
PrecipLM.COltoRow <- pivot_longer(PrecipLM, cols = (2:13))

# Change column names
colnames(PrecipLM.COltoRow) <- c("year","month", "precipitation(mm)")

# Move columns so each data frame has same format
datPrecipLM <- PrecipLM.COltoRow %>% select("month", "year", "precipitation(mm)")


# Read in all Great Lakes water level data
WaterLevels <- read.csv("Z:/students/nbarkoff/Data/GLHYD_data_metric.csv",
                        header = TRUE, skip = 12)

# Only include relevant columns
datWaterLevelsLM <- WaterLevels[,c(1,2,4)]

# Change column names
colnames(datWaterLevelsLM) <- c("year","month", "waterlevel(m)")

# Include only shared years between data sets

# https://stackoverflow.com/questions/12328056/how-do-i-delete-rows-in-a-data-frame