# Noah Barkoff
# Final Project
# 4/8/2022

# Library dplyr
library("dplyr")

# Read in data files

# Read in Lake Michigan (LM) basin precipitation data
PrecipLM <- read.csv("Z:/students/nbarkoff/Data/prc_mic_basn_mon.csv",
                   header = TRUE, skip = 5)
# USE PIVOT LONGER


########### Also ????????????? ############
PrecipLMt <- as.data.frame(t(PrecipLM))




########### ?????????????????? ############
Month.cols <- rep(c("Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec"),
                  time = 82)

PrecipLM$month <- Month.cols

# Read in all Great Lakes water level data
WaterLevels <- read.csv("Z:/students/nbarkoff/Data/GLHYD_data_metric.csv",
                        header = TRUE, skip = 12)

