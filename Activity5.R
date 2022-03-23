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
