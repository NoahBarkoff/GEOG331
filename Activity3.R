# GEOG311 Activity 3 Script
# NB 2/21/2022

#----Create Function----

# Create a function with arguments in parentheses and if statement in brackets
assert <- function(statement,err.message){
  if(statement == FALSE){
    print(err.message)
  }
  
}

# Test the assert function wigth false and true statements
assert(1 == 2, "error: unequal values")
assert(2 == 2, "error: unequal values")

# Use assert function to test if vectors are the same length
a <- c(1,2,3,4)
b <- c(8,4,5)
assert(length(a) == length(b), "error: unequal length")

# Read Bewkes weather data file
datW <- read.csv("Z:/students/nbarkoff/Data/bewkes/bewkes_weather.csv",
                 na.strings=c("#N/A"), skip=3, header=FALSE)

# Get sensor information
sensorInfo <-   read.csv("Z:/students/nbarkoff/Data/bewkes/bewkes_weather.csv",
                         na.strings=c("#N/A"), nrows=2)

# Use same column names for datW as in sensorInfo
colnames(datW) <-   colnames(sensorInfo)
