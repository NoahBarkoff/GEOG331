# Noah Barkoff
# 4/27/2022

# Set working directory
setwd("Z:/data/oneida")

# Install relevant packages
library(terra)
library(caret)
library(randomForest)

# List files from sentinel satellite image files
f <- list.files(path = "sentinel/", pattern = "T18", full.names = T)

# Read the list of files as a single multi-band spatRaster
rsdat <- rast(f)

# Create a vector of band names so we can keep track of them
b <- c("B2","B3","B4","B5","B6","B7","B8","B11","B12")

# Set band names in our raster
names(rsdat) <- b

# Read the cloud mask data file
clouds <- rast("sentinel/MSK_CLDPRB_20m.tif")

# Read in validation data
algae <- vect("Oneida/algae.shp")
agri <- vect("Oneida/agriculture.shp")
built <- vect("Oneida/built.shp")
forest <- vect("Oneida/forest.shp")
water <- vect("Oneida/water.shp")
wetlands <- vect("Oneida/wetlands.shp")

# Reclassify the cloud mask so that pixel values below 60% become 1
# And values over 60 become NA to remove clouded pixels
cloudsF <- classify(clouds, matrix(c(-Inf,60,1,60,Inf,NA), ncol = 3, byrow = T))

# Use the cloud mask to remove NA pixels from the reflectance data
rsmask <- mask(rsdat,cloudsF)

# Generate random numbers to select training and validation data points
# Set seed so samples always the same
set.seed(12153)

# Randomly select the data in each dataset to be  used
sampleType <- rep("train",120)

# Samples to randomly convert to validation data
sampleSamp <- sample(seq(1,120),60)

# Convert these random samples from training to validation
sampleType[sampleSamp] <- "valid"

# Set up table with coordinates and data type (validate or train) for each point
landExtract <-  data.frame(landcID = rep(seq(1,6),each=120),
                           x=c(crds(algae)[,1],crds(water)[,1],crds(agri)[,1],crds(built)[,1],crds(forest)[,1],crds(wetlands)[,1]),
                           y=c(crds(algae)[,2],crds(water)[,2],crds(agri)[,2],crds(built)[,2],crds(forest)[,2],crds(wetlands)[,2]))
# Add sample type
landExtract$sampleType <- rep(sampleType, times=6)

# Create id table that gives each landcover an ID
landclass <- data.frame(landcID= seq(1,6),
                        landcover = c("algal bloom", "open water","agriculture","built","forest","wetlands"))

# Extract reflectance values for our sample points
rasterEx <- data.frame(extract(rsmask,landExtract[,2:3]))[,-1]

# Combine point information with raster information
dataAll <- cbind(landExtract,rasterEx)

# Remove missing data
dataAlln <- na.omit(dataAll)

# Subset into two different data frames
trainD <- dataAlln[dataAlln$sampleType == "train",]
validD <- dataAlln[dataAlln$sampleType == "valid",]

# Kfold cross validation
tc <- trainControl(method = "repeatedcv", # repeated cross-validation of the training data
                   number = 10, # number 10 fold
                   repeats = 10) # number of repeats
### Generate random forests
# Typically square root of number of variables
rf.grid <- expand.grid(mtry=1:sqrt(9)) # number of variables available for splitting at each tree node

# Train the random forest model to the Sentinel-2 data
rf_model <- caret::train(x = trainD[,c(5:13)], #digital number data
                         y = as.factor(trainD$landcID), #land class we want to predict
                         method = "rf", #use random forest
                         metric="Accuracy", #assess by accuracy
                         trainControl = tc, #use parameter tuning method
                         tuneGrid = rf.grid) #parameter tuning grid

# Apply the random forest model to the Sentinel-2 data
rf_prediction <- terra::predict(rsmask, rf_model, na.rm = T)
#view prediction
plot(rf_prediction)

# Set up categorical colors
landclass$cols <-c("#a6d854","#8da0cb","#66c2a5",
                   "#fc8d62","#ffffb3","#ffd92f")

# Make plot and hide legend
plot(rf_prediction,
     breaks=seq(0,6), 
     col=landclass$cols ,
     legend=FALSE, axes=FALSE)
legend("bottomleft", paste(landclass$landcover),
       fill=landclass$cols ,bty="n",horiz = T) 

# Get validation data from raster by extracting cell values at the cell coordinates
rf_Eval <- extract(rf_prediction, validD[,2:3])

# Make the confusion matrix
rf_errorM <- confusionMatrix(as.factor(rf_Eval[,2]),as.factor(validD$landcID))

# Add landcover names
colnames(rf_errorM$table) <- landclass$landcover
rownames(rf_errorM$table) <- landclass$landcover

# View the matrix
rf_errorM$table

# Observe overall accuracy
rf_errorM$overall













