# Clear Workspace
rm(list = ls())

# Necessary libraries
library(caret)
library(plyr)
library(Metrics)
library(mice)
library(RANN)
library(RColorBrewer)

# Set seed for reproducibility and also set working directory
set.seed(1)
setwd("C:/Users/User/Documents/R/SpaceApps1/")




# Load the test and training data
# Data fro here: http://minorplanetcenter.net/web_service

RawData <- read.csv(file = "MergedData2.csv")


# Drop all columns with over 95% NAs

# Finding NA cols in training data
m = dim(RawData)[1]; n = dim(RawData)[2];
colNArate = NULL
for (i in 1:n ) {
  colNArate[i] = sum(is.na(RawData[,i]))/m
}
NArates <- as.data.frame(cbind(colnames(RawData), colNArate))
# if a column has more than q NAs, erase it
q = 0.7
colNArate = (((colNArate<q)*1)* (1:n))
colNArate = colNArate[colNArate!=0]

# Subsetting
RawData <- RawData[,colNArate]

####
####



# Drop unnecessary variables
drops <- c("X_id", "updated_at", "orbit_updated_at", "one_line_format", "n_or_d", 
          "moid_reference ", "last_observation_date_used", "created_at", "perihelion_date",
          "packed_designation", "reference", "moid_reference", "first_observation_date_used", "epoch", "albedo_updated_at", "color_updated_at",
          
          "objectdec", "color_ignore", "name", "nearly_numberable", "one_opposition_object_seen_prior", "object_type", "spin_updated_at", 
          "publication", "compname3", "reviseddata", "compname2", "compra2", "compra4", "mpcdesig", "compdec4", "objectra","compname1", "compdec3",
          "compra1", "compdec1", "sessiondate", "contactname", "compdec2", "compname4", "objectdec", "sessiontime", "observers", "compra3", "objectname",
          "contactinfo", 
          "origin", "panstarrs_v_minus_uprime", "v_minus_uprime", "v_minus_wprime", "v_minus_yprime", "magadjust") # these variables have zero variance

RawData <- RawData[,!(names(RawData) %in% drops)]

# Check levels
sapply(RawData, nlevels)

# Right formats of the necessary variables. 
RawData$pha=factor(as.numeric(RawData$pha))
revalue(RawData$pha, c("1" = "2")) -> RawData$pha
RawData$pha = factor(RawData$pha, labels = c(FALSE, TRUE))

lev = levels(RawData$orbit_type)
RawData$orbit_type[is.na(RawData$orbit_type)] = 0
RawData$orbit_type = factor(RawData$orbit_type, labels = lev)
RawData$designation = factor(RawData$designation)
RawData$objectnumber = factor(RawData$objectnumber)

RawData$critical_list_numbered_object=factor(as.numeric(RawData$critical_list_numbered_object))
revalue(RawData$critical_list_numbered_object, c("1" = "2")) -> RawData$critical_list_numbered_object
RawData$critical_list_numbered_object = factor(RawData$critical_list_numbered_object, labels = c(FALSE, TRUE))

RawData$neo=factor(as.numeric(RawData$neo))
revalue(RawData$neo, c("1" = "2")) -> RawData$neo
RawData$neo = factor(RawData$neo, labels = c(FALSE, TRUE))



# Make valid names
NewNames <- make.names(names= colnames(RawData), unique = FALSE, allow_ = TRUE)
colnames(RawData) = NewNames




# Time to do imputation of missing values

# Split data into two datasets by type of the variable: cathegotical and numeric, 
# then do the imputation and then merge them back together
cla <- sapply(RawData, class)
factors = RawData[names(cla[cla=="factor"])]
num = RawData[names(cla[cla!="factor"])]

#Imputing cathegorical variables


#Imputing numeric values
PreprocessObj <- preProcess(num,
                            thresh = 0.99, 
                            method = c( "knnImpute"),
                            k = 3,)
save(file="PreprocessObj.rda", x=PreprocessObj)
load("PreprocessObj.rda")
num <- predict(PreprocessObj, newdata = num)

# merge data sets back together
ProcessedData <- as.data.frame(cbind(factors, num))

# We want to predict orbit_type. In the given data 0 = unclassified, rest = classified.


# Split the data into labeled and unlabeled sets. 
# Model training and testing will be done on the labeled set, 
# and we want to actually predict orbits for unlabeled data set.
summary(factor(RawData$orbit_type))
# There is only 1 observation for "1" type of the orbit, so we will be unable to predict those. 
# That observation will be dropped.
ProcessedData$diamNotNormalised = RawData$diameter_neowise
ProcessedData$diamLOG = log(RawData$diameter_neowise)
ProcessedData <- ProcessedData[ProcessedData$orbit_type != "1",]
        
TrainData <- ProcessedData[ProcessedData$orbit_type != "0",]
TrainData$orbit_type = factor(TrainData$orbit_type)
TestData <- ProcessedData[ProcessedData$orbit_type == "0",]

# Data for predicting diameter

DiameterTrain <- ProcessedData[(is.na(ProcessedData$diamNotNormalised) == FALSE),]
DiameterTrain <- DiameterTrain[is.na(DiameterTrain$diameter_neowise)==FALSE,]
DiameterTest <- ProcessedData[(is.na(RawData$diameter_neowise) == TRUE),]






# Save prepared data for future use
save(file="RawData.rda", x=RawData)
save(file="ProcessedData.rda", x=ProcessedData)
save(file="num.rda", x=num)
save(file="TrainData.rda", x=TrainData)
save(file="TestData.rda", x=TestData)
save(file="DiameterTrain.rda", x=DiameterTrain)
save(file="DiameterTest.rda", x=DiameterTest)

load(file="DiameterTrain.rda")
load(file="DiameterTest.rda")
load("RawData.rda")
load("ProcessedData.rda")
load("TrainData.rda")
load("TestData.rda")


