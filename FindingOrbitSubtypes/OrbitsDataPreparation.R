# Clear Workspace
rm(list = ls())

# Necessary libraries
library(caret)
library(plyr)
library(Metrics)
library(rjson)
library(RODBC)
library(GGally)
library(rpart)
library(rattle)


# Set seed for reproducibility and also set working directory
set.seed(1)
setwd("C:/Users/User/Documents/R/SpaceApps1/")




# Load the test and training data
# Data from here: http://minorplanetcenter.net/web_service

RawData0 <- read.csv(file = "output0.csv")
RawData1 <- read.csv(file = "output1.csv")
RawData2 <- read.csv(file = "output2.csv")
RawData3 <- read.csv(file = "output3.csv")
RawData4 <- read.csv(file = "output4.csv")
RawData5 <- read.csv(file = "output5.csv")
RawData6 <- read.csv(file = "output6.csv")

RawData <- rbind(RawData0,RawData1,RawData2,RawData3,RawData4,RawData5,RawData6)

# For diameter prediction
# RawData <- RawData[is.na(RawData$diameter) == FALSE,]
# Drop all columns with over 95% NAs

# Finding NA cols in training data
m = dim(ProcessedData)[1]; n = dim(ProcessedData)[2];
colNArate = NULL
for (i in 1:n ) {
  colNArate[i] = sum(is.na(ProcessedData[,i]))/m
}
NArates <- cbind(colnames(ProcessedData), colNArate)
# if a column has more than q NAs, erase it
q = 0.05
colNArate = (((colNArate<q)*1)* (1:189))
colNArate = colNArate[colNArate!=0]

# Subsetting
RawData <- RawData[,colNArate]

####
####

# Complete cases:
# CC <- complete.cases(RawData)
# RawData <- RawData[CC,]

# Drop unnecessary variables
drops <- c("X_id", "updated_at", "orbit_updated_at", 
           "one_line_format", "n_or_d", 
          "moid_reference ", "last_observation_date_used", 
          "created_at", "perihelion_date",
          "packed_designation", "reference", "moid_reference", 
          "first_observation_date_used", "epoch", 
          "albedo_updated_at", "color_updated_at",
          "origin", "panstarrs_v_minus_uprime", "v_minus_uprime", 
          "v_minus_wprime", "v_minus_yprime", "magadjust",
          "lightcurve_notes", "spin_updated_at", "computer_name") 
RawData <- RawData[,!(names(RawData) %in% drops)]
sapply(RawData, nlevels)

# Right formats of the necessary variables. 
RawData$pha=factor(as.numeric(RawData$pha))
revalue(RawData$pha, c("1" = "2")) -> RawData$pha
RawData$pha = factor(RawData$pha, labels = c(FALSE, TRUE))

# Orbit types: imputing 5 NAs with 0 and treating as factor
lev = levels(RawData$orbit_type)
RawData$orbit_type[is.na(RawData$orbit_type)] = 0
RawData$orbit_type = factor(RawData$orbit_type)

# Orbit unc: imputing 5 NAs with 0 and treating as factor
RawData$orbit_uncertainty[is.na(RawData$orbit_uncertainty)] = 0
RawData$orbit_uncertainty = factor(RawData$orbit_uncertainty)

# As factors
RawData$designation = factor(RawData$designation)
RawData$objectnumber = factor(RawData$objectnumber)

# There's an empty cathegory here named "", deleting it and changing levels to nice ones
RawData$critical_list_numbered_object=factor(as.numeric(RawData$critical_list_numbered_object))
revalue(RawData$critical_list_numbered_object, c("1" = "2")) -> RawData$critical_list_numbered_object
RawData$critical_list_numbered_object = factor(RawData$critical_list_numbered_object, labels = c(FALSE, TRUE))

# NEO: treating as a factor and changing levels to nice ones
RawData$neo=factor(as.numeric(RawData$neo))
revalue(RawData$neo, c("1" = "2")) -> RawData$neo
RawData$neo = factor(RawData$neo, labels = c(FALSE, TRUE))

# Make valid names
NewNames <- make.names(names= colnames(RawData), unique = FALSE, allow_ = TRUE)
colnames(RawData) = NewNames

# NEEDED WHEN Q>0.8, FOR DEALING WITH CHARACTER VARIABLES
# Split data into two datasets by type of the variable: cathegotical and numeric, 
# then do the imputation and then merge them back together
# m = dim(RawData)[1]; n = dim(RawData)[2];
# for (i in 1:n ) {
#         if (class(RawData[,i]) == "character")
#         {
#                 RawData[,i] = factor(RawData[,i])
#                 if (nlevels(RawData[,i]) == 1)
#                 {
#                         RawData[,i] = 0
#                 }
#         }
#         
# }
cla <- sapply(RawData, class)
factors = RawData[names(cla[cla=="factor"])]
num = RawData[names(cla[cla!="factor"])]


#Imputing numeric values, scling and centering data
PreprocessObj <- preProcess(num,
                            method = c( "center", "scale","knnImpute"),
                            k = 3,)
save(file="PreprocessObj.rda", x=PreprocessObj)
load("PreprocessObj.rda")
num <- predict(PreprocessObj, newdata = num)

# merge data sets back together
ProcessedData <- as.data.frame(cbind(factors, num))

# now split datasets by known and unknown orbit type
OrbitKnownData <- ProcessedData[ProcessedData$orbit_type != "0",]
OrbitKnownData$orbit_type = factor(OrbitKnownData$orbit_type)
OrbitUnknownData <- ProcessedData[ProcessedData$orbit_type == "0",]

#plotting orbit distribution
png(filename = "OrbitTypes1.png", width = 500, height = 500)
qplot(orbit_type, data=OrbitKnownData, geom="histogram")
dev.off()

# Save prepared data for future use
save(file="RawData.rda", x=RawData)
save(file="ProcessedData.rda", x=ProcessedData)
save(file="OrbitKnownData.rda", x=OrbitKnownData)
save(file="OrbitUnknownData.rda", x=OrbitUnknownData)



load(file="RawData.rda")
load(file="ProcessedData.rda")
load(file="OrbitKnownData.rda")
load(file="OrbitUnknownData.rda")