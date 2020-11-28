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

load(file="RawData.rda")
load(file="ProcessedData.rda")
load(file="OrbitKnownData.rda")
load(file="OrbitUnknownData.rda")


# Most important parameters for orbit prediction
ParamsOrbits <- c("perihelion_distance", 
                  "eccentricity", 
                  "semimajor_axis",
                  "aphelion_distance",               
                  "mean_daily_motion", 
                  "period",
                  "tisserand_jupiter",
                  "absolute_magnitude")

# Some data preparation
OrbitRawData = RawData[,(names(RawData) %in% ParamsOrbits)]
CompCases <- complete.cases(OrbitRawData)
OrbitRawData <- OrbitRawData[CompCases,]

OrbitData <- ProcessedData[,(names(ProcessedData) %in% ParamsOrbits)]
OrbitData <- OrbitData[CompCases,]

# same thing for zeros orbits
OrbitZerosRawData = RawData[ProcessedData$orbit_type == "0",(names(RawData) %in% ParamsOrbits)]
CompCases1 <- complete.cases(OrbitZerosRawData)
OrbitZerosRawData <- OrbitZerosRawData[CompCases1,]

OrbitData1 <- OrbitUnknownData[,(names(OrbitUnknownData) %in% ParamsOrbits)]
OrbitData1 <- OrbitData1[CompCases1,]


OrbitData$orbit_type <- ProcessedData$orbit_type[CompCases]
OrbitRawData$orbit_type <- ProcessedData$orbit_type[CompCases]
OrbitData1$orbit_type <- OrbitUnknownData$orbit_type[CompCases1]
OrbitZerosRawData$orbit_type <- OrbitUnknownData$orbit_type[CompCases1]

# Plotting pairs of important variables colored by orbit types
png(filename = "PairsRawDataAllOrbits.png", width = 5500, height = 5500)
ggpairs(OrbitRawData, colour='orbit_type', alpha=0.4)
dev.off()

# Plotting pairs of important variables for orbit type "0", inside which we want to find subtypes
png(filename = "PairsRawDataZeroOrbits.png.png", width = 5500, height = 5500)
ggpairs(OrbitZerosRawData, colour='orbit_type', alpha=0.4)
dev.off()

# Plot a simple decision tree how orbit types are determined
fit0 = rpart(orbit_type~.,
             data = OrbitRawData)
png(filename = "PlotRPART.png", width = 2000, height = 2000)
fancyRpartPlot(fit0) 
dev.off()




# KMEANS
OrbitData1$orbit_type <- NULL
OrbitZerosRawData$orbit_type <- NULL

# K-Means Cluster Analysis
fit <- kmeans(OrbitData1, 2) # 2 cluster solution
# get cluster means 
aggregate(OrbitData1,by=list(fit$cluster),FUN=mean)
# append cluster assignment
clu <- fit$cluster
OrbitData1$cluster <- factor(clu)
OrbitZerosRawData$cluster <- factor(clu)

#Plot clusters
png(filename = "2ClustersAmongZeros.png", width = 5500, height = 5500)
ggpairs(OrbitZerosRawData, colour='cluster', alpha=0.4)
dev.off()

# Plot a simple decision tree how orbit subtypes inside orbit type 0 are determined
fit0 = rpart(cluster~.,
             data = OrbitZerosRawData)
png(filename = "2ClustersPlotRPART.png", width = 2000, height = 2000)
fancyRpartPlot(fit0) 
dev.off()