# Clear Workspace
rm(list = ls())

# Necessary libraries
library(caret)
library(randomForest)
library(Metrics)
library(foreach)
library(gbm)
library(party)

# Set seed for reproducibility and also set working directory
set.seed(1)
setwd("C:/Users/User/Documents/R/SpaceApps1/")

load("RawData.rda")
load("DiameterTrain.rda")
load("DiameterTest.rda")


# Further partitioning our original training data into training and test sets
inTrain         = createDataPartition(DiameterTrain$diamLOG, p = 0.8)[[1]]
training        <- DiameterTrain[inTrain,]
remainder        <- DiameterTrain[-inTrain,]




####


# Parameters for caret's train
fitControl <- trainControl(method = "repeatedcv",        # do repeated Cross Validation
                           number = 2,                   # 2-fold
                           repeats = 2)                  # repeat 2 times each 



NNET <- train(diamLOG~albedo_neowise + 
                      albedo + 
                      albedo_unc + 
                      perihelion_date_jd + 
                      h + 
                      p_vector_y + 
                      p_vector_z + 
                      q_vector_z + 
                      argument_of_perihelion + 
                      p_vector_x + 
                      q_vector_z + 
                      ascending_node + 
                      eccentricity + 
                      spin_max_amplitude + 
                      mean_anomaly + 
                      h_neowise + 
                      pabl + 
                      absolute_magnitude,  
               data = training,
               method = "brnn",
              tunelength = 2250,
               trControl = fitControl
)
save(NNET, file = "NNET.rda")
load(file = "NNET.rda")

predictionNNET <- predict(NNET, newdata = remainder)
RMSE(exp(predictionNNET),remainder$diamNotNormalised)
# RMSE = 2.075327


####
####
####
nnet_grid <- expand.grid(.decay = 10^seq(-4, -1, 1), .size = c(8))
NNET1 <- train(diamLOG~albedo_neowise + 
                       albedo + 
                       albedo_unc + 
                       perihelion_date_jd + 
                       h + 
                       p_vector_y + 
                       p_vector_z + 
                       q_vector_z + 
                       argument_of_perihelion + 
                       p_vector_x + 
                       q_vector_z + 
                       ascending_node + 
                       eccentricity + 
                       spin_max_amplitude + 
                       mean_anomaly + 
                       h_neowise + 
                       pabl + 
                       absolute_magnitude,  
              data = training,
              method = "nnet",
              MaxNWts = 100000,
              trControl = fitControl,
              maxit = 3000, 
              preProcess = "range",
              tunelength = 100,
              linout=TRUE, 
              trace = FALSE
)
save(NNET, file = "NNET1.rda")
load(file = "NNET1.rda")

predictionNNET1 <- predict(NNET1, newdata = remainder)
RMSE(exp(predictionNNET1),remainder$diamNotNormalised)
# RMSE = 1.795411




# Generalised linear model
GLM <- train(diamLOG~albedo_neowise + 
                     albedo + 
                     albedo_unc + 
                     perihelion_date_jd + 
                     h + 
                     p_vector_y + 
                     p_vector_z + 
                     q_vector_z + 
                     argument_of_perihelion + 
                     p_vector_x + 
                     q_vector_z + 
                     ascending_node + 
                     eccentricity + 
                     spin_max_amplitude + 
                     mean_anomaly + 
                     h_neowise + 
                     pabl + 
                     absolute_magnitude,  
            data = training, 
            method = "glm",
            tuneLength = 100,
            trControl = fitControl
)
save(GLM, file = "glm.rda")
load(file = "glm.rda")

predictionGLM <- predict(GLM, newdata = remainder)
RMSE(exp(predictionGLM),remainder$diamNotNormalised)
# RMSE = 8.416011




# Conditional inference trees
CT <- train(diamLOG~albedo_neowise + 
                    albedo + 
                    albedo_unc + 
                    perihelion_date_jd + 
                    h + 
                    p_vector_y + 
                    p_vector_z + 
                    q_vector_z + 
                    argument_of_perihelion + 
                    p_vector_x + 
                    q_vector_z + 
                    ascending_node + 
                    eccentricity + 
                    spin_max_amplitude + 
                    mean_anomaly + 
                    h_neowise + 
                    pabl + 
                    absolute_magnitude,  
            data = training, 
            method = "ctree",
            tuneLength = 3,
            trControl = fitControl
)
save(CT, file = "ctree.rda")
load(file = "ctree.rda")

predictionCT <- predict(CT, newdata = remainder)
RMSE(exp(predictionCT),remainder$diamNotNormalised)
# RMSE = 1.642696






##
# Random forest
set.seed(415)
RFfit <- randomForest(diamLOG~albedo_neowise + 
                              albedo + 
                              albedo_unc + 
                              perihelion_date_jd + 
                              h + 
                              p_vector_y + 
                              p_vector_z + 
                              q_vector_z + 
                              argument_of_perihelion + 
                              p_vector_x + 
                              q_vector_z + 
                              ascending_node + 
                              eccentricity + 
                              spin_max_amplitude + 
                              mean_anomaly + 
                              h_neowise + 
                              pabl + 
                              absolute_magnitude,   
                      data = training, 
                      ntrees = 1000,
                      importance = TRUE)

save(RFfit, file = "RFfit.rda")
load("RFfit.rda")

predictionRF <- predict(RFfit, newdata = remainder)
RMSE(exp(predictionRF),remainder$diamNotNormalised)
# RMSE = 0.4397909




# Plot variable importance [first was trained with all data to determine varImp]
png(filename = "RFimpDIAM.png", width = 1920, height = 1080)
varImpPlot(RFfit)
dev.off()


# indexing
# [,!(names(training) %in% c("designation", "color_ignore", "lightcurve_notes ", "name ", "spin_updated_at "
#                            , "diameter", "diameter_neowise_unc","diameter_unc", 
#                            "objectnumber", "diameter_neowise", "diamNotNormalised",
#                            "critical_list_numbered_object","neo", "pha"))]
