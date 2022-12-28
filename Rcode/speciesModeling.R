# code for RF modeling for species using ONRC drone lidar data
#
library(randomForest)
library(dplyr)
library(caret)
library(xgboost)
library(ggplot2)

#
# 7/19/22 made changes to read new manually adjusted tree locations
# changed file name, columns, bestmtry
#
# 10/26/2022...after move
# Still trying to figure out the behavior where I get the same numbers for false predictions using all data
# that I get using the testing data. This only happens if I fit the RF model using the default for maxnodes.
# If I set maxnodes = bestmtry - 1, I get behavior that makes sense.
#
#inputDataFile <- "G:/R_Stuff/ONRCDroneLidar/T3_AdjXY_Training_TreeTops_AllPlots.csv"
#inputDataFile <- "G:/R_Stuff/ONRCDroneLidar/AdjustedField_T3_Training_TreeTops_AllPlots.csv"
inputDataFile <- "D:/Backup/R_Stuff/ONRCDroneLidar/AdjustedField_T3_Training_TreeTops_AllPlots.csv"

# read data
inputData <- read.csv(inputDataFile, stringsAsFactors = FALSE)

# compute relative percentiles...divide P?? by P99. I use these when dealing with clips of individual trees
# to allow the use of percentiles in cases where one species is much higher than another
#
# for this case with the point clips for the upper 3m of crowns, these are less useful (but still useful)
inputData$RP01 <- inputData$Elev.P01 / inputData$Elev.P99
inputData$RP05 <- inputData$Elev.P05 / inputData$Elev.P99
inputData$RP10 <- inputData$Elev.P10 / inputData$Elev.P99
inputData$RP20 <- inputData$Elev.P20 / inputData$Elev.P99
inputData$RP25 <- inputData$Elev.P25 / inputData$Elev.P99
inputData$RP30 <- inputData$Elev.P30 / inputData$Elev.P99
inputData$RP40 <- inputData$Elev.P40 / inputData$Elev.P99
inputData$RP50 <- inputData$Elev.P50 / inputData$Elev.P99
inputData$RP60 <- inputData$Elev.P60 / inputData$Elev.P99
inputData$RP70 <- inputData$Elev.P70 / inputData$Elev.P99
inputData$RP75 <- inputData$Elev.P75 / inputData$Elev.P99
inputData$RP80 <- inputData$Elev.P80 / inputData$Elev.P99
inputData$RP90 <- inputData$Elev.P90 / inputData$Elev.P99
inputData$RP95 <- inputData$Elev.P95 / inputData$Elev.P99

#colnames(inputData)

DropWindyPlots <- FALSE
if (DropWindyPlots) {
  # drop plots with wind
  inputData <- dplyr::filter(inputData, Plot_Number != 18 & Plot_Number != 25 & Plot_Number != 27 & Plot_Number != 29 & Plot_Number != 34)
}

# extract useful columns...by number
# 7 species
# 44:48 metrics directly related to height...all clips should have heights from 0-3m so these metrics are OK to use for RF
# 49 height CV
# 50 height IQ distance
# 51:80 height distribution metrics
# 81:114 intensity metrics
# 125:138 relative percentiles
#modelData <- inputData[, c(7, 44:48, 49, 50, 51:80, 81:114, 125:138)]
#modelData <- inputData[, c(8, 52:89, 90:123, 134:147)]

# this is all lidar metrics...89.0% accuracy
modelData <- inputData[, c(49, 98:135, 136:169, 180:193)]

# this is only height related metrics...no intensity...84.5% accuracy
#modelData <- inputData[, c(49, 98:135, 180:193)]

# filter out all species except PSME & TSHE...only 31 trees that could be matched to lidar trees
modelData <- dplyr::filter(modelData, Species == "PSME" | Species == "TSHE")

# make species a factor...do this after filtering for PSME & TSHE so the factor values only have the 2 species
# if you make Species a factor before filtering, you get errors in the call to randomForest
# you need to have the response variable for your model as a factor or else randomForest will do regression
modelData$Species <- as.factor(modelData$Species)

allData <- modelData

# *****************************************************************************
# very important!!!
# the row/column arrangement in the confusion matrix output by randomForest
# is the opposite of that output by confusionMatrix
# for the RF output, read across the rows to evaluate the classification
# but for confusionMatrix output, read down the columns to evaluate
# *****************************************************************************

modelData <- allData

# drop P95 and P99...testing RF behavior
#modelData <- modelData[, -c(35,36)]

# limit variables
#modelData <- allData[, c("Species", "Elev.P99", "Elev.P95", "Int.L.skewness", "Int.L3")]

# set seed so we can replicate results
set.seed(153756)

# randomly split data into training (80%) and testing sets (20%)
ind = sample(2, nrow(modelData), replace=TRUE, prob=c(0.8,0.2))
trainingData = modelData[ind == 1, ]
testingData = modelData[ind == 2, ]

table(trainingData$Species)
table(testingData$Species)
table(modelData$Species)

# flag for model tuning. If TRUE, only mtry is tuned. Otherwise, more parameters are tuned
# using caret procedures from https://stackoverflow.com/questions/57939453/building-a-randomforest-with-caret
#
# *****downside is that the more complex tuning requires that you look at the outputs to find the best set of
# hyperparameters. This would be much better if the code selected the best set and then used the parameters
# to fit the final model.
#
# The more advanced tuning also takes much longer to run. In the initial testing, the improvement in model
# performance wasn't that great.
simpleTune <- FALSE

if (simpleTune) {
  # use a tuning function to find the best mtry value (lowest OOB error)
  # the best value will very depending on the random number seed (be careful if you rerun this code without initializing the seed above)
  bestmtry <- tuneRF(trainingData[, -1], trainingData$Species, ntreeTry = 1000, stepFactor = 1.5, improve = 0.01, trace=T, plot= T)
  i <- which(bestmtry==min(bestmtry), arr.ind = TRUE)
  bestmtry <- bestmtry[i[1]]
  #bestmtry <- 13
  #bestmtry <- 19
  print(paste("Best value for mtry = ", bestmtry))

  # build a random forest model using the training data... we have more TSHE than PSME so use sampsize
  # parameter to equalize the sample. Balanced sample probably isn't needed...species counts aren't that
  # different (247 PSME & 278 TSHE). Accuracy is slightly (0.65%) better when NOT balancing the sample.
  speciesRF <- randomForest(Species ~ .
                         , data = trainingData
  #                       , importance = TRUE
  #                       , sampsize = rep(sum(trainingData$Species == "PSME")
  #                                        , nlevels(trainingData$Species)
  #                                       )
                         , mtry = bestmtry
                         , ntree = 1000
  #                       , nodesize = 4
                         , maxnodes = bestmtry - 1
  )
} else {
  # create predefined folds
  set.seed(1234)
  cv_folds <- createFolds(modelData$Species, k = 5, returnTrain = TRUE)

  # create tune control
  tuneGrid <- expand.grid(.mtry = c(1 : 15))

  # default summary
  # ***** need to define metric = "Kappa" in call to train()
  # ctrl <- trainControl(method = "cv",
  #                      number = 5,
  #                      search = 'grid',
  #                      classProbs = TRUE,
  #                      savePredictions = "final",
  #                      index = cv_folds)

  ctrl <- trainControl(method = "cv",
                       number = 5,
                       search = 'grid',
                       classProbs = TRUE,
                       savePredictions = "final",
                       index = cv_folds,
                       summaryFunction = twoClassSummary) #in most cases a better summary for two class problems

  # add more parameters to tune...also adds run time
  ntrees <- c(250, 500, 1000)
  nodesize <- c(1, 2, 4, 6)

  params <- expand.grid(ntrees = ntrees,
                        nodesize = nodesize)

  # train model
  store_maxnode <- vector("list", nrow(params))
  for(i in 1:nrow(params)) {
    nodesize <- params[i,2]
    ntree <- params[i,1]
    set.seed(65)
    rf_model <- train(Species~.,
                      data = modelData,
                      method = "rf",
                      importance=TRUE,
                      metric = "ROC",
#                      metric = "Kappa",
                      tuneGrid = tuneGrid,
                      trControl = ctrl,
                      ntree = ntree,
                      nodesize = nodesize)
    store_maxnode[[i]] <- rf_model
  }

  # add names to list using parameters
  names(store_maxnode) <- paste("ntrees:", params$ntrees,
                                "nodesize:", params$nodesize)

  # combine results
  results_mtry <- resamples(store_maxnode)

  summary(results_mtry)

  # get best mtry for model
  lapply(store_maxnode, function(x) x$best)

  # get best average performance for models
  t <- lapply(store_maxnode, function(x) x$results[x$results$ROC == max(x$results$ROC),])

  # find best model using ROC...should be an easier way to access the list of RF models but
  # I couldn't figure it out so I looped.
  besti <- 0
  bestmtry <- 0
  bestnodesize <- 0
  bestntree <- 0
  bestROC <- 0
  for(i in 1:nrow(params)) {
    if (t[[i]]$ROC > bestROC) {
      besti <- i
      bestROC <- t[[i]]$ROC
      bestmtry <- t[[i]]$mtry
      bestnodesize <- params[i,2]
      bestntree <- params[i,1]
    }
  }


  # run best model after looking at results to find model with highest ROC
  speciesRF <- randomForest(Species ~ .
                            , data = trainingData
    #                       , importance = TRUE
    #                       , sampsize = rep(sum(trainingData$Species == "PSME")
    #                                        , nlevels(trainingData$Species)
    #                                       )
                            , mtry = bestmtry
                            , ntree = bestntree
                            , nodesize = bestnodesize
  )

  print(paste("Best model based on area under ROC:"))
  print(paste("   mtry = ", bestmtry))
  print(paste("   ntree = ", bestntree))
  print(paste("   nodesize = ", bestnodesize))
  print(paste("   ROC value = ", bestROC))
}

# save model for later use
saveRDS(speciesRF, "FINAL_RF_HW_DF_Model.rds")
speciesRF

# compute accuracy for training data...should give same result as model summary but row/columns will be "correct"
table(speciesRF$predicted, trainingData$Species)
CM <- table(speciesRF$predicted, trainingData$Species)

accuracy <- (sum(diag(CM)))/sum(CM)
print(paste("Accuracy for original training data: ", accuracy))

# predict using testing data
typePred <- predict(speciesRF, newdata = testingData[, -1])
table(typePred, testingData$Species)
CM <- table(typePred, testingData$Species)

accuracy <- (sum(diag(CM)))/sum(CM)
print(paste("Accuracy for hold-out testing data: ", accuracy))

# predict using all data
typePred <- predict(speciesRF, newdata = modelData[, -1])
table(typePred, modelData$Species)
CM <- table(typePred, modelData$Species)

accuracy <- (sum(diag(CM)))/sum(CM)
print(paste("Accuracy for all data (not useful!!): ", accuracy))

# look at the variable importance scores...basically tells you which variables are most useful in the classification
imp <- data.frame(importance(speciesRF, scale=TRUE, type = 2))
imp$variable <- row.names(imp)
imp <- imp[order(imp[,1], decreasing = TRUE), ]
imp <- imp[, c(2, 1)]
row.names(imp) <- NULL
imp

# plots...scatter plots show how the two species differ
# whisker plots show the "strength" of the metrics plotted. Ideally, the boxes on
# the plots don't overlap (or overlap very little)
plot(modelData$Elev.P99, modelData$Int.L.skewness, col = modelData$Species)
plot(modelData$Elev.P99, modelData$Elev.P99, col = modelData$Species)
plot(modelData$Elev.P99, modelData$Elev.P95, col = modelData$Species)
plot(modelData$Elev.P99, modelData$Elev.stddev, col = modelData$Species)

ggplot(modelData, aes(x = Species, y = Elev.P99)) + geom_boxplot()
ggplot(modelData, aes(x = Species, y = Elev.P95)) + geom_boxplot()
ggplot(modelData, aes(x = Species, y = Int.L.skewness)) + geom_boxplot()

legend(x = "topleft",          # Position
       legend = unique(modelData$Species),  # Legend texts
       col = c(1, 2),           # Line colors
       pch = c(1, 1)
)

# ******************************************************************************
# --------------------------- problem solved...don't leave maxnodes at the default value.
# may be best to set it at mtry - 1. This way, the model is fit with some uncertainty
# so the number of trees becomes important. When trees go all the way to full splits,
# we get overfitting so the predictions for new data are not as good.
#
#
# there is something strange with the code below. I get results with all data that are much
# better than with the training data. I also get exactly the same number of "incorrect" species calls
# with all data that I get with the testing data

# # predict species for testing data and generate a normal confusion matrix
speciesPred <- speciesRF$predicted
t <- data.frame("actual" = trainingData$Species, "predicted" = speciesPred)
t$actual != t$predicted

typePred <- speciesRF$predicted
table(typePred, trainingData$Species)

CM <- table(typePred, trainingData$Species)
accuracy <- (sum(diag(CM)))/sum(CM)
accuracy

# predict species for testing data and generate a normal confusion matrix
speciesPred <- predict(speciesRF, newdata = testingData)
testingCM <- confusionMatrix(speciesPred, testingData$Species)
message("--TESTING DATA--")
testingCM

# predict species for all data (PSEM & TSHE only) and generate a normal confusion matrix
speciesPred <- predict(speciesRF, newdata = modelData)
allCM <- confusionMatrix(speciesPred, modelData$Species)
message("--ALL DATA--")
allCM

# filter PSME & TSHE from original input data so we can get back the file name for the point clips
originalData <- dplyr::filter(inputData, Species == "PSME" | Species == "TSHE")
originalData$Species <- as.factor(originalData$Species)
# predict species for all data (PSEM & TSHE only) and generate a normal confusion matrix
speciesPred <- predict(speciesRF, newdata = originalData)
allCM <- confusionMatrix(speciesPred, originalData$Species)
message("--ORIGINAL DATA--")
allCM

# assign predicted species to new column and save data
originalData$PredictedSpecies <- speciesPred

write.csv(originalData, file = "G:/R_Stuff/ONRCDroneLidar/T3_Predicted_Species_AllPlots.csv", row.names = FALSE)

# read in data with predicted species and file names, look for incorrect classifications and
# build a batch file to display point clips for trees
inData <- read.csv("G:/R_Stuff/ONRCDroneLidar/T3_Predicted_Species_AllPlots.csv", stringsAsFactors = FALSE)

inData <- dplyr::filter(inData, Species != PredictedSpecies)

# # write list of misclassified tree files...full tree clips
# outFile <- file("G:/R_Stuff/ONRCDroneLidar/list.txt", "wt")
# writeLines(paste0("\"", inData$DataFile.y, "\""), outFile)
# close(outFile)

# write batch file to view trees
batFile <- file("G:/R_Stuff/ONRCDroneLidar/T3_ViewMisclassifiedTrees.bat", "wt")
writeLines(paste0("rem called ", inData$PredictedSpecies, "\n", "start pdq \"", inData$DataFile.y, "\"", "\n", "pause"), batFile)
close(batFile)

# xgboost
train_x <- data.matrix(trainingData[, -1])
train_y <- trainingData[, 1]

test_x <- data.matrix(testingData[, -1])
test_y <- testingData[, 1]

xgb_train <- xgb.DMatrix(data = train_x, label = train_y)
xgb_test <- xgb.DMatrix(data = test_x, label = test_y)

model <- xgboost(data = xgb_train,                    # the data
                 max.depth = 3,                           # max depth
                 nrounds = 50)                              # max number of boosting iterations

summary(model)

pred_test = predict(model, xgb_test)

pred_test

pred_test[(pred_test>3)] = 3
pred_y = as.factor((levels(test_y))[round(pred_test)])
print(pred_y)

conf_mat = confusionMatrix(test_y, pred_y)
print(conf_mat)














#
# 5/9/2022...trying a new approach to avoid strange behavior when predicting with all data
# https://www.janbasktraining.com/blog/random-forest-in-r/
library(caret)
modelData <- allData

# limit variables
#modelData <- allData[, c("Species", "Elev.P99", "Elev.P95", "Int.L.skewness", "Int.L3")]

# set seed so we can replicate results
set.seed(1593756)

# randomly split data into training (70%) and testing sets (30%)
ind = sample(2, nrow(modelData), replace=TRUE, prob=c(0.7,0.3))
trainingData = modelData[ind == 1, ]
testingData = modelData[ind == 2, ]

# get minimum for class
minSampleSize <- min(sum(trainingData$Species == "PSME"), sum(trainingData$Species == "TSHE"))

# 10-fold cross validation
set.seed(1593976)
trControl <- trainControl(method="cv", number=10, search="grid")
rf_default <- train(Species~.,
                    data = trainingData,
                    method = "rf",
                    metric = "Accuracy",
                    trControl = trControl
                    , strata=as.factor(trainingData$Species)
                    , sampsize = c(minSampleSize, minSampleSize)
                    )
# Print the results
print(rf_default)

# find best mtry
set.seed(1593756)

tuneGrid <- expand.grid(.mtry = seq(3, 51, by = 3))
rf_mtry <- train(Species~.,
                 data = trainingData,
                 method = "rf",
                 metric = "Accuracy",
                 tuneGrid = tuneGrid,
                 trControl = trControl,
                 strata=as.factor(trainingData$Species),
                 sampsize = c(minSampleSize, minSampleSize),
                 importance = TRUE,
                 nodesize = 1,
                 ntree = 300)
print(rf_mtry)

# store best mtry
best_mtry <- rf_mtry$bestTune$mtry

# find best maxnodes
store_maxnode <- list()
tuneGrid <- expand.grid(.mtry = best_mtry)
for (maxnodes in c(5: 40)) {
        set.seed(1234)
        rf_maxnode <- train(Species~.,
                            data = trainingData,
                            method = "rf",
                            metric = "Accuracy",
                            tuneGrid = tuneGrid,
                            trControl = trControl,
                            strata=as.factor(trainingData$Species),
                            sampsize = c(minSampleSize, minSampleSize),
                            importance = TRUE,
                            nodesize = 1,
                            maxnodes = maxnodes,
                            ntree = 300)
        current_iteration <- toString(maxnodes)
        store_maxnode[[current_iteration]] <- rf_maxnode
}
results_maxnodes <- resamples(store_maxnode)
summary(results_maxnodes)

maxnode <- 34

# search for best ntrees
store_maxtrees <- list()
for (ntree in c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000, 2000)) {
        set.seed(5678)
        rf_maxtrees <- train(Species~.,
                             data = trainingData,
                             method = "rf",
                             metric = "Accuracy",
                             tuneGrid = tuneGrid,
                             trControl = trControl,
                             strata=as.factor(trainingData$Species),
                             sampsize = c(minSampleSize, minSampleSize),
                             importance = TRUE,
                             nodesize = 1,
                             maxnodes = maxnode,
                             ntree = ntree)
        key <- toString(ntree)
        store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree)

maxtrees <- 2000

# fit tuned model
fit_rf <- train(Species~.,
                trainingData,
                method = "rf",
                metric = "Accuracy",
                tuneGrid = tuneGrid,
                trControl = trControl,
                strata=as.factor(trainingData$Species),
                sampsize = c(minSampleSize, minSampleSize),
                importance = TRUE,
                nodesize = 1,
                ntree = maxtrees,
                maxnodes = maxnode)
fit_rf

#make predictions with testing data
prediction <-predict(fit_rf, testingData)
confusionMatrix(prediction, testingData$Species)

prediction <-predict(rf_default, testingData)
confusionMatrix(prediction, testingData$Species)


# make predictions with entire data set
prediction <-predict(fit_rf, modelData)
confusionMatrix(prediction, modelData$Species)

prediction <-predict(rf_default, modelData)
confusionMatrix(prediction, modelData$Species)

