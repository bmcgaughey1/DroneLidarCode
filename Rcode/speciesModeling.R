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

# set this flag variable if you want to use the metrics for the point clips for leaning trees that were
# created using smaller point clips around the tree locations and also accounting for tree lean as
# measured when Ally & Bob adjusted tree locations. Hopefully this set of metrics produces better
# model performance.
useLeaningTrees <- TRUE

# read input file...format varies slighlty between the original file and the leaning tree file
# testing: inputDataFile <- "G:/R_Stuff/ONRCDroneLidar/T3_AdjXY_Training_TreeTops_AllPlots.csv"
# testing: inputDataFile <- "G:/R_Stuff/ONRCDroneLidar/AdjustedField_T3_Training_TreeTops_AllPlots.csv"
if (useLeaningTrees) {
  inputDataFile <- "H:/T3_DroneLidar/Leaning_TreeTops_SmallCylinder_normalized_metrics_10_25_2023.csv"
} else {
  inputDataFile <- "extras/AdjustedField_T3_Training_TreeTops_AllPlots.csv"
}

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

# do some analyses related to the amount of lean and the offset from the tree base to the tree top
# useLeaningTrees must be TRUE so we have the lean info
if (useLeaningTrees) {
  aveHt <- (inputData$Total.Height.Ally + inputData$Total.Height.Bob) / 2
  aveLean <- (inputData$Lean.Angle.From.Vertical.Ally + inputData$Lean.Angle.From.Vertical.Bob) / 2
  aveLeanAzimuth <- (inputData$Lean.Azimuth.Ally + inputData$Lean.Azimuth.Bob) / 2

  offset <- sin(aveLean * pi / 180.0) * aveHt
  cat("Summary of offset distances(m)\n")
  print(summary(offset))
  cat("Std Dev offset distances: ", sd(offset), "\n")
  cat("\nSummary of lean angles (degrees)\n")
  print(summary(aveLean))
  cat("Std Dev lean angle: ", sd(aveLean), "\n")

  hist(aveLean, main = "", xlab = "Tree lean (degrees)", labels = TRUE)
  hist(offset, main = "", xlab = "Offset from tree base to tree top (meters)", labels = TRUE)
}
# end of lean analyses

DropWindyPlots <- FALSE
if (DropWindyPlots) {
  # drop plots with wind
  inputData <- dplyr::filter(inputData, Plot_Number != 18 & Plot_Number != 25 & Plot_Number != 27 & Plot_Number != 29 & Plot_Number != 34)
}

# Sorting out column numbers can be a real pain. Easy way is to load the csv file into excel. Then copy the column labels (entire first row).
# open a new spreadsheet (or tab in the current spreadsheet), put the cursor in row 1 column B and paste special...select transpose. Then
# create sequential numbers in column A (put 1, 2, 3 in first three rows, select them and double click the small rectangle in the lower
# right corner of the selection box).

if (useLeaningTrees) {
  # extract useful columns...by number
  # 48 species
  # 91:128 metrics directly related to height...all clips should have heights from 0-3m so these metrics are OK to use for RF
  # 129:161 intensity metrics
  # 162 profile area
  # 163:176 relative percentiles

  # this is all lidar metrics...89.0% accuracy
  modelData <- inputData[, c(48, 91:128, 129:161, 162, 163:176)]
} else {
  # extract useful columns...by number
  # 49 species
  # 98:135 metrics directly related to height...all clips should have heights from 0-3m so these metrics are OK to use for RF
  # 136:168 intensity metrics
  # 169 profile area
  # 180:193 relative percentiles

  # this is all lidar metrics...89.0% accuracy
  modelData <- inputData[, c(49, 98:135, 136:168, 169, 180:193)]
}

if (useLeaningTrees) {
  #modelData <- inputData[, c(48, 91:128, 163:176)]
} else {
  # this is only height related metrics...no intensity...84.5% accuracy...for original tree metrics
  #modelData <- inputData[, c(49, 98:135, 180:193)]
}

# filter out all species except PSME & TSHE...only 31 trees that could be matched to lidar trees
modelData <- dplyr::filter(modelData, Species == "PSME" | Species == "TSHE")

# make species a factor...do this after filtering for PSME & TSHE so the factor values only have the 2 species
# if you make Species a factor before filtering, you get errors in the call to randomForest
# you need to have the response variable for your model as a factor or else randomForest will do regression
modelData$Species <- as.factor(modelData$Species)

allData <- modelData

# write the data used for model development and testing
write.csv(allData, "H:/T3_DroneLidar/Leaning_TreeTops_SmallCylinder_normalized_metrics_ModelTraining.csv")

# *****************************************************************************
# very important!!!
# the row/column arrangement in the confusion matrix output by randomForest
# is the opposite of that output by confusionMatrix
# for the RF output, read across the rows to evaluate the classification
# but for confusionMatrix output, read down the columns to evaluate
# *****************************************************************************

modelData <- allData

if (useLeaningTrees) {
  # drop rows with bad metrics
  modelData <- modelData[modelData$Elev.stddev > 0, ]
}

# drop P95 and P99...testing RF behavior
#modelData <- modelData[, -c(35,36)]

# limit variables
#modelData <- allData[, c("Species", "Elev.P99", "Elev.P95", "Int.L.skewness", "Int.L3")]

# drop the min/max values for elevation because we expect these to be 0 and 3.0 or very close
modelData <- modelData[, -c(2, 3)]

# drop all intensity variables
dropIntensity <- FALSE
dropElevation <- FALSE
if (dropIntensity) {
  modelData <- modelData[, c(1:37, 71:85)]
}
if (dropElevation) {
  modelData <- modelData[, c(1, 38:70)]
}

# set seed so we can replicate results
set.seed(15356)

# randomly split data into training and testing sets...the split seems to matter. I did
# testing (see end of code) to evaluate the split proportion and the stability of the
# error rates (cross validation with averaged OOB and accuracy). However, I didn't really
# come up with a good strategy for selecting the best split. Intuitively, you want to
# have a model that fits well (low OOB) and also performs well with "new" data (low error
# with testing data). I looked at CV of OOB and testing-data error as possible criteria for
# selecting a final split. My logic is that you want the split that gives roughly the same
# error for both and that produces low CV (indicating model and prediction stability, I think).
#
# The problem is that when you do the cross validation, you are looking at results over
# multiple iterations but you ultimately have to use a single iteration to predict species
# for "new" data so you really don't know anything about the single-iteration model's
# stability.
#
# I think the final strategy is to fit a model using a reasonable split (informed by
# the sensitivity analysis), report the OOB, Kappa, and error for the testing data. Then
# run cross validation using the same split to assess the stability of the split and
# predictor variables. Report the average OOB, Kappa and testing error to indicate that
# our overall data set is providing a classifier that is robust enough for use.
#
# Alternatively, do the cross validation testing and use the results to determine the
# split. I did this and looked for the split that produced the most similar values for
# OOB and error with testing data. 55/35 split was the "best".
#
# Another question is the data used for tuning. Should you use the training data only
# or all data? The tuneRanger() function uses prediction accuracy as the test criteria
# (measure can be changed) but doesn't use a data split. I think it makes the most sense to
# tune parameters using the split data but I haven't been able to find any guidence online.
#
# I did some testing at the end of the code. A 50/50 or 60/40 split seems to produce
# a model with fairly low OOB error and low error when predicting the testing data
train_indx = sample(x = 1:nrow(modelData),
                    size = 0.55 * nrow(modelData),
                    replace = FALSE)

trainingData <- modelData[train_indx,]
testingData <- modelData[-train_indx,]

#ind = sample(2, nrow(modelData), replace=FALSE, prob=c(0.8,0.2))
#trainingData = modelData[ind == 1, ]
#testingData = modelData[ind == 2, ]

table(trainingData$Species)
table(testingData$Species)
table(modelData$Species)

modelData %>%
  group_by(Species) %>%
  summarize(
    Count = n(),
    AveP99 = mean(Elev.P99, na.rm = TRUE),
    IntP80 = mean(Int.P80, na.rm = TRUE),
    IntP20 = mean(Int.P20, na.rm = TRUE),
    IntIQ = mean(Int.IQ, na.rm = TRUE)
  )

trainingData %>%
  group_by(Species) %>%
  summarize(
    Count = n(),
    AveP99 = mean(Elev.P99, na.rm = TRUE),
    IntP80 = mean(Int.P80, na.rm = TRUE),
    IntP20 = mean(Int.P20, na.rm = TRUE),
    IntIQ = mean(Int.IQ, na.rm = TRUE)
  )

testingData %>%
  group_by(Species) %>%
  summarize(
    Count = n(),
    AveP99 = mean(Elev.P99, na.rm = TRUE),
    IntP80 = mean(Int.P80, na.rm = TRUE),
    IntP20 = mean(Int.P20, na.rm = TRUE),
    IntIQ = mean(Int.IQ, na.rm = TRUE)
  )

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
probstTune <- TRUE

if (simpleTune) {
  # use a tuning function to find the best mtry value (lowest OOB error)
  # the best value will very depending on the random number seed (be careful if you rerun this code without initializing the seed above)
  bestmtry <- tuneRF(trainingData[, -1], trainingData$Species, ntreeTry = 1000, stepFactor = 1.5, improve = 0.01, trace=T, plot= T)
  i <- which(bestmtry==min(bestmtry), arr.ind = TRUE)
  bestmtry <- bestmtry[i[1]]
  #bestmtry <- 25
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
} else if (probstTune == FALSE) {
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
} else {
  # probst tune
  library(tuneRanger)
  library(ranger)
  library(mlr)
  library(OpenML)

  model.task = makeClassifTask(data = modelData, target = "Species")

  # Estimate runtime
  estimateTimeTuneRanger(model.task)

  set.seed(153756)

  # Tuning
#  res = tuneRanger(model.task, measure = list(multiclass.brier), num.trees = 1000,
#                   num.threads = 2, iters = 70, iters.warmup = 30)
  res = tuneRanger(model.task, measure = list(acc), num.trees = 1000,
                   num.threads = 2, iters = 70, iters.warmup = 30)

  res

  speciesRF <- ranger(Species ~ ., data = modelData
                      , num.trees = 1000
                      , mtry = res$recommended.pars$mtry
                      , min.node.size = res$recommended.pars$min.node.size
                      , sample.fraction = c (res$recommended.pars$sample.fraction, res$recommended.pars$sample.fraction)
                      , importance = "permutation"
                      )
  speciesRF
  imp <- importance(speciesRF)
  imp <- imp[order(imp)]
  imp

  # predict using training data
  typePred <- predict(speciesRF, data = trainingData[, -1])
  table(typePred$predictions, trainingData$Species)
  CM <- table(typePred$predictions, trainingData$Species)
  confusionMatrix(typePred$predictions, trainingData$Species)

  accuracy <- (sum(diag(CM)))/sum(CM)
  print(paste("Accuracy for original training data: ", accuracy))

  # predict using testing data
  typePred <- predict(speciesRF, data = testingData[, -1])
  table(typePred$predictions, testingData$Species)
  CM <- table(typePred$predictions, testingData$Species)
  confusionMatrix(typePred$predictions, testingData$Species)

  accuracy <- (sum(diag(CM)))/sum(CM)
  print(paste("Accuracy for testing data: ", accuracy))

  typePred <- predict(speciesRF, data = modelData[, -1])
  table(typePred$predictions, modelData$Species)
  CM <- table(typePred$predictions, modelData$Species)

  accuracy <- (sum(diag(CM)))/sum(CM)
  print(paste("Accuracy for all data: ", accuracy))

}

# save model for later use
if (useLeaningTrees) {
  saveRDS(speciesRF, "FINAL_RF_HW_DF_Model_LeaningTrees.rds")
} else {
  saveRDS(speciesRF, "FINAL_RF_HW_DF_Model.rds")
}
speciesRF

if (TRUE) {
  # experiment with cross validation function
  result <- rfcv(trainingData[, -1], trainingData$Species, cv.fold = 10, step = 0.8)
  with(result, plot(n.var, error.cv, log="x", type="o", lwd=2))
  varUsed(speciesRF)
  varImpPlot(speciesRF)
  plot(margin(speciesRF))
}

# https://www.guru99.com/r-random-forest-tutorial.html



# compute accuracy for training data...should give same result as model summary but row/columns will be "correct"
# when I used this code originally, I had the table constructed from the predictions that were part of the model
# object. This was giving me too many incorrect classifications (I think/thought). If I use the model to predict the
# training data, I get much better results...not sure why.
typePred <- predict(speciesRF, newdata = trainingData[, -1])
#table(speciesRF$predicted, trainingData$Species)
table(typePred, trainingData$Species)
#CM <- table(speciesRF$predicted, trainingData$Species)
CM <- table(typePred, trainingData$Species)
confusionMatrix(typePred, trainingData$Species)

accuracy <- (sum(diag(CM)))/sum(CM)
print(paste("Accuracy for original training data: ", accuracy))

# predict using testing data
typePred <- predict(speciesRF, newdata = testingData[, -1])
table(typePred, testingData$Species)
CM <- table(typePred, testingData$Species)
confusionMatrix(typePred, testingData$Species)

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

ggplot(modelData, aes(x = Species, y = Elev.P99)) + geom_boxplot() + theme(axis.text=element_text(size=12, face="bold"), axis.title=element_text(size=14,face="bold"))
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
modelData <- allData[complete.cases(allData), ]

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

maxnode <- 32

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

maxtrees <- 600

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








################################################################################
#testing for data split for training and testing
################################################################################
library(tuneRanger)
library(ranger)
library(mlr)
library(OpenML)

useRanger <- TRUE

for (split in seq(0.05, 0.95, by = 0.05)) {
  # set seed so we can replicate results
  set.seed(153756)

  # randomly split data into training (80%) and testing sets (20%)
  train_indx = sample(x = 1:nrow(modelData),
                      size = split * nrow(modelData),
                      replace = FALSE)

  trainingData <- modelData[train_indx,]
  testingData <- modelData[-train_indx,]

  if (useRanger) {
    speciesRF <- ranger(Species ~ ., data = trainingData, mtry = 12, min.node.size = 3, num.trees = 1000)
    #speciesRF

    # predict using training data
    # typePred <- predict(speciesRF, data = trainingData[, -1])
    # table(typePred$predictions, trainingData$Species)
    # CM <- table(typePred$predictions, trainingData$Species)
    # confusionMatrix(typePred$predictions, trainingData$Species)
    #
    # trainAccuracy <- (sum(diag(CM)))/sum(CM)

    modelOOB <- speciesRF$prediction.error

    # predict using testing data
    typePred <- predict(speciesRF, data = testingData[, -1])
    c <- confusionMatrix(typePred$predictions, testingData$Species)
    testAccuracy <- c$overall[[1]]
    testKappa <- c$overall[[2]]

    typePred <- predict(speciesRF, data = modelData[, -1])
    c <- confusionMatrix(typePred$predictions, modelData$Species)
    allAccuracy <- c$overall[[1]]
    allKappa <- c$overall[[2]]
  }
  else {
    speciesRF <- randomForest(Species ~ ., data = trainingData, mtry = 12, nodesize = 3, ntree = 1000
#                              , sampsize = rep(sum(trainingData$Species == "PSME")
#                                , nlevels(trainingData$Species)
#                                )
                              )
    #speciesRF

    # predict using training data
    # typePred <- predict(speciesRF, data = trainingData[, -1])
    # table(typePred$predictions, trainingData$Species)
    # CM <- table(typePred$predictions, trainingData$Species)
    # confusionMatrix(typePred$predictions, trainingData$Species)
    #
    # trainAccuracy <- (sum(diag(CM)))/sum(CM)

    modelOOB <- as.double(speciesRF$err.rate[500, 1])

    # predict using testing data
    typePred <- predict(speciesRF, newdata = testingData[, -1])
    c <- confusionMatrix(typePred, testingData$Species)
    testAccuracy <- c$overall[[1]]
    testKappa <- c$overall[[2]]

    typePred <- predict(speciesRF, newdata = modelData[, -1])
    c <- confusionMatrix(typePred, modelData$Species)
    allAccuracy <- c$overall[[1]]
    allKappa <- c$overall[[2]]
  }

  if (split == 0.05) {
    accRes <- data.frame(split = split, modelOOB = round(modelOOB * 100, 1), testACC = round(testAccuracy * 100, 1), testKappa = testKappa, allACC = round(allAccuracy * 100, 1), allKappa = allKappa)
  } else {
    t <- data.frame(split = split, modelOOB = round(modelOOB * 100, 1), testACC = round(testAccuracy * 100, 1), testKappa = testKappa, allACC = round(allAccuracy * 100, 1), allKappa = allKappa)
    accRes <- rbind(accRes, t)
  }
}
accRes



# test idea of a score for "best" model...use weighting for OOB and test data accuracy
wt <- c(0.25, 0.75)
accRes$score <- wt[1] * accRes$modelOOB + wt[2] * (100.0 - accRes$testACC)
accRes






################################################################################
# cross validation testing using training/testing data
#
# idea it to split data multiple times, build model with training data and
# apply it to testing data to get test statistic. rfcv() uses the training
# data and test statistic based on training data so not as useful for
# assessing how well the classifier performs on "new" data.
################################################################################
library(ranger)

useRanger <- TRUE
folds <- 100

# set seed so we can replicate results
set.seed(153756)

for (iter in c(1:folds)) {
  cat(iter, " of ", folds, "\n")
  for (split in seq(0.05, 0.95, by = 0.05)) {
    # randomly split data into training and testing sets
    train_indx = sample(x = 1:nrow(modelData),
                        size = split * nrow(modelData),
                        replace = FALSE)

    trainingData <- modelData[train_indx,]
    testingData <- modelData[-train_indx,]

    # use hyperparameter values from tuning
    if (useRanger) {
      speciesRF <- ranger(Species ~ ., data = trainingData
                          , mtry = 12
                          , min.node.size = 3
                          , sample.fraction = c(0.5488615, 0.5488615)
                          , num.trees = 1000)
      #speciesRF

      modelOOB <- speciesRF$prediction.error

      # predict using testing data
      typePred <- predict(speciesRF, data = testingData[, -1])
      c <- confusionMatrix(typePred$predictions, testingData$Species)
      testAccuracy <- c$overall[[1]]
      testKappa <- c$overall[[2]]

      typePred <- predict(speciesRF, data = modelData[, -1])
      c <- confusionMatrix(typePred$predictions, modelData$Species)
      allAccuracy <- c$overall[[1]]
      allKappa <- c$overall[[2]]
    }
    else {
      speciesRF <- randomForest(Species ~ ., data = trainingData, mtry = 12, nodesize = 3, ntree = 1000
                                #                              , sampsize = rep(sum(trainingData$Species == "PSME")
                                #                                , nlevels(trainingData$Species)
                                #                                )
      )
      #speciesRF

      modelOOB <- as.double(speciesRF$err.rate[500, 1])

      # predict using testing data
      typePred <- predict(speciesRF, newdata = testingData[, -1])
      c <- confusionMatrix(typePred, testingData$Species)
      testAccuracy <- c$overall[[1]]
      testKappa <- c$overall[[2]]

      typePred <- predict(speciesRF, newdata = modelData[, -1])
      c <- confusionMatrix(typePred, modelData$Species)
      allAccuracy <- c$overall[[1]]
      allKappa <- c$overall[[2]]
    }

    if (iter == 1 && split == 0.05) {
      accRes <- data.frame(iteration = iter, split = split, modelOOB = round(modelOOB * 100, 1), testACC = round(testAccuracy * 100, 1), testKappa = testKappa, allACC = round(allAccuracy * 100, 1), allKappa = allKappa)
    } else {
      t <- data.frame(iteration = iter, split = split, modelOOB = round(modelOOB * 100, 1), testACC = round(testAccuracy * 100, 1), testKappa = testKappa, allACC = round(allAccuracy * 100, 1), allKappa = allKappa)
      accRes <- rbind(accRes, t)
    }
  }
}
# compute a score for "best" model...use weighting for OOB and test data accuracy
# weights are pretty arbitrary. I think it makes sense to weight the performance with
# new data more heavily.
#
# One caution is that performance with very small training/testing subsets may not
# be very useful. It would be better if we had more trees...
#
#
#@@@@ this formula may not work because it doesn't consider SD'
wt <- c(0.5, 0.5)
accRes$score <- wt[1] * accRes$modelOOB + wt[2] * (100.0 - accRes$testACC)
#accRes

t <- accRes %>%
  group_by(split) %>%
  summarize(
    AveOOB = mean(modelOOB, na.rm = TRUE),
    SDOOB = sd(modelOOB, na.rm = TRUE),
    CVOOB = sd(modelOOB, na.rm = TRUE) / mean(modelOOB, na.rm = TRUE),
    AveTestACC = mean((100 - testACC), na.rm = TRUE),
    SDTestACC = sd((100 - testACC), na.rm = TRUE),
    CVTestACC = sd((100 - testACC), na.rm = TRUE) / mean((100 - testACC), na.rm = TRUE),
    AveTestKappa = mean(testKappa, na.rm = TRUE),
    SDTestKappa = sd(testKappa, na.rm = TRUE),
    AveScore = mean(score, na.rm = TRUE),
    SDScore = sd(score, na.rm = TRUE),
    CVScore = sd(score, na.rm = TRUE) / mean(score, na.rm = TRUE)
  )
t$CV_sum <- t$CVOOB + t$CVTestACC
t

plot(t$split, t$CVOOB, type = "l")
lines(t$split, t$CVTestACC)

plot(lowess(x = t$split, y = t$CVOOB, f = 0.3), type = "l", col = "red")
lines(lowess(x = t$split, y = t$CVTestACC, f = 0.3), col = "green")
points(t$split, t$CVOOB, col = "red")
points(t$split, t$CVTestACC, col = "green")

plot(lowess(x = t$split, y = t$AveOOB, f = 0.3), type = "l", col = "red", ylim = c(8, 20))
lines(lowess(x = t$split, y = t$AveTestACC, f = 0.3), col = "green")
points(t$split, t$AveOOB, col = "red")
points(t$split, t$AveTestACC, col = "green")

library(ggplot2)
library(reshape2)
accRes_long <- accRes
accRes_long$testACC2 <- 100 - accRes_long$testACC
accRes_long <- melt(accRes_long, id = "split", measure.vars = c(3, 9))
p <- ggplot(accRes_long, aes(x=as.factor(split), y = value, fill = variable)) +
  geom_boxplot()
p









################################################################################
# leave-one-out cross validation testing using training/testing data
#
# idea is to build model n times, each time leaving out 1 observation. Predict
# the omitted observation and summarize the prediction error to get test statistic.
################################################################################
library(ranger)

useRanger <- TRUE
folds <- nrow(modelData)

# set seed so we can replicate results
set.seed(153756)

for (iter in c(1:folds)) {
  cat(iter, " of ", folds, "\n")
  trainingData <- modelData[-iter,]
  testingData <- modelData[iter,]

  # use hyperparameter values from tuning
  if (useRanger) {
    # speciesRF <- ranger(Species ~ ., data = trainingData
    #                     , mtry = 13
    #                     , min.node.size = 5
    #                     , sample.fraction = c(0.5476666, 0.5476666)
    #                     , num.trees = 1000)

    speciesRF <- ranger(Species ~ ., data = trainingData
                        , mtry = res$recommended.pars$mtry
                        , min.node.size = res$recommended.pars$min.node.size
                        , sample.fraction = c (res$recommended.pars$sample.fraction, res$recommended.pars$sample.fraction)
                        , num.trees = 1000)
    #speciesRF

    modelOOB <- speciesRF$prediction.error

    # predict using testing data
    typePred <- predict(speciesRF, data = testingData[, -1])
    c <- confusionMatrix(typePred$predictions, testingData$Species)
    testAccuracy <- c$overall[[1]]
    testKappa <- c$overall[[2]]

    typePred <- predict(speciesRF, data = modelData[, -1])
    c <- confusionMatrix(typePred$predictions, modelData$Species)
    allAccuracy <- c$overall[[1]]
    allKappa <- c$overall[[2]]
  }
  else {
    speciesRF <- randomForest(Species ~ ., data = trainingData, mtry = 12, nodesize = 3, ntree = 1000
                              #                              , sampsize = rep(sum(trainingData$Species == "PSME")
                              #                                , nlevels(trainingData$Species)
                              #                                )
    )
    #speciesRF

    modelOOB <- as.double(speciesRF$err.rate[500, 1])

    # predict using testing data
    typePred <- predict(speciesRF, newdata = testingData[, -1])
    c <- confusionMatrix(typePred, testingData$Species)
    testAccuracy <- c$overall[[1]]
    testKappa <- c$overall[[2]]

    typePred <- predict(speciesRF, newdata = modelData[, -1])
    c <- confusionMatrix(typePred, modelData$Species)
    allAccuracy <- c$overall[[1]]
    allKappa <- c$overall[[2]]
  }

  if (iter == 1) {
    accRes <- data.frame(iteration = iter, modelOOB = round(modelOOB * 100, 1), testACC = round(testAccuracy * 100, 1), testKappa = testKappa, allACC = round(allAccuracy * 100, 1), allKappa = allKappa)
  } else {
    t <- data.frame(iteration = iter, modelOOB = round(modelOOB * 100, 1), testACC = round(testAccuracy * 100, 1), testKappa = testKappa, allACC = round(allAccuracy * 100, 1), allKappa = allKappa)
    accRes <- rbind(accRes, t)
  }
}

# some of the columns have bad values for the LOOV
t <- accRes %>%
  summarize(
    AveOOB = mean(modelOOB, na.rm = TRUE),
    SDOOB = sd(modelOOB, na.rm = TRUE),
    CVOOB = sd(modelOOB, na.rm = TRUE) / mean(modelOOB, na.rm = TRUE),
    AveTestACC = mean((100 - testACC), na.rm = TRUE),
    SDTestACC = sd((100 - testACC), na.rm = TRUE),
    CVTestACC = sd((100 - testACC), na.rm = TRUE) / mean((100 - testACC), na.rm = TRUE),
    AveTestKappa = mean(testKappa, na.rm = TRUE),
    SDTestKappa = sd(testKappa, na.rm = TRUE),
  )
t

# 11/08/2023 results...going into paper
# results (error) for model with all variables:
# OOB: 8.354783  %
# ACC: 8.173913   %
# sample.fraction = 0.4685799
# mtry = 12
# min.node.size = 2
#
# you can't compute Kappa using a single test observation
#
# results for model using only height metrics (no intensity metrics)
# OOB: 11.54504%
# ACC: 11.30435  %
# sample.fraction = 0.2077579
# mtry = 20
# min.node.size = 12
#
# results for model using only intensity metrics (no intensity metrics)
# OOB: 21.29983%
# ACC: 21.3913%
# sample.fraction = 0.5197629
# mtry = 28
# min.node.size = 21

# variable importance from model with all metrics and tuned hyperparameters
#
# Elev.maximum        Elev.minimum             Int.P05             Int.P90             Int.P01             Int.P20             Int.P10
# 0.0000000           0.2731888           0.3394540           0.3942689           0.4049650           0.4450641           0.4835252
# Int.minimum         Int.maximum             Int.P25            Elev.P25                RP25            Elev.P20            Elev.P30
# 0.5436775           0.6349475           0.6773751           0.7544077           0.7557714           0.7571829           0.7839629
# Elev.mode             Int.P99             Int.P75                RP30            Elev.P75                RP20            Elev.P70
# 0.8482853           0.8507303           0.8719415           0.9260670           0.9342235           0.9440158           0.9937904
# Elev.P80             Elev.L1             Int.P95            Elev.P40           Elev.L.CV           Elev.mean            Elev.P10
# 0.9956482           0.9987489           1.0175518           1.0190013           1.0262975           1.0288259           1.0696916
# Int.P80            Elev.P50            Elev.P05 Canopy.relief.ratio             Int.P30                RP10             Elev.CV
# 1.0827194           1.0994961           1.1599400           1.1729223           1.2014911           1.2890385           1.3066798
# Int.IQ            Elev.P60     Elev.MAD.median                RP40              Int.CV       Elev.MAD.mode            Int.L.CV
# 1.3097954           1.3312455           1.3467720           1.3830103           1.4272334           1.4291156           1.4781089
# Elev.P01                RP01     Elev.L.skewness          Int.stddev        Int.variance        Profile.area             Elev.IQ
# 1.4952360           1.5381967           1.6245036           1.6324216           1.6547444           1.6692542           1.7100219
# RP50                RP05        Int.kurtosis              Int.L2                RP90   Elev.SQRT.mean.SQ             Int.P70
# 1.7407755           1.8132019           1.8210720           1.8586834           1.8629072           2.0081296           2.0344637
# Int.AAD                RP70              Int.L4                RP60                RP95       Elev.skewness                RP80
# 2.2599548           2.3408788           2.3547475           2.3588987           2.4425398           2.4438710           2.4579269
# RP75       Elev.kurtosis             Int.P40             Int.P50      Int.L.kurtosis            Elev.AAD     Elev.L.kurtosis
# 2.4892931           2.6404525           2.6526215           3.0560931           3.2113653           3.6070808           3.7985853
# Elev.L3            Elev.P90              Int.L1             Elev.L4 Elev.CURT.mean.CUBE            Int.mean             Elev.L2
# 4.1175287           4.3850583           5.1696244           5.2707400           5.4964244           5.5260684           6.4414728
# Int.mode        Int.skewness             Int.P60              Int.L3       Elev.variance         Elev.stddev      Int.L.skewness
# 6.5648314           6.7441862           9.1125944           9.5842161          12.4267926          12.9418660          17.5671322
# Elev.P95            Elev.P99
# 23.7486927          71.1008394
#
# the list of variables contains some that are highly correlated

mod <- lm(modelData$Elev.P99 ~modelData$Elev.P95)   # corr = 0.66
summary(mod)

mod <- lm(modelData$Int.L3 ~modelData$Int.L.skewness)   # corr = 0.93
summary(mod)

mod <- lm(modelData$Elev.variance ~modelData$Elev.stddev)   # corr = 0.98
summary(mod)

mod <- lm(modelData$Int.skewness ~modelData$Int.L.skewness)   # corr = 0.77
summary(mod)

mod <- lm(modelData$Int.L1 ~modelData$Int.mean)   # corr = 1.0...same values
summary(mod)

mod <- lm(modelData$Int.P60 ~modelData$Int.mode)   # corr = 0.33
summary(mod)

hist(modelData$Elev.P99)
hist(modelData$Elev.P95)
hist(modelData$Elev.L4)
hist(modelData$Elev.L3)
hist(modelData$Elev.kurtosis)
hist(modelData$RP05)
hist(modelData$Int.L.skewness)
hist(modelData$Int.P60)
hist(modelData$Int.minimum)

#===============================================================================
##### this code needs to have the importance scores from the model with all variables
#===============================================================================
# reorder importance scores from largest to smallest
imp <- imp[order(-imp)]
imp

# drop importance scores of 0.0
imp <- imp[imp > 0]
imp

# build correlation matric for top 30 variables
#a <- modelData[, c(names(imp[1:30]))]
a <- modelData[, c(names(imp))]
c <- abs(cor(a, method = "spearman"))
c[upper.tri(c, diag = TRUE)] <- NA
mc <- apply(c, 1, max, na.rm=TRUE)
v <- mc[mc < 0.50]
v
names(v)

# list
# Elev.P99 Int.L.skewness        Int.P60        Elev.L4        Elev.L3   Elev.minimum
# mc has -Inf for the first variable. This is OK but strange to look at. Last variable is
# always included

# process for selecting subset of variables...code above does this
# look at correlation and importance scores from model with all predictors
# 1. add the variable with the highest importance score
# 2. look at the variable with the next highest importance score
# 3. check the correlation with variables already added to the list and keep
#    the variable if the correlation for all is less than some threshold
# 4. continue going through the list of importance scores until you get the
#    desired number of variables. I suspect you could go through the entire
#    list to select the best subset...might be overkill since importance
#    scores are not the most reliable when you have highly correlated variables
#    in the list of possible predictors.
#
# list of variables
# Elev.P99 + Elev.P95 + Int.L.skewness + Elev.stddev + Int.P60 + Int.mode

# build new data set with just the 5 variables and Species
#sparseData <- modelData[, c(1, 35, 36, 56, 6, 66, 43)]
sparseData <- modelData[, c("Species", names(v))]
#sparseData <- modelData[, c("Species", "Elev.P99")]
model.task = makeClassifTask(data = sparseData, target = "Species")

# Estimate runtime
estimateTimeTuneRanger(model.task)

set.seed(153756)

# Tuning
#  res = tuneRanger(model.task, measure = list(multiclass.brier), num.trees = 1000,
#                   num.threads = 2, iters = 70, iters.warmup = 30)
res = tuneRanger(model.task, measure = list(acc), num.trees = 1000,
                 num.threads = 2, iters = 70, iters.warmup = 30)

res
# results (error) for model with all variable subset:
# OOB: 8.399478 %
# ACC: 8.347826 %
# sample.fraction = 0.20687
# mtry = 1
# min.node.size = 3

speciesRF <- ranger(Species ~ ., data = sparseData
                    , num.trees = 1000
                    , mtry = res$recommended.pars$mtry
                    , min.node.size = res$recommended.pars$min.node.size
                    , sample.fraction = c (res$recommended.pars$sample.fraction, res$recommended.pars$sample.fraction)
                    , importance = "none"
)
speciesRF


# LOOCV
useRanger <- TRUE
folds <- nrow(sparseData)

# set seed so we can replicate results
set.seed(153756)

for (iter in c(1:folds)) {
  cat(iter, " of ", folds, "\n")
  trainingData <- sparseData[-iter,]
  testingData <- sparseData[iter,]

  # use hyperparameter values from tuning
  if (useRanger) {
    speciesRF <- ranger(Species ~ ., data = trainingData
                        , mtry = res$recommended.pars$mtry
                        , min.node.size = res$recommended.pars$min.node.size
                        , sample.fraction = c (res$recommended.pars$sample.fraction, res$recommended.pars$sample.fraction)
                        , num.trees = 1000)
    #speciesRF

    modelOOB <- speciesRF$prediction.error

    # predict using testing data
    typePred <- predict(speciesRF, data = testingData[, -1])
    c <- confusionMatrix(typePred$predictions, testingData$Species)
    testAccuracy <- c$overall[[1]]
    testKappa <- c$overall[[2]]

    typePred <- predict(speciesRF, data = modelData[, -1])
    c <- confusionMatrix(typePred$predictions, modelData$Species)
    allAccuracy <- c$overall[[1]]
    allKappa <- c$overall[[2]]
  }
  else {
    speciesRF <- randomForest(Species ~ ., data = trainingData, mtry = 12, nodesize = 3, ntree = 1000
                              #                              , sampsize = rep(sum(trainingData$Species == "PSME")
                              #                                , nlevels(trainingData$Species)
                              #                                )
    )
    #speciesRF

    modelOOB <- as.double(speciesRF$err.rate[500, 1])

    # predict using testing data
    typePred <- predict(speciesRF, newdata = testingData[, -1])
    c <- confusionMatrix(typePred, testingData$Species)
    testAccuracy <- c$overall[[1]]
    testKappa <- c$overall[[2]]

    typePred <- predict(speciesRF, newdata = modelData[, -1])
    c <- confusionMatrix(typePred, modelData$Species)
    allAccuracy <- c$overall[[1]]
    allKappa <- c$overall[[2]]
  }

  if (iter == 1) {
    accRes <- data.frame(iteration = iter, modelOOB = round(modelOOB * 100, 1), testACC = round(testAccuracy * 100, 1), testKappa = testKappa, allACC = round(allAccuracy * 100, 1), allKappa = allKappa)
  } else {
    t <- data.frame(iteration = iter, modelOOB = round(modelOOB * 100, 1), testACC = round(testAccuracy * 100, 1), testKappa = testKappa, allACC = round(allAccuracy * 100, 1), allKappa = allKappa)
    accRes <- rbind(accRes, t)
  }
}

# some of the columns have bad values for the LOOCV
t <- accRes %>%
  summarize(
    AveOOB = mean(modelOOB, na.rm = TRUE),
    SDOOB = sd(modelOOB, na.rm = TRUE),
    CVOOB = sd(modelOOB, na.rm = TRUE) / mean(modelOOB, na.rm = TRUE),
    AveTestACC = mean((100 - testACC), na.rm = TRUE),
    SDTestACC = sd((100 - testACC), na.rm = TRUE),
    CVTestACC = sd((100 - testACC), na.rm = TRUE) / mean((100 - testACC), na.rm = TRUE),
    AveTestKappa = mean(testKappa, na.rm = TRUE),
    SDTestKappa = sd(testKappa, na.rm = TRUE),
  )
t

# "Elev.P99"       "Int.L.skewness" "Int.P60"        "Elev.L4"        "Elev.L3"
#AveOOB     SDOOB     CVOOB AveTestACC SDTestACC CVTestACC AveTestKappa SDTestKappa
#8.501739 0.2018135 0.0237379   8.521739   27.9448  3.279237            0           0
# OOB: 8.501739 %
# ACC: 8.521739 %
# sample.fraction = 0.20687
# mtry = 1
# min.node.size = 3

# RF with only Elev.P99 and default values for hyperparameters
#AveOOB     SDOOB       CVOOB AveTestACC SDTestACC CVTestACC AveTestKappa SDTestKappa
#17.06696 0.1187344 0.006956977   17.04348  37.63417  2.208127            0           0
# mtry = 1
# min.node.size = 1









cat("accuracy with all metrics: ", round(100 - 8.347826, 2))
cat("accuracy with only height metrics: ", round(100 - 11.65217, 2))
cat("accuracy with only intensity metrics: ", round(100 - 21.3913, 2))

citation("tuneRanger")
citation("ranger")



# produce boxplots of intensity metrics
library(ggplot2)
library(gridExtra)

ggplot(modelData, aes(x = Species, y = Elev.P99)) + geom_boxplot() + theme(axis.text=element_text(size=12, face="bold"), axis.title=element_text(size=14,face="bold"))
ggplot(modelData, aes(x = Species, y = Elev.P95)) + geom_boxplot()
ggplot(modelData, aes(x = Species, y = Elev.L3)) + geom_boxplot()
ggplot(modelData, aes(x = Species, y = Elev.L4)) + geom_boxplot()
ggplot(modelData, aes(x = Species, y = Elev.minimum)) + geom_boxplot()
ggplot(modelData, aes(x = Species, y = Int.L.skewness)) + geom_boxplot()
ggplot(modelData, aes(x = Species, y = Int.skewness)) + geom_boxplot()
ggplot(modelData, aes(x = Species, y = Int.minimum)) + geom_boxplot()
ggplot(modelData, aes(x = Species, y = Int.P60)) + geom_boxplot()
ggplot(modelData, aes(x = Species, y = Int.mode)) + geom_boxplot()
ggplot(modelData, aes(x = Species, y = Int.P50)) + geom_boxplot()
ggplot(modelData, aes(x = Species, y = Int.AAD)) + geom_boxplot()

hist(modelData$Int.P50)


fs <- 12
ff <- "sans"
P99 <- ggplot(data = modelData, mapping = aes(x=Species, y=Elev.P99, fill=Species)) +
  geom_violin(draw_quantiles = 0.5) +
  #  geom_hline(yintercept=(median(modelData[modelData$Species == "PSME", "Int.L.skewness"]) + median(modelData[modelData$Species == "TSHE", "Int.L.skewness"])) / 2, linetype="dashed", color = "black") +
  #  geom_violin(draw_quantiles = c(.50), trim = FALSE) +
  #  xlab("Species") +
  ylab("99th percentile of height (m)") +
  xlab("") +
  theme(legend.position="none", text=element_text(size=fs,  family=ff), plot.margin=unit(c(0.2,0.5,0.1,0.5),"cm"))
P95 <- ggplot(data = modelData, mapping = aes(x=Species, y=Elev.P95, fill=Species)) +
  geom_violin(draw_quantiles = 0.5) +
  #  geom_hline(yintercept=(median(modelData[modelData$Species == "PSME", "Int.L.skewness"]) + median(modelData[modelData$Species == "TSHE", "Int.L.skewness"])) / 2, linetype="dashed", color = "black") +
  #  geom_violin(draw_quantiles = c(.50), trim = FALSE) +
  #  xlab("Species") +
  ylab("95th percentile of height (m)") +
  xlab("") +
  theme(legend.position="none", text=element_text(size=fs,  family=ff), plot.margin=unit(c(0.2,0.5,0.1,0.5),"cm"))
esd <- ggplot(data = modelData, mapping = aes(x=Species, y=Elev.stddev, fill=Species)) +
  geom_violin(draw_quantiles = 0.5) +
  #  geom_hline(yintercept=(median(modelData[modelData$Species == "PSME", "Int.L.skewness"]) + median(modelData[modelData$Species == "TSHE", "Int.L.skewness"])) / 2, linetype="dashed", color = "black") +
  #  geom_violin(draw_quantiles = c(.50), trim = FALSE) +
  #  xlab("Species") +
  ylab("Standard deviation of height (m)") +
  xlab("") +
  theme(legend.position="none", text=element_text(size=fs,  family=ff), plot.margin=unit(c(0.2,0.5,0.1,0.1),"cm"))
lSkew <- ggplot(data = modelData, mapping = aes(x=Species, y=Int.L.skewness, fill=Species)) +
  geom_violin(draw_quantiles = 0.5) +
  #  geom_hline(yintercept=(median(modelData[modelData$Species == "PSME", "Int.L.skewness"]) + median(modelData[modelData$Species == "TSHE", "Int.L.skewness"])) / 2, linetype="dashed", color = "black") +
  #  geom_violin(draw_quantiles = c(.50), trim = FALSE) +
  #  scale_x_discrete(labels = NULL) +
  xlab("Species") +
  ylab("L-moment skewness of intensity") +
  #  xlab("") +
  theme(legend.position="none", text=element_text(size=fs,  family=ff), plot.margin=unit(c(0.2,0.5,0.1,0.2),"cm"))
lKurt <- ggplot(data = modelData, mapping = aes(x=Species, y=Int.L.kurtosis, fill=Species)) +
  geom_violin(draw_quantiles = 0.5) +
  #  geom_hline(yintercept=(median(modelData[modelData$Species == "PSME", "Int.L.kurtosis"]) + median(modelData[modelData$Species == "TSHE", "Int.L.kurtosis"])) / 2, linetype="dashed", color = "black") +
#  scale_x_discrete(labels = NULL) +
  theme(legend.position="none", text=element_text(size=fs,  family=ff), plot.margin=unit(c(0.2,0.5,0.1,0.1),"cm")) +
  xlab("Species") +
  ylab("L-moment kurtosis of intensity")
#  xlab("")
iP60 <- ggplot(data = modelData, mapping = aes(x=Species, y=Int.P60, fill=Species)) +
  geom_violin(draw_quantiles = 0.5) +
  #  geom_hline(yintercept=(median(modelData[modelData$Species == "PSME", "Int.L.skewness"]) + median(modelData[modelData$Species == "TSHE", "Int.L.skewness"])) / 2, linetype="dashed", color = "black") +
  #  geom_violin(draw_quantiles = c(.50), trim = FALSE) +
  #  xlab("Species") +
  ylab("60th percentile of intensity") +
  xlab("") +
  theme(legend.position="none", text=element_text(size=fs,  family=ff), plot.margin=unit(c(0.2,0.5,0.1,0.1),"cm"))
mode <- ggplot(data = modelData, mapping = aes(x=Species, y=Int.mode, fill=Species)) +
  geom_violin(draw_quantiles = 0.5) +
#  geom_hline(yintercept=(median(modelData[modelData$Species == "PSME", "Int.mode"]) + median(modelData[modelData$Species == "TSHE", "Int.mode"])) / 2, linetype="dashed", color = "black") +
  xlab("Species") +
  ylab("Mode of intensity") +
  theme(legend.position="none", text=element_text(size=fs,  family=ff), plot.margin=unit(c(0.2,0.5,0.1,0.1),"cm"))
#p <- grid.arrange(P99, P95, esd, lSkew, mode, lKurt, nrow = 2, ncol = 3)
p <- grid.arrange(P99, P95, esd, lSkew, mode, iP60, nrow = 2, ncol = 3)
#grid.arrange(arrangeGrob(lSkew, mode, lKurt,
#            widths = c(0.333, 0.333, 0.333)),
#            nrow = 1)

ggsave("G:/Bob's Stuff/Documents/FY23/Papers/DF_WH_Classification/RcodeFigures/BoxPlotsMetrics.png", p)


# saving via tiff() doesn't always work...don't know why
tiff("G:/Bob's Stuff/Documents/FY23/Papers/DF_WH_Classification/RcodeFigures/BoxPlotsMetrics.tif",
     compression = "lzw", width = 4.0, height = 6.0, units = "in", res = 600)
print(p)
dev.off()
