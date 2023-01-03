# exploring height-DBH relationship
source("Rcode/predictDBH_Height.R")

inputDataFile <- "extras/AdjustedField_T3_Training_TreeTops_AllPlots.csv"

# read data
inputData <- read.csv(inputDataFile, stringsAsFactors = FALSE)

# compute the average height using height measured by Ally and Bob in the point cloud
inputData$aveHt <- (inputData$Total.Height.Ally + inputData$Total.Height.Bob) / 2

# functions don't work with multiple trees (there are if statements that fail)
fvsDBH <- c()
hanusDBH <- c()
for (i in 1:nrow(inputData)) {
  fvsDBH[i] <- predictDBH(inputData$Species[i], inputData$aveHt[i], method = "fvs", location = 1, heightUnits = "meters", DBHUnits = "cm")
  hanusDBH[i] <- predictDBH(inputData$Species[i], inputData$aveHt[i], method = "hanus", location = 1, heightUnits = "meters", DBHUnits = "cm")
}

inputData$fvsDBH <- fvsDBH
inputData$hanusDBH <- hanusDBH

DF <- inputData[inputData$Species == "PSME", ]
WH <- inputData[inputData$Species == "TSHE", ]
plot(DF$aveHt, DF$DBH_cm, col = "red", pch = 1, ylab = "Predicted DBH (cm)", xlab = "Lidar height (m)", main = "Comparing field and predicted DBH & lidar height")
points(WH$aveHt, WH$DBH_cm, col = "red", pch = 16)
legend(36, 25, c("Field PSME", "Field TSHE"), pch = c(1, 16), col = c("red", "red"))


points(DF$aveHt, DF$fvsDBH, col = "green", pch = 1)
points(WH$aveHt, WH$fvsDBH, col = "green", pch = 16)

points(DF$aveHt, DF$hanusDBH, col = "blue", pch = 1)
points(WH$aveHt, WH$hanusDBH, col = "blue", pch = 16)

legend(20, 70, c("Hanus PSME", "Hanus TSHE", "FVS PSME", "FVS TSHE"), pch = c(1, 16, 1, 16), col = c("blue", "blue", "green", "green"))




# fit new model for DF using Hanus equation form
# DBH in cm and height in m

# The model is fit to predict height given DBH. Predicting DBH is more useful from the lidar perspective. The
# equation can be rearranged to solve for DBH given height but a direct fit may produce a better (or different) fit.
DFadd_nls <- nls(data = DF, aveHt ~ 4.5 + exp(a0 + a1 * DBH_cm ^ a2),
                 start = list(a0 = 7.0, a1 = -6.0, a2 = -0.25))
DFnls_coeff <- coef(DFadd_nls)

plot(DF$aveHt, DF$DBH_cm, col = "red", pch = 1, ylab = "Field DBH (cm)", xlab = "Predicted height (m)", main = "Predicted height given field DBH")
points(WH$aveHt, WH$DBH_cm, col = "red", pch = 16)
legend(35, 24, c("Field PSME", "Field TSHE"), pch = c(1, 16), col = c("red", "red"))

# plot predicted DBH using lidar height
points(DF$aveHt, (log((DF$aveHt - 4.5) / exp(DFnls_coeff["a0"])) / DFnls_coeff["a1"])^(1 / DFnls_coeff["a2"]), col = "black", pch = 1)

# plot predicted height using field DBH
#points(4.5 + exp(DFnls_coeff[1] + DFnls_coeff[2] * DF$DBH_cm ^ DFnls_coeff[3]), DF$DBH_cm, col = "magenta", pch = 1)


# fit new model for WH using Hanus equation form
# DBH in cm and height in m
WHadd_nls <- nls(data = WH, aveHt ~ 4.5 + exp(a0 + a1 * DBH_cm ^ a2),
                 start = list(a0 = 7.0, a1 = -6.0, a2 = -0.25))
WHnls_coeff <- coef(WHadd_nls)
points(WH$aveHt, (log((WH$aveHt - 4.5) / exp(WHnls_coeff["a0"])) / WHnls_coeff["a1"])^(1 / WHnls_coeff["a2"]), col = "black", pch = 16)

# plot predicted height using field DBH
#points(4.5 + exp(WHnls_coeff[1] + WHnls_coeff[2] * WH$DBH_cm ^ WHnls_coeff[3]), WH$DBH_cm, col = "magenta", pch = 16)

legend(35, 33, c("Custom PSME", "Custom TSHE"), pch = c(1, 16), col = c("black", "black"))

# plot residuals for predicted DBH
#hist((log((DF$aveHt - 4.5) / exp(DFnls_coeff["a0"])) / DFnls_coeff["a1"])^(1 / DFnls_coeff["a2"]) - DF$DBH_cm)
#hist((log((WH$aveHt - 4.5) / exp(WHnls_coeff["a0"])) / WHnls_coeff["a1"])^(1 / WHnls_coeff["a2"]) - WH$DBH_cm)

# overall bias
mean((log((DF$aveHt - 4.5) / exp(DFnls_coeff["a0"])) / DFnls_coeff["a1"])^(1 / DFnls_coeff["a2"]) - DF$DBH_cm)
mean((log((WH$aveHt - 4.5) / exp(WHnls_coeff["a0"])) / WHnls_coeff["a1"])^(1 / WHnls_coeff["a2"]) - WH$DBH_cm)



# fit non-linear model to directly predict DBH given the lidar height. This uses the hanus form but solved for DBH given height.
# this gives a different set of coefficients than we get when we fit an equation to predict height from field DBH. This is
# expected since the model is being fit directly. We had a reviewer comment on the wind river draft that raised the point that
# simply solving for DBH in a height prediction equation doesn't necessarily give the "correct" DBH. In reality, it gives the
# DBH that produces the desired height but the model was not fit to be used in that way. Not sure I can produce a good explanation
# for this but it makes sense. The models fit directly for DBH have MUCH lower bias. Bias for height equation solved for DBH is
# 0.66 and 2.07 cm for DF and WH. For the direct DBH equation, bias is -0.001 and -0.001 for DF and WH.
library(nls2)
DFadd_nls <- nls(data = DF, DBH_cm ~ (log((aveHt - 4.5) / exp(a0)) / a1) ^ (1 / a2),
                 start = list(a0 = 7.0, a1 = -6.0, a2 = -0.25))
DFnls_coeff <- coef(DFadd_nls)

plot(DF$aveHt, DF$DBH_cm, col = "red", pch = 1, ylab = "Predicted DBH (cm)", xlab = "Lidar height (m)", main = "Predicted DBH given lidar height")
points(WH$aveHt, WH$DBH_cm, col = "red", pch = 16)
legend(35, 24, c("Field PSME", "Field TSHE"), pch = c(1, 16), col = c("red", "red"))

points(DF$aveHt, (log((DF$aveHt - 4.5) / exp(DFnls_coeff["a0"])) / DFnls_coeff["a1"])^(1 / DFnls_coeff["a2"]), col = "orange", pch = 1)

# plot predicted height using field DBH
#points(4.5 + exp(DFnls_coeff[1] + DFnls_coeff[2] * DF$DBH_cm ^ DFnls_coeff[3]), DF$DBH_cm, col = "magenta", pch = 1)

WHadd_nls <- nls(data = WH, DBH_cm ~ (log((aveHt - 4.5) / exp(a0)) / a1) ^ (1 / a2),
                 start = list(a0 = 4, a1 = -28, a2 = -1))
WHnls_coeff <- coef(WHadd_nls)
points(WH$aveHt, (log((WH$aveHt - 4.5) / exp(WHnls_coeff["a0"])) / WHnls_coeff["a1"])^(1 / WHnls_coeff["a2"]), col = "orange", pch = 16)

legend(35, 33, c("Custom PSME", "Custom TSHE"), pch = c(1, 16), col = c("orange", "orange"))


# overall bias
mean((log((DF$aveHt - 4.5) / exp(DFnls_coeff["a0"])) / DFnls_coeff["a1"])^(1 / DFnls_coeff["a2"]) - DF$DBH_cm)
mean((log((WH$aveHt - 4.5) / exp(WHnls_coeff["a0"])) / WHnls_coeff["a1"])^(1 / WHnls_coeff["a2"]) - WH$DBH_cm)
