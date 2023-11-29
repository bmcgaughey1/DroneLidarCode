# prepare various matching procedures starting with 2021 field data and GPS plot locations
#
# for all scenarios, predict tree heights using Hanus model...maybe custom model
#
# scenarios (none of these rely on segmented trees except AA):
# Original location (OL): Use plot location and tree data (distance and azimuth)
#   to build stem map. These locations serve as the starting point for other adjustment
#   strategies.
# Automated alignment (AA): Use stem map to create height "image". Then use FFT
#   correlation to move entire plot to location with the highest correlation.
# Plot adjustment (PA): Apply a plot-wide adjustment based on matched pairs of
#   field and lidar-derived tree locations. Matched pairs were manually selected
#   using OL as the starting point. Use an affine transformation.
# Individual tree adjustment (ITA): Use tree locations and CHM high points
#   to manually adjust tree locations.
# Individual tree adjustment with lean (ITAL): Use tree locations and point cloud
#   to manually move tree base and tree top (lean is allowed) to align with points
#   for each tree. Ally and I did this so we can also compare results to assess
#   the repeatability of the method.
#
#
library(readxl)
library(dplyr)
library(vec2dtransf)
library(PlotLocatoR)
library(fusionwrapr)

# Functions ---------------------------------------------------------------
computeLocalTreePositions <- function(
    trees,
    xRef = 0,                   # actual X for turning point
    yRef = 0,                   # actual Y for turning point
    azLabel = "Azimuth",        # column label in trees for azimuth
    distLabel = "Distance",     # column label in trees for distance
    dbhLabel = "DBH_cm",        # column label for tree DBH
    dbhConversionFactor = 0.01, # conversion factor to convert DBH to same units as distances
    declination = 0,            # declination...not sure but think E declination is positive (Seattle is about 16 degrees)
    adjustForDBH = FALSE        # flag indicating DBH/2 should be added to distances
) {
  # adjust distance to account for offset to tree center
  dist <- trees[, distLabel]
  if (adjustForDBH) dist <- dist + (trees[, dbhLabel] * dbhConversionFactor / 2.0)

  # compute XY using distance, azimuth...optional adjust for declination and offset to reference location
  X <- data.frame(cos((450 - (trees[, azLabel] - declination)) * pi / 180.0) * dist + xRef)
  Y <- data.frame(sin((450 - (trees[, azLabel] - declination)) * pi / 180.0) * dist + yRef)

  t <- cbind(as.data.frame(X), as.data.frame(Y))
  colnames(t) <- c("Xfield", "Yfield")

  return(t)
}

setwd("G:/R_Stuff/DroneLidarCode")

# Read field data, extract trees on plots with lidar coverage, predict heights, assign OL coordinates ----------
outputFolder <- "G:/R_Stuff/ONRCDroneLidar/AdjustmentsForPaper/"

fieldDataFilename <- "G:/R_Stuff/ONRCDroneLidar/2021 Upland Tree.xlsx"
sheetname <- "Compiled_Data"
allTrees <- read_excel(fieldDataFilename, sheet = sheetname,
                       col_types = c(rep("guess", 10), "text", "guess", "guess", "guess", "guess", "text"))

# there is some weirdness in the data read from the spreadsheet for the anomaly codes
# the value in Anomaly_Num is wrong if the spreadsheet had "1/2" but the 1st_Anomaly,
# 2nd_Anomaly, and 3rd_Anomaly values seem OK. In Excel, behavior is different and
# values in the 4 related columns are not the same as those in the allTrees data frame.
# The col_types parameter helps "fix" this but if you read the output CSV data back into
# Excel, some of the anomaly codes (like "1/2") are interpreted as a date.

# fix names for parsed anomaly information...can't start with a number
colnames(allTrees)[which(names(allTrees) == '1st_Anomaly')] <- "Anomaly1"
colnames(allTrees)[which(names(allTrees) == '2nd_Anomaly')] <- "Anomaly2"
colnames(allTrees)[which(names(allTrees) == '3rd_Anomaly')] <- "Anomaly3"

# drop trees where azimuth is "nm"
allTrees <- allTrees[allTrees$Azimuth != "nm", ]

# convert columns to numbers
allTrees$Azimuth <- as.numeric(allTrees$Azimuth)
allTrees$Distance <- as.numeric(allTrees$Distance)
allTrees$DBH_cm <- as.numeric(allTrees$DBH_cm)
allTrees$Anomaly1 <- as.numeric(allTrees$Anomaly1)
allTrees$Plot_ID <- as.integer(allTrees$Plot_Number)

# filter on species, anomaly code, and lidar visible flag
# this isn't really needed for the adjustment process. there will be some errors
# when predicting heights because we don't have equations for all species
#
# ***** if we filter here, we don't get adjusted coordinates for all trees
#psme <- dplyr::filter(allTrees, Species == "PSME", Anomaly1 == 0, LiDAR_visible == "Y")
#tshe <- dplyr::filter(allTrees, Species == "TSHE", Anomaly1 == 0, LiDAR_visible == "Y")

#allTrees <- rbind(psme, tshe)

# get list of plot numbers with field data
plotsWithTrees <- unique(allTrees$Plot_Number)

# read plot locations
plotLocationFilename <- "G:/R_Stuff/ONRCDroneLidar/plot_centers.xlsx"
sheetname <- "plot_centers"
plotLocations <- read_excel(plotLocationFilename, sheet = sheetname)

# sort by plot id
plotLocations <- plotLocations[order(plotLocations$PlotID), ]

# we have lidar data for these plots...may not have field data for all plots
plotNumbers <- c("07", "08", "09", "10", "13", "14",
                 "16", "17", "18", "19", "20", "21",
                 "22", "23", "24", "25", "26", "27",
                 "28", "29", "31", "34", "36", "37",
                 "42", "43", "46", "47")

plotsWithTreesAndLidar <- plotNumbers[plotNumbers %in% plotsWithTrees]

# need explicit folder names for each plot since some lidar collects cover more than one plot
# also folder names are horrible!!
plotFolders <- c(
  "H:/T3_DroneLidar/Ba/Plot7"
  , "H:/T3_DroneLidar/Ba/Plots8_9_36_46"
  , "H:/T3_DroneLidar/Ba/Plots8_9_36_46"
  , "H:/T3_DroneLidar/Ba/Plots10_47"
  , "H:/T3_DroneLidar/Az/Plots13_14_23_24_43"
  , "H:/T3_DroneLidar/Az/Plots13_14_23_24_43"
  , "H:/T3_DroneLidar/Aa/Plots16_31"
  , "H:/T3_DroneLidar/Aa/Plots17_18_26_27"
  , "H:/T3_DroneLidar/Aa/Plots17_18_26_27"
  , "H:/T3_DroneLidar/Da/Plot19"
  , "H:/T3_DroneLidar/Da/Plots20_21_22"
  , "H:/T3_DroneLidar/Da/Plots20_21_22"
  , "H:/T3_DroneLidar/Da/Plots20_21_22"
  , "H:/T3_DroneLidar/Az/Plots13_14_23_24_43"
  , "H:/T3_DroneLidar/Az/Plots13_14_23_24_43"
  , "H:/T3_DroneLidar/Dz/Plots25_42"
  , "H:/T3_DroneLidar/Aa/Plots17_18_26_27"
  , "H:/T3_DroneLidar/Aa/Plots17_18_26_27"
  , "H:/T3_DroneLidar/Aa/Plots28_29_34"
  , "H:/T3_DroneLidar/Aa/Plots28_29_34"
  , "H:/T3_DroneLidar/Aa/Plots16_31"
  , "H:/T3_DroneLidar/Aa/Plots28_29_34"
  , "H:/T3_DroneLidar/Ba/Plots8_9_36_46"
  , "H:/T3_DroneLidar/Ba/Plot37"
  , "H:/T3_DroneLidar/Dz/Plots25_42"
  , "H:/T3_DroneLidar/Az/Plots13_14_23_24_43"
  , "H:/T3_DroneLidar/Ba/Plots8_9_36_46"
  , "H:/T3_DroneLidar/Ba/Plots10_47"
)

# get plot locations for plots with lidar data
plotLoc <- plotLocations[plotLocations$PlotID %in% plotNumbers, ]

source(file.path("Rcode/predictDBH_Height.R"))

# Original locations ----------------------------------------------------

# extract trees for each plot and use plot location (turning point) to build stem map
for (i in 1:nrow(plotLoc)) {
  # get trees for single plot
  trees <- allTrees[allTrees$Plot_Number == plotsWithTreesAndLidar[i], ]

  # create a unique identifer
  trees$TreeID = paste0(trees$Plot_Number, "_", trees$Tag_Num, "_", trees$Species)

  # loop through trees to predict height using Hanus, FVS and custom models
  heights <- matrix(nrow = nrow(trees), ncol = 3)
  for (j in 1:nrow(trees)) {
    heights[j, 1] <- predictHeight(trees$Species[j], trees$DBH_cm[j], method = "hanus", DBHUnits = "cm", heightUnits = "meters")
    heights[j, 2] <- predictHeight(trees$Species[j], trees$DBH_cm[j], method = "fvs", DBHUnits = "cm", heightUnits = "meters")
    heights[j, 3] <- predictHeight(trees$Species[j], trees$DBH_cm[j], method = "custom", DBHUnits = "cm", heightUnits = "meters")
  }

  trees$hanusHt <- heights[, 1]
  trees$fvsHt <- heights[, 2]
  trees$customHt <- heights[, 3]

  # add original plot location to tree records
  trees$OLPlotX <- plotLoc$X[i]
  trees$OLPlotY <- plotLoc$Y[i]

  treePos <- computeLocalTreePositions(trees, xRef = plotLoc$X[i], yRef = plotLoc$Y[i])

  # change labels
  colnames(treePos) <- c("OLTreeX", "OLTreeY")

  # add X & Y columns to trees
  trees <- cbind(trees, treePos)

  # merge plot
  if (i == 1) {
    OLTrees <- trees
  } else {
    OLTrees <- rbind(OLTrees, trees)
  }
}

# write data...OL
write.csv(OLTrees, file = paste0(outputFolder, "/OLtrees.csv"), row.names = FALSE, quote = TRUE)


# Automated adjustment ----------------------------------------------------

# read OL trees...need to specify types so plot identifier keeps leading zeros
OLTrees <- read.csv(paste0(outputFolder, "/OLtrees.csv"), stringsAsFactors = FALSE,
                     colClasses = c(NA, NA, "character", rep(NA, 21)))

# read matching pairs...manually matched
matchFilename <- paste0("G:/R_Stuff/ONRCDroneLidar/Adjustments/MatchTreesNEW.xlsx")
sheetname <- "Sheet1"
matchTrees <- read_excel(matchFilename, sheet = sheetname)

# go through the plots, do the affine transformations, and adjust locations for all trees on the plots
for (i in 1:length(plotsWithTreesAndLidar)) {
  # i = 1
  # {

  # get matches for plot...plotsWithTreesAndLidar has strings for plot numbers, e.g. "07"
  matches <- matchTrees[matchTrees$Plot_Num == as.integer(plotsWithTreesAndLidar[i]),]

  # read TAOs
  TAOs <- read.csv(file = paste0(plotFolders[which(plotNumbers == plotsWithTreesAndLidar[i])], "/Processing/Trees/", "FUSIONtrees.csv"), stringsAsFactors = FALSE)

  # get field trees for plot
  allpTrees <- OLTrees[OLTrees$Plot_Number == plotsWithTreesAndLidar[i],]

  # make sure we have matches...Original set of trees used for manual matching included
  # tree with anomaly codes 2 & 6 but these are dropped in the candidate trees for training
  # the classifier
  matches <- matches[matches$Tag_Num %in% allpTrees$Tag_Num, ]

  # set up control points
  controlPts <- data.frame("xSource" = rep(0, nrow(matches)), "ySource" = rep(0, nrow(matches)), "xTarget" = rep(0, nrow(matches)), "yTarget" = rep(0, nrow(matches)))
  for (j in 1:nrow(matches)) {
    controlPts$xSource[j] <- allpTrees$OLTreeX[which(allpTrees$Tag_Num == matches$Tag_Num[j])]
    controlPts$ySource[j] <- allpTrees$OLTreeY[which(allpTrees$Tag_Num == matches$Tag_Num[j])]
    controlPts$xTarget[j] <- TAOs$X[which(TAOs$ID == matches$TAO_ID[j])]
    controlPts$yTarget[j] <- TAOs$Y[which(TAOs$ID == matches$TAO_ID[j])]
  }

  # join TAO ID for trees used for matching
#  allpTrees$Plot_ID <- as.integer(allpTrees$Plot_Number)
#  allpTrees <- left_join(allpTrees, matches, by = c("Plot_ID" = "Plot_Num", "Tag_Num" = "Tag_Num"), keep = FALSE)

  # affine transformation
  if (nrow(controlPts) < 3) {
    pAffine <- data.frame("X_Target" = NA, "Y_Target" = NA)
    pAffine$Tag_Num <- NA
    pAffine$TAO_ID <- NA
    pAffine$Plot_Num <- plotsWithTreesAndLidar[i]
    pAffine$Plot_RMSE <- NA

    allpTrees$PAfPlotX <- allpTrees$OLPlotX
    allpTrees$PAfPlotY <- allpTrees$OLPlotY
    allpTrees$PAfTreeX <- allpTrees$OLTreeX
    allpTrees$PAfTreeY <- allpTrees$OLTreeY
  } else {
    aft = AffineTransformation(controlPts)
    calculateParameters(aft)
    atp <- getParameters(aft)
    if (nrow(controlPts) < 4) {
      pAffine <- data.frame("X_Target" = NA, "Y_Target" = NA)
      pAffine$Tag_Num <- NA
      pAffine$TAO_ID <- NA
      pAffine$Plot_Num <- plotsWithTreesAndLidar[i]
      pAffine$Plot_RMSE <- NA
    } else {
      pAffine <- data.frame(getResiduals(aft))
      pAffine$Tag_Num <- matches$Tag_Num
      pAffine$TAO_ID <- matches$TAO_ID
      pAffine$Plot_Num <- plotsWithTreesAndLidar[i]
      pAffine$Plot_RMSE <- getRMSE(aft)
    }

    # print(plotsWithTreesAndLidar[i])
    # print(getResiduals(aft))
    # print(getRMSE(aft))

    # we can use the return values from getParameters(aft) to apply the transformation using:
    # x' = ax + by + c
    # y' = dx + ey + f
    #
    allpTrees$PAfPlotX <- atp["a"] * allpTrees$OLPlotX + atp["b"] * allpTrees$OLPlotY + atp["c"]
    allpTrees$PAfPlotY <- atp["d"] * allpTrees$OLPlotX + atp["e"] * allpTrees$OLPlotY + atp["f"]
    allpTrees$PAfTreeX <- atp["a"] * allpTrees$OLTreeX + atp["b"] * allpTrees$OLTreeY + atp["c"]
    allpTrees$PAfTreeY <- atp["d"] * allpTrees$OLTreeX + atp["e"] * allpTrees$OLTreeY + atp["f"]
  }

  pAffine$PAfPlotAveShift <- mean(sqrt((allpTrees$PAfPlotX - allpTrees$OLPlotX) ^ 2 + (allpTrees$PAfPlotY - allpTrees$OLPlotY) ^ 2))
  pAffine$PAfTreeAveShift <- mean(sqrt((allpTrees$PAfTreeX - allpTrees$OLTreeX) ^ 2 + (allpTrees$PAfTreeY - allpTrees$OLTreeY) ^ 2))

  if (i == 1) {
    resultsAffine <- pAffine
  } else {
    resultsAffine <- rbind(resultsAffine, pAffine)
  }

  # similarity transformation
  sft = SimilarityTransformation(controlPts)
  calculateParameters(sft)
  stp <- getParameters(sft)
  if (nrow(controlPts) < 3) {
    pSimilarity <- data.frame("X1" = NA, "X2" = NA)
    pSimilarity$Tag_Num <- NA
    pSimilarity$TAO_ID <- NA
    pSimilarity$Plot_Num <- plotsWithTreesAndLidar[i]
    pSimilarity$Plot_RMSE <- NA
  } else {
    pSimilarity <- data.frame(getResiduals(sft))
    pSimilarity$Tag_Num <- matches$Tag_Num
    pSimilarity$TAO_ID <- matches$TAO_ID
    pSimilarity$Plot_Num <- plotsWithTreesAndLidar[i]
    pSimilarity$Plot_RMSE <- getRMSE(sft)
  }

  # for a similarity transformation the equations are:
  # x' = ax + by + c
  # y' = ay - bx + d

  # similarity transformation
  allpTrees$PAsPlotX <- stp["a"] * allpTrees$OLPlotX + stp["b"] * allpTrees$OLPlotY + stp["c"]
  allpTrees$PAsPlotY <- stp["a"] * allpTrees$OLPlotY - stp["b"] * allpTrees$OLPlotX + stp["d"]
  allpTrees$PAsTreeX <- stp["a"] * allpTrees$OLTreeX + stp["b"] * allpTrees$OLTreeY + stp["c"]
  allpTrees$PAsTreeY <- stp["a"] * allpTrees$OLTreeY - stp["b"] * allpTrees$OLTreeX + stp["d"]

  pSimilarity$PAfPlotAveShift <- mean(sqrt((allpTrees$PAsPlotX - allpTrees$OLPlotX) ^ 2 + (allpTrees$PAsPlotY - allpTrees$OLPlotY) ^ 2))
  pSimilarity$PAfTreeAveShift <- mean(sqrt((allpTrees$PAsTreeX - allpTrees$OLTreeX) ^ 2 + (allpTrees$PAsTreeY - allpTrees$OLTreeY) ^ 2))

  if (i == 1) {
    resultsSimilarity <- pSimilarity
    mergedPlotTrees <- allpTrees
    PAResults <- data.frame(Plot_Num = plotsWithTreesAndLidar[i],
                            OLPlotX = allpTrees$OLPlotX[1],
                            OLPlotY = allpTrees$OLPlotY[1],
                            PAfPlotX = allpTrees$PAfPlotX[1],
                            PAfPlotY = allpTrees$PAfPlotY[1],
                            PAsPlotX = allpTrees$PAsPlotX[1],
                            PAsPlotY = allpTrees$PAsPlotY[1])
  } else {
    resultsSimilarity <- rbind(resultsSimilarity, pSimilarity)
    mergedPlotTrees <- rbind(mergedPlotTrees, allpTrees)
    PAResults <- rbind(PAResults, data.frame(Plot_Num = plotsWithTreesAndLidar[i],
                                            OLPlotX = allpTrees$OLPlotX[1],
                                            OLPlotY = allpTrees$OLPlotY[1],
                                            PAfPlotX = allpTrees$PAfPlotX[1],
                                            PAfPlotY = allpTrees$PAfPlotY[1],
                                            PAsPlotX = allpTrees$PAsPlotX[1],
                                            PAsPlotY = allpTrees$PAsPlotY[1])
    )
  }
}

# write off plot trees with adjusted locations
write.csv(mergedPlotTrees, file = paste0(outputFolder, "/PATrees.csv"), row.names = FALSE, quote = TRUE)

# write off plot-level results
PAResults$PAfdiffX <- PAResults$PAfPlotX - PAResults$OLPlotX
PAResults$PAfdiffY <- PAResults$PAfPlotY - PAResults$OLPlotY
PAResults$PAfdiff <- sqrt((PAResults$PAfPlotX - PAResults$OLPlotX) ^ 2 + (PAResults$PAfPlotY - PAResults$OLPlotY) ^ 2)
PAResults$PAsdiffX <- PAResults$PAsPlotX - PAResults$OLPlotX
PAResults$PAsdiffY <- PAResults$PAsPlotY - PAResults$OLPlotY
PAResults$PAsdiff <- sqrt((PAResults$PAsPlotX - PAResults$OLPlotX) ^ 2 + (PAResults$PAsPlotY - PAResults$OLPlotY) ^ 2)
write.csv(PAResults, file = paste0(outputFolder, "/PAResults.csv"), row.names = FALSE, quote = TRUE)

# need to write off results...maybe look at residuals to figure if match points are any good
write.csv(resultsAffine, file = paste0(outputFolder, "/AffineResults.csv"), row.names = FALSE, quote = TRUE)
write.csv(resultsSimilarity, file = paste0(outputFolder, "/SimilarityResults.csv"), row.names = FALSE, quote = TRUE)

# # testing some plots
# plot(allpTrees$PAfTreeX, allpTrees$PAfTreeY, pch = 19, col = "red")
# points(allpTrees$OLTreeX, allpTrees$OLTreeY)
# points(controlPts$xSource, controlPts$ySource, pch = 5)
# points(controlPts$xTarget, controlPts$yTarget, pch = 7)
# points(TAOs$X, TAOs$Y, pch = 9, col = "green")


# AA adjustments ----------------------------------------------------------

# read PA trees...need to specify types so plot identifier keeps leading zeros
PATrees <- read.csv(paste0(outputFolder, "/PAtrees.csv"), stringsAsFactors = FALSE,
                    colClasses = c(NA, NA, "character", rep(NA, 21)))

for (i in 1:nrow(plotLoc)) {
  # get trees for single plot
  trees <- PATrees[PATrees$Plot_Number == plotsWithTreesAndLidar[i], ]

  # add original plot location to tree records
  plotX <- trees$OLPlotX[1]
  plotY <- trees$OLPlotY[1]

  # create a subset for correlation using LIDAR_visible flag
  treesSubset <- trees[trees$LiDAR_visible == "Y", ]

  # filter using anomaly codes
  #treesSubset <- treesSubset[treesSubset$Anomaly1 == 0, ]
  treesSubset <- treesSubset[treesSubset$Anomaly1 == 0 | treesSubset$Anomaly1 == 2 | treesSubset$Anomaly1 == 6, ]

  # sort by height
  treesSubset <- treesSubset[order(-treesSubset$customHt),]

  # take largest trees
  treesSubset <- treesSubset[1:(as.integer(nrow(treesSubset) / 2)),]

  # read CHM for plot
  CHM <- readDTM(paste0(plotFolders[which(plotNumbers == plotsWithTreesAndLidar[i])], "/Processing/CHM/CHM.dtm"), type = "terra", epsg = 26910)

  # attempt adjustment
  newLocation <- findBestPlotLocationPhaseCorrelation(
    CHM,
    treesSubset,
    initialX = plotX,
    initialY = plotY,
    searchRadius = 50,
    stemLocationFields = c("OLTreeX", "OLTreeY"),
    stemHeightField = "customHt",
    CHMbuffer = 1.0,
    stemMapBuffer = 1.0,
    cropCHM = TRUE,
    method = "mask",
    corrMethod = "cross",
    normalize = TRUE,
    addNoise = FALSE,
    noiseMagnitude = 0.1, includeRasters = TRUE
  )

#  adjtree <- moveTreesToPlotXY(trees, newLocation$offsetX, newLocation$offsetY)

  treePos <- computeLocalTreePositions(trees, xRef = newLocation$newX, yRef = newLocation$newY)

  # change labels
  colnames(treePos) <- c("AATreeX", "AATreeY")

  # add plot location to trees...duplicated for every tree
  trees$AAPlotX <- newLocation$newX
  trees$AAPlotY <- newLocation$newY

  # add X & Y columns to trees
  trees <- cbind(trees, treePos)

  # merge plot
  if (i == 1) {
    AATrees <- trees
    AAResults <- data.frame(Plot_Num = plotsWithTreesAndLidar[i],
                            OLPlotX = plotX,
                            OLPlotY = plotY,
                            AAPlotX = newLocation$newX,
                            AAPlotY = newLocation$newY)
  } else {
    AATrees <- rbind(AATrees, trees)
    AAResults <- rbind(AAResults, data.frame(Plot_Num = plotsWithTreesAndLidar[i],
                                            OLPlotX = plotX,
                                            OLPlotY = plotY,
                                            AAPlotX = newLocation$newX,
                                            AAPlotY = newLocation$newY)
    )
  }
}

# write off plot trees with adjusted locations
write.csv(AATrees, file = paste0(outputFolder, "/AATrees.csv"), row.names = FALSE, quote = TRUE)

# write plot location results
AAResults$AAdiffX <- AAResults$AAPlotX - AAResults$OLPlotX
AAResults$AAdiffY <- AAResults$AAPlotY - AAResults$OLPlotY
AAResults$AAdiff <- sqrt((AAResults$AAPlotX - AAResults$OLPlotX) ^ 2 + (AAResults$AAPlotY - AAResults$OLPlotY) ^ 2)
write.csv(AAResults, file = paste0(outputFolder, "/AAResults.csv"), row.names = FALSE, quote = TRUE)


# there are 27 plots with field and lidar data
if (FALSE) {
  i <- 18
  plot_Number <- plotsWithTreesAndLidar[i]
  trees <- AATrees[AATrees$Plot_Number == plot_Number, ]
  CHM <- readDTM(paste0(plotFolders[which(plotNumbers == plotsWithTreesAndLidar[i])], "/Processing/CHM/CHM.dtm"), type = "terra", epsg = 26910)
  plot(CHM, main = paste("Plot", plot_Number), xlim = c(trees$OLPlotX[1] - 25, trees$OLPlotX[1] + 25), ylim = c(trees$OLPlotY[1] - 25, trees$OLPlotY[1] + 25))
  points(trees$OLTreeX, trees$OLTreeY, pch = 3)
  points(trees$AATreeX, trees$AATreeY, col = "red")
}








# ITA/ITAL using manually adjusted locations ------------------------------
#
# read adjustment results from Bob and Ally...average location, lean angle, lean azimuth
# and compute locations and lean
#
# Xfield and Yfield has the original location based on the plot GPS
# height data in this file were done prior to adjustments to our custom model. However, we have measured heights
# that will be used for matching so th is isn't a problem.
# (X.Ally,Y.Ally) and (X.Bob,Y.Bob) have the base locations after manual adjustments
# (TopX.Ally,TopY.Ally) and (TopX.Bob,TopY.Bob) have the treetop locations after manual adjustments
# lean is calculated using base and treetop locations
#
# IMPORTANT: not all trees on the plots were adjusted.
# Only trees with anomaly code of 0 and PSME and TSHE were given to Ally and Bob for adjustments. In hindsight,
# it may have been good to attempt adjustments for some of the dead or leaning trees. However, these would not be
# used for any modeling so maybe not useful.
adjustedTrees <- read.csv("G:/R_Stuff/DroneLidarCode/extras/AdjustedTrees_AllPlots.csv", stringsAsFactors = FALSE)

# for ITA, just use the base location with no lean
adjustedTrees$ITATreeX <- (adjustedTrees$X.Ally + adjustedTrees$X.Bob) / 2
adjustedTrees$ITATreeY <- (adjustedTrees$Y.Ally + adjustedTrees$Y.Bob) / 2

# use lean for ITAL
adjustedTrees$ITALTreeX <- (adjustedTrees$X.Ally + adjustedTrees$X.Bob) / 2
adjustedTrees$ITALTreeY <- (adjustedTrees$Y.Ally + adjustedTrees$Y.Bob) / 2
adjustedTrees$ITALTreeTopX <- (adjustedTrees$TopX.Ally + adjustedTrees$TopX.Bob) / 2
adjustedTrees$ITALTreeTopY <- (adjustedTrees$TopY.Ally + adjustedTrees$TopY.Bob) / 2

# read PA trees and join ITA/ITAL locations...don't have a location for all trees
AATrees <- read.csv(paste0(outputFolder, "/AAtrees.csv"), stringsAsFactors = FALSE,
                    colClasses = c(NA, NA, "character", rep(NA, 21)))

ITATrees <- merge(AATrees, adjustedTrees[, c("Plot_Name", "Tag_Num", "ITATreeX", "ITATreeY", "ITALTreeX", "ITALTreeY", "ITALTreeTopX", "ITALTreeTopY")],
                                         by = c("Plot_Name", "Tag_Num"),
                                         all = TRUE)

# write trees
write.csv(ITATrees, file = paste0(outputFolder, "/ITATrees.csv"), row.names = FALSE, quote = TRUE)



# Automated adjustment using ITA trees ------------------------------------

# read ITA trees...need to specify types so plot identifier keeps leading zeros
ITATrees <- read.csv(paste0(outputFolder, "/ITATrees.csv"), stringsAsFactors = FALSE,
                    colClasses = c(NA, NA, "character", rep(NA, 21)))

# go through the plots and set up control points using ITA and OL tree locations
# Then do the affine transformations and adjust locations for all trees on the plots
# This is like the AA process except we are using the manual adjustments to control
# the transformations instead of matching field and lidar-derived trees.
for (i in 1:length(plotsWithTreesAndLidar)) {
  # get field trees for plot...these will include trees with no ITA locations
  allpTrees <- ITATrees[ITATrees$Plot_Number == as.integer(plotsWithTreesAndLidar[i]),]

  # get only the trees with ITA locations
  matches <- allpTrees[!is.na(allpTrees$ITATreeX), ]

  # set up control points
  controlPts <- data.frame("xSource" = matches$OLTreeX,
                           "ySource" = matches$OLTreeY,
                           "xTarget" = matches$ITATreeX,
                           "yTarget" = matches$ITATreeY)

  # affine transformation
  if (nrow(controlPts) < 3) {
    pAffine <- data.frame("X_Target" = NA, "Y_Target" = NA)
    pAffine$Tag_Num <- NA
    pAffine$TAO_ID <- NA
    pAffine$Plot_Num <- plotsWithTreesAndLidar[i]
    pAffine$Plot_RMSE <- NA

    allpTrees$ITAPAfPlotX <- allpTrees$OLPlotX
    allpTrees$ITAPAfPlotY <- allpTrees$OLPlotY
    allpTrees$ITAPAfTreeX <- allpTrees$OLTreeX
    allpTrees$ITAPAfTreeY <- allpTrees$OLTreeY
  } else {
    aft = AffineTransformation(controlPts)
    calculateParameters(aft)
    atp <- getParameters(aft)
    if (nrow(controlPts) < 4) {
      pAffine <- data.frame("X_Target" = NA, "Y_Target" = NA)
      pAffine$Tag_Num <- NA
      pAffine$TAO_ID <- NA
      pAffine$Plot_Num <- plotsWithTreesAndLidar[i]
      pAffine$Plot_RMSE <- NA
    } else {
      pAffine <- data.frame(getResiduals(aft))
      pAffine$Tag_Num <- matches$Tag_Num
      pAffine$TAO_ID <- matches$TAO_ID
      pAffine$Plot_Num <- plotsWithTreesAndLidar[i]
      pAffine$Plot_RMSE <- getRMSE(aft)
    }

    # print(plotsWithTreesAndLidar[i])
    # print(getResiduals(aft))
    # print(getRMSE(aft))

    # we can use the return values from getParameters(aft) to apply the transformation using:
    # x' = ax + by + c
    # y' = dx + ey + f
    #
    allpTrees$ITAPAfPlotX <- atp["a"] * allpTrees$OLPlotX + atp["b"] * allpTrees$OLPlotY + atp["c"]
    allpTrees$ITAPAfPlotY <- atp["d"] * allpTrees$OLPlotX + atp["e"] * allpTrees$OLPlotY + atp["f"]
    allpTrees$ITAPAfTreeX <- atp["a"] * allpTrees$OLTreeX + atp["b"] * allpTrees$OLTreeY + atp["c"]
    allpTrees$ITAPAfTreeY <- atp["d"] * allpTrees$OLTreeX + atp["e"] * allpTrees$OLTreeY + atp["f"]
  }

  pAffine$ITAPAfPlotAveShift <- mean(sqrt((allpTrees$ITAPAfPlotX - allpTrees$OLPlotX) ^ 2 + (allpTrees$ITAPAfPlotY - allpTrees$OLPlotY) ^ 2))
  pAffine$ITAPAfTreeAveShift <- mean(sqrt((allpTrees$ITAPAfTreeX - allpTrees$OLTreeX) ^ 2 + (allpTrees$ITAPAfTreeY - allpTrees$OLTreeY) ^ 2))

  if (i == 1) {
    resultsAffine <- pAffine
  } else {
    resultsAffine <- rbind(resultsAffine, pAffine)
  }

  # similarity transformation
  sft = SimilarityTransformation(controlPts)
  calculateParameters(sft)
  stp <- getParameters(sft)
  if (nrow(controlPts) < 3) {
    pSimilarity <- data.frame("X1" = NA, "X2" = NA)
    pSimilarity$Tag_Num <- NA
    pSimilarity$TAO_ID <- NA
    pSimilarity$Plot_Num <- plotsWithTreesAndLidar[i]
    pSimilarity$Plot_RMSE <- NA
  } else {
    pSimilarity <- data.frame(getResiduals(sft))
    pSimilarity$Tag_Num <- matches$Tag_Num
    pSimilarity$TAO_ID <- matches$TAO_ID
    pSimilarity$Plot_Num <- plotsWithTreesAndLidar[i]
    pSimilarity$Plot_RMSE <- getRMSE(sft)
  }

  # for a similarity transformation the equations are:
  # x' = ax + by + c
  # y' = ay - bx + d

  # similarity transformation
  allpTrees$ITAPAsPlotX <- stp["a"] * allpTrees$OLPlotX + stp["b"] * allpTrees$OLPlotY + stp["c"]
  allpTrees$ITAPAsPlotY <- stp["a"] * allpTrees$OLPlotY - stp["b"] * allpTrees$OLPlotX + stp["d"]
  allpTrees$ITAPAsTreeX <- stp["a"] * allpTrees$OLTreeX + stp["b"] * allpTrees$OLTreeY + stp["c"]
  allpTrees$ITAPAsTreeY <- stp["a"] * allpTrees$OLTreeY - stp["b"] * allpTrees$OLTreeX + stp["d"]

  pSimilarity$ITAPAfPlotAveShift <- mean(sqrt((allpTrees$ITAPAsPlotX - allpTrees$OLPlotX) ^ 2 + (allpTrees$ITAPAsPlotY - allpTrees$OLPlotY) ^ 2))
  pSimilarity$ITAPAfTreeAveShift <- mean(sqrt((allpTrees$ITAPAsTreeX - allpTrees$OLTreeX) ^ 2 + (allpTrees$ITAPAsTreeY - allpTrees$OLTreeY) ^ 2))

  if (i == 1) {
    resultsSimilarity <- pSimilarity
    mergedPlotTrees <- allpTrees
    PAResults <- data.frame(Plot_Num = plotsWithTreesAndLidar[i],
                            OLPlotX = allpTrees$OLPlotX[1],
                            OLPlotY = allpTrees$OLPlotY[1],
                            ITAPAfPlotX = allpTrees$ITAPAfPlotX[1],
                            ITAPAfPlotY = allpTrees$ITAPAfPlotY[1],
                            ITAPAsPlotX = allpTrees$ITAPAsPlotX[1],
                            ITAPAsPlotY = allpTrees$ITAPAsPlotY[1])
  } else {
    resultsSimilarity <- rbind(resultsSimilarity, pSimilarity)
    mergedPlotTrees <- rbind(mergedPlotTrees, allpTrees)
    PAResults <- rbind(PAResults, data.frame(Plot_Num = plotsWithTreesAndLidar[i],
                                             OLPlotX = allpTrees$OLPlotX[1],
                                             OLPlotY = allpTrees$OLPlotY[1],
                                             ITAPAfPlotX = allpTrees$ITAPAfPlotX[1],
                                             ITAPAfPlotY = allpTrees$ITAPAfPlotY[1],
                                             ITAPAsPlotX = allpTrees$ITAPAsPlotX[1],
                                             ITAPAsPlotY = allpTrees$ITAPAsPlotY[1])
    )
  }
}

# write off plot trees with adjusted locations
write.csv(mergedPlotTrees, file = paste0(outputFolder, "/ITAPATrees.csv"), row.names = FALSE, quote = TRUE)

# write off plot-level results
PAResults$ITAPAfdiffX <- PAResults$ITAPAfPlotX - PAResults$OLPlotX
PAResults$ITAPAfdiffY <- PAResults$ITAPAfPlotY - PAResults$OLPlotY
PAResults$ITAPAfdiff <- sqrt((PAResults$ITAPAfPlotX - PAResults$OLPlotX) ^ 2 + (PAResults$ITAPAfPlotY - PAResults$OLPlotY) ^ 2)
PAResults$ITAPAsdiffX <- PAResults$ITAPAsPlotX - PAResults$OLPlotX
PAResults$ITAPAsdiffY <- PAResults$ITAPAsPlotY - PAResults$OLPlotY
PAResults$ITAPAsdiff <- sqrt((PAResults$ITAPAsPlotX - PAResults$OLPlotX) ^ 2 + (PAResults$ITAPAsPlotY - PAResults$OLPlotY) ^ 2)
write.csv(PAResults, file = paste0(outputFolder, "/ITAPAResults.csv"), row.names = FALSE, quote = TRUE)

# need to write off results...maybe look at residuals to figure if match points are any good
write.csv(resultsAffine, file = paste0(outputFolder, "/ITAAffineResults.csv"), row.names = FALSE, quote = TRUE)
write.csv(resultsSimilarity, file = paste0(outputFolder, "/ITASimilarityResults.csv"), row.names = FALSE, quote = TRUE)


















# plotting ----------------------------------------------------------------
#
# read ITA trees...need to specify types so plot identifier keeps leading zeros
ITATrees <- read.csv(paste0(outputFolder, "/ITAPATrees.csv"), stringsAsFactors = FALSE,
                     colClasses = c(NA, NA, "character", rep(NA, 21)))

# do plots showing various adjustment results
pdf(file = paste0(outputFolder, "/AutomatedAdjustmentResults.pdf"))
for (i in 1:length(plotsWithTreesAndLidar)) {
  plot_Number <- as.integer(plotsWithTreesAndLidar[i])
  trees <- ITATrees[ITATrees$Plot_Number == plot_Number, ]
  CHM <- readDTM(paste0(plotFolders[which(plotNumbers == plotsWithTreesAndLidar[i])], "/Processing/CHM/CHM.dtm"), type = "terra", epsg = 26910)
  plot(CHM, main = paste("Plot", plot_Number), xlim = c(trees$OLPlotX[1] - 25, trees$OLPlotX[1] + 25), ylim = c(trees$OLPlotY[1] - 25, trees$OLPlotY[1] + 25))

  # original plot location
  points(trees$OLPlotX[1], trees$OLPlotY[1], cex = 2.0, pch = 10, col = "black")

  # tree locations based on original plot location
  points(trees$OLTreeX, trees$OLTreeY, pch = 3, col = "black")

  # plot locations after affine transformation
  points(trees$PAfPlotX[1], trees$PAfPlotY[1], cex = 2.0, pch = 10, col = "red")

  # tree locations after affine transformation
  points(trees$PAfTreeX, trees$PAfTreeY, pch = 6, col = "red")

  # plot locations after similarity transformation
  points(trees$PAsPlotX[1], trees$PAsPlotY[1], cex = 2.0, pch = 10, col = "green")

  # tree locations after similarity transformation
  points(trees$PAsTreeX, trees$PAsTreeY, pch = 2, col = "green")

  # plot locations after cross correlation
  points(trees$AAPlotX[1], trees$AAPlotY[1], cex = 2.0, pch = 10, col = "cyan")

  # tree locations after cross correlation
  points(trees$AATreeX, trees$AATreeY, pch = 4, col = "cyan")

  # tree locations after individual adjustment
  points(trees$ITATreeX, trees$ITATreeY, pch = 5, col = "blue")

  legend("bottom", c("original", "affine", "similarity", "imgcorr", "manual"), pch = c(3, 6, 2, 4, 5), col = c("black", "red", "green", "cyan", "blue"), horiz = TRUE, inset = 0.05)

}
dev.off()

# matching ----------------------------------------------------------------
#
# use matching logic with each of the stem maps to pair a lidar-derived tree with
# a field tree and build separate sets of training trees.
#
# code comes from ONRCDroneLidar/TrainingTrees.R starting in line 475
library(mapview)
library(terra)
library(leafem)
library(nngeo)

ITATrees <- read.csv(paste0(outputFolder, "/ITAPATrees.csv"), stringsAsFactors = FALSE)

ITATrees <- dplyr::filter(ITATrees, Species == "PSME" | Species == "TSHE", Anomaly1 == 0, LiDAR_visible == "Y")

fieldXYColumns <- list(
  c("OLTreeX", "OLTreeY"),
  c("PAfTreeX", "PAfTreeY"),
  c("PAsTreeX", "PAsTreeY"),
  c("AATreeX", "AATreeY"),
  c("ITATreeX", "ITATreeY"),
  c("ITALTreeX", "ITALTreeY"),
  c("ITALTreeTopX", "ITALTreeTopY"),
  c("ITAPAfTreeX", "ITAPAfTreeY"),
  c("ITAPAsTreeX", "ITAPAsTreeY")
)

methods <- c(
  "OL",
  "PAf",
  "PAs",
  "AA",
  "ITA",
  "ITAL",
  "ITALTop",
  "ITAPAf",
  "ITAPAs"
)

matchDistanceThreshold <- 2.0

for (i in 1:nrow(plotLoc)) {
  #  i <- 22
  thePlot <- which(plotNumbers == plotLoc$PlotID[i])

  # reading the tree file for FUSION as these files have the height of the highest lidar point and the crown base height
  # outputs from TreeSeg have CHM cell centers and no crown base height
  trees_FUSION <- read.csv(paste0(plotFolders[thePlot], "/Processing/Trees", "/FUSIONtrees.csv"), stringsAsFactors = FALSE)

  # change names of columns
  colnames(trees_FUSION) <- c(
    "BasinID",
    "Xlidar",
    "Ylidar",
    "Elevation",
    "Height_m",
    "CBH_m",
    "MinCrownDia_m",
    "MaxCrownDia_m",
    "rotation",
    "R",
    "G",
    "B"
  )

  trees_lidar <- st_as_sf(trees_FUSION,
                          coords = c("Xlidar", "Ylidar"),
                          remove = FALSE,
                          crs = 26910)

  for (j in 1:length(methods)) {
    tTrees <- ITATrees[which(ITATrees$Plot_Number == as.numeric(plotLoc$PlotID[i])), ]

    # drop trees with missing locations...ITA methods
    tTrees <- tTrees[!is.na(tTrees[, fieldXYColumns[[j]][[1]]]), ]
    tTrees <- tTrees[!is.na(tTrees[, fieldXYColumns[[j]][[2]]]), ]
    trees_field <- st_as_sf(tTrees,
                            coords = fieldXYColumns[[j]],
                            remove = FALSE,
                            crs = 26910)

    # no matter how you do the matching, there could be cases where there are multiple trees
    # within the distance threshold so we need to be able to resolve cases where there is not a
    # 1-1 match
    # terra::nearest(from, to)
    #
    # match field trees to lidar trees...lidar tree can be the closest tree for several field trees
    t <- nearest(vect(trees_field), vect(trees_lidar))

    # match lidar trees to field trees...field tree can be the closest tree for several lidar trees
    # if you use this statement,you have to change logic below to swap to_id and from_id
    #  t <- nearest(vect(trees_lidar), vect(trees_field))

    t <- t[t$distance <= matchDistanceThreshold,]

    #  print(t)
    #  t@ptr$df

    # we may have more than 1 row with the same to_id value (lidar trees that were
    # the closest tree to more than 1 field tree)...for these cases, keep the 1 that
    # is closest

    # sort in ascending order based on to_id and distance
    t <- t[order(t$to_id, t$distance), ]

    # keep rows that are not duplicated (will drop rows after the first with duplicated to_id)
    t <- t[!duplicated(t$to_id), ]

    # merge data for field and lidar trees
    matchTrees <- data.frame(
      st_drop_geometry(trees_field[t$from_id,]),
      "Distance_m" = t$distance,
      st_drop_geometry(trees_lidar[t$to_id,])
    )

    # Build training data and write to CSV file
    trainData <- matchTrees[order(matchTrees$Species),]

    # drop columns
    drops <- c("rotation", "R", "G", "B")
    trainData <- trainData[ , !(names(trainData) %in% drops)]

    # read treetop metrics and join with basin IDs
    tt <- read.csv(paste0(plotFolders[thePlot], "/Processing/TreeTops_normalized_metrics_merged.csv"), stringsAsFactors = FALSE)
    trainData <- merge(trainData, tt, by.x = "BasinID", by.y = "Identifier")

    write.csv(trainData, paste0(outputFolder, "Training_Trees_Plot_", plotNumbers[thePlot], "_", methods[j], ".csv"), row.names = FALSE)

    # needed to prevent error while rendering
    mapviewOptions(fgb = FALSE)

    x1 <- mapview(trees_field
                  , col = "red"
                  , col.regions = "red"
                  , cex = 2
                  , layer.name = "Field trees"
    )
    x2 <- mapview(trees_lidar[t$to_id,]
                  # x2 <- mapview(trees_lidar
                  , col = "black"
                  , col.regions = "green"
                  , cex = 3
                  , layer.name = "Lidar trees"
    )
    x <- x1 + x2

    x <- addStaticLabels(x
                         , data = st_transform(trees_lidar[t$to_id,], 4326)
                         , trees_lidar[t$to_id,]$BasinID
                         , direction = 'top'
                         , textOnly = TRUE
                         , offset = c(0,10)
    )

    x <- addStaticLabels(x
                         , data = st_transform(trees_field, 4326)
                         , trees_field$Species
                         , direction = 'left'
                         , textOnly = TRUE
                         , offset = c(0,5)
    )

    # create line segments connecting match trees
    ml <- st_connect(trees_lidar[t$to_id, ], trees_field[t$from_id, ])

    x <- addFeatures(x
                     , data = st_transform(ml, 4326)
    )

    x

    # save image of map
    mapshot(x
            , file = paste0(outputFolder, "Training_Trees_Plot_", plotNumbers[thePlot], "_", methods[j], ".png")
            , remove_controls = c("zoomControl", "layersControl", "homeButton",
                                  "drawToolbar", "easyButton")
    )
  }
}

# read all the metrics and merge...original field tree locations
for (j in 1:length(methods)) {
  for (i in 1:nrow(plotLoc)) {
    thePlot <- which(plotNumbers == plotLoc$PlotID[i])

    plt <- read.csv(paste0(outputFolder, "Training_Trees_Plot_", plotNumbers[thePlot], "_", methods[j], ".csv"), stringsAsFactors = FALSE)

    plt$Method <- methods[j]

    if (j == 1 & i == 1) {
      merged <- plt
    } else {
      merged <- rbind(merged, plt)
    }
  }
#  write.csv(merged, paste0(outputFolder, "Training_Trees_AllPlots", "_", methods[j], ".csv"), row.names = FALSE)
}
write.csv(merged, paste0(outputFolder, "Training_Trees_AllPlots", "_", "AllMethods", ".csv"), row.names = FALSE)

# summarize the number of match trees by plot and method
# This includes all trees regardless of species or anomaly code making the ITA methods appear
# to miss trees (we only adjusted locations of candidate trees)
mergedT <- dplyr::filter(merged, Species == "PSME" | Species == "TSHE", Anomaly1 == 0, LiDAR_visible == "Y")
t <- mergedT %>%
  group_by(Plot_Number, Method) %>%
  summarize(n())

t <- data.frame(Plot_Number = t$Plot_Number, Method = t$Method, N = t[, "n()"])
summary <-reshape(t, timevar="Method", idvar="Plot_Number", direction="wide")

# set new column names...note that summarize() alphabetizes the columns so the order
# doesn't match the list of methods
colnames(summary) <- c("Plot_Number",
                        "AA",
                        "ITA",
                        "ITAL",
                        "ITALTop",
                        "ITAPAf",
                        "ITAPAs",
                        "OL",
                        "PAf",
                        "PAs")

at <- allTrees %>%
  group_by(Plot_ID) %>%
  summarize(n())

atc <- allTrees %>%
  filter(Species == "PSME" | Species == "TSHE", Anomaly1 == 0, LiDAR_visible == "Y") %>%
  group_by(Plot_ID) %>%
  summarize(n())

colnames(at) <- c("Plot_Number", "Total trees")
colnames(atc) <- c("Plot_Number", "Candidate trees")

summary <- merge(summary, at, by = "Plot_Number")
summary <- merge(summary, atc, by = "Plot_Number")

rownames(summary) <- NULL
write.csv(summary, paste0(outputFolder, "Training_Trees_AllPlots_Summary.csv"), row.names = FALSE)


















# *****************************************************************************
# *****************************************************************************
# read lidar metrics for all plots and merge into a single file...not sure why
# I didn't do this a long time ago...
# relies on code near beginning of this file to set up plot folders
# *****************************************************************************
# *****************************************************************************
# #*********** this doesn't give us tree IDs that are useful!!!!
outputFolder <- "G:/R_Stuff/ONRCDroneLidar"
mergedMetrics <- data.frame()
for (i in 1:nrow(plotLoc)) {
  thePlot <- which(plotNumbers == plotLoc$PlotID[i])

  tt <- read.csv(paste0(plotFolders[thePlot], "/Processing/TreeTops_normalized_metrics_merged.csv"), stringsAsFactors = FALSE)
  trainData <- rbind(mergedMetrics, tt)
}
write.csv(merged, paste0(outputFolder, "/Metrics_AllPlots.csv"), row.names = FALSE)



# keep track of the number of trees that get a match...using same criteria for max distance

