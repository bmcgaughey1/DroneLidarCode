# new code 2/13/2023
# Compare various approaches to adjusting trees and matching with lidar TAOs
#
# Most of the code to construct stem maps is in TrainingTrees.R. The FieldTrees.csv
# file is produced by the code in TrainingTrees.R. The locations in FieldTrees.csv
# are derived from the post-processed plot locations and the field azimuths and
# distances from the reference point. These locations represent the starting point
# from which adjusted tree locations are created.
#
# The code in TrainingTrees.R has sections that are somewhat experimental mixed
# with sections of code that produce "final" data. I wanted to clean this up
# so the code is a little easier to follow.
#
# I also wanted to implement additional strategies for matching field and lidar
# trees. I have defined three location adjustment approaches:
# 1. Adjust all trees (or use a subset to adjust all trees) together using either
# a manual process or a semi-automated process involving a least squares adjustment.
# 2. Move individual trees to better align with tree tops in lidar data or stem
# hits in lidar data (this could be two approaches but there are not stem hits for
# all trees).
# 3. Move individual trees to align with stem hits when available AND adjust
# tree tops to align with lidar point cloud. This produces lean information for
# the adjusted trees.
#
library(dplyr)
library(vec2dtransf)

source("Rcode/predictDBH_Height.R")

makeFUSIONTrees <- function(x, R = 0, G = 192, B = 0, statusCode = 0) {
  FUSIONtrees <- data.frame(
    "TreeID" = x$TreeID,
    "X" = x$Xfield,
    "Y" = x$Yfield,
    "Elevation" = 0.0,
    "Height_m" = x$T3Ht,
    "CBH_m" = x$T3Ht * 0.6,
    "MinCrownDia_m" = x$T3Ht * 0.16,
    "MaxCrownDia_m" = x$T3Ht * 0.16,
    "rotation" = 0.0,
    "R" = R,
    "G" = G,
    "B" = B,
    "DBH_m" = x$DBH_cm / 100,
    "LeanFromVertical" = 0,
    "LeanAzimuth" = 0,
    "StatusCode" = statusCode
  )

  return(invisible(FUSIONtrees))
}

makeAdjustedFUSIONTrees <- function(x, R = 0, G = 192, B = 0, statusCode = 0) {
  FUSIONtrees <- data.frame(
    "TreeID" = x$TreeID,
    "X" = x$AdjXfield,
    "Y" = x$AdjYfield,
    "Elevation" = 0.0,
    "Height_m" = x$T3Ht,
    "CBH_m" = x$T3Ht * 0.6,
    "MinCrownDia_m" = x$T3Ht * 0.16,
    "MaxCrownDia_m" = x$T3Ht * 0.16,
    "rotation" = 0.0,
    "R" = R,
    "G" = G,
    "B" = B,
    "DBH_m" = x$DBH_cm / 100,
    "LeanFromVertical" = 0,
    "LeanAzimuth" = 0,
    "StatusCode" = statusCode
  )

  return(invisible(FUSIONtrees))
}

# *****************************************************************************
# *****************************************************************************
# read field trees and filter to just PSME and TSHE and only trees visible
# to lidar
# *****************************************************************************
# *****************************************************************************
outputFolder <- "E:/Backup/R_Stuff/ONRCDroneLidar"
combinedTrees <- read.csv(file = paste0(outputFolder, "/FieldTrees.csv"), stringsAsFactors = FALSE)

# predict height using custom T3 equations
for (i in 1:nrow(combinedTrees)) {
  combinedTrees$T3Ht[i] <- predictHeight(combinedTrees$Species[i], combinedTrees$DBH_cm[i], method = "customT3", DBHUnits = "cm", heightUnits = "meters")
}

# filter for species, anomaly codes and field call on whether or not tree is visible from above
other <- dplyr::filter(combinedTrees, (Species != "PSME" & Species != "TSHE") | Anomaly1 != 0 | LiDAR_visible != "Y")
psme <- dplyr::filter(combinedTrees, Species == "PSME", Anomaly1 == 0, LiDAR_visible == "Y")
tshe <- dplyr::filter(combinedTrees, Species == "TSHE", Anomaly1 == 0, LiDAR_visible == "Y")

# psme and tshe are the candidate trees for matching to lidar TAOs
candidates <- rbind(psme, tshe)

# # predict height using custom T3 equations
# for (i in 1:nrow(candidates)) {
#   candidates$T3Ht[i] <- predictHeight(candidates$Species[i], candidates$DBH_cm[i], method = "customT3", DBHUnits = "cm", heightUnits = "meters")
# }

plotsWithTrees <- unique(candidates$Plot_Number)

# we have lidar data for these plots...may not have field data for all plots
plotNumbers <- c("7", "8", "9", "10", "13", "14",
                 "16", "17", "18", "19", "20", "21",
                 "22", "23", "24", "25", "26", "27",
                 "28", "29", "31", "34", "36", "37",
                 "42", "43", "46", "47")

plotsWithTreesAndLidar <- plotNumbers[plotNumbers %in% plotsWithTrees]

# need explicit folder names for each plot since some lidar collects cover more than one plot
# also folder names are horrible!!
plotFolders <- c(
  "H:/T3_DroneLidar/Ba/Plot 7"
  , "H:/T3_DroneLidar/Ba/Plots 8,9,36,46"
  , "H:/T3_DroneLidar/Ba/Plots 8,9,36,46"
  , "H:/T3_DroneLidar/Ba/Plots 10,47"
  , "H:/T3_DroneLidar/Az/Plots 13,14,23,24,43"
  , "H:/T3_DroneLidar/Az/Plots 13,14,23,24,43"
  , "H:/T3_DroneLidar/Aa/Plots 16,31"
  , "H:/T3_DroneLidar/Aa/Plots 17,18,26,27"
  , "H:/T3_DroneLidar/Aa/Plots 17,18,26,27"
#  , "H:/T3_DroneLidar/Da/Plot 19"
  , "H:/T3_DroneLidar/Da/Plots 20,21,22"
  , "H:/T3_DroneLidar/Da/Plots 20,21,22"
  , "H:/T3_DroneLidar/Da/Plots 20,21,22"
  , "H:/T3_DroneLidar/Az/Plots 13,14,23,24,43"
  , "H:/T3_DroneLidar/Az/Plots 13,14,23,24,43"
  , "H:/T3_DroneLidar/Dz/Plots 25,42"
  , "H:/T3_DroneLidar/Aa/Plots 17,18,26,27"
  , "H:/T3_DroneLidar/Aa/Plots 17,18,26,27"
  , "H:/T3_DroneLidar/Aa/Plots 28,29,34"
  , "H:/T3_DroneLidar/Aa/Plots 28,29,34"
  , "H:/T3_DroneLidar/Aa/Plots 16,31"
  , "H:/T3_DroneLidar/Aa/Plots 28,29,34"
  , "H:/T3_DroneLidar/Ba/Plots 8,9,36,46"
  , "H:/T3_DroneLidar/Ba/Plot 37"
  , "H:/T3_DroneLidar/Dz/Plots 25,42"
  , "H:/T3_DroneLidar/Az/Plots 13,14,23,24,43"
  , "H:/T3_DroneLidar/Ba/Plots 8,9,36,46"
  , "H:/T3_DroneLidar/Ba/Plots 10,47"
)

outputFolder <- "G:/R_Stuff/ONRCDroneLidar/Adjustments"

# create FUSION tree files for the field measured trees
for (i in 1:length(plotsWithTreesAndLidar)) {
#   i = 1

  # create FUSION tree files for the TAO objects that will be used for matching
  pTrees <- candidates[candidates$Plot_Number == plotsWithTreesAndLidar[i],]
  fTrees <- makeFUSIONTrees(pTrees, R = 255, G = 0, B = 255)

  # get all trees for plot...needed to apply the adjustment
  allpTrees <- combinedTrees[combinedTrees$Plot_Number == plotsWithTreesAndLidar[i],]

  write.csv(fTrees, file = paste0(outputFolder, "/FUSIONFieldLocations_Plot_", plotsWithTreesAndLidar[i], ".csv"), row.names = FALSE, quote = TRUE)
}

# need to go through all the plots and match TAO identifiers with Tag_numbers

# read match table

# go through the plots, do the affine transformations, and adjust locations for all trees on the plots

# https://geotux.tuxfamily.org/en/2012/05/07/similarity-and-affine-transformations-in-r/

TAO <- c(
  509,
  1365,
  957,
  495,
  161,
  676,
  914,
  1267
)

Field <- c(
  29,
  1,
  15,
  8,
  30,
  31,
  12,
  13
)

TAOs <- read.csv(file = paste0("D:/T3_DroneLidar/Ba/Plot 7/Processing/Trees/", "FUSIONtrees.csv"), stringsAsFactors = FALSE)

controlPts <- data.frame("xSource" = rep(0, length(TAO)), "ySource" = rep(0, length(TAO)), "xTarget" = rep(0, length(TAO)), "yTarget" = rep(0, length(TAO)))
for (j in 1:length(TAO)) {
  controlPts$xSource[j] <- pTrees$Xfield[which(pTrees$Tag_Num == Field[j])]
  controlPts$ySource[j] <- pTrees$Yfield[which(pTrees$Tag_Num == Field[j])]
  controlPts$xTarget[j] <- TAOs$X[which(TAOs$ID == TAO[j])]
  controlPts$yTarget[j] <- TAOs$Y[which(TAOs$ID == TAO[j])]

}

# transformation choices are "affine" or "similarity"
# results for the test plot (7) are nearly identical
t <- "similarity"
if (t == "affine") {
  aft = AffineTransformation(controlPts)
  calculateParameters(aft)
  atp <- getParameters(aft)
  getResiduals(aft)
  getRMSE(aft)

  # we can use the return values from getParameters(aft) to apply the transformation using:
  # x' = ax + by + c
  # y' = dx + ey + f
  #
  allpTrees$AdjXfield <- atp["a"] * allpTrees$Xfield + atp["b"] * allpTrees$Yfield + atp["c"]
  allpTrees$AdjYfield <- atp["d"] * allpTrees$Xfield + atp["e"] * allpTrees$Yfield + atp["f"]
} else {
  sft = SimilarityTransformation(controlPts)
  calculateParameters(sft)
  stp <- getParameters(sft)
  getResiduals(sft)
  getRMSE(sft)

  # for a similarity transformation the equations are:
  # x' = ax + by + c
  # y' = ay - bx + d

  # similarity transformation
  allpTrees$AdjXfield <- stp["a"] * allpTrees$Xfield + stp["b"] * allpTrees$Yfield + stp["c"]
  allpTrees$AdjYfield <- stp["a"] * allpTrees$Yfield - stp["b"] * allpTrees$Xfield + stp["d"]
}

# vec2dtransf package as a function to apply the affine transformation but it only works with sp objects

#newLines = applyTransformation(aft, pTrees)

adjfTrees <- makeAdjustedFUSIONTrees(allpTrees, R = 0, G = 255, B = 0)
write.csv(adjfTrees, file = paste0(outputFolder, "/AdjustedFUSIONFieldLocations_Plot_", plotsWithTrees[i], "_", t, ".csv"), row.names = FALSE, quote = TRUE)

