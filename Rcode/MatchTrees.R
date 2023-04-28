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
library(readxl)
library(sf)
library(mapview)
library(terra)
library(leafem)
library(nngeo)
library(fusionwrapr)

source("Rcode/predictDBH_Height.R")

makeFUSIONTrees <- function(x, R = 0, G = 192, B = 0, statusCode = 0) {
  if (nrow(x)) {
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
  } else {
    return(invisible(NULL))
  }
}

makeAdjustedFUSIONTrees <- function(x, R = 0, G = 192, B = 0, statusCode = 0) {
  FUSIONtrees <- data.frame(
    "TreeID" = x$TreeID,
    "X" = x$PA_AdjXfield,
    "Y" = x$PA_AdjYfield,
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
psme <- dplyr::filter(combinedTrees, Species == "PSME", Anomaly1 == 0 | Anomaly1 == 2 | Anomaly1 == 6, LiDAR_visible == "Y")
tshe <- dplyr::filter(combinedTrees, Species == "TSHE", Anomaly1 == 0 | Anomaly1 == 2 | Anomaly1 == 6, LiDAR_visible == "Y")
# psme <- dplyr::filter(combinedTrees, Species == "PSME", Anomaly1 == 0, LiDAR_visible == "Y")
# tshe <- dplyr::filter(combinedTrees, Species == "TSHE", Anomaly1 == 0, LiDAR_visible == "Y")

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
  "D:/T3_DroneLidar/Ba/Plot 7"
  , "D:/T3_DroneLidar/Ba/Plots 8,9,36,46"
  , "D:/T3_DroneLidar/Ba/Plots 8,9,36,46"
  , "D:/T3_DroneLidar/Ba/Plots 10,47"
  , "D:/T3_DroneLidar/Az/Plots 13,14,23,24,43"
  , "D:/T3_DroneLidar/Az/Plots 13,14,23,24,43"
  , "D:/T3_DroneLidar/Aa/Plots 16,31"
  , "D:/T3_DroneLidar/Aa/Plots 17,18,26,27"
  , "D:/T3_DroneLidar/Aa/Plots 17,18,26,27"
#  , "D:/T3_DroneLidar/Da/Plot 19"
  , "D:/T3_DroneLidar/Da/Plots 20,21,22"
  , "D:/T3_DroneLidar/Da/Plots 20,21,22"
  , "D:/T3_DroneLidar/Da/Plots 20,21,22"
  , "D:/T3_DroneLidar/Az/Plots 13,14,23,24,43"
  , "D:/T3_DroneLidar/Az/Plots 13,14,23,24,43"
  , "D:/T3_DroneLidar/Dz/Plots 25,42"
  , "D:/T3_DroneLidar/Aa/Plots 17,18,26,27"
  , "D:/T3_DroneLidar/Aa/Plots 17,18,26,27"
  , "D:/T3_DroneLidar/Aa/Plots 28,29,34"
  , "D:/T3_DroneLidar/Aa/Plots 28,29,34"
  , "D:/T3_DroneLidar/Aa/Plots 16,31"
  , "D:/T3_DroneLidar/Aa/Plots 28,29,34"
  , "D:/T3_DroneLidar/Ba/Plots 8,9,36,46"
  , "D:/T3_DroneLidar/Ba/Plot 37"
  , "D:/T3_DroneLidar/Dz/Plots 25,42"
  , "D:/T3_DroneLidar/Az/Plots 13,14,23,24,43"
  , "D:/T3_DroneLidar/Ba/Plots 8,9,36,46"
  , "D:/T3_DroneLidar/Ba/Plots 10,47"
)

outputFolder <- "G:/R_Stuff/ONRCDroneLidar/Adjustments"


# Create FUSION tree files for matching -----------------------------------
for (i in 1:length(plotsWithTreesAndLidar)) {
#   i = 12

  # create FUSION tree files for the TAO objects that will be used for matching
  pTrees <- candidates[candidates$Plot_Number == plotsWithTreesAndLidar[i],]
  fTrees <- makeFUSIONTrees(pTrees[pTrees$Anomaly1 == 0, ], R = 0, G = 255, B = 0)
  fTrees <- rbind(fTrees, makeFUSIONTrees(pTrees[pTrees$Anomaly1 == 2, ], R = 255, G = 0, B = 0)) # leaning
  fTrees <- rbind(fTrees, makeFUSIONTrees(pTrees[pTrees$Anomaly1 == 6, ], R = 0, G = 0, B = 255)) # bear damage

  # get all trees for plot...needed to apply the adjustment
  allpTrees <- combinedTrees[combinedTrees$Plot_Number == plotsWithTreesAndLidar[i],]

  # make FUSION trees for all trees
  fAllTrees <- makeFUSIONTrees(combinedTrees[combinedTrees$Plot_Number == plotsWithTreesAndLidar[i],], R = 128, G = 128, B = 128)
  write.csv(fAllTrees, file = paste0(outputFolder, "/FUSIONFieldLocations_AllTrees_Plot_", plotsWithTreesAndLidar[i], ".csv"), row.names = FALSE, quote = TRUE)

  write.csv(fTrees, file = paste0(outputFolder, "/FUSIONFieldLocations_Plot_", plotsWithTreesAndLidar[i], ".csv"), row.names = FALSE, quote = TRUE)
}

# need to go through all the plots and match TAO identifiers with Tag_numbers


# Process "matched" trees to compute adjustment parameters ----------------
# read match table
matchFilename <- paste0(outputFolder, "/MatchTrees.xlsx")
sheetname <- "Sheet1"
matchTrees <- read_excel(matchFilename, sheet = sheetname)

# set the transformation type
tType <- "Affine"
#tType <- "Similarity"

# go through the plots, do the affine transformations, and adjust locations for all trees on the plots
for (i in 1:length(plotsWithTreesAndLidar)) {
  #   i = 1

  # get matches for plot
  matches <- matchTrees[matchTrees$Plot_Num == plotsWithTreesAndLidar[i],]

  # read TAOs
  TAOs <- read.csv(file = paste0(plotFolders[i], "/Processing/Trees/", "FUSIONtrees.csv"), stringsAsFactors = FALSE)

  # get field trees for plot
  allpTrees <- combinedTrees[combinedTrees$Plot_Number == plotsWithTreesAndLidar[i],]

  # set up control points
  controlPts <- data.frame("xSource" = rep(0, nrow(matches)), "ySource" = rep(0, nrow(matches)), "xTarget" = rep(0, nrow(matches)), "yTarget" = rep(0, nrow(matches)))
  for (j in 1:nrow(matches)) {
    controlPts$xSource[j] <- allpTrees$Xfield[which(allpTrees$Tag_Num == matches$Tag_Num[j])]
    controlPts$ySource[j] <- allpTrees$Yfield[which(allpTrees$Tag_Num == matches$Tag_Num[j])]
    controlPts$xTarget[j] <- TAOs$X[which(TAOs$ID == matches$TAO_ID[j])]
    controlPts$yTarget[j] <- TAOs$Y[which(TAOs$ID == matches$TAO_ID[j])]

  }

  # join TAO ID for trees used for matching
  allpTrees <- left_join(allpTrees, matches, by = c("Plot_Number" = "Plot_Num", "Tag_Num" = "Tag_Num"), keep = FALSE)

  if (tolower(tType) == "affine") {
    aft = AffineTransformation(controlPts)
    calculateParameters(aft)
    atp <- getParameters(aft)
    p <- data.frame(getResiduals(aft))
    p$Tag_Num <- matches$Tag_Num
    p$TAO_ID <- matches$TAO_ID
    p$Plot_Num <- plotsWithTreesAndLidar[i]
    p$Plot_RMSE <- getRMSE(aft)

    # print(plotsWithTreesAndLidar[i])
    # print(getResiduals(aft))
    # print(getRMSE(aft))

    # we can use the return values from getParameters(aft) to apply the transformation using:
    # x' = ax + by + c
    # y' = dx + ey + f
    #
    allpTrees$PA_AdjXfield <- atp["a"] * allpTrees$Xfield + atp["b"] * allpTrees$Yfield + atp["c"]
    allpTrees$PA_AdjYfield <- atp["d"] * allpTrees$Xfield + atp["e"] * allpTrees$Yfield + atp["f"]

    p$Plot_AveShift <- mean(sqrt((allpTrees$AdjXfield - allpTrees$Xfield) ^ 2 + (allpTrees$AdjYfield - allpTrees$Yfield) ^ 2))

    if (i == 1) {
      results <- p
      mergedPlotTrees <- allpTrees
    } else {
      results <- rbind(results, p)
      mergedPlotTrees <- rbind(mergedPlotTrees, allpTrees)
    }
  } else {
    sft = SimilarityTransformation(controlPts)
    calculateParameters(sft)
    stp <- getParameters(sft)
    p <- data.frame(getResiduals(sft))
    p$Tag_Num <- matches$Tag_Num
    p$TAO_ID <- matches$TAO_ID
    p$Plot_Num <- plotsWithTreesAndLidar[i]
    p$Plot_RMSE <- getRMSE(sft)

    getResiduals(sft)
    getRMSE(sft)

    # for a similarity transformation the equations are:
    # x' = ax + by + c
    # y' = ay - bx + d

    # similarity transformation
    allpTrees$PA_AdjXfield <- stp["a"] * allpTrees$Xfield + stp["b"] * allpTrees$Yfield + stp["c"]
    allpTrees$PA_AdjYfield <- stp["a"] * allpTrees$Yfield - stp["b"] * allpTrees$Xfield + stp["d"]

    p$Plot_AveShift <- mean(sqrt((allpTrees$PA_AdjXfield - allpTrees$Xfield) ^ 2 + (allpTrees$PA_AdjYfield - allpTrees$Yfield) ^ 2))

    if (i == 1) {
      results <- p
      mergedPlotTrees <- allpTrees
    } else {
      results <- rbind(results, p)
      mergedPlotTrees <- rbind(mergedPlotTrees, allpTrees)
    }
  }

  write.csv(allpTrees, file = paste0(outputFolder, "/AdjustedPLOTFieldLocations_Plot_", plotsWithTreesAndLidar[i], "_", tType, ".csv"), row.names = FALSE, quote = TRUE)

  adjfTrees <- makeAdjustedFUSIONTrees(allpTrees, R = 0, G = 255, B = 0)
  write.csv(adjfTrees, file = paste0(outputFolder, "/AdjustedFUSIONFieldLocations_Plot_", plotsWithTreesAndLidar[i], "_", tType, ".csv"), row.names = FALSE, quote = TRUE)
}

#results
write.csv(results, file = paste0(outputFolder, "/", tType, "Results.csv"), row.names = FALSE, quote = TRUE)

# save merged plot trees
write.csv(mergedPlotTrees, file = paste0(outputFolder, "/", tType, "MergedPlotTrees.csv"), row.names = FALSE, quote = TRUE)

# Compare average adjustments by plot ------------------------------------

# compare plot of location shifts with the plots in TreeRelocation.Rmd that show shifts by Ally and Bob. Very similar pattern relative to plots
# and very similar magnitudes for the shifts
results$Plot_Num <- as.integer(results$Plot_Num)
plot(Plot_AveShift ~ Plot_Num, data = results
        , main = paste0("Tree location differences\n", tType, " transformation")
        , ylim = c(0, 8)
        , xlab = "Plot identifier"
        , ylab = "Average horizontal difference (m)"
        , xaxp = c(7, 47, 40)
        )



# Add leaning trees -------------------------------------------------------

# read adjustments done by Ally and Bob
# first we just want the average for the tree base, then we want to add the top location, lean angle, and lean azimuth
leaningTrees <- read.csv(paste0("E:/Backup/R_Stuff/ONRCDroneLidar", "/AdjustedField_T3_Training_TreeTops_AllPlots.csv"), stringsAsFactors = FALSE)

# compute average adjusted location for tree base
leaningTrees$ITA_Xfield <- (leaningTrees$X.Ally + leaningTrees$X.Bob) / 2.0
leaningTrees$ITA_Yfield <- (leaningTrees$Y.Ally + leaningTrees$Y.Bob) / 2.0

# compute average adjusted location for tree top
leaningTrees$ITAL_TopXfield <- (leaningTrees$TopX.Ally + leaningTrees$TopX.Bob) / 2.0
leaningTrees$ITAL_TopYfield <- (leaningTrees$TopY.Ally + leaningTrees$TopY.Bob) / 2.0

leaningTrees$ITAL_AveHeight <- (leaningTrees$Total.Height.Ally + leaningTrees$Total.Height.Bob) / 2.0
leaningTrees$ITAL_LeanAngle <- asin(sqrt((leaningTrees$ITAL_TopXfield - leaningTrees$ITA_Xfield) ^ 2 + (leaningTrees$ITAL_TopYfield - leaningTrees$ITA_Yfield) ^ 2) / leaningTrees$ITAL_AveHeight) * 180.0 / pi

leaningTrees$ITAL_LeanAzimuth <- 360.0 - ((atan2(leaningTrees$ITA_Yfield - leaningTrees$ITAL_TopYfield,
                                               leaningTrees$ITA_Xfield - leaningTrees$ITAL_TopXfield) * 180.0 / pi) + 90.0) %% 360.0

# clean up fields...don't need them all
leaningTrees <- leaningTrees[, c(2,46,48,87:186)]

# join to mergedPlotTrees
leaningTrees <- left_join(leaningTrees, mergedPlotTrees, by = c("Plot_Number", "Tag_Num"), keep = FALSE)

write.csv(leaningTrees, file = paste0(outputFolder, "/", tType, "_CorrectedPlotTrees.csv"), row.names = FALSE, quote = TRUE)






# Automated alignment using image correlation -----------------------------------------------------
# read plot locations
plotLocationFilename <- "E:/Backup/R_Stuff/ONRCDroneLidar/plot_centers.xlsx"
sheetname <- "plot_centers"
plotLocations <- read_excel(plotLocationFilename, sheet = sheetname)
plotLocations$PlotID <- as.integer(plotLocations$PlotID)

radius <- 17.68
cellSize <- 0.5
maxTrees <- 5
useMaxTrees <- FALSE

useLidarVisibleFlag <- TRUE
treeBufferRadius <- 1.0

# set of offsets...step size should be same as CHM cell size
maxOffset <- 7
offsetX <- seq(-maxOffset, maxOffset, by = 0.5)
offsetY <- seq(-maxOffset, maxOffset, by = 0.5)

shiftResults = data.frame(matrix(nrow = 0, ncol = 5))
colnames(shiftResults) = c("Plot_Num", "shiftX", "shiftY", "corr", "htErr")

for (i in 5:5) {
#for (i in 1:length(plotsWithTreesAndLidar)) {
  # get plot location
  pl <- plotLocations[plotLocations$PlotID == plotsWithTreesAndLidar[i],]

  # read CHM for plot and crop to area that bounds min/max offsets
  CHM <- readDTM(paste0(plotFolders[i], "/Processing/CHM/CHM.dtm"), type = "terra", epsg = 26910)
  plotCHM <- crop(CHM
                  , ext(pl$X - radius + min(offsetX), pl$X + radius + max(offsetX), pl$Y - radius + min(offsetY), pl$Y + radius + max(offsetY))
                  , snap = "near")

  # get field trees for plot
  allpTrees <- combinedTrees[combinedTrees$Plot_Number == plotsWithTreesAndLidar[i],]

  if (useLidarVisibleFlag) {
    allpTrees <- allpTrees[allpTrees$LiDAR_visible == "Y", ]
  }

  # sort trees from tallest to shortest
  allpTrees <- allpTrees[order(-allpTrees$T3Ht),]

  # subset trees...if useMaxTrees = TRUE
  if (useMaxTrees) {
    t <- min(maxTrees, nrow(allpTrees))
    allpTrees <- allpTrees[1:t, ]
  }

  # resort from shortest to tallest...needed when we buffer the tree locations
  allpTrees <- allpTrees[order(allpTrees$T3Ht),]

  for (j in 1:length(offsetX)) {
    for (k in 1:length(offsetY)) {
      # get plot location
      pl <- plotLocations[plotLocations$PlotID == plotsWithTreesAndLidar[i],]

      # shift location of plot
      pl$X <- pl$X + offsetX[j]
      pl$Y <- pl$Y + offsetY[k]

      # shift location of trees
      pTrees <- allpTrees
      pTrees$Xfield <- pTrees$Xfield + offsetX[j]
      pTrees$Yfield <- pTrees$Yfield + offsetY[k]

      # create plot raster with tree heights and single cells for each tree
      trees <- st_as_sf(pTrees,
                          coords = c("Xfield", "Yfield"),
                          remove = FALSE,
                          crs = 26910)

      # buffer trees
      treesBuf <- st_buffer(trees, treeBufferRadius)

      r <- rast(vect(treesBuf)
                      , extent = ext(plotCHM)
                      , resolution = cellSize
      )
      plotr <- r

      PM <- rasterize(vect(treesBuf)
                      , r
                      , field = "T3Ht"
                      , extent = ext(plotCHM)
                      , resolution = cellSize
      )

      # # Create mask for plot...don't need this since we mask using the tree XYs
      # plot <- st_as_sf(pl, coords = c("X", "Y"),
      #                  remove = FALSE,
      #                  crs = 26910)
      #
      # plotb <- buffer(vect(plot), radius, quadsegs = 15)
      #
      # mask <- rasterize(plotb
      #                   , plotr
      #                   , field = 1
      #                   , background = NA
      #                   , extent = ext(plotCHM)
      #                   , resolution = cellSize
      # )


      # Compute correlation and height error
      stack <- c(PM, mask(plotCHM, PM))

      corr <- layerCor(stack, fun = "pearson", na.rm = TRUE)
      cat(".")
      #print(paste(offsetX[j], offsetY[k], corr$pearson[1,2]))

      # compute height error
      htErr <- global((abs(PM - plotCHM) * PM * PM), "sum", na.rm = TRUE) / global(PM * PM, "sum", na.rm = TRUE)

      # save offset and correlation between CHM and stems
      shiftResults <- rbind(shiftResults, data.frame("Plot_Num" = plotsWithTreesAndLidar[i]
                                                     , "shiftX" = offsetX[j]
                                                     , "shiftY" = offsetY[k]
                                                     , "corr" = corr$pearson[1,2]
                                                     , "htErr" = htErr$sum
                                                     )
                            )
    }
    cat("\n")
  }
}

# fix row names in results...not sure why they are messed up
#rownames(shiftResults) <- seq(1:nrow(shiftResults))

# write off the results
write.csv(shiftResults, file = paste0(outputFolder, "/AutomatedAdjustmentResults.csv"), row.names = FALSE, quote = TRUE)


shiftResults <- read.csv(file = paste0(outputFolder, "/AutomatedAdjustmentResults.csv"), stringsAsFactors = FALSE)




# This code only works when we do a single plot ---------------------------

# get "best" shift...using correlation
## We also have the overall height error that could be considered but not sure how to use it.
plot(plotCHM)
bestCorr <- which.max(shiftResults$corr)

allpTrees <- combinedTrees[combinedTrees$Plot_Number == plotsWithTreesAndLidar[i],]
points(allpTrees$Xfield + shiftResults$shiftX[bestCorr], allpTrees$Yfield + shiftResults$shiftY[bestCorr])

bestHtErr <- which.min(shiftResults$sum)
points(allpTrees$Xfield + shiftResults$shiftX[bestHtErr], allpTrees$Yfield + shiftResults$shiftY[bestHtErr], col = "red", pch = 19)

# rasterize results to show correlation surface
rr <- rast(xmin = -maxOffset, ymin = -maxOffset, xmax = maxOffset, ymax = maxOffset, resolution = cellSize)
rrr <- rasterize(as.matrix(shiftResults[, 2:3]), rr, values = shiftResults[, 4])

plot(rrr)
points(shiftResults$shiftX[bestCorr], shiftResults$shiftY[bestCorr], pch=3)


# End of testing code -----------------------------------------------------




# Process full results to get best offsets --------------------------------
# go through the full results and find the best offset based on correlation and height error
overallsr = data.frame(matrix(nrow = 0, ncol = 7))
colnames(overallsr) = c("Plot_Num", "corr", "htErr", "shiftX_corr", "shiftY_corr", "shiftX_hterr", "shiftY_hterr")
for (i in 1:length(plotsWithTreesAndLidar)) {
  # subset results for the plot
  sr <- shiftResults[shiftResults$Plot_Num == plotsWithTreesAndLidar[i], ]

  # get row for highest correlation
  bestCorr <- which.max(sr$corr)

  # get row for lowest height error
  bestHtErr <- which.min(sr$sum)

  # merge to summary results
  overallsr <- rbind(overallsr, data.frame("Plot_Num" = plotsWithTreesAndLidar[i]
                                           , "corr" = sr$corr[bestCorr]
                                           , "htErr" = sr$sum[bestHtErr]
                                           , "shiftX_corr" = sr$shiftX[bestCorr]
                                           , "shiftY_corr" = sr$shiftY[bestCorr]
                                           , "shiftX_hterr" = sr$shiftX[bestHtErr]
                                           , "shiftY_hterr" = sr$shiftY[bestHtErr]
  )
  )
}
# write off results
write.csv(overallsr, file = paste0(outputFolder, "/SummaryAutomatedAdjustmentResults.csv"), row.names = FALSE, quote = TRUE)





# Create plots showing old and new tree locations -------------------------

# these are the trees that Ally and I adjusted manually. These are not all of the plot trees so comparing the
# correlation "score" between these locations and all tree locations may be problematic. However, the tree positions
# are used to mask the CHM so the correlation is only computed using the manually-adjusted trees.

leaningTrees <- read.csv(file = paste0(outputFolder, "/", "affine", "_CorrectedPlotTrees.csv"), stringsAsFactors = FALSE)
leaningTrees <- read.csv(file = paste0(outputFolder, "/", "AffineMergedPlotTrees.csv"), stringsAsFactors = FALSE)

# open destination PDF
pdf(file = paste0(outputFolder, "/AutomatedAdjustmentResults.pdf"))
for (i in 1:length(plotsWithTreesAndLidar)) {
  # subset results for the plot
  sr <- shiftResults[shiftResults$Plot_Num == plotsWithTreesAndLidar[i], ]

  # get row for highest correlation
  bestCorr <- which.max(sr$corr)

  # get row for lowest height error
  bestHtErr <- which.min(sr$sum)

  # get plot location
  pl <- plotLocations[plotLocations$PlotID == plotsWithTreesAndLidar[i],]

  # read CHM for plot and crop to area that bounds min/max offsets
  CHM <- readDTM(paste0(plotFolders[i], "/Processing/CHM/CHM.dtm"), type = "terra", epsg = 26910)
  plotCHM <- crop(CHM
                  , ext(pl$X - radius + min(offsetX), pl$X + radius + max(offsetX), pl$Y - radius + min(offsetY), pl$Y + radius + max(offsetY))
                  , snap = "near")

  # get field trees for plot
  allpTrees <- combinedTrees[combinedTrees$Plot_Number == plotsWithTreesAndLidar[i],]

  # get manually adjusted trees for plot
  lt <- leaningTrees[leaningTrees$Plot_Number == plotsWithTreesAndLidar[i],]

  plot(plotCHM, main = paste("Plot", plotsWithTreesAndLidar[i]))

  bestHtErr <- which.min(sr$sum)
  points(allpTrees$Xfield + sr$shiftX[bestHtErr], allpTrees$Yfield + sr$shiftY[bestHtErr], col = "red", pch = 19)

  bestCorr <- which.max(sr$corr)
  points(allpTrees$Xfield + sr$shiftX[bestCorr], allpTrees$Yfield + sr$shiftY[bestCorr], pch = 3)

  points(allpTrees$Xfield, allpTrees$Yfield, pch = 5)

  # add manually adjusted trees
  #points(lt$ITAL_TopXfield, lt$ITAL_TopYfield, pch = 2, col = "magenta")
  points(lt$PA_AdjXfield, lt$PA_AdjYfield, pch = 2, col = "magenta")

  # add legend for points
  legend("bottom", c("max corr", "min hterr", "original", "manual"), pch = c(3, 19, 5, 2), col = c("black", "red", "black", "magenta"), horiz = TRUE, inset = 0.01)

  # add text
  text(pl$X - radius + min(offsetX) + 2, pl$Y + radius + max(offsetY) + 1, paste("max corr =", round(sr$corr[bestCorr], 4)), adj = c(0, 0))
  text(pl$X + radius + max(offsetX) - 2, pl$Y + radius + max(offsetY) + 1, paste("min hterr =", round(sr$sum[bestHtErr], 2)), adj = c(1, 0))

  text(pl$X - radius + min(offsetX) + 2, pl$Y + radius + max(offsetY) - 1, paste("hterr =", round(sr$sum[bestCorr], 2)), adj = c(0, 0))
  text(pl$X + radius + max(offsetX) - 2, pl$Y + radius + max(offsetY) - 1, paste("corr =", round(sr$corr[bestHtErr], 4)), adj = c(1, 0))

  # add labels with values for original tree locations
  text(pl$X - radius + min(offsetX) + 2, pl$Y + radius + max(offsetY) + 3, paste("orig corr =", round(sr$corr[421], 4)), adj = c(0, 0))
  text(pl$X + radius + max(offsetX) - 2, pl$Y + radius + max(offsetY) + 3, paste("orig hterr =", round(sr$sum[421], 2)), adj = c(1, 0))

  # compute correlation and height error for manually adjusted trees
  trees <- st_as_sf(lt,
#                    coords = c("ITAL_TopXfield", "ITAL_TopYfield"),
                    coords = c("PA_AdjXfield", "PA_AdjYfield"),
                    remove = FALSE,
                    crs = 26910)

  # buffer trees
  treesBuf <- st_buffer(trees, treeBufferRadius)

  r <- rast(vect(treesBuf)
            , extent = ext(plotCHM)
            , resolution = cellSize
  )

  PM <- rasterize(vect(treesBuf)
                  , r
                  , field = "T3Ht"
                  , extent = ext(plotCHM)
                  , resolution = cellSize
  )

  # stack layers so we can compute correlation
  # masking removes all CHM area not associated with plot trees (lidar visible)
  stack <- c(PM, mask(plotCHM, PM))

  # Compute correlation and height error
  corr <- layerCor(stack, fun = "pearson", na.rm = TRUE)
  corr <- corr$pearson[1,2]

  # compute height error
  htErr <- global((abs(PM - plotCHM) * PM * PM), "sum", na.rm = TRUE) / global(PM * PM, "sum", na.rm = TRUE)
  htErr <- htErr$sum

  # add labels with values for manually adjusted tree locations
  text(pl$X - radius + min(offsetX) + 2, pl$Y + radius + max(offsetY) - 3, paste("manual corr =", round(corr, 4)), adj = c(0, 0))
  text(pl$X + radius + max(offsetX) - 2, pl$Y + radius + max(offsetY) - 3, paste("manual hterr =", round(htErr, 2)), adj = c(1, 0))
}
dev.off()



# Test new functions ------------------------------------------------------
# this code requires some variables from above
source("C:/G_Clone/R_Stuff/PlotLocatoR/Rcode/main.R")

lt <- combinedTrees[combinedTrees$Plot_Number == plotsWithTreesAndLidar[i],]
lt$Httrunc <- 4

computeCorrelationAndHeightError(
  lt[lt$LiDAR_visible == "Y", ],
  coords = c("Xfield", "Yfield"),
  htField = "T3Ht",
  plotX = pl$X,
  plotY = pl$Y,
  treeBufferSize = 2,
  crs = 26910,
  CHM = CHM,
  searchRadius = 7
)

sr <- testPlotLocations(
  lt[lt$LiDAR_visible == "Y", ],
  coords = c("Xfield", "Yfield"),
  htField = "T3Ht",
  plotX = pl$X,
  plotY = pl$Y,
  treeBufferSize = 1,
  crs = 26910,
  CHM = CHM,
  searchRadius = 7
)

# get the index of the best location
r <- findBestPlotLocation(sr)

# create a raster of the correlation error
rast <- rasterizeSearchResults(sr)

# plot the raster and add the offset for the best location
plot(rast)
points(sr$offsetX[r], sr$offsetY[r])

r <- findBestPlotLocation(sr, rule = "minheightError")
rast <- rasterizeSearchResults(sr, value = "heightError")
plot(rast)
points(sr$offsetX[r], sr$offsetY[r])

r <- findBestPlotLocation(sr, rule = "combined")
rast <- rasterizeSearchResults(sr, value = "combined")
plot(rast)
points(sr$offsetX[r], sr$offsetY[r])

# End of tests ------------------------------------------------------------






















t <- lt[, c(c("PA_AdjXfield", "PA_AdjYfield"), "T3Ht")]
coords <- c("PA_AdjXfield", "PA_AdjYfield")
t[, coords[1]] <- t[, coords[1]] - 5


plot(CHM)




treesOrig <- st_as_sf(mergedPlotTrees,
                      coords = c("Xfield", "Yfield"),
                      remove = FALSE,
                      crs = 26910)

treesAdj <- st_as_sf(mergedPlotTrees,
                      coords = c("AdjXfield", "AdjYfield"),
                      remove = FALSE,
                      crs = 26910)

mapviewOptions(fgb = FALSE)

x1 <- mapview(treesOrig
              , col = "red"
              , col.regions = "red"
              , cex = 2
              , layer.name = "Plot trees"
)
x2 <- mapview(treesAdj
              # x2 <- mapview(trees_lidar
              , col = "black"
              , col.regions = "green"
              , cex = 3
              , layer.name = "Lidar trees"
)
x <- x1 + x2

# create line segments connecting match trees
ml <- st_connect(treesOrig, treesAdj, ids = c(1:nrow(treesOrig)))

x <- addFeatures(x
                 , data = st_transform(ml, 4326)
)

x





# testing code for a single plot



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

