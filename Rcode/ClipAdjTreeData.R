# code to create new tree locations based on the adjustments done by Ally and Bob
# Goal is to extract new points for these new locations and compare classification
# results to those obtained using the original segmented trees.
#
# 10/18/2023 Realized that all of the output from this code was lost when my external drive
# failed so I needed to rerun. Folder names had to be changed since they were slightly different
# in the new copy of the data. Keven and I used different strategies when renaming folders. Keven
# cleaned up folder names to remove spaces, I did not. Unfortunately, reclipping using the leaning
# trees is not a fast process!!
library(lidR)
library(tictoc)
library(fusionwrapr)

dist3d <- function(a,b,c) {
  v1 <- b - c
  v2 <- a - b
  v3 <- cross3d_prod(v1,v2)
  area <- sqrt(sum(v3*v3))/2
  d <- 2*area/sqrt(sum(v1*v1))
}

cross3d_prod <- function(v1,v2){
  v3 <- vector()
  v3[1] <- v1[2]*v2[3]-v1[3]*v2[2]
  v3[2] <- v1[3]*v2[1]-v1[1]*v2[3]
  v3[3] <- v1[1]*v2[2]-v1[2]*v2[1]
  return(v3)
}

# this file has all of the field trees that were adjusted by Ally and Bob. Not
# all of these were successfully matched to a lidar-segmented tree so there are more trees
# here that the file that has lidar metrics used to copy the segmented trees later in this code.
allTreesOriginal <- read.csv(file = "extras/AdjustedTrees_AllPlots.csv", stringsAsFactors = FALSE)

# drop any trees where the base locations differ by more than 1m
distThreshold <- 1
allTrees <- allTreesOriginal[allTreesOriginal$diff <= distThreshold, ]

# drop trees where we had a height difference of 1m or more
heightThreshold <- 1
allTrees <- allTrees[allTrees$heightdiff <= heightThreshold, ]

# 624 original trees that were adjusted
# 580 trees with adjusted base locations within 1m
# 575 trees with adjusted base locations within 1m and heights within 1m

# compute averages
allTrees$aveBaseX <- (allTrees$X.Ally + allTrees$X.Bob) / 2
allTrees$aveBaseY <- (allTrees$Y.Ally + allTrees$Y.Bob) / 2
allTrees$aveBaseElevation <- (allTrees$Elevation.Ally + allTrees$Elevation.Bob) / 2
allTrees$aveHeight <- (allTrees$Total.Height.Ally + allTrees$Total.Height.Bob) / 2

for (i in 1:nrow(allTrees)) {
  if (allTrees$Elevation.Ally[i] < 0 || allTrees$Elevation.Bob[i] < 0) {
    allTrees$aveBaseElevation[i] <- max(allTrees$Elevation.Ally[i], allTrees$Elevation.Bob[i])
  }
}
allTrees$aveTopX <- (allTrees$TopX.Ally + allTrees$TopX.Bob) / 2
allTrees$aveTopY <- (allTrees$TopY.Ally + allTrees$TopY.Bob) / 2
allTrees$topElevation <- allTrees$aveBaseElevation + allTrees$aveHeight

allTrees$aveCrownDia <- (allTrees$Max.Crown.Diameter.Ally + allTrees$Max.Crown.Diameter.Bob + allTrees$Min.Crown.Diameter.Ally + allTrees$Min.Crown.Diameter.Bob) / 4

# build a list of folders for plots...hand coded :-(
folders <- data.frame(Plot_Number = unique(allTrees$Plot_Number), Folder = "")
folders$Folder <- c(
  "H:/T3_DroneLidar/Ba/Plot7/"
  , "H:/T3_DroneLidar/Ba/Plots8_9_36_46/"
  , "H:/T3_DroneLidar/Ba/Plots8_9_36_46/"
  , "H:/T3_DroneLidar/Ba/Plots10_47/"
  , "H:/T3_DroneLidar/Az/Plots13_14_23_24_43/"
  , "H:/T3_DroneLidar/Az/Plots13_14_23_24_43/"
  , "H:/T3_DroneLidar/Aa/Plots16_31/"
  , "H:/T3_DroneLidar/Aa/Plots17_18_26_27/"
  , "H:/T3_DroneLidar/Aa/Plots17_18_26_27/"
  , "H:/T3_DroneLidar/Da/Plots20_21_22/"
  , "H:/T3_DroneLidar/Da/Plots20_21_22/"
  , "H:/T3_DroneLidar/Da/Plots20_21_22/"
  , "H:/T3_DroneLidar/Az/Plots13_14_23_24_43/"
  , "H:/T3_DroneLidar/Az/Plots13_14_23_24_43/"
  , "H:/T3_DroneLidar/Dz/Plots25_42/"
  , "H:/T3_DroneLidar/Aa/Plots17_18_26_27/"
  , "H:/T3_DroneLidar/Aa/Plots17_18_26_27/"
  , "H:/T3_DroneLidar/Aa/Plots28_29_34/"
  , "H:/T3_DroneLidar/Aa/Plots28_29_34/"
  , "H:/T3_DroneLidar/Aa/Plots16_31/"
  , "H:/T3_DroneLidar/Aa/Plots28_29_34/"
  , "H:/T3_DroneLidar/Ba/Plots8_9_36_46/"
  , "H:/T3_DroneLidar/Ba/Plot37/"
  , "H:/T3_DroneLidar/Dz/Plots25_42/"
  , "H:/T3_DroneLidar/Az/Plots13_14_23_24_43/"
  , "H:/T3_DroneLidar/Ba/Plots8_9_36_46/"
  , "H:/T3_DroneLidar/Ba/Plots10_47/"
)

# idea is to clip points using the slanted tree represented by the base and top location
# and a crown diameter. This is done using the directed distance from a line but may require
# some tricks to clip the points below the base location and above the top location.
#
# work through a list of point files and then merge the clips into a single file
#for (plot in 1:1) {
for (plot in 1:nrow(folders)) {
  tic(paste0("Clipping trees for plot ", folders$Plot_Number[plot]))
  trees <- allTrees[allTrees$Plot_Number == folders$Plot_Number[plot],]

  pointFolder <- folders$Folder[plot]

  # create output folder for new tree clips
  verifyFolder(paste0(pointFolder, "Processing/AdjustedTrees"))

  #lazFiles <- list.files(pointFolder, pattern = ".laz")

  tic(paste0("Plot ", folders$Plot_Number[plot], ":Reading LAS catalog..."))
  pts <- readLAScatalog(pointFolder, select = "*.laz")
  toc()

#  for (tree in 1:1) {
  for (tree in 1:nrow(trees)) {
    # https://stackoverflow.com/questions/35194048/using-r-how-to-calculate-the-distance-from-one-point-to-a-line
    b <- c(trees$aveBaseX[tree], trees$aveBaseY[tree], trees$aveBaseElevation[tree]) # base
    c <- c(trees$aveTopX[tree], trees$aveTopY[tree], trees$topElevation[tree]) # top

    # build bounding box for tree
    xLeft <- min(trees$aveBaseX[tree], trees$aveTopX[tree]) - trees$aveCrownDia[tree] / 2
    yBottom <- min(trees$aveBaseY[tree], trees$aveTopY[tree]) - trees$aveCrownDia[tree] / 2
    xRight <- max(trees$aveBaseX[tree], trees$aveTopX[tree]) + trees$aveCrownDia[tree] / 2
    yTop <- max(trees$aveBaseY[tree], trees$aveTopY[tree]) + trees$aveCrownDia[tree] / 2

    tic(paste0("Plot ", folders$Plot_Number[plot], " Tree ", tree, ":Clipping to rough bounding box..."))
    tpts <- clip_rectangle(pts, xLeft, yBottom, xRight, yTop)
    toc()

    # set the classification value to 1 for points within the cylinder and 0 for points outside
    # downside to this si that we lose the original classification code
    tic(paste0("Plot ", folders$Plot_Number[plot], " Tree ", tree, ":Clipping to tree..."))
    for (i in 1:tpts@header$`Number of point records`) {
      a <- c(tpts@data$X[i], tpts@data$Y[i], tpts@data$Z[i])
      d <- dist3d(a,b,c)
      if (d <= trees$aveCrownDia[tree] / 2) {
        tpts@data$Classification[i] <- 1
      }
      else {
        tpts@data$Classification[i] <- 0
      }
    }
    toc()

    # keep only class 1 points
    tpts <- filter_poi(tpts, Classification == 1)

    # reset the point classification to an integer type, not sure why it was changed
    tpts@data$Classification <- as.integer(tpts@data$Classification)

    #plot(tpts)

    # write tree points
    writeLAS(tpts, paste0(pointFolder, "Processing/AdjustedTrees/", "Plot_", trees$Plot_Number[tree], "_Tree_", trees$Tag_Num[tree], ".las"))
  }
  toc()
}



# match trees on plots to their segmented tree and copy the segmented trees to a new folder and relabel
# them to match the labeling used above.
#
# NOTE: there are fewer segmented trees than clipped trees from the code above due to differences in the CSV files
# used. The file used here are the field trees that were actually matched to lidar segmented trees
#
# Use a different data file (AdjustedField_T3_Training_TreeTops_AllPlots.csv) that has the basin ID and test to see that I get the same set of trees.
#
# Not sure of the source for the crown widths in the adjusted tree files. I don't remember if
tTrees <- read.csv(file = "extras/AdjustedField_T3_Training_TreeTops_AllPlots.csv", stringsAsFactors = FALSE)

# drop any trees where the base locations differ by more than 1m
distThreshold <- 1
tTrees <- tTrees[tTrees$diff <= distThreshold, ]

# drop trees where we had a height difference of 1m or more
heightThreshold <- 1
tTrees <- tTrees[tTrees$heightdiff <= heightThreshold, ]

for (plot in 1:nrow(folders)) {
  trees <- tTrees[tTrees$Plot_Number == folders$Plot_Number[plot],]

  pointFolder <- folders$Folder[plot]

  # create output folder for new tree clips
  verifyFolder(paste0(pointFolder, "Processing/SegmentedTrees"))

  for (tree in 1:nrow(trees)) {
    file.copy(paste0(pointFolder, "Processing/Trees/TAOpts_GroundBiased/", "Trees_Clip_", sprintf("%07i", trees$BasinID[tree]), ".lda"),
              paste0(pointFolder, "Processing/SegmentedTrees/", "Plot_", trees$Plot_Number[tree], "_Tree_", trees$Tag_Num[tree], ".lda"),
              overwrite = TRUE)
  }
}



# compute the metrics for the upper portion of the crowns for the new tree clips. May also want to reduce the size of the cylinder
# This would be faster if I just use the top XY location and ignore lean
# ideally, the format of the metrics would match that in AdjustedField_T3_Training_TreeTops_AllPlots.csv so I can use the
# same model fitting code
# using the original tree file of adjusted trees to compute metrics
allTrees <- read.csv(file = "extras/AdjustedTrees_AllPlots.csv", stringsAsFactors = FALSE)

# drop any trees where the base locations differ by more than 1m
distThreshold <- 1
allTrees <- allTrees[allTrees$diff <= distThreshold, ]

# drop trees where we had a height difference of 1m or more
heightThreshold <- 1
allTrees <- allTrees[allTrees$heightdiff <= heightThreshold, ]

# compute averages
allTrees$aveBaseX <- (allTrees$X.Ally + allTrees$X.Bob) / 2
allTrees$aveBaseY <- (allTrees$Y.Ally + allTrees$Y.Bob) / 2
allTrees$aveBaseElevation <- (allTrees$Elevation.Ally + allTrees$Elevation.Bob) / 2
allTrees$aveHeight <- (allTrees$Total.Height.Ally + allTrees$Total.Height.Bob) / 2

for (i in 1:nrow(allTrees)) {
  if (allTrees$Elevation.Ally[i] < 0 || allTrees$Elevation.Bob[i] < 0) {
    allTrees$aveBaseElevation[i] <- max(allTrees$Elevation.Ally[i], allTrees$Elevation.Bob[i])
  }
}
allTrees$aveTopX <- (allTrees$TopX.Ally + allTrees$TopX.Bob) / 2
allTrees$aveTopY <- (allTrees$TopY.Ally + allTrees$TopY.Bob) / 2
allTrees$topElevation <- allTrees$aveBaseElevation + allTrees$aveHeight

allTrees$aveCrownDia <- (allTrees$Max.Crown.Diameter.Ally + allTrees$Max.Crown.Diameter.Bob + allTrees$Min.Crown.Diameter.Ally + allTrees$Min.Crown.Diameter.Bob) / 4

# reset options
resetGlobalCommandOptions()

# set default behavior for commands
setGlobalCommandOptions(runCmd = TRUE, saveCmd = FALSE, echoCmd = FALSE)

topDepth <- 3

# work through the folders and run metrics for adjusted tree clips
#for (plot in 1:2) {
for (plot in 1:nrow(folders)) {
    tic(paste0("Computing metrics for plot ", folders$Plot_Number[plot]))
  pointFolder <- folders$Folder[plot]

  # create output folder for new tree clips
  outFile <- paste0(pointFolder, "Processing/AdjustedTrees/metrics.csv")

  CloudMetrics(paste0(pointFolder, "Processing/AdjustedTrees/", "Plot_", folders$Plot_Number[plot], "*.las")
               , outFile
               , new = TRUE
               , rid = TRUE
  )

  # read the metrics to get the high point
  # read the metrics for the non-normalized points and get the highest elevation
  m <- read.csv(outFile, stringsAsFactors = FALSE)

  # compute the elevation for the base of the upper portion
  m$SampleBaseElev <- m$Elev.maximum - topDepth

  # build commands to clip to upper portion of each TAO
  for (i in 1:nrow(m)) {
    ClipData(m$DataFile[i]
             , paste0(pointFolder, "Processing/AdjustedTrees/TreeTops/", m$FileTitle[i], ".las")
             , zmin = m$SampleBaseElev[i]
             , zmax = m$Elev.maximum[i]
    )
  }

  # use the lower elevation value to normalize the upper crown points using ClipData and the /biaselev:minelevation option
  # bias is added to point height so it needs to be negative
  # zmin and zmax are evaluated after bias adjustment so zmin=0 and zmax=topDepth
  for (i in 1:nrow(m)) {
    ClipData(m$DataFile[i]
             , paste0(pointFolder, "Processing/AdjustedTrees/TreeTops_normalized/", m$FileTitle[i], ".las")
             , zmin = 0
             , zmax = topDepth
             , biaselev = -m$SampleBaseElev[i]
    )
  }

  # compute metrics for tree tops...use rid=TRUE to parse tree number from end of point file name
  CloudMetrics(paste0(pointFolder, "Processing/AdjustedTrees/TreeTops_normalized/", "Plot_", folders$Plot_Number[plot], "*.las")
               , paste0(pointFolder, "Processing/AdjustedTrees/TreeTops_normalized", "/TreeTops_normalized_metrics.csv")
               , new = TRUE
               , rid = TRUE
  )

  m <- read.csv(paste0(pointFolder, "Processing/AdjustedTrees/TreeTops_normalized", "/TreeTops_normalized_metrics.csv"), stringsAsFactors = FALSE)
  m$Plot_Number <- folders$Plot_Number[plot]

  # merge metrics and field data
  merged <- merge(allTrees, m, by.x = c("Plot_Number", "Tag_Num"), by.y = c("Plot_Number", "Identifier"))

  write.csv(merged, paste0(pointFolder, "Processing/AdjustedTrees/TreeTops_normalized",
                           "/Leaning_TreeTops_normalized_metrics_Plot",
                           sprintf("%02i", folders$Plot_Number[plot]), ".csv"), row.names = FALSE)

  if (plot == 1) {
    allMerged <- merged
  }
  else {
    allMerged <- rbind(allMerged, merged)
  }
}

# write off combined file
write.csv(allMerged, paste0("H:/T3_DroneLidar", "/Leaning_TreeTops_normalized_metrics.csv"), row.names = FALSE)






# small cylinder clips

# work through the folders, clip smaller cylinder for the tree and compute metrics

sampleRadius <- 1
topDepth <- 3

#for (plot in 1:2) {
for (plot in 1:nrow(folders)) {
  tic(paste0("Computing metrics for plot ", folders$Plot_Number[plot]))
  pointFolder <- folders$Folder[plot]

  # create output folder for new tree clips
  outFile <- paste0(pointFolder, "Processing/AdjustedTrees/metrics.csv")

  CloudMetrics(paste0(pointFolder, "Processing/AdjustedTrees/", "Plot_", folders$Plot_Number[plot], "*.las")
               , outFile
               , new = TRUE
               , rid = TRUE
  )

  # read the metrics to get the high point
  # read the metrics for the non-normalized points and get the highest elevation
  m <- read.csv(outFile, stringsAsFactors = FALSE)

  # this is the fix for 10/25/2023 problem...keeping code above to get FileTitle
  # build commands to clip to upper portion of each TAO
  for (i in 1:nrow(m)) {
    ClipData(m$DataFile[i]
             , paste0(pointFolder, "Processing/AdjustedTrees/Trees_SmallCylinder/", m$FileTitle[i], ".las")
             , minx = allTrees$aveTopX[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] - sampleRadius
             , miny = allTrees$aveTopY[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] - sampleRadius
             , maxx = allTrees$aveTopX[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] + sampleRadius
             , maxy = allTrees$aveTopY[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] + sampleRadius
             , shape = 1
    )
  }

  # create output folder for new tree clips
  outFile <- paste0(pointFolder, "Processing/AdjustedTrees/Trees_SmallCylinder/SmallCylindermetrics.csv")

  CloudMetrics(paste0(pointFolder, "Processing/AdjustedTrees/Trees_SmallCylinder/", "Plot_", folders$Plot_Number[plot], "*.las")
               , outFile
               , new = TRUE
               , rid = TRUE
  )

  # read the metrics to get the high point
  # read the metrics for the non-normalized points and get the highest elevation
  m <- read.csv(outFile, stringsAsFactors = FALSE)

  # ERROR: 10/25/2023: original code used the leaning tree clip with an estimated crow width
  # to get the max elevation. If there are branches from an adjacent tree above the target
  # tree, there may be no points in the small cylinder clip so we loose the tree.
  # Logic should be using the max elevation in the 1m cylinder, not the entire tree clip.
  #
  # easy fix is to do the small cylinder clips and keep all points, run cloudmetrics to get
  # the max elevation, then use this to clip the top

  # compute the elevation for the base of the upper portion
  m$SampleBaseElev <- m$Elev.maximum - topDepth

  # build commands to clip to upper portion of each TAO
  for (i in 1:nrow(m)) {
    ClipData(m$DataFile[i]
             , paste0(pointFolder, "Processing/AdjustedTrees/TreeTops_SmallCylinder/", m$FileTitle[i], ".las")
             , zmin = m$SampleBaseElev[i]
             , zmax = m$Elev.maximum[i]
             , minx = allTrees$aveTopX[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] - sampleRadius
             , miny = allTrees$aveTopY[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] - sampleRadius
             , maxx = allTrees$aveTopX[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] + sampleRadius
             , maxy = allTrees$aveTopY[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] + sampleRadius
             , shape = 1
    )
  }

  # use the lower elevation value to normalize the upper crown points using ClipData and the /biaselev:minelevation option
  # bias is added to point height so it needs to be negative
  # zmin and zmax are evaluated after bias adjustment so zmin=0 and zmax=topDepth
  for (i in 1:nrow(m)) {
    ClipData(m$DataFile[i]
             , paste0(pointFolder, "Processing/AdjustedTrees/TreeTops_SmallCylinder_normalized/", m$FileTitle[i], ".las")
             , zmin = 0
             , zmax = topDepth
             , biaselev = -m$SampleBaseElev[i]
             , minx = allTrees$aveTopX[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] - sampleRadius
             , miny = allTrees$aveTopY[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] - sampleRadius
             , maxx = allTrees$aveTopX[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] + sampleRadius
             , maxy = allTrees$aveTopY[allTrees$Plot_Number == folders$Plot_Number[plot] & allTrees$Tag_Num == m$Identifier[i]] + sampleRadius
             , shape = 1
    )
  }

  # compute metrics for tree tops...use rid=TRUE to parse tree number from end of point file name
  CloudMetrics(paste0(pointFolder, "Processing/AdjustedTrees/TreeTops_SmallCylinder_normalized/", "Plot_", folders$Plot_Number[plot], "*.las")
               , paste0(pointFolder, "Processing/AdjustedTrees/TreeTops_SmallCylinder_normalized", "/TreeTops_SmallCylinder_normalized_metrics.csv")
               , new = TRUE
               , rid = TRUE
  )

  m <- read.csv(paste0(pointFolder, "Processing/AdjustedTrees/TreeTops_SmallCylinder_normalized", "/TreeTops_SmallCylinder_normalized_metrics.csv"), stringsAsFactors = FALSE)
  m$Plot_Number <- folders$Plot_Number[plot]

  # merge metrics and field data
  merged <- merge(allTrees, m, by.x = c("Plot_Number", "Tag_Num"), by.y = c("Plot_Number", "Identifier"))

  write.csv(merged, paste0(pointFolder, "Processing/AdjustedTrees/TreeTops_SmallCylinder_normalized",
                           "/Leaning_TreeTops_SmallCylinder_normalized_metrics_Plot",
                           sprintf("%02i", folders$Plot_Number[plot]), ".csv"), row.names = FALSE)

  if (plot == 1) {
    allMerged <- merged
  }
  else {
    allMerged <- rbind(allMerged, merged)
  }
}

# write off combined file
write.csv(allMerged, paste0("H:/T3_DroneLidar", "/Leaning_TreeTops_SmallCylinder_normalized_metrics.csv"), row.names = FALSE)




