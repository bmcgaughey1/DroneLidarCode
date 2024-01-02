# processing for 2021 drone lidar data
# these data have a different folder structure than the data from 2022

library(fusionwrapr)
library(sf)
library(raster)

renameCheck <- function(from = "", to = "") {
  if (file.rename(from, to) == FALSE)
    cat("Move failed for: ", from, "\n")
}

# manually created a directory list for projects
# The following command will get you a list of all folder names. This can then be edited to
# remove the subfolder names under each project. Somewhat tedious but easier than writing code
# to do the same thing (or at least more straight forward).
# dir /s /a:d /b
#
# had to correct folder names for two folders from what Chris delivered
# no 0.5m DSMs for Willamette LTEP
# one DSM file name in riparian files (reach 2) was missing "m" in 0.5m tif image

# projection for for UTM10...this is used for several outputs
prjFile <- "H:/T3_DroneLidar/UTM10.prj"

# read in the list of project folders
dirList <- "H:/T3_DroneLidar/dirlist.txt"
dirs <- read.csv2(dirList, header = FALSE)

# fix backslashes
dirs <- lapply(dirs[,1], function(x) {gsub("\\\\", "/", x)})

# create subfolders under each project area. The actual processing code will ensure that these folders exist
# and create them if not. However, I liked the idea of having the folder structure in place before starting the
# processing. It makes it a little easier to see if things worked by simply looking for files in each folder.
for (i in 1:length(dirs)) {
#for (i in 1:2) {
  verifyFolder(paste0(dirs[i], "/Processing/Trees/SmallCylinderPts"))
  verifyFolder(paste0(dirs[i], "/Processing/Trees/SmallCylinderPts_GroundBiased"))
  verifyFolder(paste0(dirs[i], "/Processing/Trees/SmallCylinderPts_normalized"))
}

# we should have clean data ready for further processing...

# loop through folders and do processing. This code is copied from the processing script from the 2021 data with
# just a few modifications.
#
# this loop should start at 1 unless processing was interrupted by a reboot.
for (i in 1:length(dirs)) {
#for (i in 1:1) {
  # set up folder info
  dataFolder <- dirs[i]
  groundFileSpec <- paste0(dirs[i], "/ground/ground.dtm")

  outputFolder <- paste0(dirs[i], "/Processing")

  # *****************************************************************************
  # *****************************************************************************
  # clip small cylinders for each TAO and compute metrics
  # *****************************************************************************
  # *****************************************************************************
  # do the treetops...upper portion of crown

  # reset options
  resetGlobalCommandOptions()

  batchFile <- paste0(outputFolder, "/DoSmallCylinders.bat")

  # set default behavior for commands
  setGlobalCommandOptions(runCmd = FALSE, saveCmd = TRUE, echoCmd = FALSE, cmdFile = batchFile)

  # make sure we have the folder for the batch file
  verifyFolder(dirname(batchFile))

  # write comment...omit the blank line before the comment
  addToCommandFile(paste0("Processing for: ", dataFolder), addLine = FALSE, cmdClear = TRUE)

  # set the log file and clear it
  useLogFile(paste0(outputFolder, "/ProcessingSmallCylinders.log"), logClear = TRUE)

  # add comment
  addToCommandFile("Start of processing commands")

  topDepth <- 3
  sampleRadius <- 1

  # read the metrics for the non-normalized points and get the highest elevation
  m <- read.csv(paste0(outputFolder, "/TAO_metrics.csv"))
  h <- read.csv(paste0(outputFolder, "/TAO_metrics_highpts.csv"))

  # fix drive letters
  m$DataFile <- sub("E:", "H:", m$DataFile)
  h$DataFile <- sub("E:", "H:", h$DataFile)

  # compute the elevation for the base of the upper portion
  m$SampleBaseElev <- m$Elev.maximum - topDepth

  # build commands to clip small cylinder
  for (j in 1:nrow(h)) {
    ClipData(h$DataFile[j]
             , paste0(outputFolder, "/Trees/SmallCylinderPts/", basename(h$DataFile[j]))
             , h$High.point.X[j] - sampleRadius
             , h$High.point.Y[j] - sampleRadius
             , h$High.point.X[j] + sampleRadius
             , h$High.point.Y[j] + sampleRadius
             , shape = 1
    )
  }

  # use the lower elevation value to normalize the upper crown points using ClipData and the /biaselev:minelevation option
  # bias is added to point height so it needs to be negative
  # zmin and zmax are evaluated after bias adjustment so zmin=0 and zmax=topDepth
  #
  # ***** original code produced files with ".lda.lda" extension. I fixed this 12/1/2023 but did not rerun all the clips
  # however, I did rename all the clip files and fix the CSV files created when this code waas first run.
  for (j in 1:nrow(h)) {
    ClipData(h$DataFile[j]
             , paste0(outputFolder, "/Trees/SmallCylinderPts_GroundBiased/", basename(h$DataFile[j]))
             , zmin = 0
             , zmax = topDepth
             , biaselev = -m$SampleBaseElev[j]
    )
  }

  # compute metrics for tree tops...use rid=TRUE to parse tree number from end of point file name
  CloudMetrics(paste0(outputFolder, "/Trees/SmallCylinderPts_GroundBiased/", "*.lda")
               , paste0(outputFolder, "/SmallCylinderPts_GroundBiased_metrics.csv")
               , new = TRUE
               , rid = TRUE
  )

  useLogFile("")

  # run the batch file
  runCommandFile()

  # # reset options...FUSION commands below will run directly
  # resetGlobalCommandOptions()
  #
  # # *****************************************************************************
  # # *****************************************************************************
  # # merge information from segmentation, metrics, and actual highpoint XY
  # # *****************************************************************************
  # # *****************************************************************************
  #
  # # use the /highpoint option with cloudmetrics to get the XY of the highest point in a TAO or treetop
  # treeHighPtFile <- paste0(outputFolder, "/TAO_metrics_highpts.csv")
  # thp <- read.csv(treeHighPtFile)
  #
  # # get surface values (ground elevations)
  # thp <- GetSurfaceValues(thp
  #                         , xLabel = "High.point.X"
  #                         , yLabel = "High.point.Y"
  #                         , idLabel = "GroundElev"
  #                         , surfaceFile = groundFileSpec
  # )
  #
  # # *****************************************************************************
  # # use the ground elevation to bias the points for the TAOs
  # # *****************************************************************************
  # # ***** this could be better placed in the processing order but we need the ground elevation under the high point
  # files <- Sys.glob(paste0(outputFolder, "/Trees/TAOpts/*.lda"))
  # for (j in 1:length(files)) {
  #   ClipData(thp$DataFile[j]
  #            , paste0(outputFolder, "/Trees/TAOpts_GroundBiased/", basename(thp$DataFile[j]))
  #            , biaselev = -thp$GroundElev[j]
  #            , class = "~2"
  #   )
  # }
  #
  # # no height thresholds for ground-normalized points...use rid=TRUE to parse tree number from end of point file name
  # CloudMetrics(paste0(outputFolder, "/Trees/TAOpts_GroundBiased/*.lda")
  #              , paste0(outputFolder, "/TAO_GroundBiased_metrics.csv")
  #              , new = TRUE
  #              , rid = TRUE
  #              , pa = TRUE
  # )
  #
  # # match TAOs from the segmentation with the metrics...need to do this for normalized
  # # and non-normalized trees
  # treeInfoFile <- paste0(outputFolder, "/Trees/trees_Polygons.csv")
  # t <- read.csv(treeInfoFile)
  # m <- read.csv(paste0(outputFolder, "/TreeTop_metrics.csv"))
  #
  # merged <- merge(m, t, by.x = "Identifier", by.y = "BasinID")
  # merged <- merge(merged, thp, by = "Identifier")
  # write.csv(merged, paste0(outputFolder, "/TreeTop_metrics_merged.csv"), row.names = FALSE)
  #
  # m <- read.csv(paste0(outputFolder, "/TreeTops_normalized_metrics.csv"))
  #
  # merged <- merge(m, t, by.x = "Identifier", by.y = "BasinID")
  # merged <- merge(merged, thp, by = "Identifier")
  # write.csv(merged, paste0(outputFolder, "/TreeTops_normalized_metrics_merged.csv"), row.names = FALSE)
  #
  # # do the full tree metrics
  # # ***** these are for the TAOs normalized using terrain...not simple bias for ground under high point
  # m <- read.csv(paste0(outputFolder, "/TAO_normalized_metrics.csv"))
  #
  # merged <- merge(m, t, by.x = "Identifier", by.y = "BasinID")
  # merged <- merge(merged, thp, by = "Identifier")
  # write.csv(merged, paste0(outputFolder, "/TAO_normalized_metrics_merged.csv"), row.names = FALSE)
  #
  # m <- read.csv(paste0(outputFolder, "/TAO_metrics.csv"))
  #
  # merged <- merge(m, t, by.x = "Identifier", by.y = "BasinID")
  # merged <- merge(merged, thp, by = "Identifier")
  # write.csv(merged, paste0(outputFolder, "/TAO_metrics_merged.csv"), row.names = FALSE)
  #
  # # *****************************************************************************
  # # *****************************************************************************
  # # compute crown base height using the individual percentile data (P1,P2,P3,...)
  # # *****************************************************************************
  # # *****************************************************************************
  # # read the percentile data
  # PercentileData <- read.csv(paste0(outputFolder, "/TAO_GroundBiased_metrics_percentile.csv"), stringsAsFactors = FALSE)
  #
  # # Normalize the percentile heights using the 99th percentile. Output has the object identifier (extra column at beginning).
  # # percentile heights start in column 4 and P99 is column 103
  # # normalized in this context means divided by the P99 value...not normalized relative to ground elevation
  # #
  # # Basic idea is to look at the "plot" of normalized percentile heights (relative to P99) and find the
  # # segment with the steepest slope. The base height is then set to the height at the upper end of this segment.
  # #
  # # I found a similar method in the literature but don't have the reference handy. The method below seems to work
  # # pretty well provided the TAOs are actual trees. When the TAOs are only partial trees or several trees, the
  # # result isn't as good (but still not too bad).
  # NormalizedPercentileData <- PercentileData[, 4:103] / PercentileData[, 103]
  # NormalizedPercentileData$Identifier <- PercentileData$Identifier
  # NormalizedPercentileData$Plot <- PercentileData$DataFile
  # NormalizedPercentileData$Label <- basename(PercentileData$DataFile)
  # NormalizedPercentileData$cbh <- 0.0
  #
  # x <- c(1:99)
  # for (k in 1:nrow(PercentileData)) {
  #   slopes <- vector()
  #   # compute slopes
  #   for (j in 2:99) {
  #     x1 <- x[j - 1]
  #     x2 <- x[j]
  #     y1 <- NormalizedPercentileData[k, j -1]
  #     y2 <- NormalizedPercentileData[k, j]
  #     slope_i <- (y2-y1)/(x2-x1)
  #     slopes <- append(slopes, slope_i)
  #   }
  #
  #   # get max slope
  #   maxSlopeIndex <- which.max(slopes)
  #   cbhIndex <- maxSlopeIndex + 1
  #   NormalizedPercentileData$cbh[k] <- PercentileData[k, cbhIndex + 4]
  # }
  #
  # # cbh for trees is saved with the NormalizedPercentileData
  # # I should probably save this to a separate file...
  #
  # # *****************************************************************************
  # # *****************************************************************************
  # # work with crown polygons to get centroid and crown dimensions...
  # # this gives us a polygon area to use to compute a crown diameter.
  # # The centroid of the crown polygons provide an alternate location for the
  # # trees. However, I found that the actual highpoint location from the
  # # segmentation was a better match for field data.
  # # *****************************************************************************
  # # *****************************************************************************
  # library(sf)
  # treePolyFile <- paste0(outputFolder, "/Trees/trees_Polygons.shp")
  #
  # # read crown polygons
  # treePolys <- st_read(treePolyFile)
  #
  # # this prevents a warning
  # st_agr(treePolys) <- "constant"
  #
  # # compute centroids and get them as simple table
  # centroids <- st_coordinates(st_centroid(treePolys))
  #
  # # rename columns
  # colnames(centroids) <- c("centroidX", "centroidY")
  #
  # # add centroids to polygon attributes
  # treePolys <- cbind(treePolys, centroids)
  #
  # # merge with high point data including ground elevation under high XY
  # treePolys <- merge(treePolys
  #                    , thp[, c("Identifier", "High.point.X", "High.point.Y", "High.point.elevation", "GroundElev")]
  #                    , by.x = "BasinID"
  #                    , by.y = "Identifier")
  #
  # # merge in cbh
  # treePolys <- merge(treePolys
  #                    , NormalizedPercentileData[, c("Identifier", "cbh")]
  #                    , by.x = "BasinID"
  #                    , by.y = "Identifier")
  #
  # # compute crown diameter using polygon area
  # treePolys$CrownDiaByArea <- sqrt(treePolys$PolyArea / pi) * 2.0
  #
  # # write geopackage...preserves column names that are too long for shapefiles
  # st_write(treePolys, paste0(outputFolder, "/Trees/trees_Polygons.gpkg"), append = FALSE)
  #
  # # build new data frame for FUSION tree objects...allows use to display tree models in the point cloud.
  # # FUSION tree objects:
  # # ID, X, Y, Z, ht, cbh, min crown dia, max crown dia, rotation, R, G, B
  # #
  # # options for XY: high point, centroid of crown polygon, detected stem location
  # # options for Z: 0.0, ground elevation under XY point
  # # options for crown diameters: diameter of circle with same area as crown polygon (rotation=0)
  # #       major/minor axis of ellipse fit to crown polygon (rotation=computed)
  # #       W-E and S-N using crown polygon  (rotation=0)
  # #
  # # high point XY seems to align better with stem hits
  # tp <- st_drop_geometry(treePolys)
  # fTrees <- data.frame(ID = tp$BasinID
  #                      , X = tp$High.point.X
  #                      , Y = tp$High.point.Y
  #                      #                     , X = tp$centroidX
  #                      #                     , Y = tp$centroidY
  #                      , Z = 0.0
  #                      , ht = tp$High.point.elevation - tp$GroundElev
  #                      , cbh = tp$cbh
  #                      , minDia = tp$CrownDiaByArea
  #                      , maxDia = tp$CrownDiaByArea
  #                      , rotation = 0.0
  #                      , R = 255
  #                      , G = 127
  #                      , B = 0)
  # write.csv(fTrees, paste0(outputFolder, "/Trees/FUSIONtrees.csv"), row.names = FALSE)
}

