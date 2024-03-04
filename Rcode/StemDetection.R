# stem detection using DBSCAN
#
library(dbscan)
library(fusionwrapr)
library(rgl)
library(lidR)

source(file.path("predictDBH_Height.R"))

# Overall, this approach isn't very satisfactory. the "stem detection" works well for some examples
# but not for all (or even ~50%). It is challenging to set a search radius for candidate stem points
# along with a height range. This is in part due to points close to the ground and to dead branch points
# on the target and adjacent trees.
#
# I modified the logic to use the computed canopy base height as the max height for stem points. This helps
# for the cases I tested.
#
# There is a dilemma with the logic. You want to be able to find single stems accurately but you also want
# to detect cases where there are actually multiple stems in the point clip for each TAO. You also want to
# be able to handle leaning trees (tree top and base are offset). If you use a radius tied to the predicted
# stem diameter, you get food detection of single trees but miss multiple tree clips. If the tree has too much
# lean, you miss it as well. If you use cone clipping, things are a little better. If you use all points,
# you have to do something to eliminate near-ground points. I think I have the TreeSeg call dropping class 2
# points so the ones left are low vegetation.

# function to implement cone clipping
# basic idea is to specify the XYZ for the cone point and then the slope of the sides (or angle of the cone).
# putting the point below the ground simulates a conic frustum.
# *************
# I revised this function in the Baseheight.R. Corrected problems and added an option to set the base diameter
# directly. This version can do this but user has to modify the coneZ to get the same effect.
coneClip <- function(
  pts,
  coneX,
  coneY,
  coneZ,
  angle = 30,
  inside = TRUE,
  xLabel = "x",
  yLabel = "y",
  zLabel = "z"
) {
  ipts <- pts[, c(xLabel, yLabel, zLabel)]
  colnames(ipts) <- c("x", "y", "z")

  # compute horizontal nd vertical distance from apex XY to each point
  ipts$hdist <- sqrt((ipts$x - coneX)^2 + (ipts$y - coneY)^2)
  ipts$vdist <- ipts$z - coneZ

  # precompute some things
  tanAngle <- tan(angle / 2 * pi / 180)

  # do the clip
  if (inside) {
    cpts <- ipts[which(ipts$hdist <= (tanAngle * ipts$vdist)), ]
  } else {
    cpts <- ipts[which(ipts$hdist > (tanAngle * ipts$vdist)), ]
  }

  return(cpts)
}





# read tree clip into data frame
#treeFile <- "H:/T3_DroneLidar/Ba/Plot 37/Processing/Trees/TAOpts_GroundBiased/trees_Clip_0000035.lda"
#treeFile <- "H:/T3_DroneLidar/Ba/Plot 37/Processing/Trees/TAOpts_GroundBiased/trees_Clip_0000065.lda"
#treeFile <- "H:/T3_DroneLidar/Ba/Plot 37/Processing/Trees/TAOpts/trees_Clip_0000065.lda"








# *****************************************************************************
# *****************************************************************************
# compute crown base height using the individual percentile data (P1,P2,P3,...)
# *****************************************************************************
# *****************************************************************************
# read the percentile data
outputFolder <- "H:/T3_DroneLidar/Ba/Plot37/Processing"

do3DPlot <- TRUE

PercentileData <- read.csv(paste0(outputFolder, "/TAO_GroundBiased_metrics_percentile.csv"), stringsAsFactors = FALSE)

# Normalize the percentile heights using the 99th percentile. Output has the object identifier (extra column at beginning).
# percentile heights start in column 4 and P99 is column 103
# normalized in this context means divided by the P99 value...not normalized relative to ground elevation
NormalizedPercentileData <- PercentileData[, 4:103] / PercentileData[, 103]
NormalizedPercentileData$Identifier <- PercentileData$Identifier
NormalizedPercentileData$Plot <- PercentileData$DataFile
NormalizedPercentileData$Label <- basename(PercentileData$DataFile)
NormalizedPercentileData$cbh <- 0.0

x <- c(1:99)
for (i in 1:nrow(PercentileData)) {
  slopes <- vector()
  # compute slopes
  for (j in 2:99) {
    x1 <- x[j - 1]
    x2 <- x[j]
    y1 <- NormalizedPercentileData[i, j -1]
    y2 <- NormalizedPercentileData[i, j]
    slope_i <- (y2-y1)/(x2-x1)
    slopes <- append(slopes, slope_i)
  }

  # get max slope
  maxSlopeIndex <- which.max(slopes)
  cbhIndex <- maxSlopeIndex + 1
  NormalizedPercentileData$cbh[i] <- PercentileData[i, cbhIndex + 4]
}

CBHData <- NormalizedPercentileData[, 101:104]

# write data with CBH
write.csv(CBHData, paste0(outputFolder, "/TAO_GroundBiased_CBH.csv"), row.names = FALSE)

# look at a single tree...testing
treeID <- 105
treeFile <- NormalizedPercentileData$Plot[NormalizedPercentileData$Identifier == treeID]

# replace drive letter
substr(treeFile, 1, 2) <- "H:"



pts <- readLDA(treeFile, epsg = 26910)
#las <- readLDA(treeFile, type = "LAS", LASTemplate = "H:/T3_DroneLidar/Ba/Plot 37/Ba-plot-37_003.laz")
#plot(las)

# get the highest and lowest points
# these are not really the min and max for the tree as they include points close to the ground but away from the stem.
# With the bias adjusted heights, the point below the high XY should be at 0 so we may be better off just using a low
# point height of 0
highPt <- pts[which(pts$z == max(pts$z)), ]
highPtMean <- mean(pts$z)
lowPt <- pts[which(pts$z == min(pts$z)), ]

# testing cone clipping
# we are using the ground biased points so the lowest point may be far from the tree base. Bias is done using
# the ground elevation under the high XY. This means that the small opening of the cone will be in different
# places depending on the slope. For flat areas, cone apex may be on ground but for steep areas, cone apex will
# be below ground.
clipPts <- coneClip(pts, highPt$x, highPt$y, lowPt$z - 0, angle = 15)
#plot3d(x = clipPts$x, y = clipPts$y, z = clipPts$z, xlab = "", ylab = "", zlab = "", aspect = TRUE)

# sort by height
clipPts <- clipPts[order(clipPts$z), ]

if (do3DPlot) {
  open3d()
  # #plot3d(pts$x, pts$y, pts$z)
  plot3d(clipPts$x, clipPts$y, clipPts$z, main = "", sub = "", ann = FALSE, axes = FALSE, xlab = "", ylab = "", zlab = "", col = rainbow(5000))
  # #box3d()
  # #axes3d(edges = NULL, labels = FALSE, tick = FALSE, xlab = NULL, ylab = NULL, zlab = NULL)
  aspect3d("iso")
  highlevel()  # To trigger display
}


# predict DBH based on height and use to set radius for clipping cylinder
ht <- highPt$z - lowPt$z
dbh <- predictDBH("PSME", ht)
radius <- dbh * 5 / 2 / 100
#radius <- 1.5

# minHt set to -1 gives all ground points...works with cone clip
# with other options, want the min height to be such that it drops ground points...depends on slope
# since we have not normalized the elevations using ground elevations but a single height at the XY
# of the detected trees.
#minHt <- -1
minHt <- NormalizedPercentileData$cbh[NormalizedPercentileData$Identifier == treeID] * 0.2
#maxHt <- highPt$z * 0.4
maxHt <- NormalizedPercentileData$cbh[NormalizedPercentileData$Identifier == treeID] * 0.8

# get points within radius...using these assumes that the tree base XY is pretty close to the tree top XY
pts$dist <- sqrt((highPt$x - pts$x)^2 + (highPt$y - pts$y)^2)
candidatePts <- pts[which(pts$dist <= radius), ]

# use the cone clip points...using these allows more deviation from the base to the top of the tree
# may also pick up additional stems
candidatePts <- clipPts

# add the radius check to the cone clip points...doens't remove many and they should all be close to the ground
candidatePts <- clipPts[which(clipPts$hdist <= radius), ]

# using all points gives the best chance of detecting multiple stems
candidatePts <- pts

# isolate points by height
stemPts <- candidatePts[candidatePts$z >= minHt & candidatePts$z < maxHt, ]
#stemPts <- candidatePts[((candidatePts$z - lowPt$z) >= minHt & (candidatePts$z - lowPt$z) < maxHt), ]

if (do3DPlot) {
  plot3d(stemPts$x, stemPts$y, stemPts$z, main = "", sub = "", ann = FALSE, axes = FALSE, xlab = "", ylab = "", zlab = "", col = rainbow(5000))
  aspect3d("iso")
  highlevel()  # To trigger display
}

targetPts <- cbind(stemPts$x, stemPts$y)

plot(targetPts)
res <- dbscan::dbscan(targetPts, eps = 0.2, minPts = 10)
res

#kNNdistplot(targetPts, k = 9)

# code to look for stem and compute DBH...dreaming!!
if (FALSE) {
  pts <- targetPts[res$cluster == 1, ]
  hpts <- chull(pts)
  hpts <- c(hpts, hpts[1])  # close the polygon
  plot(pts)
  lines(pts[hpts, ])

  library(splancs)
  areapl(pts[hpts, ])
  dbhCircle <- sqrt(areapl(pts[hpts, ]) / pi) * 2
}

dbscan::hullplot(targetPts, res)

plot(targetPts, col = res$cluster)
points(targetPts[res$cluster == 0, ], pch = 3, col = "grey")
dbscan::hullplot(targetPts, res)

# get some information from the dbscan results
stemCount <- max(res$cluster)
for (i in 1:stemCount) {
  if (i == 1) {
    t <- colMeans(targetPts[res$cluster == i, ])
  } else {
    t <- rbind(t, colMeans(targetPts[res$cluster == i, ]))
  }
}

points(x = t[,1], y = t[,2], pch = 8, cex = 3)
points(x = t[1], y = t[2], pch = 8, cex = 3)
t

dx <- highPt$x - t[[1]]
dy <- highPt$y - t[[2]]
plot3d(x = stemPts$x, y = stemPts$y, z = stemPts$z, xlab = "", ylab = "", zlab = "", aspect = TRUE)

summary(stemPts)





###############################################################################
# want to work through all of the lidar-detected trees for a unit and attempt
# the stem detection. For all cases, write off new data for each detected stem
# location. For single stems, pair stem XY with top XY to add lean to tree
# models. For multiple stem detections, pick the location closest to the high
# XY and generate a tree, then generate additional trees using other detected
# stem locations. Maybe go back to the full point cloud for the tree and core
# out a small sample using the detected XY to get a height (max point within
# some radius from detected XY).
#
method <- "cone"
#method <- "cylinder"
#method <- "both"
#method <- "all"

outputFolder <- "H:/T3_DroneLidar/Ba/Plot 37/Processing"

doPlot <- TRUE

# read CBH data
CBHData <- read.csv(paste0(outputFolder, "/TAO_GroundBiased_CBH.csv"), stringsAsFactors = FALSE)

trees <- data.frame(Identifier = numeric(0), TreeNum = numeric(0), baseX = numeric(0), baseY = numeric(0), topx = numeric(0), topy = numeric(0))

# loop through trees and compute tree locations
#for (i in 1:nrow(CBHData)) {
for (i in 1:40) {
#i <- 34
#{
  cat("Tree: ", i)

  # read tree data
  treeID <- CBHData$Identifier[i]
  treeFile <- CBHData$Plot[i]

  # work with point data with elevation...not going to work without modifications to code below
  treeFile <- gsub("_GroundBiased", "", treeFile)

  pts <- readLDA(treeFile, epsg = 26910)

  # points have been "normalized" by biasing elevations using the ground elevation under the high XY.
  # there may not be any ground points close to the high XY so just assume the high Z is the tree height
  # need to take mean in case there are multiple points with max Z value (unlikely but possible)
  highPt <- pts[which(pts$z == max(pts$z)), ]
  highPtx <- mean(highPt$x)
  highPty <- mean(highPt$y)
  highPtz <- mean(highPt$z)

  # compute distance form high XY
  pts$dist <- sqrt((highPtx - pts$x)^2 + (highPty - pts$y)^2)

  # get lowest point under high XY (use 1m radius)
  coreRadius <- 1
  corePts <- pts[order(pts$dist), ]
  corePts <- corePts[corePts$dist <= coreRadius, ]
  lowPt <- corePts[which(corePts$z == min(corePts$z)), ]
  lowPtx <- mean(lowPt$x)
  lowPty <- mean(lowPt$y)
  lowPtz <- mean(lowPt$z)

  # predict DBH based on height and use to set radius for clipping cylinder
  ht <- highPtz - lowPtz
  dbh <- predictDBH("PSME", ht)
  radius <- dbh * 5 / 2 / 100

  # Since we are not working with normalized heights, culling ground and low veg points is necessary.
  # However, this is not a simple process given the number of near-ground points and steep slopes
  # common in the T3 study area. The min and max heights are set above the ground and below the crown
  # base. The cone clipping function allows placement of the cone apex below ground to include more
  # points around the target location. Cone clipping is applied first, then the min/max check so we
  # should end up with a set of points that works for the tree detected from the CHM. Using the wide cone
  # angle helps to capture points from other stems but it isn't perfect since you can easily miss
  # low points in adjacent stems. the cone angle could be adjusted based on ground slope but then logic
  # is needed to determine the slope and all of the point clips omit points classified as ground. The 45
  # degree cone angle should work for all but the steepest slopes. For slopes over 100%, there is a chance
  # that low points on the high side of the clip will be kept but probably not enough to result in a false
  # cluster.
  #
  # The other approach that might be worth implementing is a voxelization of the point data and then a connected
  # components analysis. You could drop points in the lowest n voxels (low veg and tree bases) and then look
  # for vertical connections. I have seen this method in the literature but usually with data that has more consistent
  # stem hits. In our data, there are fairly large gaps in the stems so the connection logic will need to
  # be smart about how it spans gaps. You would also want to connect isolated voxels or clusters of voxels
  # and stop the stem detection when you run out of isolated voxels (base of crown?).
  #
  # The logic below was working slightly better before I modified it to work with non-normalized, non-biased
  # point clips. However, the improvement was marginal and it only worked for some trees. The code below is a
  # good example of "when it works, it works well" but it fails often. Mainly due to problems identifying the set of
  # candidate points for stems. We really need the normalized data and a better height range but there is
  # variability in the density of hits on the stem and the detection of the crown base height that create problems
  # setting height ranges for candidate points and detecting stems.
  #
  #minHt <- -1
  minHt <- CBHData$cbh[i] * 0.3 + lowPtz
  #maxHt <- highPt$z * 0.4
  maxHt <- CBHData$cbh[i] * 0.7 + lowPtz

  coneOffset <- 0

  if (method == "cone") {
    candidatePts <- coneClip(pts, highPtx, highPty, lowPtz - coneOffset, angle = 45)
  } else if (method == "cylinder") {
    # get points within radius...using these assumes that the tree base XY is pretty close to the tree top XY
    candidatePts <- pts[which(pts$dist <= radius), ]
  } else if (method == "both") {
    candidatePts <- coneClip(pts, highPtx, highPty, lowPtz - coneOffset, angle = 45)

    # get points within radius...using these assumes that the tree base XY is pretty close to the tree top XY
    candidatePts <- candidatePts[which(candidatePts$dist <= radius), ]
  } else if (method == "all") {
    candidatePts <- pts
  }

  # isolate points by height
  stemPts <- candidatePts[candidatePts$z >= minHt & candidatePts$z < maxHt, ]

  if (nrow(stemPts) > 0) {
    # do clustering using only X and Y
    targetPts <- cbind(stemPts$x, stemPts$y)

    res <- dbscan::dbscan(targetPts, eps = 0.2, minPts = 10)

    if (doPlot) {
      dbscan::hullplot(targetPts, res, main = paste("Tree", treeID), xlim = c(min(pts$x), max(pts$x)), ylim = c(min(pts$y), max(pts$y)))
    }

    # add back the Z and cluster ID
    targetPts <- cbind(targetPts, stemPts$z, res$cluster)

    # get some information from the dbscan results
    stemCount <- max(res$cluster)
    if (stemCount > 0) {
      # process "stems" and add record for each tree
      # For single tree, add cluster XY (even though it is part way up the stem) and high XY
      # For multiple trees, find the cluster XY that is closest to the high XY and add a record
      #   then add records for the other trees using the high point within 1m of the cluster XY
      for (j in 1:stemCount) {
        clusterPts <- targetPts[targetPts[, 4] == j, ]

        # sort by Z and keep the lower 3 points
        clusterPts <- clusterPts[order(clusterPts[, 3], decreasing = FALSE), ]
        clusterPts <- clusterPts[1:(min(3, nrow(clusterPts))), ]

        if (j == 1) {
          t <- colMeans(clusterPts)
        } else {
          t <- rbind(t, colMeans(clusterPts))
        }
      }

      if (stemCount == 1) {
        trees[nrow(trees) + 1, ] <- c(treeID, j, t[1], t[2], highPtx, highPty)
      } else {
        # compute distance from cluster XY to high XY
        t <- cbind(t, 0)
        t <- cbind(t, 0)
        for (j in 1:stemCount) {
          t[j,5] <- sqrt((highPtx - t[j,1]) * (highPtx - t[j,1]) + (highPty - t[j,2]) * (highPty - t[j,2]))
          t[j,6] <- i
        }

        # sort
        t[order(t[,5], decreasing=FALSE),]

        # first item is closest..add tree
        trees[nrow(trees) + 1, ] <- c(treeID, 1, t[1,1], t[1,2], highPtx, highPty)

        # add remaining trees...no high point for now
        for (j in 2:stemCount) {
          trees[nrow(trees) + 1, ] <- c(treeID, j, t[j,1], t[j,2], -1.0, -1.0)
        }
      }

      if (doPlot) {
        if (stemCount > 1) {
          points(x = t[,1], y = t[,2], pch = 8, cex = 3)
        } else {
          points(x = t[1], y = t[2], pch = 8, cex = 3)
        }
      }
    } else {
      # no clusters detected
      #
      # add entry for tree indicating no detection
      trees[nrow(trees) + 1, ] <- c(treeID, 1, -1.0, -1.0, -1.0, -1.0)
    }
  } else {
    # there were no candidate points for stem detection. this could be because the cbh algorithm failed
    # or there are no stem hits or the tree has excessive lean so the base of the stem is offset from the high XY
    #
    # add entry for tree indicating no detection
    trees[nrow(trees) + 1, ] <- c(treeID, 1, -1.0, -1.0, -1.0, -1.0)
  }
}

# write tree data
write.csv(trees, paste0(outputFolder, "/DetectedTrees.csv"), row.names = FALSE)







#
# Testing the TreeLS package. Doesn't seem to like the drone data. It may be because the density of stem
# hits is fairly low compared to TLS data. I experimented with parameters and functions for the treeMap()
# function and could get it to "find" a few trees but they weren't really trees...didn't make any sense
# as trees.
# #
library(lidR)
library(TreeLS)

file = "H:/R_stuff/ONRCDroneLidar/PlotPointClips/Plot_37.las"
tls = readTLS(file)

# normalize the point cloud
tls = tlsNormalize(tls, keep_ground = F)
x = plot(tls)

# extract the tree map from a thinned point cloud
#thin = tlsSample(tls, smp.voxelize(0.25))
#map = treeMap(thin, map.hough(min_density = 0.1, pixel_size = 0.5, max_h = 18, h_step = 2, max_d = 2), 0)
map = treeMap(tls, map.eigen.voxel(vox = 0.25, min_h = 5, max_h = 15, max_d = .5, max_verticality = 25), 0)
#map = treeMap(thin, map.pick(min_h = 1, max_h = 12), 0)
add_treeMap(x, map, color='yellow', size=2)

# classify tree regions
tls = treePoints(tls, map, trp.crop())
add_treePoints(x, tls, size=4)
add_treeIDs(x, tls, cex = 2, col='yellow')

# classify stem points
tls = stemPoints(tls, stm.hough())
add_stemPoints(x, tls, color='red', size=8)

# # make the plot's inventory
# inv = tlsInventory(tls, d_method=shapeFit(shape='circle', algorithm = 'irls'))
# add_tlsInventory(x, inv)
#
# # extract stem measures
# seg = stemSegmentation(tls, sgt.ransac.circle(n = 20))
# add_stemSegments(x, seg, color='white', fast=T)

# plot everything once
tlsPlot(tls, map, fast=T)

# check out only one tree
tlsPlot(tls, inv, seg, tree_id = 11)







file = system.file("extdata", "pine.laz", package="TreeLS")
tls = readTLS(file) %>% tlsNormalize()

# calculate some point metrics
tls = fastPointMetrics(tls, ptm.knn())
x = plot(tls, color='Verticality')

# get its stem points
tls = stemPoints(tls, stm.eigen.knn(voxel_spacing = .1))
add_stemPoints(x, tls, size=3, color='red')

# get dbh and height
dbh_algo = shapeFit(shape='cylinder', algorithm = 'bf', n=15, inliers=.95, z_dev=10)
inv = tlsInventory(tls, hp = .95, d_method = dbh_algo)
add_tlsInventory(x, inv)

# segment the stem usind 3D cylinders and getting their directions
seg = stemSegmentation(tls, sgt.irls.cylinder(n=300))
add_stemSegments(x, seg, color='blue')

# check out a specific tree segment
tlsPlot(seg, tls, segment = 3)

