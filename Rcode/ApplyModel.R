# code to predict species over entire area covered by drone lidar data
#
# This code was developed for Plot 27 in Ba. It should work for other areas with minor changes
#
# assumes speciesRF is available...see speciesModeling.R code
#
library(randomForest)
library(dplyr)
library(caret)
library(terra)
library(sf)
library(mapview)

drawMaps <- FALSE
#drawMaps <- TRUE

# read TAO metrics for upper 3m
treeMetricsFile <- "E:/T3_DroneLidar/Ba/Plot37/Processing/TreeTops_normalized_metrics_merged.csv"
TAOData <- read.csv(treeMetricsFile, stringsAsFactors = FALSE)

# compute relative percentiles...divide P?? by P99. I use these when dealing with clips of individual trees
# to allow the use of percentiles in cases where one species is much higher than another
#
# for this case with the point clips for the upper 3m of crowns, these are less useful (but still useful)
TAOData$RP01 <- TAOData$Elev.P01 / TAOData$Elev.P99
TAOData$RP05 <- TAOData$Elev.P05 / TAOData$Elev.P99
TAOData$RP10 <- TAOData$Elev.P10 / TAOData$Elev.P99
TAOData$RP20 <- TAOData$Elev.P20 / TAOData$Elev.P99
TAOData$RP25 <- TAOData$Elev.P25 / TAOData$Elev.P99
TAOData$RP30 <- TAOData$Elev.P30 / TAOData$Elev.P99
TAOData$RP40 <- TAOData$Elev.P40 / TAOData$Elev.P99
TAOData$RP50 <- TAOData$Elev.P50 / TAOData$Elev.P99
TAOData$RP60 <- TAOData$Elev.P60 / TAOData$Elev.P99
TAOData$RP70 <- TAOData$Elev.P70 / TAOData$Elev.P99
TAOData$RP75 <- TAOData$Elev.P75 / TAOData$Elev.P99
TAOData$RP80 <- TAOData$Elev.P80 / TAOData$Elev.P99
TAOData$RP90 <- TAOData$Elev.P90 / TAOData$Elev.P99
TAOData$RP95 <- TAOData$Elev.P95 / TAOData$Elev.P99

# read RF model
speciesRF <- readRDS("FINAL_RF_HW_DF_Model_LeaningTrees.rds")

typePred <- predict(speciesRF, newdata = TAOData)
TAOData$PredictedSpecies <- typePred

# add species to tree data
TAOPolyFile <- "E:/T3_DroneLidar/Ba/Plot37/Processing/Trees/trees_Polygons.shp"
polys <- st_read(TAOPolyFile)

polys <- merge(polys, TAOData, by.x = "BasinID", by.y = "Identifier")

if (drawMaps) {
  mapviewOptions(fgb = FALSE)
  mapview(polys, zcol = "PredictedSpecies", col.regions = c("red", "green", "green"), alpha.regions = 1)
}

# load code for DBH prediction
# the custom model I fit for T3 has issues when height value are outside the range of our field data (used to fit the model).
# Predicted DBH is too large for TSHE
source("Rcode/predictDBH_Height.R")

polys$PredictedDBH <- NA
for (i in 1:nrow(polys)) {
  polys$PredictedDBH[i] <- predictDBH(polys$PredictedSpecies[i], polys$GridMaxHt.x[i], method = "hanus", heightUnits = "meters", DBHUnits = "cm")
}

# drop trees with bad predicted DBH...8 trees for the Ba unit containing plot 37
polys <- polys[!is.nan(polys$PredictedDBH) & !is.na(polys$PredictedSpecies), ]

#hist(polys$PredictedDBH[polys$PredictedSpecies == "PSME"], main = "DBH distribution -- PSME", xlab = "Predicted DBH (cm)")
#hist(polys$PredictedDBH[polys$PredictedSpecies == "TSHE"], main = "DBH distribution -- TSHE", xlab = "Predicted DBH (cm)")

gg <- ggplot(polys[polys$PredictedSpecies == "PSME",], aes(x = PredictedDBH)) + geom_histogram(binwidth = 5, na.rm = T, fill = "red", linetype = "solid", color = "black")
gg + theme(axis.text=element_text(size=12, face="bold", color = "black")
           , axis.title=element_text(size=14,face="bold")
) + ggtitle("DBH distribution -- PSME") + xlab("Predicted DBH (cm)") + theme(plot.title = element_text(hjust = 0.5)) + xlim(0, 100) + coord_flip() + ylim(0, 350)

gg <- ggplot(polys[polys$PredictedSpecies == "TSHE",], aes(x = PredictedDBH)) + geom_histogram(binwidth = 5, na.rm = T, fill = "green", linetype = "solid", color = "black")
gg + theme(axis.text=element_text(size=12, face="bold")
           , axis.title=element_text(size=14,face="bold")
) + ggtitle("DBH distribution -- TSHE") + xlab("Predicted DBH (cm)") + theme(plot.title = element_text(hjust = 0.5)) + xlim(0, 100) + coord_flip() + ylim(0, 350)

# height distribution
gg <- ggplot(polys[polys$PredictedSpecies == "PSME",], aes(x = GridMaxHt.x)) + geom_histogram(binwidth = 2.5, na.rm = T, fill = "red", linetype = "solid", color = "black")
gg + theme(axis.text=element_text(size=12, face="bold", color = "black")
           , axis.title=element_text(size=14,face="bold")
) + ggtitle("Height distribution -- PSME") + xlab("Lidar height (m)") + theme(plot.title = element_text(hjust = 0.5)) + xlim(0, 70) + coord_flip() + ylim(0, 350)

gg <- ggplot(polys[polys$PredictedSpecies == "TSHE",], aes(x = GridMaxHt.x)) + geom_histogram(binwidth = 2.5, na.rm = T, fill = "green", linetype = "solid", color = "black")
gg + theme(axis.text=element_text(size=12, face="bold", color = "black")
           , axis.title=element_text(size=14,face="bold")
) + ggtitle("Height distribution -- TSHE") + xlab("Lidar height (m)") + theme(plot.title = element_text(hjust = 0.5)) + xlim(0, 70) + coord_flip() + ylim(0, 350)


# code to test the idea of calibrating the number of TAOs relative to the number of trees measured on a plot
# load the plot locations, clip the TAOs using the plot area and compare the number of TAOs with the number of
# tree on the plot. Not the best because we only have 1 plot in each area (at least for most areas)
#
# I manually did the calculations using the tree data spreadsheet for plot 37. It would be fairly easy to
# load the spreadsheet, compute BA, and summarize BA and TPA for each plot.
#
# for plot 37, there are 41 live trees and 44 total trees. 73.42 sq ft basal area for live trees, 76.06 total BA
if (FALSE) {
  # load the plot locations
  plots <- st_read("E:/Backup/R_Stuff/ONRCDroneLidar/plot_centers_UTM.shp")

  plot37 <- plots[plots$PlotID == 37, ]

  plot37_buffered <- st_buffer(plot37, 17.68)

  mapviewOptions(fgb = FALSE)
  mapview(list(plot37_buffered, polys))

  # read high points
  TAOPointFile <- "E:/T3_DroneLidar/Ba/Plot37/Processing/Trees/trees_HighPoints.shp"
  points <- st_read(TAOPointFile)

  points <- merge(points, TAOData, by.x = "BasinID", by.y = "Identifier")

  points$PredictedDBH <- NA
  for (i in 1:nrow(polys)) {
    points$PredictedDBH[i] <- predictDBH(points$PredictedSpecies[i], points$GridMaxHt.x[i], method = "hanus", heightUnits = "meters", DBHUnits = "cm")
  }

  # drop trees with bad predicted DBH...8 trees for the Ba unit containing plot 37
  points <- points[!is.nan(points$PredictedDBH) & !is.na(points$PredictedSpecies), ]

  ptsInPlot <- st_intersection(points, plot37_buffered)

  mapview(list(plot37_buffered, polys, ptsInPlot))

  ptsInPlot$BA <- 0.005454 * (ptsInPlot$PredictedDBH / 2.54) ^ 2
  sum(ptsInPlot$BA)

  # for TAOS, there are 28 TAOs in plot and 56.93 sq ft basal area
  # for field plot, there are 41 live trees and 73.42 sq ft basal area
  # stem density correction factor is 41 / 28 = 1.4643
  # BA correction factor is 73.42 / 56.93 = 1.2897
  #
  # these correction factors can be used directly to adjust values since they are a pure ratio
  # and don't correspond to a specific area of plot. You could probably argu that there is
  # some correction needed for circular versus square areas but not sure how this would come into
  # play.
  #
  # I could have used the stem density correction factor in the treatment simulation below to
  # set the number of residual trees...However, I did not
}

# these are the correction factors for unit Ba (plot 37). The code in the if stmt aboe gets part of the
# information needed to compute the correction factors but you also have to use the field data to get
# the number of live trees on the plot and the basal area for the plot. This could be done with code
# but I was under a time crunch...
BAcorrection <- 1.2897
TPAcorrection <- 1.4643




if (FALSE) {
  # graphics for OESF science meeting 2023...some work, some don't
  # load CHM
  library(rasterVis)
  library(fusionwrapr)
  CHM <- readDTM("E:/T3_DroneLidar/Ba/Plot37/Processing/CHM/CHM.dtm", type = "raster", epsg = 26910)
  colfunc<-colorRampPalette(c("brown","yellow","springgreen"))
  plot3D(CHM, col = (colfunc(60)), rev = TRUE)

  # DEM renders with stripes or ridges...not sure why but not worth using as it causes questions
  DEM <- readDTM("E:/T3_DroneLidar/Ba/Plot37/Ground/ground.dtm", type = "terra", epsg = 26910)
  DEM <- as(DEM, "Raster")
  #DEM <- focal(DEM, w=matrix(1, 9, 9), mean)
  plot3D(DEM, col = topo.colors(300), maxpixels = 100000)

  if (drawMaps) {
    # plot tree crown polygons from TAOs. color using the basin identifier
    mapview(polys, zcol = "BasinID", col.regions = terrain.colors(50))


    mapview(polys, zcol = "PredictedSpecies", col.regions = c("red", "green"), alpha.regions = 1.0)
  }

  tp <- rast(nrows = 1024, ncols = 1024, extent = st_bbox(polys), crs = "epsg:26910")
  tps <- rasterize(polys, tp, field = "PredictedSpecies")

  plot(tps, col = data.frame(value = c("PSME", "TSHE"), color = c("red", "green")), plg = list(legend = c("PSME", "TSHE")), type = "classes", colNA = "white")
}



# read CHM and compute area for valid cells
library(fusionwrapr)
CHM <- readDTM("E:/T3_DroneLidar/Ba/Plot37/Processing/CHM/CHM.dtm", type = "terra", epsg = 26910)
r <- res(CHM)
validArea <- global(CHM, fun="notNA")$notNA * r[1] * r[2]

library(raster)
library(ForestGapR)
library(kableExtra)

# *****************************************************************************
# this is a modified version of the getForestGaps function from ForestGapR
# this finds clumps above the threshold height
# *****************************************************************************
getForestClumps <- function(chm_layer, threshold = 10, size = c(1, 10^4)) {
  chm_layer[chm_layer < threshold] <- NA
  chm_layer[chm_layer >= threshold] <- 1
  clumps <- raster::clump(chm_layer, directions = 8, gap = FALSE)
  rcl <- raster::freq(clumps)
  rcl[, 2] <- rcl[, 2] * raster::res(chm_layer)[1]^2
  rcl <- cbind(rcl[, 1], rcl)
  z <- raster::reclassify(clumps, rcl = rcl, right = NA)
  z[is.na(clumps)] <- NA
  clumps[z > size[2]] <- NA
  clumps[z < size[1]] <- NA
  clumps <- raster::clump(clumps, directions = 8, gap = FALSE)
  names(clumps) <- "clumps"
  return(clumps)
}
# create a raster layer for simulated CHM
tp <- rast(nrows = 1024, ncols = 1024, extent = extent(polys), crs = "epsg:26910")
tps <- rasterize(polys, tp, field = "GridMaxHt.x")

CHM <- as(tps, "Raster")

# do gap analysis with all trees
mask <- getForestClumps(chm_layer=CHM, threshold=-1.0, size=c(0.1, 100000000))
mask_stats<-GapStats(gap_layer=mask, chm_layer=CHM)
maskArea <- sum(mask_stats$gap_area)

# compute values from CHM...used for thresholds
# produce histogram with vertical lines for height thresholds
maxht <- cellStats(CHM, "max")
meanht <- cellStats(CHM, "mean")
qs <- quantile(CHM, probs = c(0.25, 0.5, 0.75))
attributes(qs) <- NULL
P25 <- qs[1]
P75 <- qs[3]
medianht <- qs[2]

thresholds <- c(2.0, 4.0, 8.0, 16.0, P25, medianht, meanht, P75)
gapcol <- c("black", "red", "coral", "cyan", "gold", "blue", "green", "purple")
extraLabels <- c("", "", "", "", "P25: ", "median: ", "mean: ", "P75: ")
size<-c(2.25, 100000000) # m^2

# plot height distribution from CHM
hist(CHM
     , main = ""
     , xlab = "Canopy height (m)"
)

for (i in 1:length(thresholds)) {
  abline(v = thresholds[i], col = gapcol[i], lwd = 2)
}

# run gap analysis and produce plots
layout(matrix(c(1,2,3,4,5,6,7,8), nrow = 4, ncol = 2, byrow = TRUE))
par(mar=c(1, 1, 1, 1))

for (i in 1:length(thresholds)) {
  gaps<-getForestGaps(chm_layer=CHM, threshold=thresholds[i], size=size)
  stats<-GapStats(gap_layer=gaps, chm_layer=CHM)

  # compute summary info
  t <- data.frame("Label" = paste0(extraLabels[i], round(thresholds[i], 1), "m"),
                  "Total area" = round(sum(stats$gap_area) / 10000, 2),
                  "Ratio" = round(sum(stats$gap_area) / maskArea * 100.0, 2),
                  "Mean" = round(mean(stats$gap_area), 2),
                  "Std. Dev." = round(sqrt(var(stats$gap_area)), 2)
  )

  if (i == 1) {
    summaryStats <- t
  } else {
    summaryStats <- rbind(summaryStats, t)
  }

  plot(gaps
       , col=gapcol[i]
       , asp = 1
       , axes = FALSE
       , add=FALSE
       , main=paste0("Gaps (below ", extraLabels[i], round(thresholds[i], 1), "m)")
       , legend=FALSE
  )
}

#This table summarizes the gap analyses. Gap proportion is the ratio of the area in gaps to the total area in the mask.

# produce table with gap analysis results
# make a data frame with 3 columns
rownames(summaryStats) <- c()

knitr::kable(summaryStats, # "latex", booktabs = T,
             digits = c(0, 2, 2, 2, 2),
             col.names = c("Threshold",
                           "Total area (ha)",
                           "Gap proportion (%)",
                           "Average gap size (sq m)",
                           "StdDev gap size (sq m)"),
             align = c('l', 'c', 'c', 'c', 'c')
) %>%
  column_spec(2:5, width = "2cm") %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                latex_options = c("striped", "repeat_header"))




# ***** THe first time I did this analysis, I made a mistake in the area calculation and used the simulated CHM instead
# of the original, untreated CHM. After fixing the problem, the stem density and BA seem low.


# compute BA using predicted DBH (sq ft)
polys$BA <- 0.005454 * (polys$PredictedDBH / 2.54) ^ 2

unitAreaAcre <- validArea * 0.000247105

cat("Starting BA: ", sum(polys$BA) / unitAreaAcre * BAcorrection, " sqft/acre\n")
cat("Starting density: ", nrow(polys) / unitAreaAcre * TPAcorrection, " trees/acre\n")

# set up logic to "cut" trees based on the number of stems for the entire area
## this could be done as a proportion of the BA. You could also treat species
# separately. For example, cut 75% of TSHE and 25% of PSME
maxRandNum <- 10000
targetTPA <- 66.0 / 1.4643

# compute the threshold value for tree selection based on TPA
targetNum <- targetTPA / (nrow(polys) / unitAreaAcre) * maxRandNum

# generate a random number for each TAO...need to be fairly large numbers
set.seed(391846)
polys$ran <- runif(nrow(polys), 0, maxRandNum)

# use random numbers to select trees to retain
rpolys <- polys[polys$ran <= targetNum, ]

cat("Post-treatment BA: ", sum(rpolys$BA) / unitAreaAcre * BAcorrection, " sqft/acre\n")
cat("Post-treatment density: ", nrow(rpolys) / unitAreaAcre * TPAcorrection, " trees/acre\n")

if (drawMaps) {
  # map the residual stems
  mapviewOptions(fgb = FALSE)
  mapview(rpolys, zcol = "PredictedSpecies", col.regions = c("red", "green", "green"), alpha.regions = 1)
}

# write off list of TAOs so we can merge them to show treatment effects
# TAO polygons have file names for the upper 3m clips but we need the full trees without any normalization. These
# can be built using the file title field
rpolys$TAOFileName <- paste0("E:/T3_DroneLidar/Ba/Plot37/Processing/Trees/TAOpts/", rpolys$FileTitle, ".lda")

# write the list of tree clips. This list is used as input for FUSION's MergeData tool to build a point
# cloud representing the treated stand. I also merged in the ground points for the unit so there are no gaps
# between the residual trees.
write.table(rpolys$TAOFileName, "RetainedTrees.txt", quote = F, row.names = F, col.names = F)

# I merged all the TAO clips and the ground points for unit Ba, plot 37. Then I created a new CHM using the
# merged point data. This CHM represents the post-treatment condition. Both the pre- and post-treatment CHMs
# are used in the gap analysis below. Visualizing the treatments using the point cloud works well but it would
# be really good if we had RGB imagery merged with the point data. The LDA format won't allow this (yet another
# reason to modify TreeSeg to output LAS format point files). The 2021 NAIP imagery is available for the T3
# area but it is in MrSID format so I need ArcGIS to read and clip to the study area. Even with that, merging
# the RGB values into the point cloud would be a challenge given the size of the point data. LDV crashes when
# trying to view the entire treated point cloud.

# *****One downside to this approach is that the ground points under trees are duplicated in the point cloud.
# This could be "fixed" by ignoring class 2 points when merging the tree clips and then merging the ground
# points. There are also duplicate points in areas with overlapping crowns. This is harder to "fix". A full-blown
# duplicate point removal process would work but I don't have tools to do this in FUSION.

# gap analysis using original CHM and simulated treatment CHM
CHM <- readDTM("E:/T3_DroneLidar/Ba/Plot37/Processing/CHM/CHM.dtm", type = "raster", epsg = 26910)
size<-c(1, 100000000) # m^2
gaps<-getForestGaps(chm_layer=CHM, threshold=4, size=size)
stats<-GapStats(gap_layer=gaps, chm_layer=CHM)

par(mar = c(6.1, 4.1, 4.1, 2.1))


GapSizeFDist(
  gaps_stats = stats, method = "Hanel_2017", col = "forestgreen", pch = 16, cex = 1.5, cex.lab = 1.5,
  axes = FALSE, ylab = "Gap Frequency", xlab = as.expression(bquote("Gap Size" ~ (m^2)))
)
axis(1)
axis(2)
grid(4, 4)

ptCHM <- readDTM("E:/T3_DroneLidar/Ba/Plot37/Simulated/CHM66.dtm", type = "raster", epsg = 26910)
gaps<-getForestGaps(chm_layer=ptCHM, threshold=4, size=size)
stats<-GapStats(gap_layer=gaps, chm_layer=ptCHM)

GapSizeFDist(
  gaps_stats = stats, method = "Hanel_2017", col = "forestgreen", pch = 16, cex = 1.5, cex.lab = 1.5,
  axes = FALSE, ylab = "Gap Frequency", xlab = as.expression(bquote("Gap Size" ~ (m^2)))
)
axis(1)
axis(2)
grid(4, 4)


par(mar = c(6.1, 4.1, 4.1, 2.1))

# clumps<-getForestClumps(chm_layer=CHM, threshold=4, size=size)
# stats<-GapStats(gap_layer=clumps, chm_layer=CHM)

clumps<-getForestClumps(chm_layer=ptCHM, threshold=4, size=size)
stats<-GapStats(gap_layer=clumps, chm_layer=ptCHM)

GapSizeFDist(
  gaps_stats = stats, method = "Hanel_2017", col = "forestgreen", pch = 16, cex = 1.5, cex.lab = 1.5,
  axes = FALSE, ylab = "Clump Frequency", xlab = as.expression(bquote("Clump Size" ~ (m^2)))
)
axis(1)
axis(2)
grid(4, 4)


# plots showing residual DBH and height distributions by species
gg <- ggplot(rpolys[rpolys$PredictedSpecies == "PSME",], aes(x = PredictedDBH)) + geom_histogram(binwidth = 5, na.rm = T, fill = "red", linetype = "solid", color = "black")
gg + theme(axis.text=element_text(size=12, face="bold", color = "black")
           , axis.title=element_text(size=14,face="bold")
) + ggtitle("DBH distribution -- PSME") + xlab("Predicted DBH (cm)") + theme(plot.title = element_text(hjust = 0.5)) + xlim(0, 100) + coord_flip() + ylim(0, 350)

gg <- ggplot(rpolys[rpolys$PredictedSpecies == "TSHE",], aes(x = PredictedDBH)) + geom_histogram(binwidth = 5, na.rm = T, fill = "green", linetype = "solid", color = "black")
gg + theme(axis.text=element_text(size=12, face="bold")
           , axis.title=element_text(size=14,face="bold")
) + ggtitle("DBH distribution -- TSHE") + xlab("Predicted DBH (cm)") + theme(plot.title = element_text(hjust = 0.5)) + xlim(0, 100) + coord_flip() + ylim(0, 350)

# height distribution
gg <- ggplot(rpolys[rpolys$PredictedSpecies == "PSME",], aes(x = GridMaxHt.x)) + geom_histogram(binwidth = 2.5, na.rm = T, fill = "red", linetype = "solid", color = "black")
gg + theme(axis.text=element_text(size=12, face="bold", color = "black")
           , axis.title=element_text(size=14,face="bold")
) + ggtitle("Height distribution -- PSME") + xlab("Lidar height (m)") + theme(plot.title = element_text(hjust = 0.5)) + xlim(0, 70) + coord_flip() + ylim(0, 350)

gg <- ggplot(rpolys[rpolys$PredictedSpecies == "TSHE",], aes(x = GridMaxHt.x)) + geom_histogram(binwidth = 2.5, na.rm = T, fill = "green", linetype = "solid", color = "black")
gg + theme(axis.text=element_text(size=12, face="bold", color = "black")
           , axis.title=element_text(size=14,face="bold")
) + ggtitle("Height distribution -- TSHE") + xlab("Lidar height (m)") + theme(plot.title = element_text(hjust = 0.5)) + xlim(0, 70) + coord_flip() + ylim(0, 350)





