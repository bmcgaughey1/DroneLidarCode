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

mapviewOptions(fgb = FALSE)
mapview(polys, zcol = "PredictedSpecies", col.regions = c("red", "green", "green"), alpha.regions = 1)

# load code for DBH prediction
# the custom model I fit for T3 has issues when height value are outside the range of our field data (used to fit the model).
# Predicted DBH is too large for TSHE
source("Rcode/predictDBH_Height.R")

polys$PredictedDBH <- NA
for (i in 1:nrow(polys)) {
  polys$PredictedDBH[i] <- predictDBH(polys$PredictedSpecies[i], polys$GridMaxHt.x[i], method = "hanus", heightUnits = "meters", DBHUnits = "cm")
}

# drop trees with bad predicted DBH...8 trees for plot 37
polys <- polys[!is.nan(polys$PredictedDBH) & !is.na(polys$PredictedSpecies), ]

#hist(polys$PredictedDBH[polys$PredictedSpecies == "PSME"], main = "DBH distribution -- PSME", xlab = "Predicted DBH (cm)")
#hist(polys$PredictedDBH[polys$PredictedSpecies == "TSHE"], main = "DBH distribution -- TSHE", xlab = "Predicted DBH (cm)")

gg <- ggplot(polys[polys$PredictedSpecies == "PSME",], aes(x = PredictedDBH)) + geom_histogram(binwidth = 5, na.rm = T, fill = "red", linetype = "solid", color = "black")
gg + theme(axis.text=element_text(size=12, face="bold", color = "black")
           , axis.title=element_text(size=14,face="bold")
) + ggtitle("DBH distribution -- PSME") + xlab("Predicted DBH (cm)") + theme(plot.title = element_text(hjust = 0.5)) + xlim(0, 100) + coord_flip()

gg <- ggplot(polys[polys$PredictedSpecies == "TSHE",], aes(x = PredictedDBH)) + geom_histogram(binwidth = 5, na.rm = T, fill = "green", linetype = "solid", color = "black")
gg + theme(axis.text=element_text(size=12, face="bold")
           , axis.title=element_text(size=14,face="bold")
) + ggtitle("DBH distribution -- TSHE") + xlab("Predicted DBH (cm)") + theme(plot.title = element_text(hjust = 0.5)) + xlim(0, 100) + coord_flip()

# height distribution
gg <- ggplot(polys[polys$PredictedSpecies == "PSME",], aes(x = GridMaxHt.x)) + geom_histogram(binwidth = 2.5, na.rm = T, fill = "red", linetype = "solid", color = "black")
gg + theme(axis.text=element_text(size=12, face="bold", color = "black")
           , axis.title=element_text(size=14,face="bold")
) + ggtitle("Height distribution -- PSME") + xlab("Lidar height (m)") + theme(plot.title = element_text(hjust = 0.5)) + xlim(0, 70) + coord_flip()

gg <- ggplot(polys[polys$PredictedSpecies == "TSHE",], aes(x = GridMaxHt.x)) + geom_histogram(binwidth = 2.5, na.rm = T, fill = "green", linetype = "solid", color = "black")
gg + theme(axis.text=element_text(size=12, face="bold", color = "black")
           , axis.title=element_text(size=14,face="bold")
) + ggtitle("Height distribution -- TSHE") + xlab("Lidar height (m)") + theme(plot.title = element_text(hjust = 0.5)) + xlim(0, 70) + coord_flip()








# graphics for OESF science meeting 2023...some work, some don't
# load CHM
library(rasterVis)
CHM <- readDTM("E:/T3_DroneLidar/Ba/Plot37/Processing/CHM/CHM.dtm", type = "raster", epsg = 26910)
colfunc<-colorRampPalette(c("brown","yellow","springgreen"))
plot3D(CHM, col = (colfunc(60)), rev = TRUE)

# DEM renders with stripes or ridges...not sure why but not worth using as it causes questions
DEM <- readDTM("E:/T3_DroneLidar/Ba/Plot37/Ground/ground.dtm", type = "terra", epsg = 26910)
DEM <- as(DEM, "Raster")
#DEM <- focal(DEM, w=matrix(1, 9, 9), mean)
plot3D(DEM, col = topo.colors(300), maxpixels = 100000)


# plot tree crown polygons from TAOs. color using the basin identifier
mapview(polys, zcol = "BasinID", col.regions = terrain.colors(50))


mapview(polys, zcol = "PredictedSpecies", col.regions = c("red", "green"), alpha.regions = 1.0)


tp <- rast(nrows = 500, ncols = 500, extent = extent(polys), crs = "epsg:26910")
tps <- rasterize(polys, tp, field = "PredictedSpecies")

plot(tps, col = data.frame(value = c("PSME", "TSHE"), color = c("red", "green")), plg = list(legend = c("PSME", "TSHE")), type = "classes", colNA = "white")

