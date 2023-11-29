# *****************************************************************************
# *****************************************************************************
# Read trees that Ally moved and the ones that Bob moved and compare locations
#
# Bob used a single file with all trees for all plots to do the adjustments.
# Bob also had problems due to a program bug where moving all trees for a plot
# actually moved all trees for all plots. Bob repeated the adjustment process
# several times before code bugs were all fixed.
#
# Ally used individual trees for each plot.
#
# In general, the two sets of new locations agreed well. There are a few (<20)
# trees where the difference between base locations exceeded 1m. It is very
# likely that these are cases where the tree was matched to the wrong tree
# in the point cloud by either Bob or Ally. This is not a hard thing to do
# when the plot is dense and individual trees are hard to discern. There
# are also cases where the tree base is the same for Bob and Ally but the
# tree top is different. Again, dense canopies can make it hard to line
# things up. This is especially true when there are no stem hits to help
# position the tree base.
#
# The first part of this code won't run anywhere except Bob's computer
# because the individual plot files used for the adjustment of tree
# locations are not included in the repository. The code starting in line
# 100 will run. It reads a data file from the extras folder that is produced
# by the first part of the code.
# *****************************************************************************
# *****************************************************************************
plotNumbers <- c("07", "08", "09", "10", "13", "14",
                 "16", "17", "18", "19", "20", "21",
                 "22", "23", "24", "25", "26", "27",
                 "28", "29", "31", "34", "36", "37",
                 "42", "43", "46", "47")

# Bob's trees
outputFolder <- "G:/R_Stuff/ONRCDroneLidar/AllyPlots"
adjustedTreesBob <- read.csv(file = paste0(outputFolder, "/WORKING_FUSIONtrees.csv"), stringsAsFactors = FALSE)
originalTrees <- read.csv(file = paste0(outputFolder, "/FieldTrees.csv"), stringsAsFactors = FALSE)

# compute top location
adjustedTreesBob$TopX <- adjustedTreesBob$X + (cos((450 - adjustedTreesBob$Lean.Azimuth) * pi / 180.0) * sin(adjustedTreesBob$Lean.Angle * pi / 180.0) * adjustedTreesBob$Total.Height)
adjustedTreesBob$TopY <- adjustedTreesBob$Y + (sin((450 - adjustedTreesBob$Lean.Azimuth) * pi / 180.0) * sin(adjustedTreesBob$Lean.Angle * pi / 180.0) * adjustedTreesBob$Total.Height)

# read Ally's plots...
# some choice for handling match
# -work plot-by-plot
# -read all of Ally's plots, then work across all plots
adjustedTreesAlly <- data.frame()
for (i in 1:length(plotNumbers)) {
  #  i <- 1
  # make sure file exists
  if (file.exists(paste0(outputFolder, "/Plot_", plotNumbers[i], "_Field_FUSIONtrees.csv"))) {
    # read Ally's trees
    t <- read.csv(file = paste0(outputFolder, "/Plot_", plotNumbers[i], "_Field_FUSIONtrees.csv"), stringsAsFactors = FALSE)
    adjustedTreesAlly <- rbind(adjustedTreesAlly, t)
  }
}

# compute top location
adjustedTreesAlly$TopX <- adjustedTreesAlly$X + (cos((450 - adjustedTreesAlly$Lean.Azimuth) * pi / 180.0) * sin(adjustedTreesAlly$Lean.Angle * pi / 180.0) * adjustedTreesAlly$Total.Height)
adjustedTreesAlly$TopY <- adjustedTreesAlly$Y + (sin((450 - adjustedTreesAlly$Lean.Azimuth) * pi / 180.0) * sin(adjustedTreesAlly$Lean.Angle * pi / 180.0) * adjustedTreesAlly$Total.Height)

# match trees by identifier
plotTrees <- merge(adjustedTreesAlly, adjustedTreesBob, by = "Tree.ID", all = FALSE, suffixes = c(".Ally", ".Bob"))

# 1528 trees that were matched

# compute difference in base and top location
plotTrees$diffX <- plotTrees$X.Ally - plotTrees$X.Bob
plotTrees$diffY <- plotTrees$Y.Ally - plotTrees$Y.Bob
plotTrees$diffTopX <- plotTrees$TopX.Ally - plotTrees$TopX.Bob
plotTrees$diffTopY <- plotTrees$TopY.Ally - plotTrees$TopY.Bob
plotTrees$diff <- sqrt(plotTrees$diffX ^ 2 + plotTrees$diffY ^ 2)
plotTrees$diffTop <- sqrt(plotTrees$diffTopX ^ 2 + plotTrees$diffTopY ^ 2)

# build a plot number
plotTrees$PlotID <- as.factor(substr(plotTrees$Tree.ID, 1, 2))

# drop all but trees with status code = 0...these are the trees we adjusted
plotTrees <- plotTrees[plotTrees$Status.Code.Ally == 0, ]
plotTrees <- plotTrees[plotTrees$Status.Code.Bob == 0, ]

# 624 trees that were relocated and matched

# compare adjusted locations to original trees
originalPlotTrees <- merge(plotTrees, originalTrees, by.x = "Tree.ID", by.y = "TreeID")

originalPlotTrees$origdiffX.Ally <- originalPlotTrees$X.Ally - originalPlotTrees$Xfield
originalPlotTrees$origdiffY.Ally <- originalPlotTrees$Y.Ally - originalPlotTrees$Yfield
originalPlotTrees$origdiff.Ally <- sqrt(originalPlotTrees$origdiffX.Ally ^ 2 + originalPlotTrees$origdiffY.Ally ^ 2)

originalPlotTrees$origdiffX.Bob <- originalPlotTrees$X.Bob - originalPlotTrees$Xfield
originalPlotTrees$origdiffY.Bob <- originalPlotTrees$Y.Bob - originalPlotTrees$Yfield
originalPlotTrees$origdiff.Bob <- sqrt(originalPlotTrees$origdiffX.Bob ^ 2 + originalPlotTrees$origdiffY.Bob ^ 2)

originalPlotTrees$heightdiff <- abs(originalPlotTrees$Total.Height.Bob - originalPlotTrees$Total.Height.Ally)

# write off data into the extras folder in the repository
#write.csv(originalPlotTrees, "extras/AdjustedTrees_AllPlots.csv", row.names = FALSE)

# *****************************************************************************
# *****************************************************************************
# *****************************************************************************
# *****************************************************************************
# read data...start here if working with the data. Repository doesn't have the tree data adjusted by Ally & Bob in the form needed for the code above
# 624 trees that were relocated and matched by Ally & Bob
originalPlotTrees <- read.csv(file = "extras/AdjustedTrees_AllPlots.csv", stringsAsFactors = FALSE)

# differences between tree base and top locations between Ally & Bob
boxplot(diff ~ PlotID, data = originalPlotTrees, main = "Tree base location differences\nBob & Ally", xlab = "Plot number", ylab = "Horizontal difference (m)")
boxplot(diffTop ~ PlotID, data = originalPlotTrees, main = "Tree top location differences\nBob & Ally", xlab = "Plot number", ylab = "Horizontal difference (m)")

# summarize the differences in base and top locations
hist(originalPlotTrees$diff, main = "Tree base location difference (m)")
hist(originalPlotTrees$diffTop, main = "Tree top location difference (m)")

# differences between the base and top locations between the original field locations and those adjusted by Ally & Bob
boxplot(origdiff.Ally ~ PlotID, data = originalPlotTrees, main = "Tree base location differences\nAlly & Field", ylim = c(0, 8), xlab = "Plot number", ylab = "Horizontal difference (m)")
boxplot(origdiff.Bob ~ PlotID, data = originalPlotTrees, main = "Tree base location differences\nBob & Field", ylim = c(0, 8), xlab = "Plot number", ylab = "Horizontal difference (m)")

# height difference between Ally & Bob
boxplot(heightdiff ~ PlotID, data = originalPlotTrees, main = "Tree height difference\nBob & Ally", ylim = c(0, 8), xlab = "Plot number", ylab = "Vertical difference (m)")

# drop any trees where the base locations differ by more than 1m
distThreshold <- 1
trainingTrees <- originalPlotTrees[originalPlotTrees$diff <= distThreshold, ]

#580 trees after filtering to remove trees with horizontal position differences > 1m

# height differences between Ally & Bob for trees within 1m
boxplot(heightdiff ~ PlotID, data = trainingTrees, main = "Tree height difference\nBob & Ally", ylim = c(0, 8), xlab = "Plot number", ylab = "Vertical difference (m)")

# drop trees where we had a height difference of 1m or more
heightThreshold <- 1
trainingTrees <- trainingTrees[trainingTrees$heightdiff <= heightThreshold, ]

# 575 trees after filtering based on location and height difference thresholds

# height difference between Bob & Ally for final trees
boxplot(heightdiff ~ PlotID, data = trainingTrees, main = "Tree height difference (Bob - Ally)", ylim = c(0, 1.1), xlab = "Plot number", ylab = "Vertical difference (m)")

# height difference by species
boxplot(heightdiff ~ PlotID, data = trainingTrees[trainingTrees$Species == "PSME",], main = "PSME -- Tree height difference (Bob - Ally)", ylim = c(0, 1.1), xlab = "Plot number", ylab = "Vertical difference (m)")
boxplot(heightdiff ~ PlotID, data = trainingTrees[trainingTrees$Species == "TSHE",], main = "TSHE -- Tree height difference (Bob - Ally)", ylim = c(0, 1.1), xlab = "Plot number", ylab = "Vertical difference (m)")

#boxplot(diff ~ PlotID, data = trainingTrees, main = "Tree base", ylim = c(0, 2))
#boxplot(diffTop ~ PlotID, data = trainingTrees, main = "Tree top", ylim = c(0, 2))

# write off the matched field trees to use for lidar matching. we have reduced the set of field
# trees to include only those where the locations that Ally and I adjusted were within 1m and also
# have heights within 1m.
#outputFolder <- "G:/R_Stuff/ONRCDroneLidar"
#write.csv(trainingTrees, paste0(outputFolder, "/TrainingTrees_Field_AllPlots.csv"), row.names = FALSE)
