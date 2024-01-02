# code to fix the files containing metrics for small cylinders associated with TAOs
#
# original code named the clips using and extension of ".lda.lda" so the CSV files
# don't have a valid identifier and the clip files are all named with the double extension.
#
# this code fixed the metric files
#

# read in the list of project folders
dirList <- "H:/T3_DroneLidar/dirlist.txt"
dirs <- read.csv2(dirList, header = FALSE)

# fix backslashes
dirs <- lapply(dirs[,1], function(x) {gsub("\\\\", "/", x)})

# loop through folders and do processing.
#
# this loop should start at 1 unless processing was interrupted by a reboot.
#i <- 2
for (i in 1:length(dirs)) {
  outputFolder <- paste0(dirs[i], "/Processing")

  metricsFile <- paste0(outputFolder, "/SmallCylinderPts_GroundBiased_metrics.csv")
  newMetricsFile <- paste0(outputFolder, "/SmallCylinderPts_GroundBiased_metrics_Corrected.csv")

  # read metrics
  metrics <- read.csv(metricsFile, stringsAsFactors = FALSE)

  # parse DataFile and drop double extension
  metrics$t1 <- gsub(".lda.lda", "", metrics$DataFile)

  # create identifier (basin number)...last 7 digits
  metrics$t2 <- as.integer(substring(metrics$t1, nchar(metrics$t1) - 7 + 1))
  metrics$Identifier <- metrics$t2

  # fix DataFile
  metrics$DataFile <- paste0(metrics$t1, ".lda")

  # fix FileTitle
  metrics$FileTitle <- gsub(".lda", "", metrics$FileTitle)

  # write new metrics
  write.csv(metrics[, c(1:86)], file = newMetricsFile, row.names = FALSE)

  # create batch file to rename clips
  metrics$t3 <- "REN"
  metrics$t4 <- paste0(metrics$FileTitle, ".lda.lda")
  metrics$t5 <- paste0(metrics$FileTitle, ".lda")
  write.table("H:", file = paste0(outputFolder, "/Trees/SmallCylinderPts_GroundBiased/renameClips.bat"), append = FALSE, quote = FALSE, row.names = FALSE, col.names = FALSE)
  write.table(paste0("cd ", outputFolder, "/Trees/SmallCylinderPts_GroundBiased"), file = paste0(outputFolder, "/Trees/SmallCylinderPts_GroundBiased/renameClips.bat"), append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
  write.table(metrics[, c(89, 90, 91)], file = paste0(outputFolder, "/Trees/SmallCylinderPts_GroundBiased/renameClips.bat"), append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)

  # run the batch file...show.output.on.console must be set to TRUE or it doesn't seem to run the commands
  system(paste0(outputFolder, "/Trees/SmallCylinderPts_GroundBiased/renameClips.bat"), show.output.on.console = TRUE)
}
