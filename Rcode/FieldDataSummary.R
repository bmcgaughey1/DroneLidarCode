# code to summarize 2021 field data. used for DF/WH classification paper

# all plots, all trees from 2021. Not all plots have lidar coverage
library(readxl)
library(ggplot2)
fieldDataFilename <- "G:/R_Stuff/ONRCDroneLidar/2021 Upland Tree.xlsx"
sheetname <- "Compiled_Data"
allTrees <- read_excel(fieldDataFilename, sheet = sheetname,
                       col_types = c(rep("guess", 10), "text", "guess", "guess", "guess", "guess", "text"))

allTrees$DBH_cm <- as.integer(allTrees$DBH_cm)

length(unique(allTrees$Plot_Number))
table(allTrees$Species)
summary(allTrees$DBH_cm)
ggplot(data = allTrees, aes(x = DBH_cm, fill = Species)) +
  geom_histogram(colour = 'white', binwidth = 2) +
  xlab("DBH (cm)")

# read all trees on plots covered by lidar data
fieldTrees <- read.csv(file = paste0("G:/R_Stuff/ONRCDroneLidar/", "FieldTrees.csv"), stringsAsFactors = FALSE)

cat("There are data for ", length(unique(fieldTrees$Plot_Number)), "plots\n")
cat("Species summary\n")
table(fieldTrees$Species)
cat("Total number of trees: ", nrow(fieldTrees), "\n")
cat("Number of PSME and TSHE: ", nrow(fieldTrees[fieldTrees$Species == "PSME" | fieldTrees$Species == "TSHE", ]), "\n")
table(fieldTrees$Anomaly1)

library(dplyr)
#psme <- dplyr::filter(fieldTrees, Species == "PSME", Anomaly1 == 0 | Anomaly1 == 2 | Anomaly1 == 6, LiDAR_visible == "Y")
#tshe <- dplyr::filter(fieldTrees, Species == "TSHE", Anomaly1 == 0 | Anomaly1 == 2 | Anomaly1 == 6, LiDAR_visible == "Y")
psme <- dplyr::filter(fieldTrees, Species == "PSME", Anomaly1 == 0 | Anomaly1 == 2, LiDAR_visible == "Y")
tshe <- dplyr::filter(fieldTrees, Species == "TSHE", Anomaly1 == 0 | Anomaly1 == 2, LiDAR_visible == "Y")
#psme <- dplyr::filter(fieldTrees, Species == "PSME", Anomaly1 == 0, LiDAR_visible == "Y")
#tshe <- dplyr::filter(fieldTrees, Species == "TSHE", Anomaly1 == 0, LiDAR_visible == "Y")
candidateTrees <- rbind(psme, tshe)
cat("Total number of candidate trees for matching: ", nrow(candidateTrees), "\n")
cat("DBH summary for candidate trees for matching:\n")
summary(candidateTrees$DBH_cm)
cat("Standard deviation DBH (cm): ", sd(candidateTrees$DBH_cm), "\n")

# read data used to build classification model. should be PSME & TSHE with anomaly1 code of 0 and lidar
# visible flag set to YES
useLeaningTrees <- TRUE

if (useLeaningTrees) {
  inputDataFile <- "extras/Leaning_TreeTops_SmallCylinder_normalized_metrics.csv"
} else {
  inputDataFile <- "extras/AdjustedField_T3_Training_TreeTops_AllPlots.csv"
}

# read data
inputData <- read.csv(inputDataFile, stringsAsFactors = FALSE)

cat("Total number of trees for training model: ", nrow(inputData), "\n")
cat("Species summary\n")
table(inputData$Species)
cat("DBH summary\n")
summary(inputData$DBH_cm)
cat("Standard deviation DBH (cm): ", sd(inputData$DBH_cm), "\n")
hist(inputData$DBH_cm)
# verify that anomaly code is 0 and lidar visible flag is "Y"
table(inputData$Anomaly1)
table(inputData$LiDAR_visible)

# histogram of DBH
library(ggplot2)
ggplot(data = inputData, aes(x = DBH_cm, fill = Species)) +
  geom_histogram(colour = 'white', binwidth = 2, cex_size = 2) +
  xlab("DBH (cm)") +
  ylab("Frequency") +
  scale_fill_discrete(name = "Species", labels = c("Douglas-fir", "Western hemlock")) +
  theme(legend.position = c(0.85, 0.7),
        text = element_text(size=18)
        )




# 10/20/2023 Having trouble resolving how many trees were available for adjustment and how many were actually adjusted.
# Look at the files that Ally used for adjustments and use these numbers
folder <- "G:/R_Stuff/ONRCDroneLidar/AllyPlots/"
f <- read.csv(paste0(folder, "filelist.txt"), header = FALSE)

totalCount <- 0
adjCount <- 0
for (i in 1:nrow(f)) {
  trees <- read.csv(paste0(folder, f$V1[i]))
  totalCount <- totalCount + nrow(trees)
  #adjCount <- adjCount + nrow(trees[trees$Status.Code != 1, ])
  adjCount <- adjCount + nrow(trees[trees$Lean.Angle.From.Vertical != 0, ])
}
cat("Total trees: ", totalCount)
cat("Number of trees adjusted: ", adjCount)

# 1528 total trees
# 744 adjusted trees using status != 1
# 748 adjusted trees using lean angle from vertical != 0


# read Bob's adjustments
trees <- read.csv(paste0(folder, "WORKING_FUSIONTrees.csv"))
cat("Total trees: ", nrow(trees))
cat("Number of trees adjusted: ", nrow(trees[trees$Lean.Angle.From.Vertical != 0, ]))
cat("Number of trees adjusted: ", nrow(trees[trees$Status.Code != 1, ]))
# 1528 total trees
# 744 adjusted trees using status != 1 and lean angle

