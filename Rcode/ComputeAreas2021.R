library(fusionwrapr)
library(sf)
library(raster)

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

# read ground models in dtm format and compute areas
t <- data.frame(dir = unlist(dirs), area = 0)
for (i in 1:length(dirs)) {
  grnd <- readDTM(paste0(dirs[i], "/ground/ground.dtm"), type = "terra", epsg = 26910)

  t$area[i] <- terra::expanse(grnd, transform = FALSE)[2] / 10000
}
