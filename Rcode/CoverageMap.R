# code to display map of drone lidar areas
# uses tile index files

library(sf)
library(mapview)

# read index files
Index2022SpringSHP <- "D:/2022SpringData/DroneIndex2022_Spring.shp"
Index2021SHP <- "D:/T3_DroneLidar/DroneIndex.shp"
Index2022SummerSHP <- "D:/ONRC_DroneLidar/DroneIndex2022.shp"

Index2022Spring <- st_read(Index2022SpringSHP)
Index2021 <- st_read(Index2021SHP)
Index2022Summer <- st_read(Index2022SummerSHP)

# get riparian areas from summer 2022
Index2022Riparian <- Index2022Summer[grep("Riparian", Index2022Summer$FileNam),]
Index2022NonRiparian <- Index2022Summer[grep("Riparian", Index2022Summer$FileNam, invert = TRUE),]

# map
mapview(Index2021, col.regions = "red") +
mapview(Index2022Spring, col.regions = "green") +
mapview(Index2022NonRiparian, col.regions = "magenta") +
mapview(Index2022Riparian, col.regions = "blue")
