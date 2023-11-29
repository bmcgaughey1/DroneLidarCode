# processing for 2021 drone lidar data
# these data have a different folder structure than the data from 2022

library(fusionwrapr)
library(sf)
library(raster)

renameCheck <- function(from = "", to = "") {
  if (file.rename(from, to) == FALSE)
    cat("Move failed for: ", from, "\n")
}

# projection for for UTM10...this is used for several outputs
prjFile <- "H:/T3_DroneLidar/UTM10.prj"

# read in the list of project folders...just using a single project area
#dirList <- "H:/T3_DroneLidar/dirlistNEW.txt"
dirs <- "H:/T3_DroneLidar/Ba/Plot37"

# create subfolders under project area for testing files.
for (i in 1:length(dirs)) {
  verifyFolder(paste0(dirs[i], "/Testing"))
  verifyFolder(paste0(dirs[i], "/Testing/CHM"))
  verifyFolder(paste0(dirs[i], "/Testing/TAO"))
}

CHMres <- c(
  0.1,
  0.2,
  0.3,
  0.4,
  0.5,
  0.6,
  0.7,
  0.8,
  0.9,
  1.0
)

CHMreslabel <- lapply(CHMres, function(x) {gsub("\\.", "p", x)})

# loop through folders and do processing.
for (i in 1:length(dirs)) {
  # set up folder info
  dataFolder <- dirs[i]
  groundFileSpec <- paste0(dirs[i], "/ground/ground.dtm")

  outputFolder <- paste0(dirs[i], "/Testing")

  # *****************************************************************************
  # *****************************************************************************
  # build CSM and CHM, do segmentation and compute metrics for Tree Approximate Objects (TAOs)
  # *****************************************************************************
  # *****************************************************************************
  # header things for batch file
  batchFile <- paste0(outputFolder, "/DoTesting.bat")

  # set default behavior for commands...basically telling the fusionwrapr package to create a
  # batch file instead of running the commands directly.
  setGlobalCommandOptions(runCmd = FALSE, saveCmd = TRUE, echoCmd = FALSE, cmdFile = batchFile)

  # make sure we have the folder for the batch file
  verifyFolder(dirname(batchFile))

  # write comment...omit the blank line before the comment
  addToCommandFile(paste0("Testing for: ", dataFolder), addLine = FALSE, cmdClear = TRUE)

  # set the log file and clear it
  useLogFile(paste0(outputFolder, "/Testing.log"), logClear = TRUE)

  # add comment
  addToCommandFile("Start of testing commands")

  # do various resolutions
  for (j in 1:length(CHMres)) {
    # create CSM and CHM with smoothing
    CanopyModel(paste0(outputFolder, "/CHM/CHM_Smooth_", CHMreslabel[j], "m.dtm")
                , CHMres[j]
                , "M"
                , "M"
                , 1
                , 10
                , 2
                , 2
                , paste0(dataFolder, "/*.laz")
                , ground = groundFileSpec
                , smooth = 3
                , class = "~7,18"
    )

    # run segmentation to produce normalized TAO clips
    # omit the ground points (class 2)
    TreeSeg(paste0(outputFolder, "/CHM/CHM_Smooth_", CHMreslabel[j], "m.dtm")
            , 2
            , paste0(outputFolder, "/TAO/trees_Smooth_", CHMreslabel[j], "m.csv")
            , shape = TRUE
            , ground = groundFileSpec
            , projection = prjFile
    )

    # create CSM and CHM without smoothing
    CanopyModel(paste0(outputFolder, "/CHM/CHM_NOsmooth_", CHMreslabel[j], "m.dtm")
                , CHMres[j]
                , "M"
                , "M"
                , 1
                , 10
                , 2
                , 2
                , paste0(dataFolder, "/*.laz")
                , ground = groundFileSpec
                , class = "~7,18"
    )

    # run segmentation to produce normalized TAO clips
    # omit the ground points (class 2)
    TreeSeg(paste0(outputFolder, "/CHM/CHM_NOsmooth_", CHMreslabel[j], "m.dtm")
            , 2
            , paste0(outputFolder, "/TAO/trees_NOsmooth_", CHMreslabel[j], "m.csv")
            , shape = TRUE
            , ground = groundFileSpec
            , projection = prjFile
    )
  }
}

useLogFile("")

# run the batch file
#runCommandFile()

# look through the results
for (i in 1:length(dirs)) {
  # set up folder info
  dataFolder <- dirs[i]
  outputFolder <- paste0(dirs[i], "/Testing")

  Smoothsum <- data.frame(label = "", Count = rep(0, length(CHMreslabel)))
  NOsmoothsum <- data.frame(label = "", Count = rep(0, length(CHMreslabel)))
  # do various resolutions
  for (j in 1:length(CHMres)) {
    # read basin stats
    TAOSmooth <- read.csv(paste0(outputFolder, "/TAO/trees_Smooth_", CHMreslabel[j], "m_Basin_Stats.csv"))
    TAONOsmooth <- read.csv(paste0(outputFolder, "/TAO/trees_NOsmooth_", CHMreslabel[j], "m_Basin_Stats.csv"))

    Smoothsum$Count[j] <- nrow(TAOSmooth)
    NOsmoothsum$Count[j] <- nrow(TAONOsmooth)

    Smoothsum$label[j] <- CHMres[j]
    NOsmoothsum$label[j] <- CHMres[j]
  }
}

dat <- Smoothsum
dat$NSCount <- NOsmoothsum$Count

plot(Smoothsum$label, Smoothsum$Count)
plot(NOsmoothsum$label, NOsmoothsum$Count)

Smoothsum$diff[1] <- 0
NOsmoothsum$diff[1] <- 0
for (i in 2:nrow(Smoothsum)) {
  Smoothsum$diff[i] <- Smoothsum$Count[i-1] - Smoothsum$Count[i]
  NOsmoothsum$diff[i] <- NOsmoothsum$Count[i-1] - NOsmoothsum$Count[i]
}

plot(Smoothsum$label, Smoothsum$diff)
plot(NOsmoothsum$label, NOsmoothsum$diff)

# test code for a single resolution
j <- 5
CHM <- readDTM(paste0(outputFolder, "/CHM/CHM_Smooth_", CHMreslabel[j], "m.dtm"), type = "terra", epsg = 26910)
plot(CHM, xlim = c(413000, 413100), ylim = c(5280900, 5281000), col = rainbow(6, rev = TRUE))

slope <- terrain(CHM, "slope", unit="radians")
aspect <- terrain(CHM, "aspect", unit="radians")
hill <- terra::shade(slope, aspect, 40, 270)
plot(hill, col = grey(0:100/100), legend = FALSE, xlim = c(413000, 413100), ylim = c(5280900, 5281000), maxcell = Inf, smooth = TRUE)
plot(CHM, col = rainbow(6, rev = TRUE, alpha=0.35), legend = TRUE, add=TRUE)




TAO <- terra::vect(paste0(outputFolder, "/TAO/trees_Smooth_", CHMreslabel[j], "m_Polygons.shp"))
TAOpts <- terra::vect(paste0(outputFolder, "/TAO/trees_Smooth_", CHMreslabel[j], "m_HighPoints.shp"))
terra::polys(TAO, border = "white")
terra::expanse(CHM)





# generate plots for all resolutions
for (i in 1:length(dirs)) {
  # set up folder info
  dataFolder <- dirs[i]
  outputFolder <- paste0(dirs[i], "/Testing")

  # do various resolutions
  for (j in 1:length(CHMres)) {
    doTreePlot(CHMreslabel[j], "Smooth", 0.4, outputFolder)
    doTreePlot(CHMreslabel[j], "NOsmooth", 0.4, outputFolder)
  }
}



library(terra)
library(tidyterra)
library(ggplot2)
library(ggspatial)
library(dplyr)
library(scales)
library(viridis)

alpha <- 0.4

# note that this function clips data using extent specific to plot 37 (doesn't cover the plot)
doTreePlot <- function(
    reslabel,
    prefix,
    alpha,
    outputFolder
    )
{
# load data
  CHM <- readDTM(paste0(outputFolder, "/CHM/CHM_", prefix, "_", reslabel, "m.dtm"), type = "terra", epsg = 26910)
  TAO <- terra::vect(paste0(outputFolder, "/TAO/trees_", prefix, "_", reslabel, "m_Polygons.shp"))
  TAOpts <- terra::vect(paste0(outputFolder, "/TAO/trees_", prefix, "_", reslabel, "m_HighPoints.shp"))

  slope <- terrain(CHM, "slope", unit="radians", neighbors = 4)
  aspect <- terrain(CHM, "aspect", unit="radians", neighbors = 4)
  hill <- shade(slope, aspect, 40, 270)

  # get extent of CHM and build a clipping region centered on extent and covering 1ha
  e <- as.vector(ext(CHM))
  attr(e, "names") <- NULL
  midx <- (e[1] + e[2]) / 2
  midy <- (e[3] + e[4]) / 2

  # crop hillshade to 1ha...if you use a cropped CHM, you get a 1-cell border with no data in the hillshade
  hill <- crop(hill, ext(midx - 50, midx + 50, midy - 50, midy + 50))
  CHM <- crop(CHM, ext(midx - 50, midx + 50, midy - 50, midy + 50))

  TAO <- crop(TAO, ext(midx - 50, midx + 50, midy - 50, midy + 50))
  TAOpts <- crop(TAOpts, ext(midx - 50, midx + 50, midy - 50, midy + 50))

  names(hill) <- "shades"

  # Hillshading, but we need a palette
  pal_greys <- hcl.colors(1000, "Grays")

  # Use a vector of colors
  index <- hill %>%
    mutate(index_col = rescale(shades, to = c(1, length(pal_greys)))) %>%
    mutate(index_col = round(index_col)) %>%
    pull(index_col)

  # Get cols
  vector_cols <- pal_greys[index]

  # Need to avoid resampling
  # and dont use aes
  hill_plot <- ggplot() +
    geom_spatraster(
      data = hill, fill = vector_cols, maxcell = Inf,
      interpolate = TRUE,
      alpha = 1
    ) +
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0.05,0)) +
    coord_sf(datum = st_crs(26910))

  #hill_plot

  r_limits <- minmax(CHM) %>% as.vector()

  base_plot <- hill_plot +
    # Avoid resampling with maxcell
    geom_spatraster(data = CHM, maxcell = Inf, interpolate = TRUE) +
    annotation_scale(location = 'bl', text_cex = 1) +
    coord_sf(datum = st_crs(26910)) +
    scale_fill_viridis(option = "H", alpha = alpha)

  #base_plot

  # add polygons
  poly_plot <- base_plot +
    geom_spatvector(color = "black", fill = NA, linewidth = 0.75, data = TAO) +
    geom_spatvector(color = "black", fill = NA, linewidth = 0.75, data = TAOpts) +
    coord_sf(datum = st_crs(26910))

  #poly_plot

  # Theming
  myload_fonts("Noto Serif", "notoserif", "G:/R_Stuff/googlefonts", download = FALSE)
  showtext::showtext_auto()

  # Adjust text size
  base_text_size <- 20

  p <- poly_plot +
    # Change guide
    guides(fill = guide_legend(
      title = "   Height (m)  ",
      direction = "horizontal",
      nrow = 1,
      keywidth = 3,
      keyheight = 0.5,
      label.position = "bottom",
      title.position = "left",
      override.aes = list(alpha = alpha)
    )) +
    theme_minimal(base_family = "notoserif") +
    theme(
      plot.background = element_rect("white", colour = NA),
      plot.margin = margin(0, 0, 0, 0),
      plot.caption = element_text(size = base_text_size * 0.5),
      plot.title = element_text(face = "bold", size = base_text_size * 1.4),
      plot.subtitle = element_text(
        margin = margin(b = 10),
        size = base_text_size
      ),
      axis.text = element_text(size = base_text_size * 0.7),
      legend.position = "bottom",
      legend.title = element_text(size = base_text_size * 0.8),
      legend.text = element_text(size = base_text_size * 0.8),
      legend.key = element_rect("white"),
      legend.spacing.x = unit(0, "pt"),
      axis.text.x=element_blank(), #remove x axis labels
      axis.ticks.x=element_blank(), #remove x axis ticks
      axis.text.y=element_blank(),  #remove y axis labels
      axis.ticks.y=element_blank()  #remove y axis ticks
      ) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

  ggsave(paste0(outputFolder, "/Image_", prefix, "_", CHMreslabel[j], "m.pdf")
         , p
         , width = 7.5, height = 7.5, units = "in")
}

myload_fonts <- function(fontname, family,
                         fontdir = tempdir(),
                         download = TRUE) {
  fontname_url <- utils::URLencode(fontname)
  fontzip <- tempfile(fileext = ".zip")
  if (download) {
    download.file(paste0("https://fonts.google.com/download?family=", fontname_url),
                  fontzip,
                  quiet = TRUE,
                  mode = "wb"
    )
    unzip(fontzip,
          exdir = fontdir,
          junkpaths = TRUE
    )
  }

  # Load fonts
  paths <- list(
    regular = "Regular.ttf",
    bold = "Bold.ttf",
    italic = "Italic.ttf",
    bolditalic = "BoldItalic.ttf"
  )


  namefile <- gsub(" ", "", fontname)
  paths_end <- file.path(
    fontdir,
    paste(namefile, paths, sep = "-")
  )


  names(paths_end) <- names(paths)

  sysfonts::font_add(family,
                     regular = paths_end["regular"],
                     bold = paths_end["bold"],
                     italic = paths_end["italic"],
                     bolditalic = paths_end["bolditalic"]
  )

  return(invisible())
}

