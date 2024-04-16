# code for canopy base height
#
# runs using a point file that represents an individual tree
# assumes the point file has been normalized relative to ground
#
# this code works but I don't think you have to normalize the percentile
# heights. I tested without normalization and get the same results for
# the few trees I tested. However, there may be issues when using with
# trees of various heights.
#
# CBH function
#
# file character Filename for tree points in LAS format
# z matrix or dataframe Z values for points associated with a single tree
# normalizePercentile int percentile height used to normalize percentiles. Good
# values are 99 or 100 but 100 may not be the best due to outlier points or points
# from branches on adjacent trees.
#
# return is an invisible list with 2 elements:
#   CanopyBaseHeight: Height for detected canopy base
#   CBHPercentile: Percentile for detected canopy base (1-100)
#
# You should assign the return value to a variable to access the list items.
#
CBH <- function (
    file = "",
    z = NULL,
    normalizePercentile = 99
) {
  if (file == "" && is.null(z))
    stop("You must specify either file or z")

  # see if we have a file
  if (file != "") {
    # make sure file exists
    if (!file.exists(file))
      stop(paste0("File: ", file, " doesn't exist"))

    # check normalizePercentile
    if (normalizePercentile > 100 || normalizePercentile < 1)
      stop("Invalid range for normalizePercentile (1-100)")

    # read points
    LAS <- readLAS(file)

    z <- LAS$Z
  }

  # compute the percentile data
  PercentileData <- quantile(z, seq(0, 1, 0.01))

  # wipe out the names
  names(PercentileData) <- NULL

  # normalize percentile heights...99th percentile is in column 100
  NormalizedPercentileData <- PercentileData / PercentileData[normalizePercentile + 1]

  # compute slopes using normalized percentile data...ignore the 1st percentile so we
  # don't include ground points. However, on steep slopes, we may need to ignore more
  # points close to the ground.
  x <- c(0:normalizePercentile)
  slopes <- vector()
  for (j in 2:(normalizePercentile + 1)) {
    x1 <- x[j - 1]
    x2 <- x[j]
    y1 <- NormalizedPercentileData[j -1]
    y2 <- NormalizedPercentileData[j]
    slope_i <- (y2 - y1) / (x2 - x1)
    slopes <- append(slopes, slope_i)
  }

  # get max slope and scale back to actual height from original PercentileData
  maxSlopeIndex <- which.max(slopes)
  cbhIndex <- maxSlopeIndex + 1
  cbh <- PercentileData[cbhIndex]

  return(invisible(list(
    "CanopyBaseHeight" = cbh,
    "CBHPercentile" = cbhIndex
  )))
}

# testing code
if (F) {
  library(fusionwrapr)

  t <- CBH("extras/trees_normalized_Clip_0000011.las")
  t

  # read file and pass z values
  LAS <- readLAS("extras/trees_normalized_Clip_0000011.las")

  t <- CBH(z = LAS$Z)
  t
}
