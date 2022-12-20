# code to create index files for ONRC drone lidar data
#
# everything below this point relies on having some other code (functions) loaded but
# they are part of a really messy code set
library(lidR)
library(sf)
library(stringr)
library(mapview)

# *****************************************************************************
# Functions
# *****************************************************************************
# this function is needed to read unsigned long data types
unsignedFourByteIntToDouble <- function(i) {
  d <- as.numeric(i)
  d[d<0] <- d[d<0] + 2^32
  d
}

ReadLASProjection <- function (
    url,
    quiet = FALSE
) {
  crs <- NA

  if (file.exists(url)) {
    # read header
    t <- tryCatch(lidR::readLASheader(url), error = function(e) {NA})
    if (is.object(t)) {
      crs <- projection(t)

      # check for bad crs but geotiff VLR records
      if (is.na(crs)) {
        flag <- tryCatch(grepl("NAD_1983_USFS_R6_Albers", t@VLR$GeoAsciiParamsTag$tags[1]), error = function(e) {NA})
        if (length(flag)) {
          if (!is.na(flag)) {
            if (flag)
              crs <- R6Albers
          }
        }
      }
    } else {
      if (!quiet) cat("Invalid LAS/LAZ file:", url, "\n")
    }
  } else {
    if (!quiet) cat("File does not exist:", url, "\n")
  }

  if (!quiet) cat(basename(url), ":", crs, "\n")

  return(crs)
}

ReadLASHeader <- function(
    url,
    quiet = FALSE
) {
  # open file and read header...value by value
  con = file(url, open = "rb")
  Signaturebytes <- readBin(con, "raw", n = 4, size = 1, endian = "little")

  Signature <- readBin(Signaturebytes, "character", size = 4, endian = "little")
  if (Signature == "LASF") {
    readBin(con, "raw", 4) # skip bytes
    readBin(con, "raw", 16) # skip bytes
    VersionMajor <- readBin(con, "integer", size = 1, n = 1, signed = FALSE)
    VersionMinor <- readBin(con, "integer", size = 1, n = 1, signed = FALSE)
    readBin(con, "raw", 64) # skip bytes
    DayOfYear <- readBin(con, "int", n = 1, size = 2, signed = FALSE)
    Year <- readBin(con, "integer", n = 1, size = 2, signed = FALSE)
    HeaderSize <- readBin(con, "integer", n = 1, size = 2, signed = FALSE)
    readBin(con, "raw", 4) # skip bytes
    VLRCount <- readBin(con, "integer", n = 1, size = 4)
    VLRCount <- unsignedFourByteIntToDouble(VLRCount)
    PointRecordFormat <- readBin(con, "integer", n = 1, size = 1, signed = FALSE)
    if (PointRecordFormat > 127) PointRecordFormat <- (PointRecordFormat - 128)
    PointRecordLength <- readBin(con, "int", 1, size = 2, signed = FALSE)
    PointCount <- readBin(con, "integer", 1, size = 4)
    PointCount <- unsignedFourByteIntToDouble(PointCount)
    readBin(con, "raw", 68) # skip bytes
    MaxX <- readBin(con, "numeric", 1, size = 8)
    MinX <- readBin(con, "numeric", 1, size = 8)
    MaxY <- readBin(con, "numeric", 1, size = 8)
    MinY <- readBin(con, "numeric", 1, size = 8)
    MaxZ <- readBin(con, "numeric", 1, size = 8)
    MinZ <- readBin(con, "numeric", 1, size = 8)
    if (VersionMajor == 1 && VersionMinor > 3) {
      readBin(con, "raw", 20) # skip bytes
      PointCount <- readBin(con, "integer", 1, size = 8)
    }

    if (!quiet)
      cat("Read extent of", basename(url), "\n")
  } else {
    VersionMajor <- NA
    VersionMinor <- NA
    DayOfYear <- NA
    Year <- NA
    HeaderSize <- NA
    VLRCount <- NA
    PointRecordFormat <- NA
    PointRecordLength <- NA
    PointCount <- NA
    MaxX <- NA
    MinX <- NA
    MaxY <- NA
    MinY <- NA
    MaxZ <- NA
    MinZ <- NA

    if (!quiet)
      cat("Failed to read extent of", basename(url), "\n")
  }
  close(con)

  # build data frame to return
  df <- data.frame(
    "URL" = url,
    "FileName" = basename(url),
    "LASVersion" = VersionMajor + VersionMinor / 10,
    "FileDayOfYear" = DayOfYear,
    "FileYear" = Year,
    "HeaderSize" = HeaderSize,
    "VLRCount" = VLRCount,
    "PointRecordFormat" = PointRecordFormat,
    "PointRecordLength" = PointRecordLength,
    "PointCount" = PointCount,
    "MinX" = MinX,
    "MinY" = MinY,
    "MinZ" = MinZ,
    "MaxX" = MaxX,
    "MaxY" = MaxY,
    "MaxZ" = MaxZ
  )

  return(df)
}

# optional: provide a list of files (complete path) in fullFileList
BuildIndexFromPoints <- function (
    baseURL,
    folderName,
    pointFolder,
    outputFile,
    projString = NA,
    outputCRS = NA,
    fileType = "\\.las|\\.laz",
    fullFileList = character(0),
    dimensionThreshold = 50000,
    rebuild = FALSE,
    quiet = FALSE
) {
  if (file.exists(outputFile) && !rebuild) {
    cat("Index already exist...skipping: ", basename(outputFile),"\n")
    return(TRUE);
  }

  if (pointFolder == "") {
    folderURL <- paste0(baseURL, "/", folderName)
  } else {
    folderURL <- paste0(baseURL, "/", folderName, "/", pointFolder)
  }

  # we only need to get a list of files if fullFileList == NA
  if (length(fullFileList) == 0) {
    # get list of .laz & .las files...full directory info
    flist <- DirList(folderURL, fileType = fileType)
  } else {
    flist <- fullFileList
  }

  if (length(flist) > 0) {
    cat("Building index: ", basename(outputFile), "\n")

    if (length(fullFileList) == 0) {
      # prepend folder path to file names
      fileURLs <- paste0(folderURL, "/", flist)
    } else {
      fileURLs <- flist
    }

    # if we don't have projection info, try to get it from the first point file
    if (is.na(projString)) {
      c <- ReadLASProjection(fileURLs[1])
      projString <- crs(c, asText = TRUE)
    }

    # read headers
    t <- lapply(fileURLs, ReadLASHeader)    # returns a list of dataframes

    # convert to a simple dataframe
    t_df <- do.call("rbind", t)

    # drop any rows with NA values...bad LAS file...only check min/max XYZ values
    t_df <- t_df[complete.cases(t_df[, 11:16]), ]

    # drop rows where width or height is >dimensionThreshold units
    t_df <- t_df[((t_df$MaxX - t_df$MinX) < dimensionThreshold & (t_df$MaxY - t_df$MinY) < dimensionThreshold), ]

    # create sf set of tile polygons
    if (nrow(t_df) > 0) {
      lst <- lapply(1:nrow(t_df), function(x) {
        # create a matrix of coordinates that also 'close' the polygon
        res <- matrix(c(t_df[x, 'MinX'], t_df[x, 'MinY'],
                        t_df[x, 'MinX'], t_df[x, 'MaxY'],
                        t_df[x, 'MaxX'], t_df[x, 'MaxY'],
                        t_df[x, 'MaxX'], t_df[x, 'MinY'],
                        t_df[x, 'MinX'], t_df[x, 'MinY'])  ## need to close the polygon
                      , ncol =2, byrow = TRUE
        )
        # create polygon objects
        st_polygon(list(res))
      }
      )

      tiles_sf <- st_sf(t_df, st_sfc(lst), crs = projString)

      # reproject to outputCRS
      if (!is.na(projString) && !is.na(outputCRS)) {
        tiles_sf <- st_transform(tiles_sf, crs = outputCRS)
      }

      # write output
      st_write(tiles_sf, outputFile, delete_dsn = TRUE, quiet = TRUE)

      return(TRUE)
    } else {
      cat("   ***No LAS polygons\n")
    }
  } else {
    cat("   ***No LAS files\n")
  }

  return(FALSE)
}

# syntax for fileType can be any valid grep pattern but the "$" will be appended to
# search for entries ending with the pattern
# e.g. "\\.las|\\.laz" will return all las and laz files
DirList <- function (
    URL,
    fileType = NULL,
    namesOnly = TRUE,
    directoryOnly = FALSE,
    ...
) {
  # get folder listing and parse into individual files
  # create an empty character vector on error
  if (directoryOnly) {
    filenames <- list.dirs(URL, full.names = FALSE, recursive = FALSE)
  } else {
    filenames <- list.files(URL, pattern = fileType)
  }

  if (length(filenames)) {
    # if not getting directories, get file size and sort
    if (!directoryOnly) {
      # create a dataframe and sort on the attribute (size for files)
      if (!namesOnly) {
        # build full paths to filenames
        tnames <- paste0(URL, "/", filenames)

        # get file info
        df <- file.info(tnames, extra_cols = FALSE)

        # add filenames
        df$FileName <- filenames

        # drop row labels
        rownames(df) <- NULL

        df <- df[, c(7, 1:6)]

        return(df)
      }
    }
  }
  return(filenames)
}

DirListByType <- function (
    URL,
    fileType,
    ...
) {
  # get folder listing and parse into individual files
  # create an empty character vector on error
  filenames <- list.files(URL, pattern = fileType)
  return(filenames)
}

DirListByName <- function (
    URL,
    fileName,
    ...
) {
  filenames <- list.files(paste0(URL), pattern = paste0(fileName, ".*"))
  return(filenames)
}

# *****************************************************************************
# *****************************************************************************
# *****************************************************************************
# *****************************************************************************
# read list of data foldes
dirList <- "D:/ONRC_DroneLidar/dirlist.txt"
folders <- read.csv2(dirList, header = FALSE)

# fix backslashes
folders <- lapply(folders[,1], function(x) {gsub("\\\\", "/", x)})

# get all the LAZ files in all folders
LASfiles <- Sys.glob(file.path(folders, "*.laz"))

# create an index using the footprints for all LAZ files
# uses functions from BuildR6ServerIndex.R
BuildIndexFromPoints("", "", "", "D:/ONRC_DroneLidar/DroneIndex2022.gpkg", projString = 26910, fullFileList = LASfiles, rebuild = TRUE)

# read and display along with units
index <- st_read("D:/ONRC_DroneLidar/DroneIndex2022.gpkg")
units <- st_read("D:/T3_GIS/Final_layout/Units_UTM10.shp")
mapview(list(index, units))

# write shapefile...field names are changed to make Arc happy
st_write(index, "D:/ONRC_DroneLidar/DroneIndex2022.shp")
