
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DroneLidarCode

<!-- badges: start -->
<!-- badges: end -->

DroneLidarCode is a repository to share analysis and processing code
related to the T3 Upland Silviculture Study. The T3 study is taking
place on the Olympic Peninsula in Washington state. The study is a joint
effort of the Washington Department of Natural Resources (WADNR) and the
University of Washington’s Olympic Natural Resources Center (ONRC).

Lidar data was collected from a UAS platform in the summers of 2021 and
2022 over specific study areas. In addition, field measurements on plots
within the study areas were also collected. The data are high density
(over 800 pulses per square meter) and provide highly detailed
information about the upper- and mid-canopy vegetation. The point cloud
data is not available in the repository but is accessible on a shared
drive and available to project collaborators.

## Lidar data

Samples of the data for specific field plots are available
[here](http://forsys.sefs.uw.edu/transfer/T3Plots/). This
[document](extras/T3_drone_lidar_plots.pdf) has some instructions for
using the plot data in FUSION to visualize the point cloud and measured
trees.

![Point cloud data showing young trees with scattered older
trees](extras/lidardata.png?raw=true)

This repository contains the field data and analysis code used to
develop useful products from the lidar data and to explore some specific
research topics such as distinguishing Douglas-fir (Pseudotsuga
menziesii) from western hemlock (Tsuga heterophylla) using upper crown
morphology and intensity values.

While not specifically intended for broad distribution, the code is
available in this public repository for all to enjoy!!

## R code

All R code files are located in the Rcode folder. While I eventually
want to move all of the analysis code into this repository, I am still
organizing and documenting code so not everything is ready to move into
the repository. Most of this code has some file paths and file names
that are specific to my computer. For the most part, such things are
near the beginning of the code and can be easily changed to match your
own computer and drive letters. Each of the code files are described
below.

### [Process2022Data.R](Rcode/Process2022Data.R)

This code takes the drone lidar data flow in 2022 as input and produces
a set of outputs that includes the canopy height and surface models,
individual tree related products (high point locations, crown perimeter
polygons, point clips for each tree), and point clips for the upper 3m
of the tree crowns used for species classification work using random
forest. This code uses the
[fusionwrapr](https://github.com/bmcgaughey1/fusionwrapr) package to
interface with FUSION command line tools so you will need a full working
installation of FUSION to use this code. For the 2022 data, this code
takes several days to run on a moderately powerful laptop. When I
processed the 2021 data, I was running on a desktop system with better
performance. The 2021 data was smaller and processed in less than 1 day.

### [speciesModeling.R](Rcode/speciesModeling.R)

This code uses the lidar-derived metrics (2021 lidar and 2021 plots) for
the upper 3m of the individual tree crowns (matched to field-measured
trees) to build a classification model using random forest (RF) to
distinguish Douglas-fir from western hemlock. In the code, there are
some options for tuning the hyperparameters in random forest. The simple
tuning only works on the mtry parameter. The other tuning deals with
mtry, ntrees, and nodesize. Be warned that the more complex tuning takes
MUCH longer to run. This files also has some code to produce plots of
the most useful metrics, based on importance scores from RF, and to
craete a list of point clips for misclassified individaul trees. This
last product is useful for understanding what causes classification
errors.

### [IndexDroneLidar2022.R](Rcode/IndexDroneLidar2022.R)

This code creates an index of all of the drone data flown in 2022 by
reading the extent of the individual point files. This code includes
some functions that read LAS/LAZ file headers directly (byte values) and
functions to build directory listings based on a wildcard specification,
e.g. “\*.laz”. Output is a geopackage file and a shapefile with all tile
extents. Some field names are truncated in the shapefile to comply with
limitations of the shapefile format (and ESRI’s implementation of the
format).

### [predictDBH_Height.R](Rcode/predictDBH_Height.R)

This code implements functions to predict DBH or height using equations
from FVS (Pacific coast variant) or

> Hanus, M.L., D.D. Marshall, and D.W. Hann. 1999. Height-diameter
> equations for six species in the coastal regions of the Pacific
> Northwest. Forestry Research Laboratory, contribution 25. 11 p.

Note that there are functions in the code that have very specific
requirements for the units of measurement for DBH and height (both input
and output). These were the original functions that I coded to test the
Hanus, et al. and FVS equations. I added over-arching functions that
allow you to use both methods (methods = “hanus” or method = “fvs”). Use
these functions instead of the original ones!!

### [HeightDBHModel.R](Rcode/HeightDBHModel.R)

Code the explores the height-DBH relationship using the field-measured
DBH and lidar-derived height. The Hanus et al. and FVS methods vary
somewhat but there isn’t really a clear choice for our data. I also fit
new equations using the same form as Hanus et al. and our data. This new
model is better. The Hanus et al. and FVS methods fit equations that
predict height given DBH (since this is typically measured in the
field). I also fit equations to directly predict DBH given our
lidar-derived heights. These models are even better and produce less
biased DBH predictions. We had a reviewer comment regarding the process
of rearranging a height prediction equation to predict DBH on the wind
river draft. At the time, I kind of ignored the comment given that the
only way to “do better” involves field data (which we don’t have for the
wind river site). Given that we have field measurements of DBH nd
lidar-derived heights, it makes sense to fit our own model and then use
it to predict DBH from the lidar-derived heights.

This graph shows the Hanus et al. and FVS predictions and how they
relate to our data.

![Predicted DBH values given our lidar-derived
heights](extras/DBH_Ht.png?raw=true)

These next two graphs show new equations fit using the Hanus et
al. equation form and our data. The first shows predictions from an
equation fit to predict height (which can be rearranged to predict DBH)
and the second shows predictions from an equation fit to predict DBH
given height. The axes on the first graph are somewhat unconventional in
that the predicted value is still on the x axis so it matches the other
graphs.

![Field data and DBH predictions using an equation fit to predict
height](extras/predictHt.png?raw=true)

![Field data and DBH predictions using an equation fit to predict DBH
directly](extras/predictDBH.png?raw=true)

## Field data

The field data consists of a single excel spreadsheet with all of the
measured trees and a shapefile with the plot center locations. Locations
were collected using a Javad Triumph 2 GNSS receiver and post processed
using a nearby base station. With the receiver, we expect locations
within 1-2m of the true location. However, larger errors are possible
given the dense canopy conditions and steep slopes. Plots, except plot
6, are 1/4 acre with a radius of 17.68m. Plot 6 has a radius of 7.8m.

Individual tree locations were computing using the field-measured
distance and azimuth from the plot reference point (usually a single
tree dor each plot). Tree locations were adjusted manually using
specially modified versions of FUSION and LDV. The adjustment
capabilities included the ability to shift the entire plot as well as
individual trees. In addition, tree tops can be moved while keeping the
tree base fixed to allow alignment with the point cloud for leaning
trees.

The following files are available:

-   [Field plot locations](extras/plot_centers_UTM.zip)
-   [T3 treatment units](extras/Units_UTM10.zip)
-   [Tree measurement data](extras/2021_T3_Upland_Trees.xlsx)
-   [Lidar-derived metrics for upper 3m of individual tree crowns used
    to develop the RF classification
    model](extras/AdjustedField_T3_Training_TreeTops_AllPlots.csv)

## Installation

There is no formal install process related to this code. If you want
everything, you can clone the repository to your local computer. The
easiest way to do this is to click the button labeled “\<\> Code” on the
repository home page. Then click “download ZIP”. Once on your local
computer, you can unzip the repository into a folder of your choosing.
You can also copy code from individual files into your local RStudio
session. The process for accessing code in individual files (and CSV or
XLSX files) is different depending on the file type. In general, you
always want to copy “raw” text to avoid getting line numbers or other
decoration in the copied text.
