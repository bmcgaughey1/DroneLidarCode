
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
drive and available to project collaborators. Samples of the data for
specific field plots are available
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
want to move all o the analysis code into this repository, I am still
organizing and documenting code so not everything is ready to move into
the repository. Each of the code files are described below.

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
-   [Tree measurement data](2021_T3_Upland_Trees.xlsx)

## Installation

There is no formal install process related to this code. However, you
can simply clone the repository to your local computer. The easiest way
to do this is to click the button labeled “\<\> Code” on the repository
home page. Then click “download ZIP”. Once on your local computer, you
can unzip the repository into a folder of your choosing.
