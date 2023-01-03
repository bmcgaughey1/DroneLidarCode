
<!-- This document is generated from HeightDBH.Rmd. Please edit that file -->

# Predicting DBH using lidar-derived heights

This report explores the relationship between DBH and height in our
field data. Initially, we used the equations from

> Hanus, M.L., D.D. Marshall, and D.W. Hann. 1999. Height-diameter
> equations for six species in the coastal regions of the Pacific
> Northwest. Forestry Research Laboratory, contribution 25. 11 p.

to predict DBH given height. This was accomplished by rearranging the
equations presented in Hanus et al. to produce a DBH from height values
(derived from the individual trees identified using the lidar data).
While this seems to be a reasonable approach, the rearranged equations
introduce bias into the predictions and may not be the best
representation of the relationship between height and DBH. We had a
reviewer for the wind river draft raise this issue but I pretty much
ignored the comment since we didn’t have a better solution at the time.
I also looked into the equations used in the Pacific Coast variant of
FVS as a possible solution. The FVS equations are documented in the
variant description and ar specific to geographic regions within the
area covered by the variant. The FVS code provides functions that give
DBH or height but the equations and coefficients are the same.
Basically, an equation was fit describing the relationship between DBH
and height (not sure which was used for fitting) and then rearranged to
solve for the “other” variable.

I developed R functions to do the predictions using both sets of
equations along with a pair of functions that allow you to select the
method (“hanus” or “fvs”). You should use this pair of functions as they
also handle unit conversions more cleanly than the functions the work
with the individual methods. The code for the prediction functions is in
[predictDBH_Height.R](../Rcode/predictDBH_Height.R).