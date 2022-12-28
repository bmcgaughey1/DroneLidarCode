# functions to predict DBH and height using equations and coefficients from:
# Hanus, M.L., D.D. Marshall, and D.W. Hann. 1999. Height-diameter equations for six
# species in the coastal regions of the Pacific Northwest.  Forestry Research
# Laboratory, contribution 25.  11 p.

# *****All of these functions should be used to make estimates for individual trees and not lists of trees.
# *****The logic won't use the species information correctly when given a list of species.
# *****This means you need to use a loop to all the functions instead of passing a list of "trees"

# *****Functions have requirements for units for both DBH and height mentioned in the comments. The Hanus
# *****functions have a conversion factor for the input measurement but output will always be in the "native"
# *****units. The FVS function must have DBH in inches and height in feet. This could (and probably will) be
# *****changed in the future so the functions will work correctly and allow specification of the units of
# *****measure through function parameters.

# function to predict height (m) given a DBH (cm)
# DBH can be in inches with conversionFactor = 1.0 but height will still be returned in meters
predictHeight <- function(spp, DBH, conversionFactor = 2.54) {
  # Ht = 4.5 + exp(a0 + a1 * DBH^a2)
  # DBH = (ln((Ht - 4.5) / exp(a0)) / a1)^(1 / a2)
  # Ht is in feet and DBH is in inches...hence the need for the conversion factor
  species <- c("PSME", "ALRU", "PISI", "TSHE", "THPL", "PIMO")
  a0 <- c(7.262195456, 4.41820972,  5.404491308, 6.555344622, 7.232880669, 7.946192109)
  a1 <- c(-5.899759104, -12.00274935, -6.570862442, -5.137174162, -5.746899904, -6.278973035)
  a2 <- c(-0.287207389, -2.13835482,  -0.819705048, -0.3645508,   -0.271564741, -0.208892429)

  # match spp
  sppIndex <- which(species == spp)

  if (length(sppIndex) == 0) {
    message("Invalid species: ", spp, "...must be one of: ", paste(species, collapse = ","), "...assuming PSME")
    sppIndex <- 1
  }

  Ht = 4.5 + exp(a0[sppIndex] + a1[sppIndex] * (DBH / conversionFactor)^(a2[sppIndex]))

  return(Ht / 3.2808)
}

# function to predict DBH (cm) given a height (m)
# height can be in feet with conversionFactor = 1.0 but DBH will still be returned in cm
predictDBH <- function(spp, height, conversionFactor = 3.2808) {
  # Ht = 4.5 + exp(a0 + a1 * DBH^a2)
  # DBH = (ln((Ht - 4.5) / exp(a0)) / a1)^(1 / a2)
  # Ht is in feet and DBH is in inches...hence the need for the conversion factor
  species <- c("PSME", "ALRU", "PISI", "TSHE", "THPL", "PIMO")
  a0 <- c(7.262195456, 4.41820972,  5.404491308, 6.555344622, 7.232880669, 7.946192109)
  a1 <- c(-5.899759104, -12.00274935, -6.570862442, -5.137174162, -5.746899904, -6.278973035)
  a2 <- c(-0.287207389, -2.13835482,  -0.819705048, -0.3645508,   -0.271564741, -0.208892429)

  # match spp
  sppIndex <- which(species == spp)

  if (length(sppIndex) == 0) {
    message("Invalid species: ", spp, "...must be one of: ", paste(species, collapse = ","), "...assuming PSME")
    sppIndex <- 1
  }

  DBH <- (log((height*conversionFactor - 4.5) / exp(a0[sppIndex])) / a1[sppIndex])^(1 / a2[sppIndex])

  return(DBH * 2.54)
}

# function to implement height/dbh prediction logic from Pacific Coast variant of FVS
# FORTRAN code: https://sourceforge.net/p/open-fvs/code/HEAD/tree/FVSmodel/trunk/pn/htdbh.f#l226
#
# In these equations, dbh must be in inches and height must be in feet. any/all unit conversion
# needs to be handled by the calling code
#
# species codes are different for FVS than those used in the Hanus, et al. equations for ALRU and PIMO
#
predictFVS <- function(spp, height = 0, dbh = 0, location = 1, mode = 0) {
  # species list
  species <- c(
    "ABAM", "ABCO", "ABGR", "ABLA", "ABMA", "PISI", "ABPR", "CANO9", "LIDE", "PIEN",
    "PICO", "PIJE", "PILA", "PIMO3", "PIPO", "PSME", "SESE3", "THPL", "TSHE", "TSME",
    "ACMA3","ALRU2", "ALRH2", "BEPA", "CHCHC4", "POTR5", "POBAT", "QUGA4", "JUOC", "LALY",
    "PIAL", "PIAT", "TABR2", "CONU4", "CR__", "PREM", "SA__", "", "OTHER"
  )

  # set up model coefficients
  oly1 <- c(
    697.6316,  604.8450,  356.1148,   89.0298,  202.8860,
    3844.3880,  483.3751, 1220.0963, 4691.6337,  206.3211,
    100.0   , 1031.5203,  702.1856,  433.7807, 1181.7244,
    1091.8526,  595.1068,  665.0944,  609.4235,  170.2653,
    600.0957,  139.4551,  139.4551, 1709.7229,10707.3906,
    1709.7229,  178.6441,   89.4301,  503.6619,  503.6619,
    89.5535,34749.4736,  127.1698,  403.3221,   55.0   ,
    73.3348,  149.5861, 1709.7229, 1709.7229
  )

  oly2 <- c(
    6.6807,    5.9835,    6.4100,    6.9507,    8.7469,
    7.0680,    7.2443,    7.2995,    7.4671,    9.1227,
    6.0   ,    7.6616,    5.7025,    6.3318,    6.6981,
    5.2936,    5.8103,    5.5002,    5.5919,   10.0684,
    3.8297,    4.6989,    4.6989,    5.8887,    8.4670,
    5.8887,    4.5852,    6.6321,    4.9544,    4.9544,
    4.2281,    9.1287,    4.8977,    4.3271,    5.5   ,
    2.6548,    2.4231,    5.8887,    5.8887
  )

  oly3 <- c(
    -0.4161,   -0.3789,   -0.5572,   -0.9871,   -0.8317,
    -0.2122,   -0.5111,   -0.3211,   -0.1989,   -0.8281,
    -0.86  ,   -0.3599,   -0.3798,   -0.4988,   -0.3151,
    -0.2648,   -0.3821,   -0.3246,   -0.3841,   -0.8791,
    -0.2380,   -0.7682,   -0.7682,   -0.2286,   -0.1863,
    -0.2286,   -0.6746,   -0.8876,   -0.2085,   -0.2085,
    -0.6438,   -0.1417,   -0.4668,   -0.2422,   -0.95  ,
    -1.2460,   -0.1800,   -0.2286,   -0.2286
  )

  sius1 <- c(
    697.6316,  604.8450,  432.2186,  133.8689,  202.8860,
    708.7788,  483.3751, 1220.0963, 4691.6337,  206.3211,
    100.0   , 1031.5203,  702.1856,  514.1575, 1181.7244,
    407.1595,  595.1068,  227.1400, 1196.6191,  170.2653,
    92.2964,  254.8634,  254.8634, 1709.7229,10707.3906,
    1709.7229,  178.6441,   89.4301,  503.6619,  503.6619,
    89.5535,34749.4736,  139.0727,  403.3221,   55.0   ,
    73.3348,  149.5861, 1709.7229, 1709.7229
  )

  sius2 <- c(
    6.6807,    5.9835,    6.2941,    6.7798,    8.7469,
    5.7677,    7.2443,    7.2995,    7.4671,    9.1227,
    6.0   ,    7.6616,    5.7025,    6.3004,    6.6981,
    7.2885,    5.8103,    6.1092,    5.7904,   10.0684,
    4.1890,    3.8495,    3.8495,    5.8887,    8.4670,
    5.8887,    4.5852,    6.6321,    4.9544,    4.9544,
    4.2281,    9.1287,    5.2062,    4.3271,    5.5   ,
    2.6548,    2.4231,    5.8887,    5.8887
  )

  sius3 <- c(
    -0.4161,   -0.3789,   -0.5028,   -0.7375,   -0.8317,
    -0.3629,   -0.5111,   -0.3211,   -0.1989,   -0.8281,
    -0.86  ,   -0.3599,   -0.3798,   -0.4651,   -0.3151,
    -0.5908,   -0.3821,   -0.6009,   -0.2906,   -0.8791,
    -0.9830,   -0.4149,   -0.4149,   -0.2286,   -0.1863,
    -0.2286,   -0.6746,   -0.8876,   -0.2085,   -0.2085,
    -0.6438,   -0.1417,   -0.5409,   -0.2422,   -0.95  ,
    -1.2460,   -0.1800,   -0.2286,   -0.2286
  )

  mthd1 <- c(
    223.3492,  475.1698,  432.2186,  290.5142,  375.3820,
    375.3820,  247.7348,  255.4638, 4691.6337,  206.3211,
    139.7159, 1031.5203,  702.1856, 1333.8176, 1181.7244,
    949.1046,  595.1068, 1560.6848,  317.8257, 2478.0988,
    76.5170,  484.4591,  133.7965, 1709.7229,10707.3906,
    1709.7229,  178.6441,   59.4214,  503.6619,  503.6619,
    73.9147,34749.4736,   77.2207,  403.3221,   55.0   ,
    73.3348,  149.5861, 1709.7229, 1709.7229
  )

  mthd2 <- c(
    6.3964,    6.2472,    6.2941,    6.4143,    6.0880,
    6.0880,    6.1830,    5.5577,    7.4671,    9.1227,
    4.0091,    7.6616,    5.7025,    6.6219,    6.6981,
    5.8482,    5.8103,    6.2328,    6.8287,    7.0762,
    2.2107,    4.5713,    6.4050,    5.8887,    8.4670,
    5.8887,    4.5852,    5.3178,    4.9544,    4.9544,
    3.9630,    9.1287,    3.5181,    4.3271,    5.5   ,
    2.6548,    2.4231,    5.8887,    5.8887
  )

  mthd3 <- c(
    -0.6566,   -0.4812,   -0.5028,   -0.4724,   -0.4720,
    -0.4720,   -0.6335,   -0.6054,   -0.1989,   -0.8281,
    -0.7080,   -0.3599,   -0.3798,   -0.3120,   -0.3151,
    -0.3251,   -0.3821,   -0.2541,   -0.6034,   -0.2456,
    -0.6365,   -0.3643,   -0.8329,   -0.2286,   -0.1863,
    -0.2286,   -0.6746,   -1.0367,   -0.2085,   -0.2085,
    -0.8277,   -0.1417,   -0.5894,   -0.2422,   -0.95  ,
    -1.2460,   -0.1800,   -0.2286,   -0.2286
  )

  willam1 <- c(
    237.9189,  475.1698,  432.2186,  133.8689,  375.3820,
    375.3820,  483.3751,   97.7769, 4691.6337,  206.3211,
    105.4453, 1031.5203,  702.1856,  514.1575, 1181.7244,
    439.1195,  595.1068, 1012.1267,  395.4976,  192.9609,
    160.2171,10099.7209,  133.7965, 1709.7229,10707.3906,
    1709.7229,  178.6441,   55.0   ,  503.6619,  503.6619,
    73.9147,34749.4736,  139.0727,  444.5618,   55.0   ,
    73.3348,  149.5861, 1709.7229, 1709.7229
  )

  willam2 <- c(
    7.7948,    6.2472,    6.2941,    6.7798,    6.0880,
    6.0880,    7.2443,    8.8202,    7.4671,    9.1227,
    7.9694,    7.6616,    5.7025,    6.3004,    6.6981,
    5.8176,    5.8103,    6.0957,    6.4222,    7.3876,
    3.3044,    7.6375,    6.4050,    5.8887,    8.4670,
    5.8887,    4.5852,    5.5   ,    4.9544,    4.9544,
    3.9630,    9.1287,    5.2062,    3.9205,    5.5   ,
    2.6548,    2.4231,    5.8887,    5.8887
  )

  willam3 <- c(
    -0.7261,   -0.4812,   -0.5028,   -0.7375,   -0.4720,
    -0.4720,   -0.5111,   -1.0534,   -0.1989,   -0.8281,
    -1.0916,   -0.3599,   -0.3798,   -0.4651,   -0.3151,
    -0.4854,   -0.3821,   -0.3083,   -0.5320,   -0.7231,
    -0.5299,   -0.1621,   -0.8329,   -0.2286,   -0.1863,
    -0.2286,   -0.6746,   -0.95  ,   -0.2085,   -0.2085,
    -0.8277,   -0.1417,   -0.5409,   -0.2397,   -0.95  ,
    -1.2460,   -0.1800,   -0.2286,   -0.2286
  )

  # get species index
  sppIndex <- which(species == spp)

  if (length(sppIndex) == 0) {
    message("Invalid species: ", spp, "...must be one of: ", paste(species, collapse = ","), "...assuming OTHER")
    sppIndex <- 39
  }

  # pick set of coefficients()
  if (length(sppIndex) != 0) {
    if (location == 1 || location == 3) {
      P2 <- oly1[sppIndex]
      P3 <- oly2[sppIndex]
      P4 <- oly3[sppIndex]
    } else if (location == 4) {
      P2 <- mthd1[sppIndex]
      P3 <- mthd2[sppIndex]
      P4 <- mthd3[sppIndex]
    } else if (location == 5) {
      P2 <- willam1[sppIndex]
      P3 <- willam2[sppIndex]
      P4 <- willam3[sppIndex]
    } else {
      P2 <- sius1[sppIndex]
      P3 <- sius2[sppIndex]
      P4 <- sius3[sppIndex]
    }
  }

  if ((location == 2 || location == 4 || location == 6) && sppIndex == 16) {
    #  DOUGLAS-FIR ON THE SIUSLAW SPLINES AT 5.0 INCHES.
    if (mode == 0) {
      if (dbh >= 5.0) {
        height <- 4.5 + P2 * exp(-1.0 * P3 * dbh ^P4)
      } else {
        height <- ((4.5 + P2 * exp(-1.0 * P3 * (5.0^P4)) - 4.51) * (dbh - 0.3)/4.7) + 4.51
      }
    } else {
      hat5 <- 4.5 + P2 * exp(-1.0 * P3 * 5.0^P4)
      if (height >= hat5) {
        dbh <- exp(log((log(height - 4.5) - log(P2)) / (-1.0 * P3)) * 1.0 / P4)
      } else {
        dbh <- (((height - 4.51) * 4.7) / (4.5 + P2 * exp(-1.0 * P3 * (5.0^P4)) - 4.51)) + 0.3
      }
    }
  } else {
    if (mode == 0) {
      if (dbh >= 3.0) {
        height <- 4.5 + P2 * exp(-1.0 * P3 * dbh^P4)
        if (dbh >= 100.0 && sppIndex == 6 && (location == 1 || location == 3)) {
          height <- 0.25 * dbh + 248
        }
      } else {
        height <- ((4.5 + P2 * exp(-1.0 * P3 * (3.0^P4)) - 4.51) * (dbh - 0.3) / 2.7) + 4.51
      }
    } else {
      hat3 <- 4.5 + P2 * exp(-1.0 * P3 * 3.0^P4)
      if (height >= hat3) {
        dbh <- exp(log((log(height - 4.5) - log(P2)) / (-1.0 * P3)) * 1.0 / P4)
        if (height >= 273 && sppIndex == 6 && (location == 1 || location == 3)) {
          dbh <- (height - 248) / 0.25
        }
      } else {
        dbh <- (((height -4.51) * 2.7) / (4.5 + P2 * exp(-1.0 * P3 * (3.0^P4)) - 4.51)) + 0.3
      }
    }
  }
  if (mode == 0) return(invisible(height))
  if (mode == 1) return(invisible(dbh))
}

if (FALSE) {
  # test code
  height = predictFVS("PSME", dbh = 30)
  dbh <- predictFVS("PSME", height = height, mode = 1)
  dbh

  height <- predictHeight("PSME", 30, conversionFactor = 1.0) * 3.2808

  FVSspp <- "PSME"
  Hanusspp <- "PSME"
  dbh = seq(4.0, 40.0, by = 1.0)
  heightFVS <- list()
  heightHanus <- list()
  for (i in 1:length(dbh)) {
    heightFVS[i] <- predictFVS(FVSspp, dbh = dbh[i])
    heightHanus[i] <- predictHeight(Hanusspp, dbh[i] * 2.54) * 3.2808
  }

  # figure out max height for two models
  maxFVSHt <- max(unlist(heightFVS))
  maxHanusHt <- max(unlist(heightHanus))
  maxHt <- max(maxFVSHt, maxHanusHt)
  plot(dbh, heightHanus, main = paste(FVSspp, "height predictions"), ylim = c(20, maxHt))
  points(dbh, heightFVS, col = "red")
  legend("topleft", legend = c("Hanus", "FVS"), col = c("black", "red"), pch = 1)



  speciesHanus <- c("PSME", "ALRU", "PISI", "TSHE", "THPL", "PIMO")
  speciesFVS <- c("PSME", "ALRU3", "PISI", "TSHE", "THPL", "PIMO3")

  for (j in 1:6) {
    FVSspp <- speciesFVS[j]
    Hanusspp <- speciesHanus[j]

      for (i in 1:length(dbh)) {
      heightFVS[i] <- predictFVS(FVSspp, dbh = dbh[i])
      heightHanus[i] <- predictHeight(Hanusspp, dbh[i] * 2.54) * 3.2808
    }

    # figure out max height for two models
    maxFVSHt <- max(unlist(heightFVS))
    maxHanusHt <- max(unlist(heightHanus))
    maxHt <- max(maxFVSHt, maxHanusHt)
    plot(dbh, heightHanus, main = paste(FVSspp, "height predictions"), ylim = c(20, maxHt + 20))
    points(dbh, heightFVS, col = "red")
    legend("topleft", legend = c("Hanus", "FVS"), col = c("black", "red"), pch = 1)
  }
}
