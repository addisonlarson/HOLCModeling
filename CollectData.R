rm(list=ls())
pack <- function(pkg){
  newpkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(newpkg)) 
    install.packages(newpkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("foreign", "tidycensus", "tidyverse",
              "rgdal", "stringr", "tigris", "raster")
pack(packages)
sp.na.omit <- function(x, col.name = NULL, margin = 1) {
  if (!inherits(x, "SpatialPointsDataFrame") & 
      !inherits(x, "SpatialPolygonsDataFrame") & 
      !inherits(x, "SpatialLinesDataFrame") )
    stop("MUST BE sp SpatialPointsDataFrame OR SpatialPolygonsDataFrame class object")
  if(!is.null(col.name)) {
    if(is.na(match(col.name, names(x)))) stop(col.name, "does not exist in data") 
    return( x[-which(is.na(x@data[,col.name])),] )
  } else {    
    na.index <- unique(as.data.frame(which(is.na(x@data), arr.ind = TRUE))[, margin])
    if (margin == 1) {
      cat("Deleting rows: ", na.index, "\n")
      return(x[-na.index, ])
    }
    if (margin == 2) {
      cat("Deleting columns: ", na.index, "\n")
      return(x[, -na.index])
    }
  }
}
setwd("D:/AP Larson/HOLC")
# Source for HOLC Data:
# https://dsl-ur.carto.com/u/digitalscholarshiplab/tables/holc_polygons/public
HOLCgrade <- readOGR(".", "holc_polygons")
cityList <- read.csv("HOLCListOfCities.csv")
states <- unique(cityList$State)
# Get all ACS data for the states where HOLC records exist
collect <- get_acs(geography = "tract",
                   state = states,
                   geometry = FALSE,
                   output = "wide",
                   variables = c(popData = "B01001_001E",
                                 incomeData = "B06011_001E",
                                 povUniverse = "B08122_001E",
                                 pov100 = "B08122_002E",
                                 pov149 = "B08122_003E",
                                 pov150 = "B08122_004E",
                                 racUniverse = "B02001_001E",
                                 racWhite = "B02001_002E",
                                 racBlack = "B02001_003E",
                                 racAsian = "B02001_005E",
                                 hispUniverse = "B03001_001E",
                                 hisp = "B03001_003E",
                                 medRent = "B25064_001E",
                                 # someday incl. hh size here,
                                 medValue = "B25077_001E",
                                 medAge = "B25035_001E",
                                 tenureUniverse = "B25003_001E",
                                 tenureOwn = "B25003_002E",
                                 comUniverse = "B08012_001E",
                                 com4 = "B08012_002E",
                                 com5 = "B08012_003E",
                                 com10 = "B08012_004E",
                                 com15 = "B08012_005E",
                                 com20 = "B08012_006E",
                                 com25 = "B08012_007E",
                                 com30 = "B08012_008E",
                                 com35 = "B08012_009E",
                                 com40 = "B08012_010E",
                                 com45 = "B08012_011E",
                                 com60 = "B08012_012E",
                                 com90 = "B08012_013E",
                                 bedUniverse = "B25041_001E",
                                 bed0 = "B25041_002E",
                                 bed1 = "B25041_003E",
                                 bed2 = "B25041_004E",
                                 bed3 = "B25041_005E",
                                 bed4 = "B25041_006E",
                                 bed5 = "B25041_007E",
                                 plumbUniverse = "B25048_001E",
                                 completePlumb = "B25048_002E",
                                 kitchUniverse = "B25051_001E",
                                 completeKitch = "B25051_002E",
                                 laborUniverse = "B23025_002E",
                                 unemployed = "B23025_005E",
                                 edUniverse = "B15003_001E",
                                 edHighSchool = "B15003_017E",
                                 edGED = "B15003_018E",
                                 edSomeColl = "B15003_019E",
                                 edSomeColl2 = "B15003_020E",
                                 edBach = "B15003_022E",
                                 edMast = "B15003_023E",
                                 edProf = "B15003_024E",
                                 edDoc = "B15003_025E",
                                 carUniverse = "B08201_001E",
                                 zeroCar = "B08201_002E",
                                 grapi = "B25071_001E",
                                 famUniverse = "B11001_001E",
                                 malHH = "B11001_005E",
                                 femHH = "B11001_006E"))
# Clean up fields
collect <- collect[, -( grep("\\M$" , colnames(collect), perl = TRUE))]
# Get state/county info so we can split df by MSA
collect$st <- substr(collect$GEOID, 1, 2)
collect$cty <- substr(collect$GEOID, 3, 5)
collect$st <- as.numeric(collect$st); collect$cty <- as.numeric(collect$cty)
fullCensus <- collect
# Some calculations can be done in full data frame
fullCensus$logInc <- log(fullCensus$incomeData)
fullCensus$thouInc <- fullCensus$incomeData / 1000
fullCensus$pct100 <- fullCensus$pov100 / fullCensus$povUniverse * 100
fullCensus$pct149 <- fullCensus$pov149 / fullCensus$povUniverse * 100
fullCensus$pct150 <- fullCensus$pov150 / fullCensus$povUniverse * 100
fullCensus$pctWht <- fullCensus$racWhite / fullCensus$racUniverse * 100
fullCensus$pctBlk <- fullCensus$racBlack / fullCensus$racUniverse * 100
fullCensus$pctAsn <- fullCensus$racAsian / fullCensus$racUniverse * 100
fullCensus$pctHisp <- fullCensus$hisp / fullCensus$hispUniverse * 100
fullCensus$pluWht <- NA
fullCensus$pluWht <- ifelse(fullCensus$pctWht > fullCensus$pctBlk &
                              fullCensus$pctWht > fullCensus$pctAsn &
                              fullCensus$pctWht > fullCensus$pctHisp,
                            1, 0)
fullCensus$pluBlk <- ifelse(fullCensus$pctBlk > fullCensus$pctWht &
                              fullCensus$pctBlk > fullCensus$pctAsn &
                              fullCensus$pctBlk > fullCensus$pctHisp,
                            1, 0)
fullCensus$pluAsn <- ifelse(fullCensus$pctAsn > fullCensus$pctBlk &
                              fullCensus$pctAsn > fullCensus$pctWht &
                              fullCensus$pctAsn > fullCensus$pctHisp,
                            1, 0)
fullCensus$pluHisp <- ifelse(fullCensus$pctHisp > fullCensus$pctBlk &
                               fullCensus$pctHisp > fullCensus$pctAsn &
                               fullCensus$pctHisp > fullCensus$pctWht,
                             1, 0)
fullCensus$logMedRent <- log(fullCensus$medRent)
fullCensus$hunMedRent <- fullCensus$medRent / 100
fullCensus$logHousVal <- log(fullCensus$medValue)
fullCensus$thouHousVal <- fullCensus$medValue / 1000
fullCensus$medAge <- 2018 - fullCensus$medAge
fullCensus$pctOwn <- fullCensus$tenureOwn / fullCensus$tenureUniverse * 100

fullCensus$comBl10 <- (fullCensus$com4 + fullCensus$com5) / fullCensus$comUniverse * 100
fullCensus$com10 <- (fullCensus$com10 + fullCensus$com15) / fullCensus$comUniverse * 100
fullCensus$com20 <- (fullCensus$com20 + fullCensus$com25) / fullCensus$comUniverse * 100
fullCensus$com30 <- (fullCensus$com30 + fullCensus$com35) / fullCensus$comUniverse * 100
fullCensus$com40 <- (fullCensus$com40 + fullCensus$com45) / fullCensus$comUniverse * 100
fullCensus$com60 <- (fullCensus$com60 + fullCensus$com90) / fullCensus$comUniverse * 100

fullCensus$bed0 <- fullCensus$bed0 / fullCensus$bedUniverse * 100
fullCensus$bed1 <- fullCensus$bed1 / fullCensus$bedUniverse * 100
fullCensus$bed2 <- fullCensus$bed2 / fullCensus$bedUniverse * 100
fullCensus$bed3 <- fullCensus$bed3 / fullCensus$bedUniverse * 100
fullCensus$bed4 <- fullCensus$bed4 / fullCensus$bedUniverse * 100
fullCensus$bed5 <- fullCensus$bed5 / fullCensus$bedUniverse * 100

fullCensus$completePlumb <- fullCensus$completePlumb / fullCensus$plumbUniverse * 100

fullCensus$completeKitch <- fullCensus$completeKitch / fullCensus$plumbUniverse * 100

fullCensus$pctUnemp <- fullCensus$unemployed / fullCensus$laborUniverse * 100

fullCensus$edHighSchool <- (fullCensus$edHighSchool + fullCensus$edGED) / fullCensus$edUniverse * 100
fullCensus$edSomeColl <- (fullCensus$edSomeColl + fullCensus$edSomeColl2) / fullCensus$edUniverse * 100
fullCensus$edBach <- fullCensus$edBach / fullCensus$edUniverse * 100
fullCensus$edGrad <- (fullCensus$edMast + fullCensus$edProf + fullCensus$edDoc) / fullCensus$edUniverse * 100

fullCensus$zeroCar <- fullCensus$zeroCar / fullCensus$carUniverse * 100

fullCensus$singParentHH <- (fullCensus$malHH + fullCensus$femHH) / fullCensus$famUniverse * 100
# Drop low population census tracts
fullCensus <- fullCensus[which(fullCensus$popData >= 300), ]
# Remove unnecessary columns
excludeVars <- names(fullCensus) %in% c("povUniverse",
                                        "racUniverse",
                                        "racWhite",
                                        "racBlack",
                                        "racAsian",
                                        "hispUniverse",
                                        "hisp",
                                        "comUniverse",
                                        "com4",
                                        "com5",
                                        "com15",
                                        "com25",
                                        "com35",
                                        "com45",
                                        "com90",
                                        "NAME1",
                                        "GEOID1",
                                        "bedUniverse",
                                        "plumbUniverse",
                                        "kitchUniverse",
                                        "laborUniverse",
                                        "unemployed",
                                        "edUniverse",
                                        "edSomeColl2",
                                        "edGED",
                                        "edMast",
                                        "edProf",
                                        "edDoc",
                                        "carUniverse",
                                        "famUniverse",
                                        "malHH",
                                        "femHH")
fullCensus <- fullCensus[!excludeVars]
# Export result so you don't have to use Census API again
write.csv(fullCensus, file = "AllCensusData.csv", row.names = FALSE)
# Download all applicable state census tract shapefiles
stateTracts <- list()
for (i in states){
  stateTracts[[i]] <- tracts(i)
}
stateTracts2 <- do.call("rbind", stateTracts)
HOLCgrade <- spTransform(HOLCgrade, proj4string(stateTracts2))
writeOGR(stateTracts2, ".", "stateTracts", driver = "ESRI Shapefile")
# Union operation between HOLC and tracts in QGIS
HOLCcensus <- readOGR(".", "HOLCCensusFull")
# Drop unnecessary shapefile fields
keepVars <- c("holc_grade","STATEFP", "COUNTYFP", "GEOID", "ALAND", "INTPTLAT", "INTPTLON")
HOLCcensusClip <- HOLCcensus[,(names(HOLCcensus) %in% keepVars)]
# Remove where HOLC does not intersect with census tracts
HOLCcensusClip <- sp.na.omit(HOLCcensusClip)
# Assign weighted HOLC score to each tract
HOLCcensusDf <- as.data.frame(HOLCcensusClip)
HOLCcensusDf$quantScore <- NA
HOLCcensusDf$quantScore <- ifelse(HOLCcensusDf$holc_grade == "A",
                                  4, HOLCcensusDf$quantScore)
HOLCcensusDf$quantScore <- ifelse(HOLCcensusDf$holc_grade == "B",
                                  3, HOLCcensusDf$quantScore)
HOLCcensusDf$quantScore <- ifelse(HOLCcensusDf$holc_grade == "C",
                                  2, HOLCcensusDf$quantScore)
HOLCcensusDf$quantScore <- ifelse(HOLCcensusDf$holc_grade == "D",
                                  1, HOLCcensusDf$quantScore)
resGEOID <- aggregate(HOLCcensusDf$quantScore,
                      by = list(HOLCcensusDf$GEOID), FUN = mean)
colnames(resGEOID) <- c("GEOID", "quantScore")
setwd("D:/AP LARSON/HOLC")
write.csv(resGEOID, file = "HOLCbyTract.csv", row.names = FALSE)
# Merge weighted HOLC score with ACS Estimates
ACS <- read.csv("AllCensusData.csv")
fullData <- merge(ACS, resGEOID, by = "GEOID")
# 2753 tracts disappear because low population
# Collapse by MSA
# Crosswalk source: http://www.nber.org/data/ssa-fips-state-county-crosswalk.html
xwalk <- read.csv("ssa_fips_state_county2017.csv")
xwalk <- xwalk[c(4:6)]
fullData$cty <- substr(fullData$GEOID, 3, 5)
fullData$stcty <- paste0(fullData$st, fullData$cty)
fullData <- merge(fullData, xwalk, by.x = "stcty", by.y = "fipscounty")
# Remove where MSA DNE
fullData <- fullData[!is.na(fullData$cbsa),]
# Split by MSA
fullData$cbsaname <- as.character(fullData$cbsaname)
fullDataByCBSA <- split(fullData, fullData$cbsaname)
# Remove MSAs where there are fewer than 30 census tracts
keepVector <- NULL; posVector <- NULL
for (i in 1:length(fullDataByCBSA)){
  keepVals <- ifelse(length(fullDataByCBSA[[i]][,1]) >= 30, 1, 0)
  keepVector <- append(keepVector, keepVals)
  posVector <- append(posVector, i)
}
workaround <- cbind(keepVector, posVector)
workaround <- subset(workaround, keepVector == 0)

fullDataByCBSA[[96]] <- NULL
fullDataByCBSA[[96]] <- NULL
fullDataByCBSA[[90]] <- NULL
fullDataByCBSA[[85]] <- NULL
fullDataByCBSA[[84]] <- NULL
fullDataByCBSA[[83]] <- NULL
fullDataByCBSA[[82]] <- NULL
fullDataByCBSA[[81]] <- NULL
fullDataByCBSA[[77]] <- NULL
fullDataByCBSA[[72]] <- NULL
fullDataByCBSA[[68]] <- NULL
fullDataByCBSA[[64]] <- NULL
fullDataByCBSA[[62]] <- NULL
fullDataByCBSA[[61]] <- NULL
fullDataByCBSA[[59]] <- NULL
fullDataByCBSA[[53]] <- NULL
fullDataByCBSA[[52]] <- NULL
fullDataByCBSA[[50]] <- NULL
fullDataByCBSA[[49]] <- NULL
fullDataByCBSA[[45]] <- NULL
fullDataByCBSA[[44]] <- NULL
fullDataByCBSA[[40]] <- NULL
fullDataByCBSA[[33]] <- NULL
fullDataByCBSA[[32]] <- NULL
fullDataByCBSA[[31]] <- NULL
fullDataByCBSA[[30]] <- NULL
fullDataByCBSA[[27]] <- NULL
fullDataByCBSA[[23]] <- NULL
fullDataByCBSA[[21]] <- NULL
fullDataByCBSA[[17]] <- NULL
fullDataByCBSA[[16]] <- NULL
fullDataByCBSA[[10]] <- NULL
fullDataByCBSA[[9]] <- NULL
fullDataByCBSA[[7]] <- NULL
fullDataByCBSA[[4]] <- NULL
fullDataByCBSA[[3]] <- NULL

fullData <- do.call("rbind", fullDataByCBSA)
write.csv(fullData, "HOLCbyTractFinal.csv", row.names = FALSE)
