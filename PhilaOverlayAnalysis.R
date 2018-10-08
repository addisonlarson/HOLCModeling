rm(list=ls())
library(rgdal); library(sp); library(raster)
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
setwd("D:/AP LARSON/HOLC")

# ADD HERE: spatial overlay with original redlining file

crime <- raster("finalViolentCrime.tif") # Kernel density of shootings
schVor <- readOGR(".", "finalSchoolPts") # School assignment by nearest school (Voronoi polygons)
schCat <- readOGR(".", "finalSchoolCatch") # School assignment by catchment area
jobs <- readOGR(".", "finalJobAccess") # Number of jobs accessible by transit, 30 mins.
appr <- readOGR(".", "finalAppraisalSampTEST") # Cleaned residential appraisal sample

apprPts <- as(appr, "SpatialPoints")
appr$Crime <- raster::extract(crime, apprPts, method = "bilinear")
schVor <- schVor[c(4)]
schVorOver <- sp::over(appr, schVor)
appr$ScoreVor <- schVorOver$ScoreV
schCat <- schCat[c(3)]
schCatOver <- sp::over(appr, schCat)
appr$ScoreCat <- schCatOver$ScoreC
jobs <- jobs[c(3)]
jobsOver <- sp::over(appr, jobs)
appr$Jobs <- jobsOver$tot_jobs
appr <- sp.na.omit(appr)

# Rescale variables
appr$CrimeRes <- pnorm(appr$Crime,
                       mean = mean(appr$Crime),
                       sd = sd(appr$Crime)) * 100
appr$JobsRes <- appr$Jobs / 1000

# Just for fun
testDf <- as.data.frame(appr)
testDf <- testDf[c(6,8,9,11,12)]
cor(testDf)
testLm <- lm(CostSqFt ~ ScoreVor + CrimeRes + JobsRes, data = testDf)
summary(testLm)
# Varibles are in the direction we'd expect.
