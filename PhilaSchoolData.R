rm(list = ls())
pack <- function(pkg){
  newpkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(newpkg)) 
    install.packages(newpkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("foreign", "rgdal", "sp", "deldir", "SDraw")
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

setwd("D:/AP LARSON/HOLC")
pts <- readOGR(".", "Schools")
polys <- readOGR(".", "Catchment_HS_2016")
state <- read.csv("highSchoolInfo.csv")
state <- state[c(2,12)]
colnames(state)[2] <- "Score"

pts <- merge(pts, state, by = "MatchID")
pts <- pts[c(1,7,17)]
ptsShort <- sp.na.omit(pts)
names(ptsShort@data)[names(ptsShort@data) == "FACILNAME_"] <- "Facility"

polys <- merge(polys, state, by = "MatchID")
polys <- polys[c(1,3,9)]
names(polys@data)[names(polys@data) == "HS_Name"] <- "Facility"
polys <- spTransform(polys, proj4string(ptsShort))

# Voronoi for point data
vor <- voronoi.polygons(ptsShort, polys)
hsRating <- as.data.frame(cbind(ptsShort@coords, ptsShort$Score))
colnames(hsRating) <- c("x", "y", "Score")
vor2 <- merge(vor, hsRating, by = c("x", "y"))

# Export
writeOGR(polys, ".", "finalSchoolCatch", driver = "ESRI Shapefile")
writeOGR(vor2, ".", "finalSchoolPts", driver = "ESRI Shapefile")
