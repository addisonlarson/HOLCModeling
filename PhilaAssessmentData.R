# First, filter down appraisal data. Five stages:
# 1) CAT CD / CATEGORY CODE == 1 (Residential)
# 2) First two digits of PARCEL / PARCEL NUMBER <= 66
# (Wards 77, 78, 88 are not used here. Note that this excludes condominiums)
# 3) Remove UNF / UNFINISHED properties
# 4) Drop old values of ASSMT DT / ASSESSMENT DATE
# (Format is YYYYMM)
# 5) Keep fields:
      # PARCEL / PARCEL NUMBER
      # SALE TY / SALE TYPE
      # ASSMT DT / ASSESSMENT DATE
      # TX LND / TAXABLE LAND
      # TX BLDG / TAXABLE BUILDING
      # CAT CD / CATEGORY CODE
      # TOT LIV AREA / TOTAL LIVABLE AREA
# Second, analyze appraisal data. Two stages:
# 1) Based on ASSMT DT, adjust TX BLDG for inflation
# 2) Divide TXBLDG2018 by TOT LIV AREA to obtain a cost per sq. ft.

rm(list = ls())
library(rgdal)
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
# Read in sample of appraisal data. n = 145,179, 25% sample of original
appr <- readOGR(".", "appraisalDataSamp")

# 1) CAT CD / CATEGORY CODE == 1 (Residential)
apprSub <- subset(appr, category_c == "1")
# 2) First two digits of PARCEL / PARCEL NUMBER <= 66
# (Wards 77, 78, 88 are not used here. Note that this excludes condominiums)
apprSub$parcel_type <- as.numeric(substr(apprSub$parcel_num, 1, 2))
apprSub <- subset(apprSub, parcel_type <= 66)
# 3) Remove UNF / UNFINISHED properties
apprSub$unfinished <- as.character(apprSub$unfinished)
apprSub$unfinished[is.na(apprSub$unfinished)] <- "F"
apprSub <- subset(apprSub, unfinished == "F")
# 4) Drop old values of ASSMT DT / ASSESSMENT DATE
# (Format is YYYYMM)
# Though ASSMT DT is in the data dictionary, all vals are NA in dataset.
# 5) Keep fields:
# PARCEL / PARCEL NUMBER
# SALE TY / SALE TYPE # NOTE: cannot find this in actual file.
# TX LND / TAXABLE LAND
# TX BLDG / TAXABLE BUILDING
# TOT LIV AREA / TOTAL LIVABLE AREA
apprSub <- apprSub[c("parcel_num",
                     "parcel_type",
                     "taxable_bu",
                     "taxable_la",
                     "total_liva")]
names(apprSub@data)[names(apprSub@data) == "parcel_num"] <- "Parcel"
names(apprSub@data)[names(apprSub@data) == "parcel_type"] <- "Ward"
names(apprSub@data)[names(apprSub@data) == "taxable_bu"] <- "ApprBldg"
names(apprSub@data)[names(apprSub@data) == "taxable_la"] <- "ApprLand"
names(apprSub@data)[names(apprSub@data) == "total_liva"] <- "SqFt"
apprSub$ApprBldg <- as.numeric(as.character(apprSub$ApprBldg))
apprSub$SqFt <- as.numeric(as.character(apprSub$SqFt))
apprSub$SqFt[apprSub$SqFt == 0] <- NA
apprSub <- sp.na.omit(apprSub)
apprSub$CostSqFt <- apprSub$ApprBldg / apprSub$SqFt
# Final n = 105,792
writeOGR(apprSub, ".", "finalAppraisalSamp", driver = "ESRI Shapefile")
