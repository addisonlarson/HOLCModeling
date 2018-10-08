rm(list=ls())
library(rgdal)
setwd("D:/AP LARSON/HOLC")
# Access Across America
# https://conservancy.umn.edu/handle/11299/195065
jobUrl <- "https://conservancy.umn.edu/bitstream/handle/11299/195065/37980_tr_2016_0700-0859.zip?sequence=34&isAllowed=y"
download.file(jobUrl, "jobAccess.zip", mode = "wb")
unzip("jobAccess.zip")
access <- readOGR(".", "37980_tr_2016_0700-0859")
access$stcty <- substr(as.character(access$geoid), 1, 5)
accessShort <- subset(access, stcty == "42101")

# Export
writeOGR(accessShort, ".", "finalJobAccess", driver = "ESRI Shapefile")
