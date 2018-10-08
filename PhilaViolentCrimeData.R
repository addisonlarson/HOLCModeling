rm(list=ls())
library(rgdal); library(spatstat); library(raster)
setwd("D:/AP LARSON/HOLC")
# Violent Crime Proxy
# https://www.opendataphilly.org/dataset/shooting-victims/resource/5b88d920-b157-4bb6-8a7d-60b1eb892ce3
crimeUrl <- "https://phl.carto.com/api/v2/sql?q=SELECT+*+FROM+shootings&filename=shootings&format=shp&skipfields=cartodb_id"
download.file(crimeUrl, "shootings.zip", mode = "wb")
unzip("shootings.zip")
shootings <- readOGR(".", "shootings")
# Convert to PPP
shootingsPts <- as(shootings, "SpatialPoints")
shootingsPts@bbox
shootingsPPP <- as.ppp(shootingsPts@coords, c(-75.27360, -74.96476, 39.87532, 40.11493))
# Create kernel density and export to raster
dens <- raster(density(shootingsPPP, sigma = 0.003))
crs(dens) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
plot(dens)
writeRaster(dens, "finalViolentCrime.tiff", overwrite = TRUE)
