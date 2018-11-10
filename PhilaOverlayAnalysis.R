rm(list=ls())
library(rgdal); library(sp); library(raster)
library(stargazer); library(car); library(spatialEco)
library(tigris)
setwd("D:/AP LARSON/HOLC")
philaTrct <- tracts(42, 101)
holc <- readOGR(".", "holc_polygons", stringsAsFactors = FALSE) # HOLC neighborhood grades
crime <- raster("finalViolentCrime.tif") # Kernel density of shootings
schVor <- readOGR(".", "finalSchoolPts") # School assignment by nearest school (Voronoi polygons)
schCat <- readOGR(".", "finalSchoolCatch") # School assignment by catchment area
jobs <- readOGR(".", "finalJobAccess") # Number of jobs accessible by transit, 30 mins.
appr <- readOGR(".", "finalAppraisalSamp") # Cleaned residential appraisal sample

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
holc$holcA <- ifelse(holc$holc_grade == "A", 1, 0)
holc$holcB <- ifelse(holc$holc_grade == "B", 1, 0)
holc$holcC <- ifelse(holc$holc_grade == "C", 1, 0)
holc$holcD <- ifelse(holc$holc_grade == "D", 1, 0)
holc <- holc[c(10:13)]
holcOver <- sp::over(appr, holc)
appr$holcA <- holcOver$holcA
appr$holcB <- holcOver$holcB
appr$holcC <- holcOver$holcC
appr$holcD <- holcOver$holcD
appr <- sp.na.omit(appr)

# Rescale variables
appr$CrimeRes <- pnorm(appr$Crime,
                       mean = mean(appr$Crime),
                       sd = sd(appr$Crime)) * 100
appr$JobsRes <- appr$Jobs / 1000

# Linear regression
apprDf <- as.data.frame(appr)
apprDf <- apprDf[c(6,8,9,11:16)]
round(cor(apprDf), 3)
lm1 <- lm(CostSqFt ~ ScoreVor + CrimeRes + JobsRes + holcA + holcB + holcC, data = apprDf)
summary(lm1)
vif(lm1)
# Variables are in the direction we'd expect.

texFileName <- "D:/AP LARSON/HOLC/philaModel1.tex"
writeLines(capture.output(stargazer(lm1,
                                    style = "qje",
                                    dep.var.labels = "Cost Per Sq. Ft.",
                                    covariate.labels = c("Overall Performance Score of Nearest HS",
                                                         "Relative Density of Shootings",
                                                         "Number of Jobs Within 30 Mins.",
                                                         "HOLC Grade A",
                                                         "HOLC Grade B",
                                                         "HOLC Grade C"),
                                    title = "Single-Family Housing Values")), texFileName)

philaTrct <- spTransform(philaTrct, proj4string(appr))
apprResid <- sp::over(appr, philaTrct)
apprResid$resid <- lm1$residuals
apprResid$appr <- appr$CostSqFt
apprResidAgg <- aggregate(apprResid$resid, list(apprResid$GEOID), mean)
apprValAgg <- aggregate(apprResid$appr, list(apprResid$GEOID), mean)
colnames(apprResidAgg) <- c("GEOID", "meanResid")
colnames(apprValAgg) <- c("GEOID", "meanVal")
apprAgg <- merge(apprResidAgg, apprValAgg, by = "GEOID")
philaTrct <- merge(philaTrct, apprAgg, by = "GEOID", all.x = TRUE)
philaTrct <- spTransform(philaTrct, CRS("+init=epsg:26918"))
writeOGR(philaTrct, ".", "PhilaResiduals", driver = "ESRI Shapefile")
