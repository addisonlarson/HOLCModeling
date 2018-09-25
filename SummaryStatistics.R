rm(list=ls())
setwd("D:/AP LARSON/HOLC")
dat <- read.csv("HOLCbyTractFinal.csv")
dat$pct149 <- dat$pct149 + dat$pct100
dat$grade <- NA
dat$grade <- cut(dat$quantScore,
                      breaks = c(0, 1.5, 2.5, 3.5, 4.5),
                      labels = c("D", "C", "B", "A"))
byGrade <- split(dat, dat$grade)

for (i in 4:1){
  testDf <- byGrade[[i]]
  print(paste("Class", i, sep = " "))
  print(mean(testDf$popData, na.rm=TRUE))
  print(weighted.mean(testDf$thouInc, testDf$popData, na.rm=TRUE))
  print(weighted.mean(testDf$pct100, testDf$popData, na.rm=TRUE))
  print(weighted.mean(testDf$pct149, testDf$popData, na.rm=TRUE))
  print(weighted.mean(testDf$singParentHH, testDf$popData, na.rm=TRUE))
  print(weighted.mean(testDf$hunMedRent, testDf$popData, na.rm=TRUE))
  print(weighted.mean(testDf$thouHousVal, testDf$popData, na.rm=TRUE))
  print(weighted.mean(testDf$medAge, testDf$popData, na.rm=TRUE))
  print(weighted.mean(testDf$edHighSchool, testDf$popData, na.rm=TRUE))
  print(weighted.mean(testDf$edSomeColl, testDf$popData, na.rm=TRUE))
  print(weighted.mean(testDf$edBach, testDf$popData, na.rm=TRUE))
  print(weighted.mean(testDf$edGrad, testDf$popData, na.rm=TRUE))
  print(weighted.mean(testDf$pctWht, testDf$popData, na.rm=TRUE))
  print(weighted.mean(testDf$pctBlk, testDf$popData, na.rm=TRUE))
  print(weighted.mean(testDf$pctAsn, testDf$popData, na.rm=TRUE))
  print(weighted.mean(testDf$pctHisp, testDf$popData, na.rm=TRUE))
  print(weighted.mean(testDf$pctUnemp, testDf$popData, na.rm=TRUE))
  print(weighted.mean(testDf$pctOwn, testDf$popData, na.rm=TRUE))
  print(weighted.mean(testDf$zeroCar, testDf$popData, na.rm=TRUE))
  print(weighted.mean(testDf$grapi, testDf$popData, na.rm=TRUE))
}

for (i in 4:1){
  testDf <- byGrade[[i]]
  testDf$mjWht <- ifelse(testDf$pctWht > testDf$pctBlk &
                           testDf$pctWht > testDf$pctAsn &
                           testDf$pctWht > testDf$pctHisp,
                         1, 0)
  testDf$mjBlk <- ifelse(testDf$pctBlk > testDf$pctWht &
                           testDf$pctBlk > testDf$pctAsn &
                           testDf$pctBlk > testDf$pctHisp,
                         1, 0)
  testDf$mjAsn <- ifelse(testDf$pctAsn > testDf$pctBlk &
                           testDf$pctAsn > testDf$pctWht &
                           testDf$pctAsn > testDf$pctHisp,
                         1, 0)
  testDf$mjHisp <- ifelse(testDf$pctHisp > testDf$pctBlk &
                           testDf$pctHisp > testDf$pctAsn &
                           testDf$pctHisp > testDf$pctWht,
                         1, 0)
  print(paste("Class", i, sep = " "))
  print(as.data.frame(table(testDf$mjWht)))
  print(as.data.frame(table(testDf$mjBlk)))
  print(as.data.frame(table(testDf$mjAsn)))
  print(as.data.frame(table(testDf$mjHisp)))
}

# Split by region
# See https://www2.census.gov/programs-surveys/popest/geographies/2015/state-geocodes-v2015.xls
xwalk <- read.csv("state-geocodes-v2015.csv")
xwalk <- xwalk[c(2:3)]
dat <- merge(dat, xwalk, by.x = "st", by.y = "statefp")
byRegion <- split(dat, dat$name)

for (i in 1:4){
  testDf <- byRegion[[i]]
  testDf$grade <- NA
  testDf$grade <- cut(testDf$quantScore,
                   breaks = c(0, 1.5, 2.5, 3.5, 4.5),
                   labels = c("D", "C", "B", "A"))
  print(unique(as.character(testDf$name)))
  print(mean(testDf$popData, na.rm=TRUE))
  print(weighted.mean(testDf$thouInc, testDf$popData, na.rm=TRUE))
  print(weighted.mean(testDf$pct100, testDf$popData, na.rm=TRUE))
  print(weighted.mean(testDf$pct149, testDf$popData, na.rm=TRUE))
  print(weighted.mean(testDf$singParentHH, testDf$popData, na.rm=TRUE))
  print(weighted.mean(testDf$hunMedRent, testDf$popData, na.rm=TRUE))
  print(weighted.mean(testDf$thouHousVal, testDf$popData, na.rm=TRUE))
  print(weighted.mean(testDf$medAge, testDf$popData, na.rm=TRUE))
  print(weighted.mean(testDf$edHighSchool, testDf$popData, na.rm=TRUE))
  print(weighted.mean(testDf$edSomeColl, testDf$popData, na.rm=TRUE))
  print(weighted.mean(testDf$edBach, testDf$popData, na.rm=TRUE))
  print(weighted.mean(testDf$edGrad, testDf$popData, na.rm=TRUE))
  print(weighted.mean(testDf$pctWht, testDf$popData, na.rm=TRUE))
  print(weighted.mean(testDf$pctBlk, testDf$popData, na.rm=TRUE))
  print(weighted.mean(testDf$pctAsn, testDf$popData, na.rm=TRUE))
  print(weighted.mean(testDf$pctHisp, testDf$popData, na.rm=TRUE))
  print(weighted.mean(testDf$pctUnemp, testDf$popData, na.rm=TRUE))
  print(weighted.mean(testDf$pctOwn, testDf$popData, na.rm=TRUE))
  print(weighted.mean(testDf$zeroCar, testDf$popData, na.rm=TRUE))
  print(weighted.mean(testDf$grapi, testDf$popData, na.rm=TRUE))
  print(as.data.frame(table(testDf$grade)))
}

# Split by region AND score
dat$nameGrade <- paste(dat$name, dat$grade, sep = "_")
byRegionGrade <- split(dat, as.factor(dat$nameGrade))
byRegionGrade[[9]] <- NULL
for (i in 1:16){
  testDf <- byRegionGrade[[i]]
  print(unique(as.character(testDf$nameGrade)))
  print(length(testDf$st))
  print(mean(testDf$popData, na.rm=TRUE))
  print(weighted.mean(testDf$thouInc, testDf$popData, na.rm=TRUE))
  print(weighted.mean(testDf$pct100, testDf$popData, na.rm=TRUE))
  print(weighted.mean(testDf$pct149, testDf$popData, na.rm=TRUE))
  print(weighted.mean(testDf$singParentHH, testDf$popData, na.rm=TRUE))
  print(weighted.mean(testDf$hunMedRent, testDf$popData, na.rm=TRUE))
  print(weighted.mean(testDf$thouHousVal, testDf$popData, na.rm=TRUE))
  print(weighted.mean(testDf$medAge, testDf$popData, na.rm=TRUE))
  print(weighted.mean(testDf$edHighSchool, testDf$popData, na.rm=TRUE))
  print(weighted.mean(testDf$edSomeColl, testDf$popData, na.rm=TRUE))
  print(weighted.mean(testDf$edBach, testDf$popData, na.rm=TRUE))
  print(weighted.mean(testDf$edGrad, testDf$popData, na.rm=TRUE))
  print(weighted.mean(testDf$pctWht, testDf$popData, na.rm=TRUE))
  print(weighted.mean(testDf$pctBlk, testDf$popData, na.rm=TRUE))
  print(weighted.mean(testDf$pctAsn, testDf$popData, na.rm=TRUE))
  print(weighted.mean(testDf$pctHisp, testDf$popData, na.rm=TRUE))
  print(weighted.mean(testDf$pctUnemp, testDf$popData, na.rm=TRUE))
  print(weighted.mean(testDf$pctOwn, testDf$popData, na.rm=TRUE))
  print(weighted.mean(testDf$zeroCar, testDf$popData, na.rm=TRUE))
  print(weighted.mean(testDf$grapi, testDf$popData, na.rm=TRUE))
}

# Indices of dissimilarity
dat$wht <- dat$popData * dat$pctWht
dat$blk <- dat$popData * dat$pctBlk
CBSAsplit <- split(dat, dat$cbsaname)
resultDf <- NULL
for (i in 1:63){
  myDf <- CBSAsplit[[i]]
  totWht <- sum(myDf$wht)
  totBlk <- sum(myDf$blk)
  myDf$newWht <- myDf$wht / totWht
  myDf$newBlk <- myDf$blk / totBlk
  myDf$absDiff <- abs(myDf$newWht - myDf$newBlk)
  totDiff <- round((sum(myDf$absDiff) / 2), digits = 3)
  myCity <- as.character(unique(myDf$cbsaname))
  resultDf <- rbind(resultDf, c(myCity, totDiff))
}
write.csv(resultDf, "DissimIdx.csv", row.names = FALSE)
