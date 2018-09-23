rm(list=ls())
library(car); library(stargazer)
setwd("D:/AP LARSON/HOLC")
dat <- read.csv("HOLCbyTractFinal.csv")
str(dat$cbsaname)
# Set up for panel model
namevector <- as.character(unique(dat$cbsaname))
for (i in namevector){
  dat[,namevector] <- NA
}
for (i in 1:length(namevector)){
  dat[i + 60] <- ifelse(dat$cbsaname == namevector[[i]], 1, 0)
}

# Control by region
# See https://www2.census.gov/programs-surveys/popest/geographies/2015/state-geocodes-v2015.xls
xwalk <- read.csv("state-geocodes-v2015.csv")
xwalk <- xwalk[c(2:3)]
dat <- merge(dat, xwalk, by.x = "st", by.y = "statefp")
dat$regSouth <- ifelse(dat$name == "South Region", 1, 0)
dat$regMidwest <- ifelse(dat$name == "Midwest Region", 1, 0)
dat$regNortheast <- ifelse(dat$name == "Northeast Region", 1, 0)
dat$regWest <- ifelse(dat$name == "West Region", 1, 0)

housingValue <- lm(thouHousVal ~ quantScore +
                     `Akron, OH` +
                     `Albany-Schenectady-Troy, NY` +
                     `Atlanta-Sandy Springs-Roswell, GA` +
                     `Atlantic City-Hammonton, NJ` +
                     `Baltimore-Columbia-Towson, MD` +
                     `Binghamton, NY` +
                     `Boston, MA` +
                     `Buffalo-Cheektowaga-Niagara Falls, NY` +
                     `Cambridge-Newton-Framingham, MA` +
                     `Camden, NJ` +
                     `Charlotte-Concord-Gastonia, NC-SC` +
                     `Chattanooga, TN-GA` +
                     `Chicago-Naperville-Arlington Heights, IL` +
                     `Cleveland-Elyria, OH` +
                     `Columbus, OH` +
                     `Dallas-Plano-Irving, TX` +
                     `Dayton, OH` +
                     `Detroit-Dearborn-Livonia, MI` +
                     `Duluth, MN-WI` +
                     `Erie, PA` +
                     `Evansville, IN-KY` +
                     `Flint, MI` +
                     `Fort Wayne, IN` +
                     `Gary, IN` +
                     `Grand Rapids-Wyoming, MI` +
                     `Houston-The Woodlands-Sugar Land, TX` +
                     `Indianapolis-Carmel-Anderson, IN` +
                     `Jacksonville, FL` +
                     `Kansas City, MO-KS` +
                     `Knoxville, TN` +
                     `Lake County-Kenosha County, IL-WI` +
                     `Louisville/Jefferson County, KY-IN` +
                     `Madison, WI` +
                     `Manchester-Nashua, NH` +
                     `Miami-Miami Beach-Kendall, FL` +
                     `Milwaukee-Waukesha-West Allis, WI` +
                     `Minneapolis-St. Paul-Bloomington, MN-WI` +
                     `Montgomery County-Bucks County-Chester County, PA` +
                     `Nashville-Davidson--Murfreesboro--Franklin, TN` +
                     `New Orleans-Metairie, LA` +
                     `New York-Jersey City-White Plains, NY-NJ` +
                     `Newark, NJ-PA` +
                     `Philadelphia, PA` +
                     `Pittsburgh, PA` +
                     `Portland-Vancouver-Hillsboro, OR-WA` +
                     `Richmond, VA` +
                     `Roanoke, VA` +
                     `Rochester, NY` +
                     `Rockford, IL` +
                     `Seattle-Bellevue-Everett, WA` +
                     `South Bend-Mishawaka, IN-MI` +
                     `Spokane-Spokane Valley, WA` +
                     `St. Louis, MO-IL` +
                     `Syracuse, NY` +
                     `Tacoma-Lakewood, WA` +
                     `Tampa-St. Petersburg-Clearwater, FL` +
                     `Toledo, OH` +
                     `Trenton, NJ` +
                     `Utica-Rome, NY` +
                     `Virginia Beach-Norfolk-Newport News, VA-NC` +
                     `Warren-Troy-Farmington Hills, MI` +
                     `Winston-Salem, NC` +
                     # `Youngstown-Warren-Boardman, OH-PA` +
                     bed0 +
                     bed1 +
                     bed2 +
                     bed3 +
                     bed4 +
                     medAge +
                     completePlumb +
                     completeKitch, data = dat)
# print(summary(housingValue), digits = 3)
texFileName <- "D:/AP LARSON/HOLC/housingValue.tex"
writeLines(capture.output(stargazer(housingValue,
                                    style = "qje",
                                    omit = 2:64,
                                    dep.var.labels = "Home Value, $1000s",
                                    covariate.labels = c("HOLC Rating",
                                                         "1 Bedroom",
                                                         "2 Bedrooms",
                                                         "3 Bedrooms",
                                                         "4 Bedrooms",
                                                         "Median Home Age",
                                                         "Complete Plumbing Facilities",
                                                         "Complete Kitchen Facilities"),
                                    title = "Home Value, $1000s")), texFileName)

# Tests for multicollinearity. Must re-run
myCols <- c("quantScore", "thouHousVal", "bed0", "bed1",
            "bed2", "bed3", "bed4", "medAge", "completePlumb", "completeKitch")
testCase <- dat[myCols]
round(cor(testCase, method = "pearson", use = "complete.obs"), digits = 3)
vif(housingValue)

tenure <- lm(pctOwn ~ quantScore +
               `Akron, OH` +
               `Albany-Schenectady-Troy, NY` +
               `Atlanta-Sandy Springs-Roswell, GA` +
               `Atlantic City-Hammonton, NJ` +
               `Baltimore-Columbia-Towson, MD` +
               `Binghamton, NY` +
               `Boston, MA` +
               `Buffalo-Cheektowaga-Niagara Falls, NY` +
               `Cambridge-Newton-Framingham, MA` +
               `Camden, NJ` +
               `Charlotte-Concord-Gastonia, NC-SC` +
               `Chattanooga, TN-GA` +
               `Chicago-Naperville-Arlington Heights, IL` +
               `Cleveland-Elyria, OH` +
               `Columbus, OH` +
               `Dallas-Plano-Irving, TX` +
               `Dayton, OH` +
               `Detroit-Dearborn-Livonia, MI` +
               `Duluth, MN-WI` +
               `Erie, PA` +
               `Evansville, IN-KY` +
               `Flint, MI` +
               `Fort Wayne, IN` +
               `Gary, IN` +
               `Grand Rapids-Wyoming, MI` +
               `Houston-The Woodlands-Sugar Land, TX` +
               `Indianapolis-Carmel-Anderson, IN` +
               `Jacksonville, FL` +
               `Kansas City, MO-KS` +
               `Knoxville, TN` +
               `Lake County-Kenosha County, IL-WI` +
               `Louisville/Jefferson County, KY-IN` +
               `Madison, WI` +
               `Manchester-Nashua, NH` +
               `Miami-Miami Beach-Kendall, FL` +
               `Milwaukee-Waukesha-West Allis, WI` +
               `Minneapolis-St. Paul-Bloomington, MN-WI` +
               `Montgomery County-Bucks County-Chester County, PA` +
               `Nashville-Davidson--Murfreesboro--Franklin, TN` +
               `New Orleans-Metairie, LA` +
               `New York-Jersey City-White Plains, NY-NJ` +
               `Newark, NJ-PA` +
               `Philadelphia, PA` +
               `Pittsburgh, PA` +
               `Portland-Vancouver-Hillsboro, OR-WA` +
               `Richmond, VA` +
               `Roanoke, VA` +
               `Rochester, NY` +
               `Rockford, IL` +
               `Seattle-Bellevue-Everett, WA` +
               `South Bend-Mishawaka, IN-MI` +
               `Spokane-Spokane Valley, WA` +
               `St. Louis, MO-IL` +
               `Syracuse, NY` +
               `Tacoma-Lakewood, WA` +
               `Tampa-St. Petersburg-Clearwater, FL` +
               `Toledo, OH` +
               `Trenton, NJ` +
               `Utica-Rome, NY` +
               `Virginia Beach-Norfolk-Newport News, VA-NC` +
               `Warren-Troy-Farmington Hills, MI` +
               `Winston-Salem, NC`,
               # `Youngstown-Warren-Boardman, OH-PA` +
               data = dat)
# print(summary(tenure), digits = 3)
vif(tenure)
texFileName <- "D:/AP LARSON/HOLC/tenure.tex"
writeLines(capture.output(stargazer(tenure,
                                    style = "qje",
                                    omit = 2:64,
                                    dep.var.labels = "Pct. Own Home",
                                    covariate.labels = "HOLC Rating",
                                    title = "Percentage Home Ownership")), texFileName)

# May need to add additional ed variables
income <- lm(thouInc ~ quantScore +
               `Akron, OH` +
               `Albany-Schenectady-Troy, NY` +
               `Atlanta-Sandy Springs-Roswell, GA` +
               `Atlantic City-Hammonton, NJ` +
               `Baltimore-Columbia-Towson, MD` +
               `Binghamton, NY` +
               `Boston, MA` +
               `Buffalo-Cheektowaga-Niagara Falls, NY` +
               `Cambridge-Newton-Framingham, MA` +
               `Camden, NJ` +
               `Charlotte-Concord-Gastonia, NC-SC` +
               `Chattanooga, TN-GA` +
               `Chicago-Naperville-Arlington Heights, IL` +
               `Cleveland-Elyria, OH` +
               `Columbus, OH` +
               `Dallas-Plano-Irving, TX` +
               `Dayton, OH` +
               `Detroit-Dearborn-Livonia, MI` +
               `Duluth, MN-WI` +
               `Erie, PA` +
               `Evansville, IN-KY` +
               `Flint, MI` +
               `Fort Wayne, IN` +
               `Gary, IN` +
               `Grand Rapids-Wyoming, MI` +
               `Houston-The Woodlands-Sugar Land, TX` +
               `Indianapolis-Carmel-Anderson, IN` +
               `Jacksonville, FL` +
               `Kansas City, MO-KS` +
               `Knoxville, TN` +
               `Lake County-Kenosha County, IL-WI` +
               `Louisville/Jefferson County, KY-IN` +
               `Madison, WI` +
               `Manchester-Nashua, NH` +
               `Miami-Miami Beach-Kendall, FL` +
               `Milwaukee-Waukesha-West Allis, WI` +
               `Minneapolis-St. Paul-Bloomington, MN-WI` +
               `Montgomery County-Bucks County-Chester County, PA` +
               `Nashville-Davidson--Murfreesboro--Franklin, TN` +
               `New Orleans-Metairie, LA` +
               `New York-Jersey City-White Plains, NY-NJ` +
               `Newark, NJ-PA` +
               `Philadelphia, PA` +
               `Pittsburgh, PA` +
               `Portland-Vancouver-Hillsboro, OR-WA` +
               `Richmond, VA` +
               `Roanoke, VA` +
               `Rochester, NY` +
               `Rockford, IL` +
               `Seattle-Bellevue-Everett, WA` +
               `South Bend-Mishawaka, IN-MI` +
               `Spokane-Spokane Valley, WA` +
               `St. Louis, MO-IL` +
               `Syracuse, NY` +
               `Tacoma-Lakewood, WA` +
               `Tampa-St. Petersburg-Clearwater, FL` +
               `Toledo, OH` +
               `Trenton, NJ` +
               `Utica-Rome, NY` +
               `Virginia Beach-Norfolk-Newport News, VA-NC` +
               `Warren-Troy-Farmington Hills, MI` +
               `Winston-Salem, NC` +
             # `Youngstown-Warren-Boardman, OH-PA` +
               edHighSchool, data = dat)
# print(summary(income), digits = 3)
texFileName <- "D:/AP LARSON/HOLC/income.tex"
writeLines(capture.output(stargazer(income,
                                    style = "qje",
                                    omit = 2:64,
                                    dep.var.labels = "Median Income, $1000s",
                                    covariate.labels = c("HOLC Rating", "Pct. HS Grad or Equivalent"),
                                    title = "Median Annual Household Income, $1000s")), texFileName)

myCols <- c("quantScore", "thouInc", "edHighSchool",
            "edSomeColl", "edBach", "edGrad")
testCase <- dat[myCols]
round(cor(testCase, method = "pearson", use = "complete.obs"), digits = 3)

unemp <- lm(pctUnemp ~ quantScore +
              `Akron, OH` +
              `Albany-Schenectady-Troy, NY` +
              `Atlanta-Sandy Springs-Roswell, GA` +
              `Atlantic City-Hammonton, NJ` +
              `Baltimore-Columbia-Towson, MD` +
              `Binghamton, NY` +
              `Boston, MA` +
              `Buffalo-Cheektowaga-Niagara Falls, NY` +
              `Cambridge-Newton-Framingham, MA` +
              `Camden, NJ` +
              `Charlotte-Concord-Gastonia, NC-SC` +
              `Chattanooga, TN-GA` +
              `Chicago-Naperville-Arlington Heights, IL` +
              `Cleveland-Elyria, OH` +
              `Columbus, OH` +
              `Dallas-Plano-Irving, TX` +
              `Dayton, OH` +
              `Detroit-Dearborn-Livonia, MI` +
              `Duluth, MN-WI` +
              `Erie, PA` +
              `Evansville, IN-KY` +
              `Flint, MI` +
              `Fort Wayne, IN` +
              `Gary, IN` +
              `Grand Rapids-Wyoming, MI` +
              `Houston-The Woodlands-Sugar Land, TX` +
              `Indianapolis-Carmel-Anderson, IN` +
              `Jacksonville, FL` +
              `Kansas City, MO-KS` +
              `Knoxville, TN` +
              `Lake County-Kenosha County, IL-WI` +
              `Louisville/Jefferson County, KY-IN` +
              `Madison, WI` +
              `Manchester-Nashua, NH` +
              `Miami-Miami Beach-Kendall, FL` +
              `Milwaukee-Waukesha-West Allis, WI` +
              `Minneapolis-St. Paul-Bloomington, MN-WI` +
              `Montgomery County-Bucks County-Chester County, PA` +
              `Nashville-Davidson--Murfreesboro--Franklin, TN` +
              `New Orleans-Metairie, LA` +
              `New York-Jersey City-White Plains, NY-NJ` +
              `Newark, NJ-PA` +
              `Philadelphia, PA` +
              `Pittsburgh, PA` +
              `Portland-Vancouver-Hillsboro, OR-WA` +
              `Richmond, VA` +
              `Roanoke, VA` +
              `Rochester, NY` +
              `Rockford, IL` +
              `Seattle-Bellevue-Everett, WA` +
              `South Bend-Mishawaka, IN-MI` +
              `Spokane-Spokane Valley, WA` +
              `St. Louis, MO-IL` +
              `Syracuse, NY` +
              `Tacoma-Lakewood, WA` +
              `Tampa-St. Petersburg-Clearwater, FL` +
              `Toledo, OH` +
              `Trenton, NJ` +
              `Utica-Rome, NY` +
              `Virginia Beach-Norfolk-Newport News, VA-NC` +
              `Warren-Troy-Farmington Hills, MI` +
              `Winston-Salem, NC` +
            # `Youngstown-Warren-Boardman, OH-PA` +
              edHighSchool, data = dat)
# print(summary(unemp), digits = 3)
texFileName <- "D:/AP LARSON/HOLC/unemp.tex"
writeLines(capture.output(stargazer(unemp,
                                    style = "qje",
                                    omit = 2:64,
                                    dep.var.labels = "Pct. Unemployed",
                                    covariate.labels = c("HOLC Rating", "Pct. HS Grad or Equivalent"),
                                    title = "Unemployment Rate")), texFileName)

myCols <- c("quantScore", "pctUnemp", "edHighSchool",
            "edSomeColl", "edBach", "edGrad")
testCase <- dat[myCols]
round(cor(testCase, method = "pearson", use = "complete.obs"), digits = 3)

zeroCar <- lm(zeroCar ~ quantScore +
                `Akron, OH` +
                `Albany-Schenectady-Troy, NY` +
                `Atlanta-Sandy Springs-Roswell, GA` +
                `Atlantic City-Hammonton, NJ` +
                `Baltimore-Columbia-Towson, MD` +
                `Binghamton, NY` +
                `Boston, MA` +
                `Buffalo-Cheektowaga-Niagara Falls, NY` +
                `Cambridge-Newton-Framingham, MA` +
                `Camden, NJ` +
                `Charlotte-Concord-Gastonia, NC-SC` +
                `Chattanooga, TN-GA` +
                `Chicago-Naperville-Arlington Heights, IL` +
                `Cleveland-Elyria, OH` +
                `Columbus, OH` +
                `Dallas-Plano-Irving, TX` +
                `Dayton, OH` +
                `Detroit-Dearborn-Livonia, MI` +
                `Duluth, MN-WI` +
                `Erie, PA` +
                `Evansville, IN-KY` +
                `Flint, MI` +
                `Fort Wayne, IN` +
                `Gary, IN` +
                `Grand Rapids-Wyoming, MI` +
                `Houston-The Woodlands-Sugar Land, TX` +
                `Indianapolis-Carmel-Anderson, IN` +
                `Jacksonville, FL` +
                `Kansas City, MO-KS` +
                `Knoxville, TN` +
                `Lake County-Kenosha County, IL-WI` +
                `Louisville/Jefferson County, KY-IN` +
                `Madison, WI` +
                `Manchester-Nashua, NH` +
                `Miami-Miami Beach-Kendall, FL` +
                `Milwaukee-Waukesha-West Allis, WI` +
                `Minneapolis-St. Paul-Bloomington, MN-WI` +
                `Montgomery County-Bucks County-Chester County, PA` +
                `Nashville-Davidson--Murfreesboro--Franklin, TN` +
                `New Orleans-Metairie, LA` +
                `New York-Jersey City-White Plains, NY-NJ` +
                `Newark, NJ-PA` +
                `Philadelphia, PA` +
                `Pittsburgh, PA` +
                `Portland-Vancouver-Hillsboro, OR-WA` +
                `Richmond, VA` +
                `Roanoke, VA` +
                `Rochester, NY` +
                `Rockford, IL` +
                `Seattle-Bellevue-Everett, WA` +
                `South Bend-Mishawaka, IN-MI` +
                `Spokane-Spokane Valley, WA` +
                `St. Louis, MO-IL` +
                `Syracuse, NY` +
                `Tacoma-Lakewood, WA` +
                `Tampa-St. Petersburg-Clearwater, FL` +
                `Toledo, OH` +
                `Trenton, NJ` +
                `Utica-Rome, NY` +
                `Virginia Beach-Norfolk-Newport News, VA-NC` +
                `Warren-Troy-Farmington Hills, MI` +
                `Winston-Salem, NC`,
              # `Youngstown-Warren-Boardman, OH-PA` +
              data = dat)
# print(summary(zeroCar), digits = 3)
texFileName <- "D:/AP LARSON/HOLC/zeroCar.tex"
writeLines(capture.output(stargazer(zeroCar,
                                    style = "qje",
                                    omit = 2:64,
                                    dep.var.labels = "Pct. Zero-Car Households",
                                    covariate.labels = "HOLC Rating",
                                    title = "Percentage Zero-Car Households")), texFileName)

myCols <- c("zeroCar", "incomeData")
testCase <- dat[myCols]
cor(testCase, method = "pearson", use = "complete.obs")

singParent <- lm(singParentHH ~ quantScore +
                   `Akron, OH` +
                   `Albany-Schenectady-Troy, NY` +
                   `Atlanta-Sandy Springs-Roswell, GA` +
                   `Atlantic City-Hammonton, NJ` +
                   `Baltimore-Columbia-Towson, MD` +
                   `Binghamton, NY` +
                   `Boston, MA` +
                   `Buffalo-Cheektowaga-Niagara Falls, NY` +
                   `Cambridge-Newton-Framingham, MA` +
                   `Camden, NJ` +
                   `Charlotte-Concord-Gastonia, NC-SC` +
                   `Chattanooga, TN-GA` +
                   `Chicago-Naperville-Arlington Heights, IL` +
                   `Cleveland-Elyria, OH` +
                   `Columbus, OH` +
                   `Dallas-Plano-Irving, TX` +
                   `Dayton, OH` +
                   `Detroit-Dearborn-Livonia, MI` +
                   `Duluth, MN-WI` +
                   `Erie, PA` +
                   `Evansville, IN-KY` +
                   `Flint, MI` +
                   `Fort Wayne, IN` +
                   `Gary, IN` +
                   `Grand Rapids-Wyoming, MI` +
                   `Houston-The Woodlands-Sugar Land, TX` +
                   `Indianapolis-Carmel-Anderson, IN` +
                   `Jacksonville, FL` +
                   `Kansas City, MO-KS` +
                   `Knoxville, TN` +
                   `Lake County-Kenosha County, IL-WI` +
                   `Louisville/Jefferson County, KY-IN` +
                   `Madison, WI` +
                   `Manchester-Nashua, NH` +
                   `Miami-Miami Beach-Kendall, FL` +
                   `Milwaukee-Waukesha-West Allis, WI` +
                   `Minneapolis-St. Paul-Bloomington, MN-WI` +
                   `Montgomery County-Bucks County-Chester County, PA` +
                   `Nashville-Davidson--Murfreesboro--Franklin, TN` +
                   `New Orleans-Metairie, LA` +
                   `New York-Jersey City-White Plains, NY-NJ` +
                   `Newark, NJ-PA` +
                   `Philadelphia, PA` +
                   `Pittsburgh, PA` +
                   `Portland-Vancouver-Hillsboro, OR-WA` +
                   `Richmond, VA` +
                   `Roanoke, VA` +
                   `Rochester, NY` +
                   `Rockford, IL` +
                   `Seattle-Bellevue-Everett, WA` +
                   `South Bend-Mishawaka, IN-MI` +
                   `Spokane-Spokane Valley, WA` +
                   `St. Louis, MO-IL` +
                   `Syracuse, NY` +
                   `Tacoma-Lakewood, WA` +
                   `Tampa-St. Petersburg-Clearwater, FL` +
                   `Toledo, OH` +
                   `Trenton, NJ` +
                   `Utica-Rome, NY` +
                   `Virginia Beach-Norfolk-Newport News, VA-NC` +
                   `Warren-Troy-Farmington Hills, MI` +
                   `Winston-Salem, NC`,
                 # `Youngstown-Warren-Boardman, OH-PA` +
                 data = dat)
# print(summary(singParent), digits = 3)
texFileName <- "D:/AP LARSON/HOLC/singParent.tex"
writeLines(capture.output(stargazer(singParent,
                                    style = "qje",
                                    omit = 2:64,
                                    dep.var.labels = "Pct. Single-Parent HHs",
                                    covariate.labels = "HOLC Rating",
                                    title = "Percentage Single-Parent Households")), texFileName)

comBl10 <- lm(comBl10 ~ quantScore +
                `Akron, OH` +
                `Albany-Schenectady-Troy, NY` +
                `Atlanta-Sandy Springs-Roswell, GA` +
                `Atlantic City-Hammonton, NJ` +
                `Baltimore-Columbia-Towson, MD` +
                `Binghamton, NY` +
                `Boston, MA` +
                `Buffalo-Cheektowaga-Niagara Falls, NY` +
                `Cambridge-Newton-Framingham, MA` +
                `Camden, NJ` +
                `Charlotte-Concord-Gastonia, NC-SC` +
                `Chattanooga, TN-GA` +
                `Chicago-Naperville-Arlington Heights, IL` +
                `Cleveland-Elyria, OH` +
                `Columbus, OH` +
                `Dallas-Plano-Irving, TX` +
                `Dayton, OH` +
                `Detroit-Dearborn-Livonia, MI` +
                `Duluth, MN-WI` +
                `Erie, PA` +
                `Evansville, IN-KY` +
                `Flint, MI` +
                `Fort Wayne, IN` +
                `Gary, IN` +
                `Grand Rapids-Wyoming, MI` +
                `Houston-The Woodlands-Sugar Land, TX` +
                `Indianapolis-Carmel-Anderson, IN` +
                `Jacksonville, FL` +
                `Kansas City, MO-KS` +
                `Knoxville, TN` +
                `Lake County-Kenosha County, IL-WI` +
                `Louisville/Jefferson County, KY-IN` +
                `Madison, WI` +
                `Manchester-Nashua, NH` +
                `Miami-Miami Beach-Kendall, FL` +
                `Milwaukee-Waukesha-West Allis, WI` +
                `Minneapolis-St. Paul-Bloomington, MN-WI` +
                `Montgomery County-Bucks County-Chester County, PA` +
                `Nashville-Davidson--Murfreesboro--Franklin, TN` +
                `New Orleans-Metairie, LA` +
                `New York-Jersey City-White Plains, NY-NJ` +
                `Newark, NJ-PA` +
                `Philadelphia, PA` +
                `Pittsburgh, PA` +
                `Portland-Vancouver-Hillsboro, OR-WA` +
                `Richmond, VA` +
                `Roanoke, VA` +
                `Rochester, NY` +
                `Rockford, IL` +
                `Seattle-Bellevue-Everett, WA` +
                `South Bend-Mishawaka, IN-MI` +
                `Spokane-Spokane Valley, WA` +
                `St. Louis, MO-IL` +
                `Syracuse, NY` +
                `Tacoma-Lakewood, WA` +
                `Tampa-St. Petersburg-Clearwater, FL` +
                `Toledo, OH` +
                `Trenton, NJ` +
                `Utica-Rome, NY` +
                `Virginia Beach-Norfolk-Newport News, VA-NC` +
                `Warren-Troy-Farmington Hills, MI` +
                `Winston-Salem, NC`,
              # `Youngstown-Warren-Boardman, OH-PA` +
              data = dat)
# print(summary(comBl10), digits = 3)
texFileName <- "D:/AP LARSON/HOLC/comBl10.tex"
writeLines(capture.output(stargazer(comBl10,
                                    style = "qje",
                                    omit = 2:64,
                                    title = "Percentage Commutes Below 10 Minutes")), texFileName)

com10 <- lm(com10 ~ quantScore +
              `Akron, OH` +
              `Albany-Schenectady-Troy, NY` +
              `Atlanta-Sandy Springs-Roswell, GA` +
              `Atlantic City-Hammonton, NJ` +
              `Baltimore-Columbia-Towson, MD` +
              `Binghamton, NY` +
              `Boston, MA` +
              `Buffalo-Cheektowaga-Niagara Falls, NY` +
              `Cambridge-Newton-Framingham, MA` +
              `Camden, NJ` +
              `Charlotte-Concord-Gastonia, NC-SC` +
              `Chattanooga, TN-GA` +
              `Chicago-Naperville-Arlington Heights, IL` +
              `Cleveland-Elyria, OH` +
              `Columbus, OH` +
              `Dallas-Plano-Irving, TX` +
              `Dayton, OH` +
              `Detroit-Dearborn-Livonia, MI` +
              `Duluth, MN-WI` +
              `Erie, PA` +
              `Evansville, IN-KY` +
              `Flint, MI` +
              `Fort Wayne, IN` +
              `Gary, IN` +
              `Grand Rapids-Wyoming, MI` +
              `Houston-The Woodlands-Sugar Land, TX` +
              `Indianapolis-Carmel-Anderson, IN` +
              `Jacksonville, FL` +
              `Kansas City, MO-KS` +
              `Knoxville, TN` +
              `Lake County-Kenosha County, IL-WI` +
              `Louisville/Jefferson County, KY-IN` +
              `Madison, WI` +
              `Manchester-Nashua, NH` +
              `Miami-Miami Beach-Kendall, FL` +
              `Milwaukee-Waukesha-West Allis, WI` +
              `Minneapolis-St. Paul-Bloomington, MN-WI` +
              `Montgomery County-Bucks County-Chester County, PA` +
              `Nashville-Davidson--Murfreesboro--Franklin, TN` +
              `New Orleans-Metairie, LA` +
              `New York-Jersey City-White Plains, NY-NJ` +
              `Newark, NJ-PA` +
              `Philadelphia, PA` +
              `Pittsburgh, PA` +
              `Portland-Vancouver-Hillsboro, OR-WA` +
              `Richmond, VA` +
              `Roanoke, VA` +
              `Rochester, NY` +
              `Rockford, IL` +
              `Seattle-Bellevue-Everett, WA` +
              `South Bend-Mishawaka, IN-MI` +
              `Spokane-Spokane Valley, WA` +
              `St. Louis, MO-IL` +
              `Syracuse, NY` +
              `Tacoma-Lakewood, WA` +
              `Tampa-St. Petersburg-Clearwater, FL` +
              `Toledo, OH` +
              `Trenton, NJ` +
              `Utica-Rome, NY` +
              `Virginia Beach-Norfolk-Newport News, VA-NC` +
              `Warren-Troy-Farmington Hills, MI` +
              `Winston-Salem, NC`,
            # `Youngstown-Warren-Boardman, OH-PA` +
            data = dat)
# print(summary(com10), digits = 3)
texFileName <- "D:/AP LARSON/HOLC/com10.tex"
writeLines(capture.output(stargazer(com10,
                                    style = "qje",
                                    omit = 2:64,
                                    title = "Percentage Commutes 10-19 Minutes")), texFileName)

com20 <- lm(com20 ~ quantScore +
              `Akron, OH` +
              `Albany-Schenectady-Troy, NY` +
              `Atlanta-Sandy Springs-Roswell, GA` +
              `Atlantic City-Hammonton, NJ` +
              `Baltimore-Columbia-Towson, MD` +
              `Binghamton, NY` +
              `Boston, MA` +
              `Buffalo-Cheektowaga-Niagara Falls, NY` +
              `Cambridge-Newton-Framingham, MA` +
              `Camden, NJ` +
              `Charlotte-Concord-Gastonia, NC-SC` +
              `Chattanooga, TN-GA` +
              `Chicago-Naperville-Arlington Heights, IL` +
              `Cleveland-Elyria, OH` +
              `Columbus, OH` +
              `Dallas-Plano-Irving, TX` +
              `Dayton, OH` +
              `Detroit-Dearborn-Livonia, MI` +
              `Duluth, MN-WI` +
              `Erie, PA` +
              `Evansville, IN-KY` +
              `Flint, MI` +
              `Fort Wayne, IN` +
              `Gary, IN` +
              `Grand Rapids-Wyoming, MI` +
              `Houston-The Woodlands-Sugar Land, TX` +
              `Indianapolis-Carmel-Anderson, IN` +
              `Jacksonville, FL` +
              `Kansas City, MO-KS` +
              `Knoxville, TN` +
              `Lake County-Kenosha County, IL-WI` +
              `Louisville/Jefferson County, KY-IN` +
              `Madison, WI` +
              `Manchester-Nashua, NH` +
              `Miami-Miami Beach-Kendall, FL` +
              `Milwaukee-Waukesha-West Allis, WI` +
              `Minneapolis-St. Paul-Bloomington, MN-WI` +
              `Montgomery County-Bucks County-Chester County, PA` +
              `Nashville-Davidson--Murfreesboro--Franklin, TN` +
              `New Orleans-Metairie, LA` +
              `New York-Jersey City-White Plains, NY-NJ` +
              `Newark, NJ-PA` +
              `Philadelphia, PA` +
              `Pittsburgh, PA` +
              `Portland-Vancouver-Hillsboro, OR-WA` +
              `Richmond, VA` +
              `Roanoke, VA` +
              `Rochester, NY` +
              `Rockford, IL` +
              `Seattle-Bellevue-Everett, WA` +
              `South Bend-Mishawaka, IN-MI` +
              `Spokane-Spokane Valley, WA` +
              `St. Louis, MO-IL` +
              `Syracuse, NY` +
              `Tacoma-Lakewood, WA` +
              `Tampa-St. Petersburg-Clearwater, FL` +
              `Toledo, OH` +
              `Trenton, NJ` +
              `Utica-Rome, NY` +
              `Virginia Beach-Norfolk-Newport News, VA-NC` +
              `Warren-Troy-Farmington Hills, MI` +
              `Winston-Salem, NC`,
            # `Youngstown-Warren-Boardman, OH-PA` +
            data = dat)
# print(summary(com20), digits = 3)
texFileName <- "D:/AP LARSON/HOLC/com20.tex"
writeLines(capture.output(stargazer(com20,
                                    style = "qje",
                                    omit = 2:64,
                                    title = "Percentage Commutes 20-29 Minutes")), texFileName)

com30 <- lm(com30 ~ quantScore +
              `Akron, OH` +
              `Albany-Schenectady-Troy, NY` +
              `Atlanta-Sandy Springs-Roswell, GA` +
              `Atlantic City-Hammonton, NJ` +
              `Baltimore-Columbia-Towson, MD` +
              `Binghamton, NY` +
              `Boston, MA` +
              `Buffalo-Cheektowaga-Niagara Falls, NY` +
              `Cambridge-Newton-Framingham, MA` +
              `Camden, NJ` +
              `Charlotte-Concord-Gastonia, NC-SC` +
              `Chattanooga, TN-GA` +
              `Chicago-Naperville-Arlington Heights, IL` +
              `Cleveland-Elyria, OH` +
              `Columbus, OH` +
              `Dallas-Plano-Irving, TX` +
              `Dayton, OH` +
              `Detroit-Dearborn-Livonia, MI` +
              `Duluth, MN-WI` +
              `Erie, PA` +
              `Evansville, IN-KY` +
              `Flint, MI` +
              `Fort Wayne, IN` +
              `Gary, IN` +
              `Grand Rapids-Wyoming, MI` +
              `Houston-The Woodlands-Sugar Land, TX` +
              `Indianapolis-Carmel-Anderson, IN` +
              `Jacksonville, FL` +
              `Kansas City, MO-KS` +
              `Knoxville, TN` +
              `Lake County-Kenosha County, IL-WI` +
              `Louisville/Jefferson County, KY-IN` +
              `Madison, WI` +
              `Manchester-Nashua, NH` +
              `Miami-Miami Beach-Kendall, FL` +
              `Milwaukee-Waukesha-West Allis, WI` +
              `Minneapolis-St. Paul-Bloomington, MN-WI` +
              `Montgomery County-Bucks County-Chester County, PA` +
              `Nashville-Davidson--Murfreesboro--Franklin, TN` +
              `New Orleans-Metairie, LA` +
              `New York-Jersey City-White Plains, NY-NJ` +
              `Newark, NJ-PA` +
              `Philadelphia, PA` +
              `Pittsburgh, PA` +
              `Portland-Vancouver-Hillsboro, OR-WA` +
              `Richmond, VA` +
              `Roanoke, VA` +
              `Rochester, NY` +
              `Rockford, IL` +
              `Seattle-Bellevue-Everett, WA` +
              `South Bend-Mishawaka, IN-MI` +
              `Spokane-Spokane Valley, WA` +
              `St. Louis, MO-IL` +
              `Syracuse, NY` +
              `Tacoma-Lakewood, WA` +
              `Tampa-St. Petersburg-Clearwater, FL` +
              `Toledo, OH` +
              `Trenton, NJ` +
              `Utica-Rome, NY` +
              `Virginia Beach-Norfolk-Newport News, VA-NC` +
              `Warren-Troy-Farmington Hills, MI` +
              `Winston-Salem, NC`,
            # `Youngstown-Warren-Boardman, OH-PA` +
            data = dat)
# print(summary(com30), digits = 3)
texFileName <- "D:/AP LARSON/HOLC/com30.tex"
writeLines(capture.output(stargazer(com30,
                                    style = "qje",
                                    omit = 2:64,
                                    title = "Percentage Commutes 30-39 Minutes")), texFileName)

com40 <- lm(com40 ~ quantScore +
              `Akron, OH` +
              `Albany-Schenectady-Troy, NY` +
              `Atlanta-Sandy Springs-Roswell, GA` +
              `Atlantic City-Hammonton, NJ` +
              `Baltimore-Columbia-Towson, MD` +
              `Binghamton, NY` +
              `Boston, MA` +
              `Buffalo-Cheektowaga-Niagara Falls, NY` +
              `Cambridge-Newton-Framingham, MA` +
              `Camden, NJ` +
              `Charlotte-Concord-Gastonia, NC-SC` +
              `Chattanooga, TN-GA` +
              `Chicago-Naperville-Arlington Heights, IL` +
              `Cleveland-Elyria, OH` +
              `Columbus, OH` +
              `Dallas-Plano-Irving, TX` +
              `Dayton, OH` +
              `Detroit-Dearborn-Livonia, MI` +
              `Duluth, MN-WI` +
              `Erie, PA` +
              `Evansville, IN-KY` +
              `Flint, MI` +
              `Fort Wayne, IN` +
              `Gary, IN` +
              `Grand Rapids-Wyoming, MI` +
              `Houston-The Woodlands-Sugar Land, TX` +
              `Indianapolis-Carmel-Anderson, IN` +
              `Jacksonville, FL` +
              `Kansas City, MO-KS` +
              `Knoxville, TN` +
              `Lake County-Kenosha County, IL-WI` +
              `Louisville/Jefferson County, KY-IN` +
              `Madison, WI` +
              `Manchester-Nashua, NH` +
              `Miami-Miami Beach-Kendall, FL` +
              `Milwaukee-Waukesha-West Allis, WI` +
              `Minneapolis-St. Paul-Bloomington, MN-WI` +
              `Montgomery County-Bucks County-Chester County, PA` +
              `Nashville-Davidson--Murfreesboro--Franklin, TN` +
              `New Orleans-Metairie, LA` +
              `New York-Jersey City-White Plains, NY-NJ` +
              `Newark, NJ-PA` +
              `Philadelphia, PA` +
              `Pittsburgh, PA` +
              `Portland-Vancouver-Hillsboro, OR-WA` +
              `Richmond, VA` +
              `Roanoke, VA` +
              `Rochester, NY` +
              `Rockford, IL` +
              `Seattle-Bellevue-Everett, WA` +
              `South Bend-Mishawaka, IN-MI` +
              `Spokane-Spokane Valley, WA` +
              `St. Louis, MO-IL` +
              `Syracuse, NY` +
              `Tacoma-Lakewood, WA` +
              `Tampa-St. Petersburg-Clearwater, FL` +
              `Toledo, OH` +
              `Trenton, NJ` +
              `Utica-Rome, NY` +
              `Virginia Beach-Norfolk-Newport News, VA-NC` +
              `Warren-Troy-Farmington Hills, MI` +
              `Winston-Salem, NC`,
            # `Youngstown-Warren-Boardman, OH-PA` +
            data = dat)
# print(summary(com40), digits = 3)
texFileName <- "D:/AP LARSON/HOLC/com40.tex"
writeLines(capture.output(stargazer(com40,
                                    style = "qje",
                                    omit = 2:64,
                                    title = "Percentage Commutes 40-59 Minutes")), texFileName)

com60 <- lm(com60 ~ quantScore +
              `Akron, OH` +
              `Albany-Schenectady-Troy, NY` +
              `Atlanta-Sandy Springs-Roswell, GA` +
              `Atlantic City-Hammonton, NJ` +
              `Baltimore-Columbia-Towson, MD` +
              `Binghamton, NY` +
              `Boston, MA` +
              `Buffalo-Cheektowaga-Niagara Falls, NY` +
              `Cambridge-Newton-Framingham, MA` +
              `Camden, NJ` +
              `Charlotte-Concord-Gastonia, NC-SC` +
              `Chattanooga, TN-GA` +
              `Chicago-Naperville-Arlington Heights, IL` +
              `Cleveland-Elyria, OH` +
              `Columbus, OH` +
              `Dallas-Plano-Irving, TX` +
              `Dayton, OH` +
              `Detroit-Dearborn-Livonia, MI` +
              `Duluth, MN-WI` +
              `Erie, PA` +
              `Evansville, IN-KY` +
              `Flint, MI` +
              `Fort Wayne, IN` +
              `Gary, IN` +
              `Grand Rapids-Wyoming, MI` +
              `Houston-The Woodlands-Sugar Land, TX` +
              `Indianapolis-Carmel-Anderson, IN` +
              `Jacksonville, FL` +
              `Kansas City, MO-KS` +
              `Knoxville, TN` +
              `Lake County-Kenosha County, IL-WI` +
              `Louisville/Jefferson County, KY-IN` +
              `Madison, WI` +
              `Manchester-Nashua, NH` +
              `Miami-Miami Beach-Kendall, FL` +
              `Milwaukee-Waukesha-West Allis, WI` +
              `Minneapolis-St. Paul-Bloomington, MN-WI` +
              `Montgomery County-Bucks County-Chester County, PA` +
              `Nashville-Davidson--Murfreesboro--Franklin, TN` +
              `New Orleans-Metairie, LA` +
              `New York-Jersey City-White Plains, NY-NJ` +
              `Newark, NJ-PA` +
              `Philadelphia, PA` +
              `Pittsburgh, PA` +
              `Portland-Vancouver-Hillsboro, OR-WA` +
              `Richmond, VA` +
              `Roanoke, VA` +
              `Rochester, NY` +
              `Rockford, IL` +
              `Seattle-Bellevue-Everett, WA` +
              `South Bend-Mishawaka, IN-MI` +
              `Spokane-Spokane Valley, WA` +
              `St. Louis, MO-IL` +
              `Syracuse, NY` +
              `Tacoma-Lakewood, WA` +
              `Tampa-St. Petersburg-Clearwater, FL` +
              `Toledo, OH` +
              `Trenton, NJ` +
              `Utica-Rome, NY` +
              `Virginia Beach-Norfolk-Newport News, VA-NC` +
              `Warren-Troy-Farmington Hills, MI` +
              `Winston-Salem, NC`,
            # `Youngstown-Warren-Boardman, OH-PA` +
            data = dat)
# print(summary(com60), digits = 3)
texFileName <- "D:/AP LARSON/HOLC/com60.tex"
writeLines(capture.output(stargazer(com60,
                                    style = "qje",
                                    omit = 2:64,
                                    title = "Percentage Commutes 60 or More Minutes")), texFileName)
texFileName <- "D:/AP LARSON/HOLC/allCom.tex"
writeLines(capture.output(stargazer(comBl10, com10, com20, com30, com40, com60,
                                    style = "qje",
                                    omit = 2:64,
                                    title = "Percentage Commutes by Duration (Minutes)")), texFileName)

dat$allBl149 <- dat$pct100 + dat$pct149
poverty <- lm(allBl149 ~ quantScore +
                `Akron, OH` +
                `Albany-Schenectady-Troy, NY` +
                `Atlanta-Sandy Springs-Roswell, GA` +
                `Atlantic City-Hammonton, NJ` +
                `Baltimore-Columbia-Towson, MD` +
                `Binghamton, NY` +
                `Boston, MA` +
                `Buffalo-Cheektowaga-Niagara Falls, NY` +
                `Cambridge-Newton-Framingham, MA` +
                `Camden, NJ` +
                `Charlotte-Concord-Gastonia, NC-SC` +
                `Chattanooga, TN-GA` +
                `Chicago-Naperville-Arlington Heights, IL` +
                `Cleveland-Elyria, OH` +
                `Columbus, OH` +
                `Dallas-Plano-Irving, TX` +
                `Dayton, OH` +
                `Detroit-Dearborn-Livonia, MI` +
                `Duluth, MN-WI` +
                `Erie, PA` +
                `Evansville, IN-KY` +
                `Flint, MI` +
                `Fort Wayne, IN` +
                `Gary, IN` +
                `Grand Rapids-Wyoming, MI` +
                `Houston-The Woodlands-Sugar Land, TX` +
                `Indianapolis-Carmel-Anderson, IN` +
                `Jacksonville, FL` +
                `Kansas City, MO-KS` +
                `Knoxville, TN` +
                `Lake County-Kenosha County, IL-WI` +
                `Louisville/Jefferson County, KY-IN` +
                `Madison, WI` +
                `Manchester-Nashua, NH` +
                `Miami-Miami Beach-Kendall, FL` +
                `Milwaukee-Waukesha-West Allis, WI` +
                `Minneapolis-St. Paul-Bloomington, MN-WI` +
                `Montgomery County-Bucks County-Chester County, PA` +
                `Nashville-Davidson--Murfreesboro--Franklin, TN` +
                `New Orleans-Metairie, LA` +
                `New York-Jersey City-White Plains, NY-NJ` +
                `Newark, NJ-PA` +
                `Philadelphia, PA` +
                `Pittsburgh, PA` +
                `Portland-Vancouver-Hillsboro, OR-WA` +
                `Richmond, VA` +
                `Roanoke, VA` +
                `Rochester, NY` +
                `Rockford, IL` +
                `Seattle-Bellevue-Everett, WA` +
                `South Bend-Mishawaka, IN-MI` +
                `Spokane-Spokane Valley, WA` +
                `St. Louis, MO-IL` +
                `Syracuse, NY` +
                `Tacoma-Lakewood, WA` +
                `Tampa-St. Petersburg-Clearwater, FL` +
                `Toledo, OH` +
                `Trenton, NJ` +
                `Utica-Rome, NY` +
                `Virginia Beach-Norfolk-Newport News, VA-NC` +
                `Warren-Troy-Farmington Hills, MI` +
                `Winston-Salem, NC`,
              # `Youngstown-Warren-Boardman, OH-PA` +
              data = dat)
# print(summary(poverty), digits = 3)
texFileName <- "D:/AP LARSON/HOLC/poverty.tex"
writeLines(capture.output(stargazer(poverty,
                                    style = "qje",
                                    omit = 2:64,
                                    dep.var.labels = "Pct. Below 149% FPL",
                                    covariate.labels = "HOLC Rating",
                                    title = "Percentage Residents Below 149% FPL")), texFileName)

deepPoverty <- lm(pct100 ~ quantScore +
                    `Akron, OH` +
                    `Albany-Schenectady-Troy, NY` +
                    `Atlanta-Sandy Springs-Roswell, GA` +
                    `Atlantic City-Hammonton, NJ` +
                    `Baltimore-Columbia-Towson, MD` +
                    `Binghamton, NY` +
                    `Boston, MA` +
                    `Buffalo-Cheektowaga-Niagara Falls, NY` +
                    `Cambridge-Newton-Framingham, MA` +
                    `Camden, NJ` +
                    `Charlotte-Concord-Gastonia, NC-SC` +
                    `Chattanooga, TN-GA` +
                    `Chicago-Naperville-Arlington Heights, IL` +
                    `Cleveland-Elyria, OH` +
                    `Columbus, OH` +
                    `Dallas-Plano-Irving, TX` +
                    `Dayton, OH` +
                    `Detroit-Dearborn-Livonia, MI` +
                    `Duluth, MN-WI` +
                    `Erie, PA` +
                    `Evansville, IN-KY` +
                    `Flint, MI` +
                    `Fort Wayne, IN` +
                    `Gary, IN` +
                    `Grand Rapids-Wyoming, MI` +
                    `Houston-The Woodlands-Sugar Land, TX` +
                    `Indianapolis-Carmel-Anderson, IN` +
                    `Jacksonville, FL` +
                    `Kansas City, MO-KS` +
                    `Knoxville, TN` +
                    `Lake County-Kenosha County, IL-WI` +
                    `Louisville/Jefferson County, KY-IN` +
                    `Madison, WI` +
                    `Manchester-Nashua, NH` +
                    `Miami-Miami Beach-Kendall, FL` +
                    `Milwaukee-Waukesha-West Allis, WI` +
                    `Minneapolis-St. Paul-Bloomington, MN-WI` +
                    `Montgomery County-Bucks County-Chester County, PA` +
                    `Nashville-Davidson--Murfreesboro--Franklin, TN` +
                    `New Orleans-Metairie, LA` +
                    `New York-Jersey City-White Plains, NY-NJ` +
                    `Newark, NJ-PA` +
                    `Philadelphia, PA` +
                    `Pittsburgh, PA` +
                    `Portland-Vancouver-Hillsboro, OR-WA` +
                    `Richmond, VA` +
                    `Roanoke, VA` +
                    `Rochester, NY` +
                    `Rockford, IL` +
                    `Seattle-Bellevue-Everett, WA` +
                    `South Bend-Mishawaka, IN-MI` +
                    `Spokane-Spokane Valley, WA` +
                    `St. Louis, MO-IL` +
                    `Syracuse, NY` +
                    `Tacoma-Lakewood, WA` +
                    `Tampa-St. Petersburg-Clearwater, FL` +
                    `Toledo, OH` +
                    `Trenton, NJ` +
                    `Utica-Rome, NY` +
                    `Virginia Beach-Norfolk-Newport News, VA-NC` +
                    `Warren-Troy-Farmington Hills, MI` +
                    `Winston-Salem, NC`,
                  # `Youngstown-Warren-Boardman, OH-PA` +
                  data = dat)
# print(summary(deepPoverty), digits = 3)
texFileName <- "D:/AP LARSON/HOLC/deepPoverty.tex"
writeLines(capture.output(stargazer(deepPoverty,
                                    style = "qje",
                                    omit = 2:64,
                                    dep.var.labels = "Pct. Below 100% FPL",
                                    covariate.labels = "HOLC Rating",
                                    title = "Percentage Residents Below 100% FPL")), texFileName)

nWht <- lm(pctWht ~ quantScore +
             `Akron, OH` +
             `Albany-Schenectady-Troy, NY` +
             `Atlanta-Sandy Springs-Roswell, GA` +
             `Atlantic City-Hammonton, NJ` +
             `Baltimore-Columbia-Towson, MD` +
             `Binghamton, NY` +
             `Boston, MA` +
             `Buffalo-Cheektowaga-Niagara Falls, NY` +
             `Cambridge-Newton-Framingham, MA` +
             `Camden, NJ` +
             `Charlotte-Concord-Gastonia, NC-SC` +
             `Chattanooga, TN-GA` +
             `Chicago-Naperville-Arlington Heights, IL` +
             `Cleveland-Elyria, OH` +
             `Columbus, OH` +
             `Dallas-Plano-Irving, TX` +
             `Dayton, OH` +
             `Detroit-Dearborn-Livonia, MI` +
             `Duluth, MN-WI` +
             `Erie, PA` +
             `Evansville, IN-KY` +
             `Flint, MI` +
             `Fort Wayne, IN` +
             `Gary, IN` +
             `Grand Rapids-Wyoming, MI` +
             `Houston-The Woodlands-Sugar Land, TX` +
             `Indianapolis-Carmel-Anderson, IN` +
             `Jacksonville, FL` +
             `Kansas City, MO-KS` +
             `Knoxville, TN` +
             `Lake County-Kenosha County, IL-WI` +
             `Louisville/Jefferson County, KY-IN` +
             `Madison, WI` +
             `Manchester-Nashua, NH` +
             `Miami-Miami Beach-Kendall, FL` +
             `Milwaukee-Waukesha-West Allis, WI` +
             `Minneapolis-St. Paul-Bloomington, MN-WI` +
             `Montgomery County-Bucks County-Chester County, PA` +
             `Nashville-Davidson--Murfreesboro--Franklin, TN` +
             `New Orleans-Metairie, LA` +
             `New York-Jersey City-White Plains, NY-NJ` +
             `Newark, NJ-PA` +
             `Philadelphia, PA` +
             `Pittsburgh, PA` +
             `Portland-Vancouver-Hillsboro, OR-WA` +
             `Richmond, VA` +
             `Roanoke, VA` +
             `Rochester, NY` +
             `Rockford, IL` +
             `Seattle-Bellevue-Everett, WA` +
             `South Bend-Mishawaka, IN-MI` +
             `Spokane-Spokane Valley, WA` +
             `St. Louis, MO-IL` +
             `Syracuse, NY` +
             `Tacoma-Lakewood, WA` +
             `Tampa-St. Petersburg-Clearwater, FL` +
             `Toledo, OH` +
             `Trenton, NJ` +
             `Utica-Rome, NY` +
             `Virginia Beach-Norfolk-Newport News, VA-NC` +
             `Warren-Troy-Farmington Hills, MI` +
             `Winston-Salem, NC`,
           # `Youngstown-Warren-Boardman, OH-PA` +
           data = dat)
# print(summary(nWht), digits = 4)
texFileName <- "D:/AP LARSON/HOLC/nWht.tex"
writeLines(capture.output(stargazer(nWht,
                                    style = "qje",
                                    omit = 2:64,
                                    dep.var.labels = "Pct. White Residents",
                                    covariate.labels = "HOLC Rating",
                                    title = "Percentage White Residents")), texFileName)

nBlk <- lm(pctBlk ~ quantScore +
             `Akron, OH` +
             `Albany-Schenectady-Troy, NY` +
             `Atlanta-Sandy Springs-Roswell, GA` +
             `Atlantic City-Hammonton, NJ` +
             `Baltimore-Columbia-Towson, MD` +
             `Binghamton, NY` +
             `Boston, MA` +
             `Buffalo-Cheektowaga-Niagara Falls, NY` +
             `Cambridge-Newton-Framingham, MA` +
             `Camden, NJ` +
             `Charlotte-Concord-Gastonia, NC-SC` +
             `Chattanooga, TN-GA` +
             `Chicago-Naperville-Arlington Heights, IL` +
             `Cleveland-Elyria, OH` +
             `Columbus, OH` +
             `Dallas-Plano-Irving, TX` +
             `Dayton, OH` +
             `Detroit-Dearborn-Livonia, MI` +
             `Duluth, MN-WI` +
             `Erie, PA` +
             `Evansville, IN-KY` +
             `Flint, MI` +
             `Fort Wayne, IN` +
             `Gary, IN` +
             `Grand Rapids-Wyoming, MI` +
             `Houston-The Woodlands-Sugar Land, TX` +
             `Indianapolis-Carmel-Anderson, IN` +
             `Jacksonville, FL` +
             `Kansas City, MO-KS` +
             `Knoxville, TN` +
             `Lake County-Kenosha County, IL-WI` +
             `Louisville/Jefferson County, KY-IN` +
             `Madison, WI` +
             `Manchester-Nashua, NH` +
             `Miami-Miami Beach-Kendall, FL` +
             `Milwaukee-Waukesha-West Allis, WI` +
             `Minneapolis-St. Paul-Bloomington, MN-WI` +
             `Montgomery County-Bucks County-Chester County, PA` +
             `Nashville-Davidson--Murfreesboro--Franklin, TN` +
             `New Orleans-Metairie, LA` +
             `New York-Jersey City-White Plains, NY-NJ` +
             `Newark, NJ-PA` +
             `Philadelphia, PA` +
             `Pittsburgh, PA` +
             `Portland-Vancouver-Hillsboro, OR-WA` +
             `Richmond, VA` +
             `Roanoke, VA` +
             `Rochester, NY` +
             `Rockford, IL` +
             `Seattle-Bellevue-Everett, WA` +
             `South Bend-Mishawaka, IN-MI` +
             `Spokane-Spokane Valley, WA` +
             `St. Louis, MO-IL` +
             `Syracuse, NY` +
             `Tacoma-Lakewood, WA` +
             `Tampa-St. Petersburg-Clearwater, FL` +
             `Toledo, OH` +
             `Trenton, NJ` +
             `Utica-Rome, NY` +
             `Virginia Beach-Norfolk-Newport News, VA-NC` +
             `Warren-Troy-Farmington Hills, MI` +
             `Winston-Salem, NC`,
           # `Youngstown-Warren-Boardman, OH-PA` +
           data = dat)
# print(summary(nBlk), digits = 4)
texFileName <- "D:/AP LARSON/HOLC/nBlk.tex"
writeLines(capture.output(stargazer(nBlk,
                                    style = "qje",
                                    omit = 2:64,
                                    dep.var.labels = "Pct. Black Residents",
                                    covariate.labels = "HOLC Rating",
                                    title = "Percentage Black Residents")), texFileName)

nHisp <- lm(pctHisp ~ quantScore +
              `Akron, OH` +
              `Albany-Schenectady-Troy, NY` +
              `Atlanta-Sandy Springs-Roswell, GA` +
              `Atlantic City-Hammonton, NJ` +
              `Baltimore-Columbia-Towson, MD` +
              `Binghamton, NY` +
              `Boston, MA` +
              `Buffalo-Cheektowaga-Niagara Falls, NY` +
              `Cambridge-Newton-Framingham, MA` +
              `Camden, NJ` +
              `Charlotte-Concord-Gastonia, NC-SC` +
              `Chattanooga, TN-GA` +
              `Chicago-Naperville-Arlington Heights, IL` +
              `Cleveland-Elyria, OH` +
              `Columbus, OH` +
              `Dallas-Plano-Irving, TX` +
              `Dayton, OH` +
              `Detroit-Dearborn-Livonia, MI` +
              `Duluth, MN-WI` +
              `Erie, PA` +
              `Evansville, IN-KY` +
              `Flint, MI` +
              `Fort Wayne, IN` +
              `Gary, IN` +
              `Grand Rapids-Wyoming, MI` +
              `Houston-The Woodlands-Sugar Land, TX` +
              `Indianapolis-Carmel-Anderson, IN` +
              `Jacksonville, FL` +
              `Kansas City, MO-KS` +
              `Knoxville, TN` +
              `Lake County-Kenosha County, IL-WI` +
              `Louisville/Jefferson County, KY-IN` +
              `Madison, WI` +
              `Manchester-Nashua, NH` +
              `Miami-Miami Beach-Kendall, FL` +
              `Milwaukee-Waukesha-West Allis, WI` +
              `Minneapolis-St. Paul-Bloomington, MN-WI` +
              `Montgomery County-Bucks County-Chester County, PA` +
              `Nashville-Davidson--Murfreesboro--Franklin, TN` +
              `New Orleans-Metairie, LA` +
              `New York-Jersey City-White Plains, NY-NJ` +
              `Newark, NJ-PA` +
              `Philadelphia, PA` +
              `Pittsburgh, PA` +
              `Portland-Vancouver-Hillsboro, OR-WA` +
              `Richmond, VA` +
              `Roanoke, VA` +
              `Rochester, NY` +
              `Rockford, IL` +
              `Seattle-Bellevue-Everett, WA` +
              `South Bend-Mishawaka, IN-MI` +
              `Spokane-Spokane Valley, WA` +
              `St. Louis, MO-IL` +
              `Syracuse, NY` +
              `Tacoma-Lakewood, WA` +
              `Tampa-St. Petersburg-Clearwater, FL` +
              `Toledo, OH` +
              `Trenton, NJ` +
              `Utica-Rome, NY` +
              `Virginia Beach-Norfolk-Newport News, VA-NC` +
              `Warren-Troy-Farmington Hills, MI` +
              `Winston-Salem, NC`,
            # `Youngstown-Warren-Boardman, OH-PA` +
            data = dat)
# print(summary(nHisp), digits = 4)
texFileName <- "D:/AP LARSON/HOLC/nHisp.tex"
writeLines(capture.output(stargazer(nHisp,
                                    style = "qje",
                                    omit = 2:64,
                                    dep.var.labels = "Pct. Hispanic/Latino Residents",
                                    covariate.labels = "HOLC Rating",
                                    title = "Percentage Hispanic/Latino Residents")), texFileName)

rentCost <- lm(hunMedRent ~ quantScore +
                 `Akron, OH` +
                 `Albany-Schenectady-Troy, NY` +
                 `Atlanta-Sandy Springs-Roswell, GA` +
                 `Atlantic City-Hammonton, NJ` +
                 `Baltimore-Columbia-Towson, MD` +
                 `Binghamton, NY` +
                 `Boston, MA` +
                 `Buffalo-Cheektowaga-Niagara Falls, NY` +
                 `Cambridge-Newton-Framingham, MA` +
                 `Camden, NJ` +
                 `Charlotte-Concord-Gastonia, NC-SC` +
                 `Chattanooga, TN-GA` +
                 `Chicago-Naperville-Arlington Heights, IL` +
                 `Cleveland-Elyria, OH` +
                 `Columbus, OH` +
                 `Dallas-Plano-Irving, TX` +
                 `Dayton, OH` +
                 `Detroit-Dearborn-Livonia, MI` +
                 `Duluth, MN-WI` +
                 `Erie, PA` +
                 `Evansville, IN-KY` +
                 `Flint, MI` +
                 `Fort Wayne, IN` +
                 `Gary, IN` +
                 `Grand Rapids-Wyoming, MI` +
                 `Houston-The Woodlands-Sugar Land, TX` +
                 `Indianapolis-Carmel-Anderson, IN` +
                 `Jacksonville, FL` +
                 `Kansas City, MO-KS` +
                 `Knoxville, TN` +
                 `Lake County-Kenosha County, IL-WI` +
                 `Louisville/Jefferson County, KY-IN` +
                 `Madison, WI` +
                 `Manchester-Nashua, NH` +
                 `Miami-Miami Beach-Kendall, FL` +
                 `Milwaukee-Waukesha-West Allis, WI` +
                 `Minneapolis-St. Paul-Bloomington, MN-WI` +
                 `Montgomery County-Bucks County-Chester County, PA` +
                 `Nashville-Davidson--Murfreesboro--Franklin, TN` +
                 `New Orleans-Metairie, LA` +
                 `New York-Jersey City-White Plains, NY-NJ` +
                 `Newark, NJ-PA` +
                 `Philadelphia, PA` +
                 `Pittsburgh, PA` +
                 `Portland-Vancouver-Hillsboro, OR-WA` +
                 `Richmond, VA` +
                 `Roanoke, VA` +
                 `Rochester, NY` +
                 `Rockford, IL` +
                 `Seattle-Bellevue-Everett, WA` +
                 `South Bend-Mishawaka, IN-MI` +
                 `Spokane-Spokane Valley, WA` +
                 `St. Louis, MO-IL` +
                 `Syracuse, NY` +
                 `Tacoma-Lakewood, WA` +
                 `Tampa-St. Petersburg-Clearwater, FL` +
                 `Toledo, OH` +
                 `Trenton, NJ` +
                 `Utica-Rome, NY` +
                 `Virginia Beach-Norfolk-Newport News, VA-NC` +
                 `Warren-Troy-Farmington Hills, MI` +
                 `Winston-Salem, NC`,
               # `Youngstown-Warren-Boardman, OH-PA` +
               data = dat)
# print(summary(rentCost), digits = 3)
texFileName <- "D:/AP LARSON/HOLC/rentCost.tex"
writeLines(capture.output(stargazer(rentCost,
                                    style = "qje",
                                    omit = 2:64,
                                    dep.var.labels = "Median Rent, $100s",
                                    covariate.labels = "HOLC Rating",
                                    title = "Median Rent, $100s")), texFileName)

grapi <- lm(grapi ~ quantScore +
              `Akron, OH` +
              `Albany-Schenectady-Troy, NY` +
              `Atlanta-Sandy Springs-Roswell, GA` +
              `Atlantic City-Hammonton, NJ` +
              `Baltimore-Columbia-Towson, MD` +
              `Binghamton, NY` +
              `Boston, MA` +
              `Buffalo-Cheektowaga-Niagara Falls, NY` +
              `Cambridge-Newton-Framingham, MA` +
              `Camden, NJ` +
              `Charlotte-Concord-Gastonia, NC-SC` +
              `Chattanooga, TN-GA` +
              `Chicago-Naperville-Arlington Heights, IL` +
              `Cleveland-Elyria, OH` +
              `Columbus, OH` +
              `Dallas-Plano-Irving, TX` +
              `Dayton, OH` +
              `Detroit-Dearborn-Livonia, MI` +
              `Duluth, MN-WI` +
              `Erie, PA` +
              `Evansville, IN-KY` +
              `Flint, MI` +
              `Fort Wayne, IN` +
              `Gary, IN` +
              `Grand Rapids-Wyoming, MI` +
              `Houston-The Woodlands-Sugar Land, TX` +
              `Indianapolis-Carmel-Anderson, IN` +
              `Jacksonville, FL` +
              `Kansas City, MO-KS` +
              `Knoxville, TN` +
              `Lake County-Kenosha County, IL-WI` +
              `Louisville/Jefferson County, KY-IN` +
              `Madison, WI` +
              `Manchester-Nashua, NH` +
              `Miami-Miami Beach-Kendall, FL` +
              `Milwaukee-Waukesha-West Allis, WI` +
              `Minneapolis-St. Paul-Bloomington, MN-WI` +
              `Montgomery County-Bucks County-Chester County, PA` +
              `Nashville-Davidson--Murfreesboro--Franklin, TN` +
              `New Orleans-Metairie, LA` +
              `New York-Jersey City-White Plains, NY-NJ` +
              `Newark, NJ-PA` +
              `Philadelphia, PA` +
              `Pittsburgh, PA` +
              `Portland-Vancouver-Hillsboro, OR-WA` +
              `Richmond, VA` +
              `Roanoke, VA` +
              `Rochester, NY` +
              `Rockford, IL` +
              `Seattle-Bellevue-Everett, WA` +
              `South Bend-Mishawaka, IN-MI` +
              `Spokane-Spokane Valley, WA` +
              `St. Louis, MO-IL` +
              `Syracuse, NY` +
              `Tacoma-Lakewood, WA` +
              `Tampa-St. Petersburg-Clearwater, FL` +
              `Toledo, OH` +
              `Trenton, NJ` +
              `Utica-Rome, NY` +
              `Virginia Beach-Norfolk-Newport News, VA-NC` +
              `Warren-Troy-Farmington Hills, MI` +
              `Winston-Salem, NC` +
            # `Youngstown-Warren-Boardman, OH-PA` +
              hunMedRent,
            data = dat)
# print(summary(grapi), digits = 3)
texFileName <- "D:/AP LARSON/HOLC/grapi.tex"
writeLines(capture.output(stargazer(grapi,
                                    style = "qje",
                                    omit = 2:64,
                                    dep.var.labels = "GRAPI",
                                    covariate.labels = c("HOLC Rating", "Median Rent, $100s"),
                                    title = "Gross Rent as a Percentage of Annual Income")), texFileName)

myCols <- c("grapi", "hunMedRent")
testCase <- dat[myCols]
cor(testCase, method = "pearson", use = "complete.obs")

# For fun, by region. Doesn't control for within-city corr, though
housingValueR <- lm(thouHousVal ~ quantScore +
                     regWest +
                     regNortheast +
                     regSouth +
                     bed0 +
                     bed1 +
                     bed2 +
                     bed3 +
                     bed4 +
                     medAge +
                     completePlumb +
                     completeKitch, data = dat)
# print(summary(housingValue), digits = 3)
