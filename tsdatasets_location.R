#######################################################################################
#######################################################################################
### tsdatasets_location.R (version 3.0, 2024/01/06)                                 ###
### by: Aurelio Tobias (aurelio.tobias@idaea.csic.es)                               ###
###     Lina Madaniyazi (lina.madaniyazi@nagasaki-u.ac.jp)                          ###
#######################################################################################
### Compilation of open access time-series datasets for studying                    ###
### temperature-mortality association and other environmental stressors             ###
### by location.                                                                    ###
#######################################################################################
#######################################################################################

# Remove objects.
rm(list = ls())

# Install packages.
#install.packages("dplyr")
#install.packages("foreign")

# Load packages.
library(dplyr)
library(foreign)

#######################################################################################
### Baltimore dataset (city level)
#######################################################################################

# Upload raw data from tsModel library.
#install.packages("tsModel")
library(tsModel)
data(balt)

# Select variables.
balt.aux <- subset(balt, select=c(date, death, cvd, resp, agecat, tmpd, dptp, pm10tmean))

# Collapse dataset.
balt.aux <- balt.aux %>% group_by(date) %>%
  summarize(all = sum(death), 
            cvd = sum(cvd),
            res = sum(resp),
            temp = mean(tmpd),
            dewp = mean(dptp),
            pm10 = mean(pm10tmean)) 

# Convert temperature from Fareneith to Celsius.
balt.aux$temp <- (balt.aux$temp-32)*5/9

# Format decimals.
balt.aux$temp <- round(balt.aux$temp, 1)
balt.aux$dewp <- round(balt.aux$dewp, 1)
balt.aux$pm10 <- round(balt.aux$pm10, 1)

# Check format date.
class(balt.aux$date)

# Save Baltimore dataset cleaned up.
balt.us8700 <- data.frame(balt.aux)
head(balt.us8700)
rm(balt, balt.aux)

#######################################################################################
### Chicago dataset (city level)
#######################################################################################

# Upload raw data from dlnm library.
#install.packages("dlnm")
library(dlnm)
data(chicagoNMMAPS)

# Select variables.
chicago.aux <- subset(chicagoNMMAPS, select=c(date, death, cvd, resp, temp, dptp, rhum, pm10, o3))

# Rename variables.
chicago.aux <- chicago.aux %>% 
  rename(all = death,
         res = resp,
         dewp = dptp)

# Format decimals.
chicago.aux$temp <- round(chicago.aux$temp, 1)
chicago.aux$dewp <- round(chicago.aux$dewp, 1)
chicago.aux$rhum <- round(chicago.aux$rhum, 1)
chicago.aux$pm10 <- round(chicago.aux$pm10, 1)
chicago.aux$o3   <- round(chicago.aux$o3, 1)

# Check format date.
class(chicago.aux$date)

# Save Chicago dataset cleaned up.
chic.us8700 <- chicago.aux
head(chic.us8700)
rm(chicagoNMMAPS, chicago.aux)

#######################################################################################
### Valencia dataset (city level)
#######################################################################################

# Load Valencia dataset from GitHub.
urlfile <- "https://raw.githubusercontent.com/aureliotobias/valenciatempmort/master/valencia0107.csv"
valencia <- read.csv(urlfile)

# Select variables.
valencia.aux <- subset(valencia, select=c(date, all, cv, res, tmean, rh, pm10, o3))

# Rename variables.
valencia.aux <- valencia.aux %>% 
  rename(cvd = cv,
         temp = tmean,
         rhum = rh)

# Check format date.
valencia.aux$date <- as.Date(valencia.aux$date)
class(valencia.aux$date)

# Save Valencia dataset cleaned up.
vale.sp0107 <- valencia.aux
head(vale.sp0107)
rm(valencia, valencia.aux)

#######################################################################################
### UK (regional level)
#######################################################################################

# Load UK 10 Regions dataset from GitHub.
urlfile <- "https://raw.githubusercontent.com/gasparrini/2016_gasparrini_AJE_Rcodedata/master/regEngWales19902012.csv"
uk <- read.csv(urlfile)

# Select variables.
uk.aux <- subset(uk, select=c(regnames, date, death, tmean))

# Rename variables.
uk.aux <- uk.aux %>% 
  rename(all = death,
         temp = tmean)

# Format decimals.
uk.aux$temp <- round(uk.aux$temp, 1)

# Check format date.
uk.aux$date <- as.Date(uk.aux$date)
class(uk.aux$date)

# Split in 10 data frames.
dlist <- split(uk.aux, uk.aux$regnames)
summary(dlist)
emid.uk9012 <- dlist[[1]]
east.uk9012 <- dlist[[2]]
lond.uk9012 <- dlist[[3]]
neas.uk9012 <- dlist[[4]]
nwes.uk9012 <- dlist[[5]]
seas.uk9012 <- dlist[[6]]
swes.uk9012 <- dlist[[7]]
wmid.uk9012 <- dlist[[8]]
wale.uk9012 <- dlist[[9]]
york.uk9012 <- dlist[[10]]

# Remove variables.
emid.uk9012 <- data.frame(subset(emid.uk9012, select=-c(regnames)))
east.uk9012 <- data.frame(subset(east.uk9012, select=-c(regnames)))
lond.uk9012 <- data.frame(subset(lond.uk9012, select=-c(regnames)))
neas.uk9012 <- data.frame(subset(neas.uk9012, select=-c(regnames)))
nwes.uk9012 <- data.frame(subset(nwes.uk9012, select=-c(regnames)))
seas.uk9012 <- data.frame(subset(seas.uk9012, select=-c(regnames)))
swes.uk9012 <- data.frame(subset(swes.uk9012, select=-c(regnames)))
wmid.uk9012 <- data.frame(subset(wmid.uk9012, select=-c(regnames)))
wale.uk9012 <- data.frame(subset(wale.uk9012, select=-c(regnames)))
york.uk9012 <- data.frame(subset(york.uk9012, select=-c(regnames)))

#######################################################################################
### UK (country level)
#######################################################################################

# Collapse UK dataset.
uk.aux <- uk.aux %>% group_by(date) %>%
  summarize(all = sum(all), 
            temp = mean(temp)) 

# Save UK dataset cleaned up.
engw.uk9012 <- data.frame(uk.aux)
head(engw.uk9012)
rm(dlist, uk, uk.aux)

#######################################################################################
### Update London dataset (metropolitan region level)
#######################################################################################

##### Merge cause-specific mortality and relative humidity for London 1993-2003.

# Load London data from GitHub.
urlfile <- "https://raw.githubusercontent.com/gasparrini/2016_vicedo-cabrera_AJE_Rcodedata/master/london.RData"
load(url(urlfile))

# Rename variables.
london <- london %>% 
  rename(res = resp)

# Format decimals.
london$rhum <- round(london$rhum, 1)

# Check format date.
class(london$date)

# Select variables.
london.aux1 <- subset(london, select=c(date, cvd, res, rhum))
head(london.aux1)

# Merge datasets.
london.aux0 <- lond.uk9012
london_merge <- left_join(london.aux0, london.aux1, by = "date")
head(london_merge)

##### Merge ozone for London 2002-2006.

# Load London data from GitHub.
urlfile <- "https://raw.githubusercontent.com/gasparrini/2013_bhaskaran_IJE_Codedata/master/londondataset2002_2006.dta"
london2 <- read.dta(urlfile)

# Format decimals into a new variable.
london2$o3 <- round(london2$ozone, 1)

# Check format date.
class(london2$date)

# Select variables.
london.aux2 <- subset(london2, select=c(date, o3))
head(london.aux2)

# Merge datasets.
lond.uk9012 <- left_join(london_merge, london.aux2, by = "date")
lond.uk9012 <- lond.uk9012 %>%
  select(date, all, cvd, res, temp, rhum, o3)
head(lond.uk9012)

# Remove auxiliary London datasets.
rm(london, london2, london.aux0, london.aux1, london.aux2, london_merge)

#######################################################################################
### Greece dataset (country level)
#######################################################################################

# Upload raw data from dlnm library.
#install.packages("FluMoDL")
library(FluMoDL)
data(greece)
greece.aux <- greece$daily

# Rename variables.
greece.aux <- greece.aux %>% 
  rename(all =deaths,
         temp = temperature)

# Check format date.
class(greece.aux$date)

# Save Greece dataset cleaned up.
gree.gr1317 <- data.frame(greece.aux)
head(gree.gr1317)
rm(greece, greece.aux)

#######################################################################################
### Create list of datasets
#######################################################################################

dlist <- list(balt.us8700, chic.us8700, vale.sp0107, lond.uk9012, 
              neas.uk9012, nwes.uk9012, york.uk9012, emid.uk9012, wmid.uk9012, east.uk9012, seas.uk9012, swes.uk9012, wale.uk9012,
              engw.uk9012, gree.gr1317)
names(dlist) <- c("balt.us8700", "chic.us8700", "vale.sp0107", "lond.uk9012", 
                  "neas.uk9012", "nwes.uk9012", "york.uk9012", "emid.uk9012", "wmid.uk9012", "east.uk9012", "seas.uk9012", "swes.uk9012", "wale.uk9012",
                  "engw.uk9012", "gree.gr1317")
names(dlist)

#######################################################################################
### Create list of locations
#######################################################################################

locations <- data.frame(
  location = c("balt.us8700", "chic.us8700", "vale.sp0107", "lond.uk9012", 
               "neas.uk9012", "nwes.uk9012", "york.uk9012", "emid.uk9012", "wmid.uk9012", "east.uk9012", "seas.uk9012", "swes.uk9012", "wale.uk9012",
               "engw.uk9012", "gree.gr1317"),
  locationname = c("Baltimore", "Chicago", "Valencia", "London", 
                   "N-East", "N-West", "Yorkshire & Humber", "E-Midlands", "W-Midlands", "East", "S-East", "S-West", "Wales",
                   "England & Wales", "Greece"),
  countryname = c("US", "US", "Spain", "UK", 
                  "UK", "UK", "UK", "UK", "UK", "UK", "UK", "UK", "UK",
                  "UK", "Greece"),
  typearea = c("city", "city", "city", "metropolitan", 
               "region", "region", "region", "region", "region", "region", "region", "region", "region",
               "country", "country") 
)
locations

#######################################################################################
### Save list of datasets
#######################################################################################

save(dlist, locations, file = "tsdatasets_location.RData")

#######################################################################################
#######################################################################################
###                           End of script file                                    ###
#######################################################################################
#######################################################################################
