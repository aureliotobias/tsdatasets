#######################################################################################
#######################################################################################
### tsdatasets_age.R (version 3.0, 2024/01/06)                                      ###
### by: Aurelio Tobias (aurelio.tobias@idaea.csic.es)                               ###
###     Lina Madaniyazi (lina.madaniyazi@nagasaki-u.ac.jp)                          ###
#######################################################################################
### Compilation of open access time-series datasets for studying                    ###
### temperature-mortality association and other environmental stressors             ###
### by location and age group.                                                      ###
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
balt.aux <- subset(balt, select=c(date, agecat, death, cvd, resp, agecat, tmpd, dptp, pm10tmean))

# Rename variables.
balt.aux <- balt.aux %>% 
  rename(all = death, 
         res = resp,
         temp = tmpd,
         dewp = dptp,
         pm10 = pm10tmean) 

# Convert temperature from Fareneith to Celsius.
balt.aux$temp <- (balt.aux$temp-32)*5/9

# Format decimals.
balt.aux$temp <- round(balt.aux$temp, 1)
balt.aux$dewp <- round(balt.aux$dewp, 1)
balt.aux$pm10 <- round(balt.aux$pm10, 1)

# Check format date.
class(balt.aux$date)

# Subset under 65 yr.
balt.under <- subset(balt.aux, agecat=="under65", select=-c(agecat, agecat.1))
balt.under$age <- 0
head(balt.under)

# Subset over 65 yr.
balt.over <- subset(balt.aux, agecat=="65to74" | agecat=="75p", select=-c(agecat, agecat.1))

# Collapse dataset.
balt.over <- balt.over %>% group_by(date) %>%
  summarize(all = sum(all), 
            cvd = sum(cvd),
            res = sum(res),
            temp = mean(temp),
            dewp = mean(dewp),
            pm10 = mean(pm10),
            age = 1)
head(balt.over)

# Append age files.
balt.age <- rbind(balt.under, balt.over)
head(balt.age)
table(balt.age$age)

# Label age.
balt.age$age <- factor(balt.age$age, levels = c(0, 1), labels = c("under65", "above65"))
table(balt.age$age)

# Save Baltimore dataset cleaned up.
balt.age <- balt.age %>%
  select(date, age, all, cvd, res, temp, dewp, pm10)
balt.age.us8700 <- balt.age
head(balt.age.us8700)
rm(balt, balt.aux, balt.under, balt.over, balt.age)

#######################################################################################
### Valencia dataset (city level)
#######################################################################################

# Load Valencia dataset from GitHub.
urlfile <- "https://raw.githubusercontent.com/aureliotobias/valenciatempmort/master/valencia0107.csv"
valencia <- read.csv(urlfile)

# Rename variables.
valencia <- valencia  %>% 
  rename(temp = tmean,
         rhum = rh)

# Check format date.
valencia$date <- as.Date(valencia$date)
class(valencia$date)

# Subset under 65 yr.
valencia.under <- subset(valencia, select=c(date, all_0_14, all_15_64, temp, rhum, pm10, o3))
valencia.under$all <- valencia.under$all_0_14 + valencia.under$all_15_64
valencia.under$age <- 0
valencia.under <- subset(valencia.under, select=-c(all_0_14, all_15_64))
head(valencia.under)

# Subset over 65 yr.
valencia.over <- subset(valencia, select=c(date, all_65, temp, rhum, pm10, o3))
valencia.over$all <- valencia.over$all_65
valencia.over$age <- 1
valencia.over <- subset(valencia.over, select=-c(all_65))
head(valencia.over)

# Append age files.
valencia.age <- rbind(valencia.under, valencia.over)
head(valencia.age)
table(valencia.age$age)

# Label age.
valencia.age$age <- factor(valencia.age$age, levels = c(0, 1), labels = c("under65", "above65"))
table(valencia.age$age)

# Save Valencia dataset cleaned up.
valencia.age <- valencia.age %>%
  select(date, age, all, temp, rhum, pm10, o3)
vale.age.sp0107 <- valencia.age 
head(vale.age.sp0107)
rm(valencia, valencia.under, valencia.over, valencia.age)

#######################################################################################
### London (metropolitan region)
#######################################################################################

# Load London dataset from GitHub.
urlfile <- "https://raw.githubusercontent.com/gasparrini/2019_vicedo-cabrera_Epidem_Rcodedata/master/lndn_obs.csv"
london <- read.csv(urlfile)

# Rename variables.
london  <- london  %>% 
  rename(temp = tmean)

# Format decimals.
london$temp <- round(london$temp, 1)

# Check format date.
london$date <- as.Date(london$date, format = "%m/%d/%Y")
class(london$date)

# Subset under 65 yr.
london.under <- subset(london, select=c(date, all_0_64, temp))
london.under$all <- london.under$all_0_64
london.under$age <- 0
london.under <- subset(london.under, select=-c(all_0_64))
head(london.under)

# Subset over 65 yr.
london.over <- subset(london, select=c(date, all_65_74, all_75_84, all_85plus, temp))
london.over$all <- london.over$all_65_74 + london.over$all_75_84 + london.over$all_85plus
london.over$age <- 1
london.over <- subset(london.over, select=-c(all_65_74, all_75_84, all_85plus))
head(london.over)

# Append age files.
london.age <- rbind(london.under, london.over)
head(london.age)
table(london.age$age)

# Label age.
london.age$age <- factor(london.age$age, levels = c(0, 1), labels = c("under65", "above65"))
table(london.age$age)

# Save London dataset cleaned up.
london.age <- london.age %>%
  select(date, age, all, temp)
lond.age.uk9012 <- london.age 
head(lond.age.uk9012)
rm(london, london.under, london.over, london.age)

#######################################################################################
### Update London dataset (metropolitan region level)
#######################################################################################

##### Merge relative humidity for London 1993-2003.

# Load London data from GitHub.
urlfile <- "https://raw.githubusercontent.com/gasparrini/2016_vicedo-cabrera_AJE_Rcodedata/master/london.RData"
load(url(urlfile))

# Format decimals.
london$rhum <- round(london$rhum, 1)

# Check format date.
class(london$date)

# Select variables.
london.aux1 <- subset(london, select=c(date, rhum))
head(london.aux1)

# Merge datasets.
london.aux0 <- lond.age.uk9012
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
lond.age.uk9012 <- left_join(london_merge, london.aux2, by = "date")
lond.age.uk9012 <- lond.age.uk9012 %>%
  select(date, age, all, temp, rhum, o3)
head(lond.age.uk9012)

# Remove auxiliary London datasets.
rm(london, london2, london.aux0, london.aux1, london.aux2, london_merge)

#######################################################################################
### Create list of datasets
#######################################################################################

dlist <- list(balt.age.us8700, vale.age.sp0107, lond.age.uk9012)
names(dlist) <- c("balt.age.us8700", "vale.age.sp0107", "lond.age.uk9012")
names(dlist)

#######################################################################################
### Create list of locations
#######################################################################################

locations <- data.frame(
  location = c("balt.age.us8700", "vale.age.sp0107", "lond.age.uk9012"),
  locationname = c("Baltimore", "Valencia", "London"),
  countryname = c("US", "Spain", "UK"),
  typearea = c("city", "city", "metropolitan") 
)
locations

#######################################################################################
### Save list of datasets
#######################################################################################

save(dlist, locations, file = "tsdatasets_age.RData")

#######################################################################################
#######################################################################################
###                           End of script file                                    ###
#######################################################################################
#######################################################################################
