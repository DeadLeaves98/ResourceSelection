# Resource Selection  Real vs Random _ Scale to Course
# Last Edited: Jan 08 2025
# Autumn Randall 

# Read in the "clean" csv for telemetry data from github repository
library(rgdal); library(raster); library(adehabitatHR); library(rgeos); library(sf); library(dplyr)
library(lubridate); library(stringr); library(hablar); library(AICcmodavg);  library(lme4); library(lwgeom)
nobo1 <- read.csv("./Data/Telemetry/cleaned_Data_28June2024_Part1.csv")
nrow(nobo1)

# Generate Randoms ----
# save Alber's Equal Area Conic projection
albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
OP <- readOGR("./shapefiles/OrtonCourses_JustTreatmentSites.shp")

# Also fix the names in the shapefile
OP$course <- ifelse(OP$course == "campcrane2", "campcrane", OP$course) # change "campcrane2" to "campcrane"
OP$course <- ifelse(OP$course == "campcrane1", "campcrane", OP$course) # change "campcrane2" to "campcrane"

# blankDF
RandomsDF <- data.frame("X" = NA, "Bird.ID" = NA, "ObjectID" = NA, "Date" = NA, "Observer" = NA, 
                        "Bird.Status" = NA, "Fate" = NA, "Location.Type" = "Random", "x" = NA, "y" = NA, "encounter" = NA,
                        "year" = NA, "ordinal" = NA, "week" = NA, "breedingseason" = NA, "breedingseasonCov" = NA)

# make blank data.frame() to hold randoms
nobo2 <- nobo1[0,] # blank data.frame

#### The for() loop:
for(i in 1:length(unique(nobo1$Bird.ID))){
  
  # i = 100 
  # subset one bird using the subset function
  nobo_i <- subset(nobo1, Bird.ID == unique(nobo1$Bird.ID)[i]) # randomly subsetting the data and taking the 100th bird'
  
  # turn nobo_i into spatial. Used for a figure below
  nobo_i_spatial <- SpatialPoints(coords = data.frame("x" = nobo_i$x, "y" = nobo_i$y)) # convert DF to Spatial Points
  crs(nobo_i_spatial) <- CRS("+init=epsg:4326") # define CRS for the spatial points (EPSG 4326 == lat/lon WGS84 etc)
  nobo_i_spatial <- spTransform(nobo_i_spatial, crs(OP)) # make sure it matches OP shapefile: "North American Datum 1983" 
  plot(OP); plot(nobo_i_spatial, add = TRUE, col = "blue") 
  
  # isolate course_i
  course_i_sh <- subset(OP, course == nobo_i$CentroidCourses[1]) 
  plot(course_i_sh); plot(nobo_i_spatial, add = TRUE, col = "blue") # plot to check it 
  
  # DJ generates random points
  course_i_sh2 <- st_as_sf(course_i_sh) # convert to sf
  points1 = st_sample(course_i_sh2, size = nrow(nobo_i) * 2) # generate 2 random points for each "used" point using st_sample()
  points1 <- as_Spatial(points1) # convert back to sp
  plot(course_i_sh); plot(points1, add = TRUE, col = "red"); plot(nobo_i_spatial, add = TRUE, col = "blue") # plot to confirm it worked
  points2 <- spTransform(points1, CRS("+init=epsg:4326")) # convert to lat/long because this is what nobo1 uses
  randomcoords <- data.frame(points2@coords) # extract lat/long from the points and convert to data.frame
  # NEW: We need to add random coordinates to a dataframe that will 
  
  # generate pseudo-dates for the randoms
  random_dates <- c(nobo_i$Date, nobo_i$Date)
  
  # Create an empty df for randoms suitable for rbind()
  head(nobo1) # take a look at NOBO data.frame
  newrandoms <- data.frame("X" = NA, "Bird.ID" = nobo_i$Bird.ID, "ObjectID" = NA, "Date" = random_dates, 
                           "Observer" = NA, "Bird.Status" = NA, "Fate" = NA, 
                           "Location.Type" = "Random", "x" = randomcoords$coords.x1, 
                           "y" = randomcoords$coords.x2, "chick" = NA, "encounter" = NA,
                           "year" = NA, "ordinal" = NA, "week" = NA, "breedingseason" = NA, 
                           "breedingseasonCov" = NA, "n" = nobo_i$n,
                           "CentroidCourses" = nobo_i$CentroidCourses)
  
  nobo2 <- rbind(nobo2, newrandoms) # rbind randoms and used pts
  
  # progress bar
  compl <- round(i/length(unique(nobo1$Bird.ID))*15,0)
  cat(paste0("\r [", strrep("|", compl),strrep(".", 50-compl),"] ", round(100*i/length(unique(nobo1$Bird.ID)),0), "% complete, bird #", i, " done"))
} 

nrow(nobo1) == nrow(nobo2)/2 # true --> the number of randoms should be 2 times the amount for used 
nobo1 = nobo1 %>% relocate("ObjectID", .after = "Bird.ID") # change placement of ObjectID column

nobo3 <- rbind(nobo1, nobo2) # 128,238  rows 

# asssign point status with a binary value (1/0) to indicate used or random 
nobo3$response <- ifelse(nobo3$Location.Type == "Regular", 1, 0) 


# Adding Covariates ----

##### Course: Management Unit  ---- 
# Turn nobo2 to nobo for the sake of ease 
nobo = nobo3

# Just as a reminder the course is in the spatial feature 'OP' 
head(OP)

# bird locations as spatial
nobo_sp <- SpatialPoints(coords = data.frame("x" = nobo$x, "y" = nobo$y)) # convert DF to Spatial Points
crs(nobo_sp) <- CRS("+init=epsg:4326") # define CRS for the spatial points (EPSG 4326 == lat/lon WGS84 etc)
nobo_sp <- spTransform(nobo_sp, crs(OP)) # transform nobo_sp from WGS84 to match "roads"

# make sure they look OK
plot(OP); plot(nobo_sp, add = TRUE)

# extract course ID for points
extraction <- over(nobo_sp, OP)
extraction$course <- ifelse(is.na(extraction$course), "other", extraction$course) # change "NA" to "other"
extraction$course <- ifelse(extraction$course == "campcrane2", "campcrane", extraction$course) # change "campcrane2" to "campcrane"
extraction$course <- ifelse(extraction$course == "campcrane1", "campcrane", extraction$course) # change "campcrane1" to "campcrane"
unique(extraction$course)

# same number of rows for "nobo" and "extract"
nrow(nobo) # 81k
length(extraction$course) # 81k

# cbind course with bird observations
nobo$course <- extraction$course
head(nobo) 

# make sure it worked
acb <- subset(nobo, course == "allenscreek")
acb_sp <- SpatialPoints(coords = data.frame("x" = acb$x, "y" = acb$y)) # convert DF to Spatial Points
crs(acb_sp) <- CRS("+init=epsg:4326") # define CRS for the spatial points (EPSG 4326 == lat/lon WGS84 etc)
acb_sp <- spTransform(acb_sp, crs(OP)) # transform nobo_sp from WGS84 to match "roads"
plot(OP); plot(acb_sp, add = TRUE, cex = 1, pch = ".")


##### Burn Status ---- 
# read in OP shapefile
burn22 <- readOGR("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2022/Adult data/Resource Use/shapefiles/BurnMap2022.shp")
burn23 = readOGR("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2022/Adult data/Resource Use/shapefiles/BurnMap2023.shp")
burn24 = readOGR("E:/NOBO R Projects_Github/NOBO_ResourceSelection/Shapefiles/2024BurnMapSHP/2024BurnMapSHP.shp")

nobo$Date = ymd(nobo$Date) # make this into a date column and not a character column
nobo21 = nobo[nobo$Date >= "2021-03-15" & nobo$Date <="2022-03-31", ] # dataframe to hold 2021
nobo22 = nobo[nobo$Date >= "2022-04-01" & nobo$Date <="2023-03-31", ] # dataframe to hold 2022 birds and randoms
nobo23 = nobo[nobo$Date >= "2023-04-01" & nobo$Date <="2024-03-31", ] # dataframe to hold 2023 birds and randoms
nobo24 = nobo[nobo$Date >= "2024-04-01" & nobo$Date <="2025-03-31", ] 

nrow(nobo24) 

# 2021 -- this is going to be unorthodox and there is definitely a better way 
nobo21_sp <- SpatialPoints(coords = data.frame("x" = nobo21$x, "y" = nobo21$y)) # convert DF to Spatial Points
crs(nobo21_sp) <- CRS("+init=epsg:4326") # define CRS for the spatial points (EPSG 4326 == lat/lon WGS84 etc)
nobo21_sp <- spTransform(nobo21_sp, crs(burn22)) # transform nobo_sp from WGS84 to match "burns22"
plot(burn22); plot(nobo21_sp, add = TRUE) # check to see if it worked
extraction21 <- over(nobo21_sp, burn22)# extract burn status for points
extraction21["Burns_2022"][is.na(extraction21["Burns_2022"])] <- "no"  # if it is an NA it gets a yes because if it wasnt burned in 22 then it was burned in 21 
unique(extraction21$Burns_2022)
nobo21$burn_stat = extraction21$Burns_2022

nobo21$burnyear = 2021 # giving it a year (honestly, might be unnecesary)
nobo21$yearBinary = 0 #making year binary  YEAR 2022 = 0

nobo.real.yes = subset(nobo21, burn_stat == 'no') # subset it by just the nos
nobo.real.no = subset(nobo21, burn_stat == 'Yes') # subset it by just the yes

# using the burn map from 2022, we can assume everything that wasnt burned was burned in 2021 and everything that was burned was not burned in 2021 
# so we need to switch since the birds that were monitored from jan 1 - mar will be based off the previous yrs burning 
nobo.real.yes$burn_stat = "Yes" # change the no's to yes
nobo.real.no$burn_stat = "no" # change the yes to no
nobo21 = rbind(nobo.real.yes, nobo.real.no) # rbind it back together 

# 2022
nobo22_sp <- SpatialPoints(coords = data.frame("x" = nobo22$x, "y" = nobo22$y)) # convert DF to Spatial Points
crs(nobo22_sp) <- CRS("+init=epsg:4326") # define CRS for the spatial points (EPSG 4326 == lat/lon WGS84 etc)
nobo22_sp <- spTransform(nobo22_sp, crs(burn22)) # transform nobo_sp from WGS84 to match "roads"
plot(burn22); plot(nobo22_sp, add = TRUE) # check to see if it worked
extraction22 <- over(nobo22_sp, burn22)# extract burn status for points
extraction22["Burns_2022"][is.na(extraction22["Burns_2022"])] <- "no" 
unique(extraction22$Burns_2022)
nobo22$burn_stat = extraction22$Burns_2022

nobo22$burnyear = 2022 # giving it a year (honestly, might be unnecesary)
nobo22$yearBinary = 0 #making year binary  YEAR 2022 = 0

# 2023
nobo23_sp <- SpatialPoints(coords = data.frame("x" = nobo23$x, "y" = nobo23$y)) # convert DF to Spatial Points
crs(nobo23_sp) <- CRS("+init=epsg:4326") # define CRS for the spatial points (EPSG 4326 == lat/lon WGS84 etc)
nobo23_sp <- spTransform(nobo23_sp, crs(burn23)) # transform nobo_sp from WGS84 to match "roads"
plot(burn23);plot(nobo23_sp, add = TRUE) # check to see if it worked
extraction23 <- over(nobo23_sp, burn23)# extract burn status for points
extraction23["Burns_2023"][is.na(extraction23["Burns_2023"])] <- "no" 
nobo23$burn_stat = extraction23$Burns_2023

nobo23$burnyear = 2023 # giving it a year (honestly, might be unnecesary)
nobo23$yearBinary = 1 #making year binary YEAR 2023 = 1

# 2024 
nobo24_sp <- SpatialPoints(coords = data.frame("x" = nobo24$x, "y" = nobo24$y)) # convert DF to Spatial Points
crs(nobo24_sp) <- CRS("+init=epsg:4326") # define CRS for the spatial points (EPSG 4326 == lat/lon WGS84 etc)
nobo24_sp <- spTransform(nobo24_sp, crs(burn24)) # transform nobo_sp from WGS84 to match "roads"
plot(burn24);plot(nobo24_sp, add = TRUE) # check to see if it worked
extraction24 <- over(nobo24_sp, burn24)# extract burn status for points
extraction24["burns_2024"][is.na(extraction24["burns_2024"])] <- "no" 
nobo24$burn_stat = extraction24$burns_2024

nobo24$burnyear = 2024 # giving it a year (honestly, might be unnecesary)
nobo24$yearBinary = 1 # making year binary YEAR 2023 = 1

#rbind the 2022 and 2023 back together 
nobo = rbind(nobo24, nobo23, nobo22, nobo21)
names(nobo)

##### Individual Covariates ----
# adding row number to nobo for record-keeping
nobo$row <- seq(1, nrow(nobo))

length(unique(nobo$Bird.ID)) # 1049 individuals 

inddata = read.csv("E:/NOBO R Projects_Github/NOBO Survival/Thesis_Final/New folder/Trapdata_21May2024.csv") # Autumn's pathway
inddata$bird_id = paste0(inddata$Frequency_at_Release, "_", inddata$Band.ID)

# create a dataframe that only includes what we need 
inddata1 = dplyr::select(inddata, "BirdID", "Sex")
head(inddata1)
inddata2= inddata1[!duplicated(inddata1), ] # removes duplicate rows 

# merge trap data and nobo to give ind covs (sex, age, weight) to birds 
nobo.merge <- merge(x = nobo, y = inddata2, by.x = "Bird.ID", by.y = "BirdID", all.x = TRUE) 
#   TO NOTE: in the trap data I had to manually fix the 'BirdID' column  so that every bird with a freq ending in a 0 
#   would combine properly with the band id during concatenate. W/o this edit, frequencies such as '149.000' or '162.570' would 
#   be read as '149' or '162.57' leading to an issue of bird.id not matching trap data. 

nrow(nobo.merge) # 128k gucci :) 
unique(nobo.merge$Sex)

# any birds w/o indvidual bird data will receive 'Female', 'Juvenile', mean weight -- only a few are missing sex and age 
nobo.merge[, 26][is.na(nobo.merge[, 26])] <- "Female" # make the NA female
unique(nobo.merge$Sex)
nobo.merge1 = nobo.merge # for ease 

# check to see number of males vs female s
nrow(nobo.merge1)
realz = subset(nobo.merge1, Location.Type == "Regular")
nrow(realz)
unique(realz$Sex)
m = subset(realz, Sex == "Male")
length(unique(m$Bird.ID)) # 439 
f = subset(realz, Sex == "Female")
length(unique(f$Bird.ID)) # 610

##### Add Treatment ----
# two treatments: distribution method (blower/spinner) and quantity (2, 3, 4 bushels of supplemental feed)
unique(nobo.merge1$course)

nobo_blower <- subset(nobo.merge1, course == "bigbay" | course == "bluepond" | course == "billjones")
nobo_blower$treatment = "blower"
P = nrow(nobo_blower)
P # 55669

nobo_spinner = subset(nobo.merge1, course == "other" | course == "fencecove" | course == "allenscreek" | course == "campcrane" | course == "darkbranch")
nobo_spinner$treatment = "spinner"
U = nrow(nobo_spinner)
U # 72569
P + U # should be 128238 

nobo.merge1 = rbind(nobo_blower, nobo_spinner)
nrow(nobo.merge1)


##### Habitat Covariates----
nobo = nobo.merge1 # putting this into an easier object


# EXTRACTING RASTER VALUES
nobo_sp <- SpatialPoints(coords = data.frame("x" = nobo$x, "y" = nobo$y)) # convert DF to Spatial Points
crs(nobo_sp) <- CRS("+init=epsg:4326") # define CRS for the spatial points (EPSG 4326 == lat/lon WGS84 etc)

# read in rasters
list.files("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters")
NDVI <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/2019NDVI.tif")
perc_mpine <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/PercentMaturePine.tif")
perc_grassy <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/Percentgrassy.tif")
perc_decid <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/PercentDeciduous.tif")
perc_bf <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/PercentBroodField.tif")
perc_water <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/PercentWater.tif")
DTN_road <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/DTN_road.tif")
DTN_mpine <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/DTN_maturepine.tif")
DTN_grassy <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/DTN_grassy.tif")
DTN_decid <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/DTN_deciduous.tif")
DTN_bf <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/DTN_broodfield1.tif")
DTN_water <- raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/DTN_water.tif")

# Updated rasters with 250m buffer (instead of 100) (only need these if wanting to explore different buffers)
perc_mpine_250m = raster("E:/NOBO Project Data/Updated Rasters/PercentMaturePine_250.tif")
perc_grassy_250m = raster("E:/NOBO Project Data/Updated Rasters/PercentGrassy_250.tif")
perc_decid_250m = raster("E:/NOBO Project Data/Updated Rasters/PercentDeciduous_250.tif")
perc_bf_250m = raster("E:/NOBO Project Data/Updated Rasters/PercentBroodField_250m.tif")
perc_bf_120m = raster("E:/NOBO Project Data/Updated Rasters/PercentBroodField_120m.tif")
perc_bf_180m = raster("E:/NOBO Project Data/Updated Rasters/PercentBroodField_180m.tif")

# reproject nobo and extract
nobo_sp1 <- spTransform(nobo_sp, crs(NDVI)) # transform to NAD83
plot(NDVI); plot(nobo_sp1, add = TRUE)
ndv1_ex <- raster::extract(x = NDVI, y = nobo_sp1)
perc_mpine_ex <- raster::extract(x = perc_mpine, y = nobo_sp1)
perc_grassy_ex <- raster::extract(x = perc_grassy, y = nobo_sp1)
perc_decid_ex <- raster::extract(x = perc_decid, y = nobo_sp1)
perc_bf_ex <- raster::extract(x = perc_bf, y = nobo_sp1)
perc_water_ex <- raster::extract(x = perc_water, y = nobo_sp1)

nobo_sp2 <- spTransform(nobo_sp, crs(DTN_road)) # transform to pseudo-mercator
plot(DTN_road); plot(nobo_sp2, add = TRUE)
DTN_road_ex <- raster::extract(x = DTN_road, y = nobo_sp2)
DTN_mpine_ex <- raster::extract(x = DTN_mpine, y = nobo_sp2)
DTN_grassy_ex <- raster::extract(x = DTN_grassy, y = nobo_sp2)
DTN_decid_ex <- raster::extract(x = DTN_decid, y = nobo_sp2)
DTN_bf_ex <- raster::extract(x = DTN_bf, y = nobo_sp2)
DTN_water_ex <- raster::extract(x = DTN_water, y = nobo_sp2)

# reproject updated 250m buffered covs 
nobo_sp1 <- spTransform(nobo_sp, crs(NDVI)) # transform to NAD83
plot(NDVI); plot(nobo_sp1, add = TRUE)
perc_mpine250_ex <- raster::extract(x = perc_mpine_250m, y = nobo_sp1)
perc_grassy250_ex <- raster::extract(x = perc_grassy_250m, y = nobo_sp1)
perc_decid250_ex <- raster::extract(x = perc_decid_250m, y = nobo_sp1)
perc_bf250_ex <- raster::extract(x = perc_bf_250m, y = nobo_sp1)
perc_bf120_ex<- raster::extract(x = perc_bf_120m, y = nobo_sp1)
perc_bf180_ex<- raster::extract(x = perc_bf_180m, y = nobo_sp1) 

hist(DTN_water_ex)

# add columns to NOBO and export
nobo$ndvi <- ndv1_ex
nobo$perc_mpine <- perc_mpine_ex
nobo$perc_grassy <- perc_grassy_ex
nobo$perc_decid <- perc_decid_ex
nobo$perc_bf <- perc_bf_ex
nobo$perc_water <- perc_water_ex
nobo$DTN_road <- DTN_road_ex
nobo$DTN_mpine <- DTN_mpine_ex
nobo$DTN_grassy <- DTN_grassy_ex
nobo$DTN_decid <- DTN_decid_ex
nobo$DTN_bf <- DTN_bf_ex
nobo$DTN_water <- DTN_water_ex 
 
# add the updated 
nobo$perc_mpine250 <- perc_mpine250_ex
nobo$perc_grassy250 <- perc_grassy250_ex
nobo$perc_decid250 <- perc_decid250_ex
nobo$perc_bf250 <- perc_bf250_ex
nobo$perc_bf120 = perc_bf120_ex
nobo$perc_bf180 = perc_bf180_ex
###

# View(nobo) # check it 
nrow(nobo) # check it 

# make burn stat binary: 1 = burned, 0 = unburned 
nobo$burn_stat <- str_replace(nobo$burn_stat , "no", "0")
nobo$burn_stat <- str_replace(nobo$burn_stat , "Yes", "1")

# Other binary columns to keep in mind...
  # response: 0 = random, 1 = real 
  # year: 0 = 2022, 1 = 2023 

# Remove Extra columns
nobo1 = dplyr::select(nobo, -"X", -"chick", -"encounter", -"breedingseasonCov", -"n",
               -"Observer", -"ObjectID", -"Bird.Status", -"Fate", -"year", -"Location.Type",
                -"treatment", -"CentroidCourses", -"ordinal", -"breedingseason") # adjust if more/less columns are wanted --> i want bare minimum for ease 

#### Extra: Assess Sex ----
# data exploration for later 
male = subset(nobo1, Sex == "Male")
female = subset(nobo1, Sex == "Female")
nrow(female)

# MALES - number of individual 
male_end_2022 <- male[male$Date >= "2022-11-01" & male$Date <= "2022-12-31", ]
male_end_2023 = male[male$Date >= "2023-11-01" & male$Date <= "2023-12-31", ]
length(unique(male_end_2022$Bird.ID)) # 104
length(unique(male_end_2023$Bird.ID)) # 112


# FEMALES - number of individual 
female_end_2022 <- female[female$Date >= "2022-11-01" & female$Date <= "2022-12-31", ]
female_end_2023 = female[female$Date >= "2023-11-01" & female$Date <= "2023-12-31", ]
length(unique(female_end_2022$Bird.ID)) # 120
length(unique(female_end_2023$Bird.ID)) # 115

length(unique(nobo1$Bird.ID)) # 815 

hist(nobo1$DTN_road)

# for ease rename 
nobo_c = nobo1

#### Days Since Last Burn ---- 
# Need to find the date of the last, most recent,  burn with this file 
burnmap = readOGR("E:/NOBO R Projects_Github/NOBO_ResourceSelection/Shapefiles/MasterBurnLayer__2023/Master_Burn_Plan.shp")
plot(burnmap)
summary(burnmap)

# turn nobo_i into spatial. Used for a figure below
nobo_sp <- SpatialPoints(coords = data.frame("x" = nobo_c$x, "y" = nobo_c$y)) # convert DF to Spatial Points
crs(nobo_sp) <- CRS("+init=epsg:4326") # define CRS for the spatial points (EPSG 4326 == lat/lon WGS84 etc)
nobo_sp <- spTransform(nobo_sp, crs(burnmap)) # make sure it matches OP shapefile: "North American Datum 1983" 
plot(burnmap); plot(nobo_sp, add = TRUE) # check it 
extraction <- over(nobo_sp, burnmap)# extract burn status for points
nobo_c$burn_hist = extraction$Burn_Hist # Add a column to OG data file and put exracted values in

test <- data.frame(do.call('rbind', strsplit(as.character(nobo_c$burn_hist),',',fixed=TRUE))) # create a df to hold burn dates 

nobo_c$BurnDate1 = test$X1 # create col for first burn in area -- most recent burn
nobo_c$BurnDate1 = as.Date(nobo_c$BurnDate1, "%Y-%m-%d") # make it a date 

nobo_c$BurnDate2 = test$X2 # create col for second burn in area 
nobo_c$BurnDate2 = as.Date(nobo_c$BurnDate2, "%Y-%m-%d") # make it a date 

nobo_c = dplyr::select(nobo_c, -31) # remove the "burn_hist" containing both 
nobo_c = dplyr::select(nobo_c, -25:-30) # removes unecessary  columns that are required in a later part (exploring 100ft radius around the pts)
nobo_c = dplyr::select(nobo_c, -5) # remove  "week" 

# Making sure the date column is a date column for this next piece 
nobo_c$Date = as.Date(nobo_c$Date, "%Y-%m-%d")

# ifelse statement: If the date of the observation is greater than the BurnDate1 column then it gets a 1, if not then it gets a 0

#   TO NOTE: the BurnDate1 column contains the date of the most recent burn in 
#              a given block (which was previously extracted). Therefore, if it is
#              greater  than that given date it will be given a 1.  
nobo_c$LastBurn = with(nobo_c, ifelse(nobo_c$Date > nobo_c$BurnDate1, 1, 0))
#View(nobo_c)

# Explore the NA 
TheNA <- nobo_c %>% filter_all(any_vars(is.na(nobo_c$LastBurn))) # create its own little dataframe for just the NA
head(TheNA)

#View(new_data)

OP <- readOGR("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2022/Adult data/Resource Use/shapefiles/OrtonCourses_JustTreatmentSites.shp")
TheNA_sp <- SpatialPoints(coords = data.frame("x" = TheNA$x, "y" = TheNA$y)) # convert NA's to Spatial Points
crs(TheNA_sp) <- CRS("+init=epsg:4326") # define CRS for the spatial points (EPSG 4326 == lat/lon WGS84 etc) 
TheNA_sp <- spTransform(TheNA_sp, crs(OP)) # make sure it matches OP shapefile: "North American Datum 1983" 
plot(OP); plot(TheNA_sp, add = TRUE, col = "blue") # notice all the points are surrounding drains or the property line ~8000 pts
head(TheNA)
# the issue lies because the drains are not being counted as a burned area -- makes sense 
nrow(TheNA) # will just be assigning it the maximum day aka 730

# Turning my focus to the ones that do have dates 
#       May just rbind the nas seperately back onto the dataframe once this piece is done 
nobo1 = subset(nobo_c, LastBurn == 1) #subset to those thats last burn date is in column BurnDate1
nobo0 = subset(nobo_c, LastBurn == 0) #subset to those thats last burn date is in column BurnDate2 
head(nobo1)

nobo1$LastBurn = nobo1$BurnDate1 #change the name for an rbind 
nobo1 = dplyr::select(nobo1, -"BurnDate2", -"BurnDate1") # delete the two columns I dont need anymore 

#repeat
nobo0$LastBurn = nobo0$BurnDate2 #change the name for an rbind 
nobo0 = dplyr::select(nobo0, -"BurnDate2", -"BurnDate1") # delete the two columns I dont need anymore 

# #combine the two back together.
nobo_c2 = rbind(nobo0, nobo1) # NOTE: this currently does not include any observations that had NA in the Burn columns 

#rbinding the NAs back into the dataframe 
# cries internnally because there was probably a more concise way of doing all this and now we are here 

head(TheNA)
nrow(TheNA)
TheNA = dplyr::select(TheNA, -"BurnDate2", -"BurnDate1") # delete the two columns I dont need anymore 
nobo_c2 = rbind(nobo_c2, TheNA)

nrow(nobo_c2) # check to see if its the same number as we started? 

nobo_c2$Daysinceburn = difftime(nobo_c2$Date, nobo_c2$LastBurn, units="days")
nobo_c2$Daysinceburn = round(nobo_c2$Daysinceburn, 0)

mean(nobo_c2$Daysinceburn, na.rm=TRUE) # instead of giving the drains a max day - potentially influencing the data - i am going to make it average 

head(nobo_c2)
nobo_c2 = nobo_c2 %>% mutate(Daysinceburn = ifelse(is.na(Daysinceburn), 386, Daysinceburn)) # changing any NA in the Daysinceburn to 730 [max # of days]

#### % Burn ----
# Adding % burn to each of the points 
burn22 = raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/BurnStat2022.tif")
burn23 = raster("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2023/Adult/rasters/BurnStat2023.tif")
burn24 = raster("E:/NOBO Project Data/Updated Rasters/Burn2024_perc.tif")

# need to subset for year and do this seperately unfortunately
nobo21 = nobo_c2[nobo_c2$Date >= "2021-04-01" & nobo_c2$Date <= "2022-03-31",]
nobo22 = nobo_c2[nobo_c2$Date >= "2022-04-01" & nobo_c2$Date <= "2023-03-31",]
nobo23 = nobo_c2[nobo_c2$Date >= "2023-04-01" & nobo_c2$Date <= "2024-03-31",]
nobo24 = nobo_c2[nobo_c2$Date >= "2024-04-01" & nobo_c2$Date <= "2025-03-31",]

# 2021 burn year should, in theory, match the 2023 burn year 
nobo21_sp <- SpatialPoints(coords = data.frame("x" = nobo21$x, "y" = nobo21$y)) # convert DF to Spatial Points
nobo22_sp <- SpatialPoints(coords = data.frame("x" = nobo22$x, "y" = nobo22$y)) # convert DF to Spatial Points
nobo23_sp <- SpatialPoints(coords = data.frame("x" = nobo23$x, "y" = nobo23$y)) # convert DF to Spatial Points

crs(nobo21_sp) <- CRS("+init=epsg:4326") # define CRS for the spatial points (EPSG 4326 == lat/lon WGS84 etc)
crs(nobo22_sp) <- CRS("+init=epsg:4326") # define CRS for the spatial points (EPSG 4326 == lat/lon WGS84 etc)
crs(nobo23_sp) <- CRS("+init=epsg:4326") # define CRS for the spatial points (EPSG 4326 == lat/lon WGS84 etc)

nobo21_sp = spTransform(nobo21_sp, crs(burn23)) # match the crs of the necessary tiff 
nobo22_sp = spTransform(nobo22_sp, crs(burn22)) # match the crs of the necessary tiff 
nobo23_sp = spTransform(nobo23_sp, crs(burn23)) # match the crs of the necessary tiff 

# 2021
plot(burn23); plot(nobo21_sp, add = TRUE) # plot 
burn21_ex <- raster::extract(x = burn23, y = nobo21_sp) # extract 
nobo21$perc_burn = burn21_ex # add column and add extracted values 

# 2022 
plot(burn22); plot(nobo22_sp, add = TRUE) # plot 
burn22_ex <- raster::extract(x = burn22, y = nobo22_sp) # extract 
nobo22$perc_burn = burn22_ex # add column and add extracted values 

# 2023
plot(burn23); plot(nobo23_sp, add = TRUE) #plot 
burn23_ex <- raster::extract(x = burn23, y = nobo23_sp) #extract 
nobo23$perc_burn = burn23_ex # add column and add extracted values 

# 2024
plot(burn24); plot(nobo24_sp, add = TRUE) #plot 
burn24_ex <- raster::extract(x = burn24, y = nobo24_sp) #extract 
nobo24$perc_burn = burn24_ex # add column and add extracted values 

nobo_c3 = rbind(nobo23, nobo22, nobo21, nobo24)

#### Ordinal Date ----
nobo_c3$Date <- as.Date(nobo_c3$Date) # convert from char to date
nobo_c3$Day_Month <- paste0(day(nobo_c3$Date), "_", month(nobo_c3$Date)) # key for merging w weeklookup

nobo_c3 <- dplyr::select(nobo_c3, -X)
rows <- data.frame("row" = seq(from = 1, to = nrow(nobo_c3)))
nobo <- cbind(rows, nobo_c3)

# merge with weeklookup
weeklookup <- read.csv("MISC/week_lookuptable.csv")

merge1 <- merge(x = nobo, y = weeklookup, by.x = "Day_Month", by.y = "day_month", all.x = TRUE)
merge1 <- merge1[order(merge1$row, decreasing = FALSE),] # re-sort for some f### reason
nobo$ordinal <- merge1$ordinal

head(nobo)
nrow(merge1)
hist(nobo$DTN_road)

nrow(nobo)
# write.csv(nobo, "./ResSelData_28june2024.csv")


   