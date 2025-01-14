# Resource Selection 
# Tidying/cleaning the data 
# Last Editted: 6/28/2024
# Autumn Randall 

library(rgdal); library(dplyr); library(lubridate); library(stringr); library(hablar);library(stringr); library(lubridate); library(tidyr)
setwd("E:/NOBO R Projects_Github/NOBO_ResourceSelection/Thesis Final")

# read in the data 
nobo1 <- read.csv("E:/NOBO R Projects_Github/NOBO_ResourceSelection/Thesis Final/telemetry data/Orton_Bobwhite_TelemetryData_15May2024.csv")
nrow(nobo1) # 54243

# clean this file up by removing the columns we do not need
nobo1 <- dplyr::select(nobo1, -GlobalID, -Time, -Burn.Status, -Habitat.Type,
                       -Associated.Bird..1, -Associated.Bird..2, -Associated.Bird..3,
                       -Associated.Bird..4, -Associated.Bird..5, -Associated.Bird..6,
                       -Associated.Bird..7, -Associated.Bird..8, -Associated.Bird..9,
                       -Associated.Bird..10, -Parent.Present., -Enter.Adult.ID,
                       -Comments, -CreationDate, -Creator, -EditDate, -Editor)
# dates are currently characters; need to convert to date format (mm/dd/yyyy)
nobo1 <- nobo1 %>% mutate(Date = as.Date(Date, format = "%m/%d/%Y"))

# remove chicks ----
nobo1 <- subset(nobo1, Location.Type != "Chick") 
nobo1 <- arrange(nobo1, ObjectID) # re-order rows in order of objectID

# Some chicks were entered as broods and we need to remove these chicks from the data 
# isolate the first 3 numbers of band number because to remove chicks entered as "brood" 
nobo1$chick <- paste0("band" = substr(nobo1$Bird.ID, 9, 11)) 

# subset the entire year of 2022 (1st line) then eliminate chick bands (which start with 225)
nobosub2022 <- nobo1[nobo1$Date >= "2022-01-01" & nobo1$Date <= "2022-10-31", ] # after october it will be considered a part of the nonbreeding birds
nobosub2022_end = nobo1[nobo1$Date >= "2022-11-01" & nobo1$Date <= "2022-12-31", ] # a placeholder to contain the rest of the year 
nobosub2022 <- subset(nobosub2022, chick!=225) # remove chicks 
nobosub2022 = rbind(nobosub2022, nobosub2022_end)

# subset the entire year of 2023 (1st line) then eliminate chick bands (which start with 235)
nobosub2023 <- nobo1[nobo1$Date >= "2023-01-01" & nobo1$Date <= "2023-12-31", ]
nobosub2023_end = nobo1[nobo1$Date >= "2023-11-01" & nobo1$Date <= "2023-12-31", ] # a placeholder to contain the rest of the year 
nobosub2023 <- subset(nobosub2023, chick!=235)
nobosub2023 = rbind(nobosub2023, nobosub2023_end)

# subset the entire year of 2023 (1st line) then eliminate chick bands (which start with 235)
nobosub2024 <- nobo1[nobo1$Date >= "2024-01-01" & nobo1$Date <= "2024-12-31", ]
nobosub2024_end = nobo1[nobo1$Date >= "2024-11-01" & nobo1$Date <= "2024-12-31", ] # a placeholder to contain the rest of the year 
nobosub2024 <- subset(nobosub2024, chick!=245)
nobosub2024 = rbind(nobosub2024, nobosub2024_end)

# re-combine the 3 chunks from 2022-24
nobo2 <- rbind(nobosub2022, nobosub2023, nobosub2024)

nobo1 <- nobo2 # rename nobo1 for ease
nrow(nobo1) #  50866

# 162.603_220346 should actually be called 162.376_220019 so we need to fix that
nobo1$Bird.ID <- str_replace(nobo1$Bird.ID , "162.603_220346", "162.376_220019") 


# Removing fates/censors/dnh ----
# Add "encounter" as 0 (censor), 1 (alive), or 2 (dead)
unique(nobo1$Bird.Status)

# alive
alives <- subset(nobo1, Bird.Status == "Alive & Active" | Bird.Status == "Nest" | Bird.Status == "Alive & Inactive" |
                   Bird.Status == "Brood" | Bird.Status == "Alive - Mort Check Only" | Bird.Status == "Suspected Nest") # 13201 rows
# fate (also includes some censors)
fates <- subset(nobo1, Bird.Status == "Fate") # all birds with status = "Fate" = fate
fatedead <- subset(fates, Fate != "Censor") # all fate birds where fate is not "censor", fatedead
fatecensor <- subset(fates, Fate == "Censor") # all fate birds where fate IS "censor", censor
nrow(fatedead) + nrow(fatecensor) == nrow(fates) # make sure the math adds up... should say TRUE

# censors
censors <- subset(nobo1, Bird.Status == "Suspected Fate - RF" | Bird.Status == "Suspected Fate - DNH" | Bird.Status == "Suspected Fate - RIP") # 351 rows
nrow(alives) + nrow(fatedead) + nrow(fatecensor) + nrow(censors) == nrow(nobo1) # 50866 rows / TRUE

# add encounter
alives$encounter = 1
fatedead$encounter = 2
fatecensor$encounter = 0
censors$encounter = 0
nobo1 <- alives
#nobo1 <- rbind(alives, fatedead, fatecensor, censors) # Restore this (and uncheck prev line) if fated birds are desired
nobo1 <- nobo1 %>% arrange(ObjectID) # re-order columns



# Date Modification ----
# modifying the dates to something easier to handle
nobodates <- data.frame("Date" = nobo1$Date)
nobodates$Date <- str_replace(nobodates$Date, " AM", "") #replace AM with nothing
nobodates$Date <- str_replace(nobodates$Date, " PM", "") #replace PM with nothing
nobodates <- separate(nobodates, Date, into = c("date", "time"), sep = " ") # split up day and time
  # WARNING MESSAGE! "Expected 2 pieces. Missing pieces..." this is b/c some records have no times - just dates
nobodates <- separate(nobodates, date, into = c("year", "month", "day"), sep = "-") # split up month, day, year
head(nobodates)
nobodates <- dplyr::select(nobodates, -time) # remove time
nobo2 <- cbind(nobo1, nobodates) # add the year, month, and day back to nobo1

# make month, day, and year numeric
nobo2$month <- as.numeric(nobo2$month); nobo2$day <- as.numeric(nobo2$day); nobo2$year <- as.numeric(nobo2$year)
nobo2$CombinedDate0 <- paste0(nobo2$day, "_", nobo2$month) # create a "combined" date

# add ordinal date using the lookup table
lookuptable1 <- read.csv("MISC/week_lookuptable.csv")
nobo_ord <- data.frame("ObjectID" = nobo2$ObjectID,"CombinedDate0" = nobo2$CombinedDate0)
merge1 <- merge(x = nobo_ord, by.x = "CombinedDate0", y = lookuptable1, by.y = "day_month", all.x = TRUE)
merge1 <- merge1 %>% arrange(ObjectID) # re-order columns...  NO F---ing CLUE why merge() re-orders our columns...
nobo2 <-cbind(nobo2, merge1[,5:8]) # adding breeding season, ordinal, and week
nobo1 <- nobo2 # turn it back into nobo1

# clean nobo1 up by removing extra columns
nobo1 <- dplyr::select(nobo1, -month, -day, -CombinedDate0)

length(unique(nobo1$Bird.ID)) # 1194 birds 

# Isolate the broods ----
  # PROBLEM:  Location.Type and Bird.Status both have 'broods' as an option therefore some broods may say the location.type as regular with the bird.status as brood  OR location.type is brood with the bird.status as regular. 
nobo00 = within(nobo1, Location.Type[Location.Type == 'Regular' & Bird.Status == 'Brood'] <- 'Brood') # within the nobo1 dataset, if the location.type
# column says regular but the bird.status column says brood, then change the location.type to brood
nobo1 = nobo00 # revert it back for ease 

broods = subset(nobo1, Location.Type == "Brood") #subset for broods 
notbroods = subset(nobo1, Location.Type == "Regular") #subset for regular adults
unique(broods$Bird.Status)
unique(notbroods$Bird.Status) 

nests = subset(nobo1, Location.Type == "Nest") #just to see how many there are 
length(unique(nests$Bird.ID)) #270 unique Bird.IDs labeled nest

x = length(unique(notbroods$Bird.ID)) # 1192
y = length(unique(broods$Bird.ID)) # 180 brooooooods  !!!!

# Randomly select 1 occasion for every day tracked 
broods$bird.day = paste0(broods$Bird.ID, "_", broods$Date) # create a column that combines bird id and date -- helps identify the duplicate values 
length(unique(broods$Bird.ID)) # 180 check to see how many broods their are before we handle the duplicate dates 
nrow(broods) # 5897
broods1 = broods[sample(1:nrow(broods)), ]  # randomize the order by shuffling the dataset
broods1 = broods[!duplicated(broods$bird.day),] # this will take the first instance of every duplicate for bird.day
length(unique(broods1$Bird.ID)) # 180 - to make sure the same number of broods are left 
broods1 <- broods1[,-17] #remove the unneeded col  to be able to rbind

nobo2 = rbind(notbroods, broods1) #rbind notbroods and the new broods dataframe back together 
length(unique(nobo2$Bird.ID)) # 1194 - most likely because nests were removed 

nobo1 = nobo2 # revert back to original name for ease 

# data thinning ---- 

# Generate table of bird observation frequencies b/c mcp can only be made for birds w > 4 obs
lookup1 <- data.frame("birdID" = NA, "n" = 1) 
for(i in 1:length(unique(nobo1$Bird.ID))){
  bob_i <- subset(nobo1, Bird.ID == unique(nobo1$Bird.ID)[i])  # subset ith bird
  newrow_i <- c(bob_i$Bird.ID[1], nrow(bob_i))
  lookup1 <- rbind(lookup1, newrow_i)
}

nobo1 <- merge(x = nobo1, y = lookup1, by.x = "Bird.ID", by.y = "birdID", all.x = TRUE)
length(unique(nobo1$Bird.ID))# still 1194 birds
nobo1$n = as.numeric(nobo1$n) # change to numeric

l = nrow(nobo1[nobo1$n == '1', ]) # 53
m = nrow(nobo1[nobo1$n == '2', ]) # 70  
a = nrow(nobo1[nobo1$n == '3', ]) # 75
o = nrow(nobo1[nobo1$n == '4', ]) # 100

# checking as to why so many birds have so few # of observations
m2 = subset(nobo1, n == 2)
head(m2) # most can be attributed to birds dying before observation 4 or how close the telemetry data being used is to a trapping period 

nobo1 <- subset(nobo1, n > 4) # over 4 locations (5 or more or else during the mcp i get an error)
length(unique(nobo1$Bird.ID)) # only 1056  birds...  
nrow(nobo1) #42922
unique(nobo1$Location.Type)


########################## FINDING COURSE BASED OFF CENTROID FOR LATER FORLOOPS ----
############################################################################################################
#nobo2 <- nobo1[,1:9]
#nobo2 <- nobo1[,1:10]


# one last tweak so at the end we only have regular and random pts
nobo1 = within(nobo1, Location.Type[Location.Type == 'Brood'] <- 'Regular')

#############################
############################# MCP FORLOOP()----
# Read in the orton shapefile 
OP <- readOGR("E:/NOBO Project Data/Analyses/Breeding Season/Summer 2022/Adult data/Resource Use/shapefiles/OrtonCourses_JustTreatmentSites.shp")

# save Alber's Equal Area Conic projection
albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

# blankDF
RandomsDF <- data.frame("X" = NA, "Bird.ID" = NA, "ObjectID" = NA, "Date" = NA, "Observer" = NA, 
                        "Bird.Status" = NA, "Fate" = NA, "Location.Type" = "Random", "x" = NA, "y" = NA, "encounter" = NA,
                        "year" = NA, "ordinal" = NA, "week" = NA, "breedingseason" = NA, "breedingseasonCov" = NA, "yearBinary" = NA)


#### for() loop that identifies the bird's course via centroid
# This is needed b/c the NEXT for() loop generates random points
# within the course that each bird lives in

CentroidCourses <- c() # blank object to hold course info

# write.csv(nobo1, "./cleaned_NOBO_telem.csv")

for(i in 1:length(unique(nobo1$Bird.ID))){
  
  # i = 100
  # subset one bird using the subset function
  nobo_i <- subset(nobo1, Bird.ID == unique(nobo1$Bird.ID)[i]) # randomly subsetting the data and taking the 100th bird'
  
  # get the average point and make IT also a spatial object
  nobo_i_avg_sp <- SpatialPoints(coords = data.frame("x" = mean(nobo_i$x), "y" = mean(nobo_i$y))) # convert DF to Spatial Points
  crs(nobo_i_avg_sp) = CRS("+init=epsg:4326") # define CRS for the spatial points (EPSG 4326 == lat/lon WGS84 etc)
  nobo_i_avg_sp <- spTransform(nobo_i_avg_sp, crs(OP))
  
  # make sure they look OK
  plot(OP); plot(nobo_i_avg_sp, add = TRUE, col = "blue") 
  # extract course ID for points
  extraction <- over(nobo_i_avg_sp, OP) # place the centroid over the top of the course shapefile
  course_i <- extraction$course # extract course from the centroid 'bob_avg_sp'
  course_i <- ifelse(is.na(course_i) == TRUE, "other", course_i)
  
  # add new course info to CentroidCourses
  NewCourseData <- rep(course_i, nrow(nobo_i))
  CentroidCourses <- c(CentroidCourses, NewCourseData)
  
  # progress bar
  compl <- round(i/length(unique(nobo1$Bird.ID))*50,0)
  cat(paste0("\r [", strrep("|", compl),strrep(".", 50-compl),"] ", round(100*i/length(unique(nobo1$Bird.ID)),0), "% complete"))
  #print(paste0("[",strrep("|", compl),strrep(".", 50-compl),"] ", round(100*i/length(unique(nobo1$Bird.ID)),0), "% complete"))
}

# add CentroidCourses to nobo1
nobo1$CentroidCourses <- CentroidCourses

# combine campcranes 1 and 2
nobo1$CentroidCourses <- ifelse(nobo1$CentroidCourses == "campcrane2", "campcrane", nobo1$CentroidCourses) # change "campcrane2" to "campcrane"
nobo1$CentroidCourses <- ifelse(nobo1$CentroidCourses == "campcrane1", "campcrane", nobo1$CentroidCourses) # change "campcrane2" to "campcrane"

# Also fix the names in the shapefile
OP$course <- ifelse(OP$course == "campcrane2", "campcrane", OP$course) # change "campcrane2" to "campcrane"
OP$course <- ifelse(OP$course == "campcrane1", "campcrane", OP$course) # change "campcrane2" to "campcrane"

# remove birds that are not in real courses
unique(nobo1$CentroidCourses)
nobo1 <- subset(nobo1, CentroidCourses != "other")
unique(nobo1$CentroidCourses)

nrow(nobo1)
#write.csv(nobo1, "./cleaned_Data_28June2024_Part1.csv")
