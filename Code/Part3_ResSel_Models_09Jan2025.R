# Models incoproating course as a fixed effect 
# Date created: 7/4/2024 

# SetWD 
setwd("E:/NOBO R Projects_Github/NOBO_ResourceSelection/ResourceSelection_Final_07Jan2025")

library(grid)
library(gridBase)
library(dotwhisker)

# Year Model ----

## Prep ----
# upload the data 
nobo_c <- read.csv("E:/NOBO R Projects_Github/NOBO_ResourceSelection/ResourceSelection_Final_07Jan2025/ResSelData_28june2024.csv") # read in the course level data 
nrow(nobo_c) # 128238 -- just to check it 

#Remove unnecessary columns 
nobo_c = nobo_c[,-(26)] 
nobo_c = nobo_c[,-(28)]
nobo_c = nobo_c[,-(1)]
nobo_c = nobo_c[,-(11)]

# change location of perc_burn so it is with the others
nobo_c = nobo_c %>% relocate(perc_burn, .after = DTN_water)  

# Calculate the mean distance to nearest road 
mean(nobo_c$DTN_road)

# Calc averages for the reals 
#View(nobo_c)
birdavg = subset(nobo_c, response == "1") # Subset to just used pts (no randoms )
nrow(birdavg) # 39094 # of rows 
summary(birdavg)

frequency = table(birdavg$Bird.ID)

## Scale Variables ----
nobo_c[,12:24] <- scale(nobo_c[,12:24]) # scale all the variables
length(unique(nobo_c$Bird.ID)) # 1049
unique(nobo_c$course)

# subset to no "other" for course 
nobo_t = subset(nobo_c, course != "other") # Removes birds outside study area
unique(nobo_t$course) # check to see if it worked 
nobo_c = nobo_t  # convert back for ease 
nobo_c

## Models ----
#### random  ----
all_mod_randomcourse = glmer(response ~ DTN_road + DTN_bf + ndvi + burn_stat + (1|course) + (1|Bird.ID), family = binomial, data = nobo_c, 
                             control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(all_mod_randomcourse)
#### fixed  ----
all_mod_fixedcourse = glmer(response ~ DTN_road + DTN_bf + ndvi + burn_stat + course + (1|Bird.ID), family = binomial, data = nobo_c,  
                            control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(all_mod_fixedcourse)

length(getME(all_mod_randomcourse,"theta")) # 2 # random effect parameters
length(getME(all_mod_fixedcourse,"theta")) # 1 # random effect parameters

#### Save Models ----
# save the random and the fixed effect for course model 

#saveRDS(all_mod_randomcourse, file = "E:/NOBO R Projects_Github/NOBO_ResourceSelection/Thesis Final/ModelSummary/ResSel_RaEf_AnnualModel_summary_13Aug.rds")
#saveRDS(all_mod_fixedcourse, file = "E:/NOBO R Projects_Github/NOBO_ResourceSelection/Thesis Final/ModelSummary/ResSel_FiEf_AnnualModel_summary_13Aug.rds")

# Upload Model - Fixed effect 
all_mod_fixedcourse = readRDS("E:/NOBO R Projects_Github/NOBO_ResourceSelection/ResourceSelection_Final_07Jan2025/ModelSummary/ResSel_FiEf_AnnualModel_summary_13Aug.rds")
summary(all_mod_fixedcourse)
confint(all_mod_fixedcourse)

# Breeding vs nonbreeding ----
## Prep ----
# Read in data (this is done for every model as a precaution) 
nobo_c <- read.csv("E:/NOBO R Projects_Github/NOBO_ResourceSelection/ResourceSelection_Final_07Jan2025/ResSelData_28june2024.csv") # read in the course level data 
nobo_c = nobo_c[,-(26)] 
nobo_c = nobo_c[,-(28)]
nobo_c = nobo_c[,-(1)]
nobo_c = nobo_c[,-(11)]

nobo_c = nobo_c %>% relocate(perc_burn, .after = DTN_water) # change location of perc_burn so it is with the others 

# subset to no "other" for course 
nobo_t = subset(nobo_c, course != "other")
unique(nobo_t$course)
nobo_c = nobo_t # revert back for ease 

## Scale Variables ---- 
nobo_c[,12:24] <- scale(nobo_c[,12:24]) # scale all the variables

## Temporal Split ----
#     breeding data subset
breeding22 <- nobo_c[nobo_c$Date >= "2022-04-01" & nobo_c$Date <= "2022-09-30", ]
breeding23 <- nobo_c[nobo_c$Date >= "2023-04-01" & nobo_c$Date <= "2023-09-30", ] 
breeding24 = nobo_c[nobo_c$Date >= "2024-04-01" & nobo_c$Date <= "2024-09-30", ]
breeding = rbind(breeding22, breeding23, breeding24)
nrow(breeding) # 54207

# nonbreeding data subset
non_breeding1 = nobo_c[nobo_c$Date >= "2022-01-01" & nobo_c$Date <= "2022-03-31", ]
non_breeding2 = nobo_c[nobo_c$Date >= "2022-10-01" & nobo_c$Date <= "2023-03-31", ]
non_breeding3 = nobo_c[nobo_c$Date >= "2023-10-01" & nobo_c$Date <= "2024-03-31", ]
nonbreeding = rbind(non_breeding1, non_breeding2, non_breeding3)
nrow(nonbreeding) # 55266

mean(breeding$DTN_road)
mean(nonbreeding$DTN_road)

## Models ----
#### Fixed ----
nonbreeding_mod = glmer(response ~ DTN_road + DTN_bf + ndvi + burn_stat + course + (1|Bird.ID), family = binomial, data = nonbreeding, 
                        control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))) #  increase the amount of iterations
breeding_mod = glmer(response ~ DTN_road +  DTN_bf + ndvi + burn_stat + course + (1|Bird.ID), family = binomial, data = breeding,
                     control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(breeding_mod)
summary(nonbreeding_mod)

#### Save Models ----
#saveRDS(nonbreeding_mod , file = "E:/NOBO R Projects_Github/NOBO_ResourceSelection/Thesis Final/ModelSummary/Nonbreeding_13Aug.rds")
#saveRDS(breeding_mod, file = "E:/NOBO R Projects_Github/NOBO_ResourceSelection/Thesis Final/ModelSummary/Breeding_13Aug.rds")

# Upload Models
nonbreeding_mod = readRDS("./ModelSummary/Nonbreeding_13Aug.rds")
breeding_mod = readRDS("./ModelSummary/Breeding_13Aug.rds")

#### Random ----
nonbreeding_mod_re = glmer(response ~ DTN_road  + DTN_bf + ndvi + burn_stat + (1|course) + (1|Bird.ID), family = binomial, data = nonbreeding,
                           control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
breeding_mod_re = glmer(response ~ DTN_road  + DTN_bf + ndvi + burn_stat + (1|course) + (1|Bird.ID), family = binomial, data = breeding, 
                        control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(breeding_mod_re)
summary(nonbrohfoeeding_mod_re)

#### Save Models ----
#saveRDS(nonbreeding_mod , file = "E:/NOBO R Projects_Github/NOBO_ResourceSelection/Thesis Final/ModelSummary/Nonbreeding_13Aug.rds")
#saveRDS(breeding_mod, file = "E:/NOBO R Projects_Github/NOBO_ResourceSelection/Thesis Final/ModelSummary/Breeding_13Aug.rds")

# Upload Models 
#breeding_mod = readRDS("E:/NOBO R Projects_Github/NOBO_ResourceSelection/Thesis Final/ModelSummary/Breeding_RE_13Aug.rds")
#nonbreeding_mod = readRDS("E:/NOBO R Projects_Github/NOBO_ResourceSelection/Thesis Final/ModelSummary/Nonbreeding_RE_13Aug.rds")
summary(breeding_mod)


# Seasonal ---- 

## Prep ----
# this is just a precaution
nobo_c <- read.csv("./ResSelData_28june2024.csv") # read in the course level data 
nobo_c = nobo_c[,-(26)] 
nobo_c = nobo_c[,-(28)]
nobo_c = nobo_c[,-(1)]
nobo_c = nobo_c[,-(11)]

nobo_c = nobo_c %>% relocate(perc_burn, .after = DTN_water) # change location of burn_stat so it is with the others 

# subset to no "other" for course 
nobo_t = subset(nobo_c, course != "other")
unique(nobo_t$course)
nobo_c = nobo_t

## Scale Variables ---- 
nobo_c[,12:24] <- scale(nobo_c[,12:24]) # scale all the variables

## Temporal Split ----
# spring 
spring22 = nobo_c[nobo_c$Date >= "2022-03-01" & nobo_c$Date <= "2022-05-31", ]
spring23 = nobo_c[nobo_c$Date >= "2023-03-01" & nobo_c$Date <= "2023-05-31", ]
spring24 = nobo_c[nobo_c$Date >= "2024-03-01" & nobo_c$Date <= "2024-05-31", ]
spring = rbind(spring22, spring23, spring24)

# summer 
summer22 = nobo_c[nobo_c$Date >= "2022-06-01" & nobo_c$Date <= "2022-08-31", ]
summer23 = nobo_c[nobo_c$Date >= "2023-06-01" & nobo_c$Date <= "2023-08-31", ]
summer = rbind(summer22, summer23)

# fall 
fall22 = nobo_c[nobo_c$Date >= "2022-09-01" & nobo_c$Date <= "2022-11-30", ]
fall23 = nobo_c[nobo_c$Date >= "2023-09-01" & nobo_c$Date <= "2023-11-30", ]
fall = rbind(fall22, fall23)

# winter 
winter22 = nobo_c[nobo_c$Date >= "2022-12-01" & nobo_c$Date <= "2023-02-28", ]
winter23 = nobo_c[nobo_c$Date >= "2023-12-01" & nobo_c$Date <= "2024-02-28", ]
winter24 = nobo_c[nobo_c$Date >= "2024-12-01" & nobo_c$Date <= "2025-02-28", ]

winter = rbind(winter22, winter23, winter24)


## Models ----

#### Fixed ----
spring_mod = glmer(response ~ DTN_road  + DTN_bf + ndvi + burn_stat + course + (1|Bird.ID), family = binomial, data = spring, 
                   control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summer_mod = glmer(response ~ DTN_road + DTN_bf + ndvi + burn_stat + course + (1|Bird.ID), family = binomial, data = summer, 
                   control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
fall_mod = glmer(response ~ DTN_road  + DTN_bf + ndvi + burn_stat + course + (1|Bird.ID), family = binomial, data = fall, 
                 control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
winter_mod = glmer(response ~ DTN_road + DTN_bf + ndvi + burn_stat + course + (1|Bird.ID), family = binomial, data = winter, 
                   control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

summary(spring_mod)
summary(summer_mod)
summary(fall_mod)
summary(winter_mod)

#### Save Models ----
# saveRDS(spring_mod, file = "E:/NOBO R Projects_Github/NOBO_ResourceSelection/Thesis Final/ModelSummary/spring_13Aug.rds")
# saveRDS(summer_mod, file = "E:/NOBO R Projects_Github/NOBO_ResourceSelection/Thesis Final/ModelSummary/summer_13Aug.rds")
# saveRDS(fall_mod, file = "E:/NOBO R Projects_Github/NOBO_ResourceSelection/Thesis Final/ModelSummary/fall_13Aug.rds")
# saveRDS(winter_mod, file = "E:/NOBO R Projects_Github/NOBO_ResourceSelection/Thesis Final/ModelSummary/winter_13Aug.rds")

# Upload Models 
spring_mod= readRDS("E:/NOBO R Projects_Github/NOBO_ResourceSelection/Thesis Final/ModelSummary/spring_13Aug.rds")
summer_mod= readRDS("E:/NOBO R Projects_Github/NOBO_ResourceSelection/Thesis Final/ModelSummary/summer_13Aug.rds")
fall_mod= readRDS("E:/NOBO R Projects_Github/NOBO_ResourceSelection/Thesis Final/ModelSummary/fall_13Aug.rds")
winter_mod= readRDS("E:/NOBO R Projects_Github/NOBO_ResourceSelection/Thesis Final/ModelSummary/winter_13Aug.rds")
summary(spring_mod)


#### Random ----
spring_mod_re = glmer(response ~ DTN_road  + DTN_bf + ndvi + burn_stat +  (1|course) + (1|Bird.ID), family = binomial, data = spring, 
                      control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summer_mod_re = glmer(response ~ DTN_road  + DTN_bf + ndvi + burn_stat +  (1|course) + (1|Bird.ID), family = binomial, data = summer, 
                      control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
fall_mod_re = glmer(response ~ DTN_road  + DTN_bf + ndvi + burn_stat +  (1|course) + (1|Bird.ID), family = binomial, data = fall, 
                    control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
winter_mod_re = glmer(response ~ DTN_road  + DTN_bf + ndvi + burn_stat +  (1|course) + (1|Bird.ID), family = binomial, data = winter, 
                      control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

summary(spring_mod_re)
summary(summer_mod_re)
summary(fall_mod_re)
summary(winter_mod_re)

#### Save Models ----
#saveRDS(spring_mod, file = "E:/NOBO R Projects_Github/NOBO_ResourceSelection/Thesis Final/ModelSummary/spring_04July.rds")
#saveRDS(summer_mod, file = "E:/NOBO R Projects_Github/NOBO_ResourceSelection/Thesis Final/ModelSummary/summer_04July.rds")
#saveRDS(fall_mod, file = "E:/NOBO R Projects_Github/NOBO_ResourceSelection/Thesis Final/ModelSummary/fall_04July.rds")
#saveRDS(winter_mod, file = "E:/NOBO R Projects_Github/NOBO_ResourceSelection/Thesis Final/ModelSummary/winter_04July.rds")


# Bimonthly ---- 
# this is just a precaution
nobo_c <- read.csv("E:/NOBO R Projects_Github/NOBO_ResourceSelection/ResourceSelection_Final_07Jan2025/ResSelData_28june2024.csv") # read in the course level data 
nobo_c = nobo_c[,-(26)] 
nobo_c = nobo_c[,-(28)]
nobo_c = nobo_c[,-(1)]
nobo_c = nobo_c[,-(11)]

nobo_c = nobo_c %>% relocate(perc_burn, .after = DTN_water) # change location of burn_stat so it is with the others 

# subset to no "other" for course 
nobo_t = subset(nobo_c, course != "other")
unique(nobo_t$course)
nobo_c = nobo_t

## Scale Variables ----
nobo_c[,12:24] <- scale(nobo_c[,12:24]) # scale all the variables

## Temporal Split ----
# January - February 
jf_22 = nobo_c[nobo_c$Date >= "2022-01-01" & nobo_c$Date <= "2022-02-28", ]
jf_23 = nobo_c[nobo_c$Date >= "2023-01-01" & nobo_c$Date <= "2023-02-31", ]
jf_24 = nobo_c[nobo_c$Date >= "2024-01-01" & nobo_c$Date <= "2024-02-31", ]
jf = rbind(jf_22, jf_23, jf_24)
nrow(jf) # 11445

# March - April 
ma_22 = nobo_c[nobo_c$Date >= "2022-03-01" & nobo_c$Date <= "2022-04-31", ]
ma_23 = nobo_c[nobo_c$Date >= "2023-03-01" & nobo_c$Date <= "2023-04-31", ]
ma_24 = nobo_c[nobo_c$Date >= "2024-03-01" & nobo_c$Date <= "2024-04-31", ]
ma = rbind(ma_22, ma_23, ma_24)
nrow(ma) # 7776

# May - June 
mj_22 = nobo_c[nobo_c$Date >= "2022-05-01" & nobo_c$Date <= "2022-06-31", ]
mj_23 = nobo_c[nobo_c$Date >= "2023-05-01" & nobo_c$Date <= "2023-06-31", ]
mj_24 = nobo_c[nobo_c$Date >= "2024-05-01" & nobo_c$Date <= "2024-06-31",]
mj = rbind(mj_22, mj_23)
nrow(mj) # 7776

# July - August 
ja_22 = nobo_c[nobo_c$Date >= "2022-07-01" & nobo_c$Date <= "2022-08-31", ]
ja_23 = nobo_c[nobo_c$Date >= "2023-07-01" & nobo_c$Date <= "2023-08-31", ]
ja = rbind(ja_22, ja_23)
nrow(ja) # 21612

# September - October 
so_22 = nobo_c[nobo_c$Date >= "2022-09-01" & nobo_c$Date <= "2022-10-31", ]
so_23 = nobo_c[nobo_c$Date >= "2023-09-01" & nobo_c$Date <= "2023-10-31", ]
so = rbind(so_22, so_23)
nrow(so) # 8670

# November - December 
nd_22 = nobo_c[nobo_c$Date >= "2022-11-01" & nobo_c$Date <= "2022-12-31", ]
nd_23 = nobo_c[nobo_c$Date >= "2023-11-01" & nobo_c$Date <= "2023-12-31", ]
nd = rbind(nd_22, nd_23)
nrow(nd) # 21612

## Models ---- 

#### Fixed ----
jf_mod = glmer(response ~ DTN_road + DTN_bf + ndvi + burn_stat + course + (1|Bird.ID), family = binomial, data = jf, 
               control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
ma_mod = glmer(response ~ DTN_road + DTN_bf + ndvi + burn_stat + course + (1|Bird.ID), family = binomial, data = ma, 
               control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
mj_mod = glmer(response ~ DTN_road + DTN_bf + ndvi + burn_stat + course + (1|Bird.ID), family = binomial, data = mj, 
               control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
ja_mod = glmer(response ~ DTN_road + DTN_bf + ndvi + burn_stat + course + (1|Bird.ID), family = binomial, data = ja, 
               control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
so_mod = glmer(response ~ DTN_road + DTN_bf + ndvi + burn_stat + course + (1|Bird.ID), family = binomial, data = so, 
               control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
nd_mod = glmer(response ~ DTN_road + DTN_bf + ndvi + burn_stat + course + (1|Bird.ID), family = binomial, data = nd, 
               control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(jf_mod)
summary(ma_mod)
summary(mj_mod)
summary(ja_mod)
summary(so_mod)
summary(nd_mod)

#### Random ----
jf_mod_re = glmer(response ~ DTN_road + DTN_bf + ndvi + burn_stat + (1|course) + (1|Bird.ID), family = binomial, data = jf, 
                  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
ma_mod_re = glmer(response ~ DTN_road + DTN_bf + ndvi + burn_stat + (1|course) + (1|Bird.ID), family = binomial, data = ma,
                  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
mj_mod_re = glmer(response ~ DTN_road + DTN_bf + ndvi + burn_stat + (1|course) + (1|Bird.ID), family = binomial, data = mj, 
                  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
ja_mod_re = glmer(response ~ DTN_road + DTN_bf + ndvi + burn_stat + (1|course) + (1|Bird.ID), family = binomial, data = ja, 
                  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
so_mod_re = glmer(response ~ DTN_road + DTN_bf + ndvi + burn_stat + (1|course) + (1|Bird.ID), family = binomial, data = so, 
                  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
nd_mod_re = glmer(response ~ DTN_road + DTN_bf + ndvi + burn_stat + (1|course) + (1|Bird.ID), family = binomial, data = nd,
                  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

summary(jf_mod_re)
summary(ma_mod_re)
summary(mj_mod_re)
summary(ja_mod_re)
summary(so_mod_re)
summary(nd_mod_re)

#### Save Models ----
#saveRDS(jf_mod, file = "jf_Course_mod2.rds")
#saveRDS(ma_mod, file = "ma_Course_mod2.rds")
#saveRDS(mj_mod, file = "mj_Course_mod2.rds")
#saveRDS(ja_mod, file = "ja_Course_mod2.rds")
#saveRDS(so_mod, file = "so_Course_mod2.rds")
#saveRDS(nd_mod, file = "nd_Course_mod2.rds")


# Monthly ----
# this is just a precaution
nobo_c <- read.csv("E:/NOBO R Projects_Github/NOBO_ResourceSelection/ResourceSelection_Final_07Jan2025/ResSelData_28june2024.csv") # read in the course level data 
nobo_c = nobo_c[,-(26)] 
nobo_c = nobo_c[,-(28)]
nobo_c = nobo_c[,-(1)]
nobo_c = nobo_c[,-(11)]

nobo_c = nobo_c %>% relocate(perc_burn, .after = DTN_water) # change location of burn_stat so it is with the others 

# subset to no "other" for course 
nobo_t = subset(nobo_c, course != "other")
unique(nobo_t$course)
nobo_c = nobo_t

## Scale Variables ---- 
nobo_c[,12:24] <- scale(nobo_c[,12:24]) # scale all the variables

## Temporal Split ----
# January  
jan_22 = nobo_c[nobo_c$Date >= "2022-01-01" & nobo_c$Date <= "2022-01-31", ]
jan_23 = nobo_c[nobo_c$Date >= "2023-01-01" & nobo_c$Date <= "2023-01-31", ]
jan_24 = nobo_c[nobo_c$Date >= "2024-01-01" & nobo_c$Date <= "2024-01-31", ]

jan = rbind(jan_22, jan_23, jan_24)
nrow(jan) # 6177

# February   
feb_22 = nobo_c[nobo_c$Date >= "2022-02-01" & nobo_c$Date <= "2022-02-31", ]
feb_23 = nobo_c[nobo_c$Date >= "2023-02-01" & nobo_c$Date <= "2023-02-31", ]
feb_24 = nobo_c[nobo_c$Date >= "2024-02-01" & nobo_c$Date <= "2024-02-31", ]

feb = rbind(feb_22, feb_23, feb_24)
nrow(feb) # 5268

# March   
mar_22 = nobo_c[nobo_c$Date >= "2022-03-01" & nobo_c$Date <= "2022-03-31", ]
mar_23 = nobo_c[nobo_c$Date >= "2023-03-01" & nobo_c$Date <= "2023-03-31", ]
mar_24 = nobo_c[nobo_c$Date >= "2024-03-01" & nobo_c$Date <= "2024-03-31", ]

mar = rbind(mar_22, mar_23, mar_24)
nrow(mar) # 2283

# April  
apr_22 = nobo_c[nobo_c$Date >= "2022-04-01" & nobo_c$Date <= "2022-04-31", ]
apr_23 = nobo_c[nobo_c$Date >= "2023-04-01" & nobo_c$Date <= "2023-04-31", ]
apr = rbind(apr_22, apr_23)
nrow(apr) # 5493

# May  
may_22 = nobo_c[nobo_c$Date >= "2022-05-01" & nobo_c$Date <= "2022-05-31", ]
may_23 = nobo_c[nobo_c$Date >= "2023-05-01" & nobo_c$Date <= "2023-05-31", ]
may = rbind(may_22, may_23)
nrow(may) # 7230

# June  
jun_22 = nobo_c[nobo_c$Date >= "2022-06-01" & nobo_c$Date <= "2022-06-31", ]
jun_23 = nobo_c[nobo_c$Date >= "2023-06-01" & nobo_c$Date <= "2023-06-31", ]
jun = rbind(jun_22, jun_23)
nrow(jun) # 14634

# July  
jul_22 = nobo_c[nobo_c$Date >= "2022-07-01" & nobo_c$Date <= "2022-07-31", ]
jul_23 = nobo_c[nobo_c$Date >= "2023-07-01" & nobo_c$Date <= "2023-07-31", ]
jul = rbind(jul_22, jul_23)
nrow(jul) # 12666

# August  
aug_22 = nobo_c[nobo_c$Date >= "2022-08-01" & nobo_c$Date <= "2022-08-31", ]
aug_23 = nobo_c[nobo_c$Date >= "2023-08-01" & nobo_c$Date <= "2023-08-31", ]
aug = rbind(aug_22, aug_23)
nrow(aug) # 8946

# September   
sept_22 = nobo_c[nobo_c$Date >= "2022-09-01" & nobo_c$Date <= "2022-09-31", ]
sept_23 = nobo_c[nobo_c$Date >= "2023-09-01" & nobo_c$Date <= "2023-09-31", ]
sept = rbind(sept_22, sept_23)
nrow(sept) # 5766

# October  
oct_22 = nobo_c[nobo_c$Date >= "2022-10-01" & nobo_c$Date <= "2022-10-31", ]
oct_23 = nobo_c[nobo_c$Date >= "2023-10-01" & nobo_c$Date <= "2023-10-31", ]
oct = rbind(oct_22, oct_23)
nrow(oct) # 2904

# November   
nov_22 = nobo_c[nobo_c$Date >= "2022-11-01" & nobo_c$Date <= "2022-11-31", ]
nov_23 = nobo_c[nobo_c$Date >= "2023-11-01" & nobo_c$Date <= "2023-11-31", ]
nov = rbind(nov_22, nov_23)
nrow(nov) # 516

# December   
dec_22 = nobo_c[nobo_c$Date >= "2022-12-01" & nobo_c$Date <= "2022-12-31", ]
dec_23 = nobo_c[nobo_c$Date >= "2023-12-01" & nobo_c$Date <= "2023-12-31", ]
dec = rbind(dec_22, dec_23)
nrow(dec) # 678

## Models ----

#### Fixed ---- 
jan_mod = glmer(response ~ DTN_road + DTN_bf + ndvi + burn_stat + course + (1|Bird.ID), family = binomial, data = jan, 
                control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
feb_mod = glmer(response ~ DTN_road + DTN_bf + ndvi + burn_stat + course + (1|Bird.ID), family = binomial, data = feb, 
                control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
mar_mod = glmer(response ~ DTN_road + DTN_bf + ndvi + burn_stat + course + (1|Bird.ID), family = binomial, data = mar, 
                control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
apr_mod = glmer(response ~ DTN_road + DTN_bf + ndvi + burn_stat + course + (1|Bird.ID), family = binomial, data = apr, 
                control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
may_mod = glmer(response ~ DTN_road + DTN_bf + ndvi + burn_stat + course + (1|Bird.ID), family = binomial, data = may, 
                control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
jun_mod = glmer(response ~ DTN_road + DTN_bf + ndvi + burn_stat + course + (1|Bird.ID), family = binomial, data = jun, 
                control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
jul_mod = glmer(response ~ DTN_road + DTN_bf + ndvi + burn_stat + course + (1|Bird.ID), family = binomial, data = jul, 
                control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
aug_mod = glmer(response ~ DTN_road + DTN_bf + ndvi + burn_stat + course + (1|Bird.ID), family = binomial, data = aug, 
                control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
sept_mod = glmer(response ~ DTN_road+ DTN_bf + ndvi + burn_stat + course + (1|Bird.ID), family = binomial, data = sept, 
                 control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
oct_mod = glmer(response ~ DTN_road + DTN_bf + ndvi + burn_stat + course + (1|Bird.ID), family = binomial, data = oct, 
                control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
nov_mod = glmer(response ~ DTN_road + DTN_bf + ndvi + burn_stat + course + (1|Bird.ID), family = binomial, data = nov, 
                control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
dec_mod = glmer(response ~ DTN_road + DTN_bf + ndvi + burn_stat + course + (1|Bird.ID), family = binomial, data = dec, 
                control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

summary(jan_mod)
summary(feb_mod)
summary(mar_mod)
summary(apr_mod)
summary(may_mod)
summary(jun_mod)
summary(jul_mod)
summary(aug_mod)
summary(sept_mod)
summary(oct_mod)
summary(nov_mod)
summary(dec_mod)

#### Save Models ----
#saveRDS(jan_mod, file = "jan_Course_mod2.rds")
#saveRDS(feb_mod, file = "feb_Course_mod2.rds")
#saveRDS(mar_mod, file = "mar_Course_mod2.rds")
#saveRDS(apr_mod, file = "apr_Course_mod2.rds")
#saveRDS(may_mod, file = "may_Course_mod2.rds")
#saveRDS(jun_mod, file = "jun_Course_mod2.rds")
#saveRDS(jul_mod, file = "jul_Course_mod2.rds")
#saveRDS(aug_mod, file = "aug_Course_mod2.rds")
#saveRDS(sept_mod, file = "sept_Course_mod2.rds")
#saveRDS(oct_mod, file = "oct_Course_mod2.rds")
#saveRDS(nov_mod, file = "nov_Course_mod2.rds")
#saveRDS(dec_mod, file = "dec_Course_mod2.rds")

#### Random ----
jan_mod_re = glmer(response ~ DTN_road + DTN_bf + ndvi + burn_stat + (1|course) + (1|Bird.ID), family = binomial, data = jan, 
                   control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
feb_mod_re = glmer(response ~ DTN_road + DTN_bf + ndvi + burn_stat + (1|course) + (1|Bird.ID), family = binomial, data = feb, 
                   control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
mar_mod_re = glmer(response ~ DTN_road + DTN_bf + ndvi + burn_stat + (1|course) + (1|Bird.ID), family = binomial, data = mar, 
                   control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
apr_mod_re = glmer(response ~ DTN_road + DTN_bf + ndvi + burn_stat + (1|course) + (1|Bird.ID), family = binomial, data = apr, 
                   control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
may_mod_re = glmer(response ~ DTN_road + DTN_bf + ndvi + burn_stat + (1|course) + (1|Bird.ID), family = binomial, data = may, 
                   control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
jun_mod_re = glmer(response ~ DTN_road + DTN_bf + ndvi + burn_stat + (1|course) + (1|Bird.ID), family = binomial, data = jun, 
                   control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
jul_mod_re = glmer(response ~ DTN_road + DTN_bf + ndvi + burn_stat + (1|course) + (1|Bird.ID), family = binomial, data = jul,
                   control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
aug_mod_re = glmer(response ~ DTN_road + DTN_bf + ndvi + burn_stat + (1|course) + (1|Bird.ID), family = binomial, data = aug, 
                   control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
sept_mod_re = glmer(response ~ DTN_road + DTN_bf + ndvi + burn_stat + (1|course) + (1|Bird.ID), family = binomial, data = sept, 
                    control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
oct_mod_re = glmer(response ~ DTN_road + DTN_bf + ndvi + burn_stat + (1|course) + (1|Bird.ID), family = binomial, data = oct, 
                   control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
nov_mod_re = glmer(response ~ DTN_road + DTN_bf + ndvi + burn_stat + (1|course) + (1|Bird.ID), family = binomial, data = nov, 
                   control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
dec_mod_re = glmer(response ~ DTN_road + DTN_bf + ndvi + burn_stat + (1|course) + (1|Bird.ID), family = binomial, data = dec, 
                   control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

summary(jan_mod_re)
summary(feb_mod_re)
summary(mar_mod_re)
summary(apr_mod_re)
summary(may_mod_re)
summary(jun_mod_re)
summary(jul_mod_re)
summary(aug_mod_re)
summary(sept_mod_re)
summary(oct_mod_re)
summary(nov_mod_re)
summary(dec_mod_re)

#### Save Models ----
#saveRDS(jan_mod, file = "jan_Course_mod2.rds")
#saveRDS(feb_mod, file = "feb_Course_mod2.rds")
#saveRDS(mar_mod, file = "mar_Course_mod2.rds")
#saveRDS(apr_mod, file = "apr_Course_mod2.rds")
#saveRDS(may_mod, file = "may_Course_mod2.rds")
#saveRDS(jun_mod, file = "jun_Course_mod2.rds")
#saveRDS(jul_mod, file = "jul_Course_mod2.rds")
#saveRDS(aug_mod, file = "aug_Course_mod2.rds")
#saveRDS(sept_mod, file = "sept_Course_mod2.rds")
#saveRDS(oct_mod, file = "oct_Course_mod2.rds")
#saveRDS(nov_mod, file = "nov_Course_mod2.rds")
#saveRDS(dec_mod, file = "dec_Course_mod2.rds")


# Summarys: All ---- 

# Year
  summary(all_mod_fixedcourse)
  summary(all_mod_randomcourse) 
# Breeding vs Nonbreeding 
  # FIXED ( BOTH CONVERGED WITH WARNINGS FOR NOBREEDING )
  summary(breeding_mod) 
  summary(nonbreeding_mod)
  # RANDOM
  summary(breeding_mod_re)
  summary(nonbreeding_mod_re)

# SEASON #
  # FIXED  (ALL CONVERGED)
  summary(spring_mod)
  summary(summer_mod)
  summary(fall_mod) 
  summary(winter_mod)
  # RANDOM
  summary(spring_mod_re)
  summary(summer_mod_re)
  summary(fall_mod_re)
  summary(winter_mod_re)

# BI MONTHLY # 
  # FIXED 
  summary(jf_mod)
  summary(ma_mod)  
  summary(mj_mod)
  summary(ja_mod)
  summary(so_mod) 
  summary(nd_mod)

  # RANDOM
  summary(jf_mod_re)
  summary(ma_mod_re)
  summary(mj_mod_re)
  summary(ja_mod_re)
  summary(so_mod_re)
  summary(nd_mod_re)

# MONTHLY #
  # FIXED 
  summary(jan_mod)
  summary(feb_mod) 
  summary(mar_mod)
  summary(apr_mod) 
  summary(may_mod)
  summary(jun_mod)
  summary(jul_mod)
  summary(aug_mod)
  summary(sept_mod) 
  summary(oct_mod) 
  summary(nov_mod)
  summary(dec_mod)
  
  # RANDOM
  summary(jan_mod_re)
  summary(feb_mod_re)
  summary(mar_mod_re)
  summary(apr_mod_re)
  summary(may_mod_re)
  summary(jun_mod_re)
  summary(jul_mod_re)
  summary(aug_mod_re)
  summary(sept_mod_re)
  summary(oct_mod_re)
  summary(nov_mod_re)
  summary(dec_mod_re)


  
# Daily Moving Window  ----

## Prep ----
# this is just a precaution
nobo_c <- read.csv("E:/NOBO R Projects_Github/NOBO_ResourceSelection/ResourceSelection_Final_07Jan2025/ResSelData_28june2024.csv") # read in the course level data 
nobo_c = nobo_c[,-(26)] 
nobo_c = nobo_c[,-(28)]
nobo_c = nobo_c[,-(1)]
nobo_c = nobo_c[,-(11)]

nobo_c = nobo_c %>% relocate(perc_burn, .after = DTN_water) # change location of burn_stat so it is with the others 

# subset to no "other" for course 
nobo_t = subset(nobo_c, course != "other")
unique(nobo_t$course)
nobo_c = nobo_t

# Fix NA values within the ordinal column 
na = nobo_c[is.na(nobo_c$ordinal), ] # subset to just the rows within NA in the ordinal column (n = 273) 
head(na) # just look at the ordinal column
nrow(na) 

# remove NA values (just february 29th )
nobo_c = nobo_c %>% drop_na(ordinal)

## Scale Variables ----
nobo_c[,12:24] <- scale(nobo_c[,12:24]) # scale all the variables

# pull in 30 day lookuptable and add it to nobo
day30 <- read.csv("E:/NOBO R Projects_Github/NOBO_ResourceSelection/30DayWindowtable.csv")
day30 <- day30[,1:31] # remove weird extra blank columns


# blank table to hold results
BigDataFrame <- data.frame("ordinal" = 0, 
                           "DTN_road" = 0, "DTN_road_SE" = 0,
                           "DTN_bf" = 0, "DTN_bf_SE" = 0,
                           "ndvi" = 0, "ndvi_SE" = 0,
                           "burn_stat" = 0, "burn_stat_SE" = 0)

## ForLoop ----
#   To note: TO SAVE TIME SKIP TO CONFIDENCE INTERVALS AND LOAD IN PROCESSED DATAFRAME  
for(i in 1:365){
  #i= 209
  DesiredDates <- subset(day30, ordinal == i)
  subset_i <- subset(nobo_c, ordinal == DesiredDates[,2] |
                       ordinal == DesiredDates[,3] |
                       ordinal == DesiredDates[,4] |
                       ordinal == DesiredDates[,5] |
                       ordinal == DesiredDates[,6] |
                       ordinal == DesiredDates[,7] |
                       ordinal == DesiredDates[,8] |
                       ordinal == DesiredDates[,9] |
                       ordinal == DesiredDates[,10] |
                       ordinal == DesiredDates[,11] |
                       ordinal == DesiredDates[,12] |
                       ordinal == DesiredDates[,13] |
                       ordinal == DesiredDates[,14] |
                       ordinal == DesiredDates[,15] |
                       ordinal == DesiredDates[,16] |
                       ordinal == DesiredDates[,17] |
                       ordinal == DesiredDates[,18] |
                       ordinal == DesiredDates[,19] |
                       ordinal == DesiredDates[,20] |
                       ordinal == DesiredDates[,21] |
                       ordinal == DesiredDates[,22] |
                       ordinal == DesiredDates[,23] |
                       ordinal == DesiredDates[,24] |
                       ordinal == DesiredDates[,25] |
                       ordinal == DesiredDates[,26] |
                       ordinal == DesiredDates[,27] |
                       ordinal == DesiredDates[,28] |
                       ordinal == DesiredDates[,29] |
                       ordinal == DesiredDates[,30] |
                       ordinal == DesiredDates[,31])
  
  mod1 = glmer(response ~ DTN_road + DTN_bf + ndvi + burn_stat + course + (1|Bird.ID), family = binomial, data = subset_i, 
               control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
  sum1 <- summary(mod1)
  newrow <- c(i,
              sum1$coefficients[2,1],sum1$coefficients[2,2],
              sum1$coefficients[3,1],sum1$coefficients[3,2],
              sum1$coefficients[4,1],sum1$coefficients[4,2],
              sum1$coefficients[5,1],sum1$coefficients[5,2],
              sum1$coefficients[6,1],sum1$coefficients[6,2],
              sum1$coefficients[7,1],sum1$coefficients[7,2],
              sum1$coefficients[8,1],sum1$coefficients[8,2])
  BigDataFrame <- rbind(BigDataFrame, newrow)
  print(paste0("Day ", i, " is done"))
}

# take the data frame and remove the first row 
BigDataFrame = BigDataFrame[2:366,]


## Confidence Intervals ----

# DTN_road 
BigDataFrame$DTN_road_UCI = BigDataFrame$DTN_road + (1.96 * BigDataFrame$DTN_road_SE) # upper CI 
BigDataFrame$DTN_road_LCI = BigDataFrame$DTN_road - (1.96 * BigDataFrame$DTN_road_SE) # lower CI 
# ndvi
BigDataFrame$ndvi_UCI = BigDataFrame$ndvi + (1.96 * BigDataFrame$ndvi_SE) # upper CI 
BigDataFrame$ndvi_LCI = BigDataFrame$ndvi - (1.96 * BigDataFrame$ndvi_SE) # lower CI 
# DTN_bf
BigDataFrame$DTN_bf_UCI = BigDataFrame$DTN_bf + (1.96 * BigDataFrame$DTN_bf_SE) # upper CI 
BigDataFrame$DTN_bf_LCI = BigDataFrame$DTN_bf - (1.96 * BigDataFrame$DTN_bf_SE) # lower CI 
# burn_stat
BigDataFrame$burn_stat_UCI = BigDataFrame$burn_stat + (1.96 * BigDataFrame$burn_stat_SE) # upper CI 
BigDataFrame$burn_stat_LCI = BigDataFrame$burn_stat - (1.96 * BigDataFrame$burn_stat_SE) # lower CI 

#write.csv(BigDataFrame, "E:/NOBO R Projects_Github/NOBO_ResourceSelection/ResourceSelection_Final_07Jan2025/AllBirds_BigDataFrame_14Aug.csv")

# already processed dataframe to save time
# BigDataFrame = read.csv("E:/NOBO R Projects_Github/NOBO_ResourceSelection/ResourceSelection_Final_07Jan2025/AllBirds_BigDataFrame_14Aug.csv")


# Figures: All ----
## Daily moving window ----
hist(BigDataFrame$ndvi) # to assess where most of the values for covariates lie within the data
OLD1 = plot(x = BigDataFrame$ordinal, y = BigDataFrame$ndvi) # To see what the figure should look like 

## start new graphics frame
plot.new()
par(mar = c(5, 5, 5, 5)) # Set the margin on all sides to 2

par(mfrow = c(2,2))

#### NDVI ----
ndvi_fig = plot(x = BigDataFrame$ordinal, y = BigDataFrame$ndvi, xlim = c(0, 365), 
           ylim = c(-0,1), type = 'l', xlab = "Ordinal Date", ylab = "NDVI (Beta +/- 95% CI)", 
           cex.lab=2,
           cex.axis = 1.7#change font size of axis labels
           
) +
  polygon(c(rev(BigDataFrame$ordinal), BigDataFrame$ordinal), c(rev(BigDataFrame$ndvi_UCI), BigDataFrame$ndvi_LCI), col = 'darkseagreen1', border = NA) +
  lines(BigDataFrame$ordinal, y = BigDataFrame$ndvi_UCI, lty = 'solid', col = 'darkgreen', lwd = 1.5) + 
  lines(BigDataFrame$ordinal, y = BigDataFrame$ndvi_LCI, lty = 'solid', col = 'darkgreen', lwd = 1.5) +
  lines(BigDataFrame$ordinal, y = BigDataFrame$ndvi, lty = 'solid', col = 'black', lwd = 2)  +
  abline(h= 0, col = "darkblue", lty = "dashed", lwd = 3)
  

#### Burn Status ----  
burn_stat_fig = plot(x = BigDataFrame$ordinal, y = BigDataFrame$burn_stat, xlim = c(0, 365), 
           ylim = c(-1.5,0.8), type = 'l', xlab = "Ordinal Date", ylab = "% Burn (Beta +/- 95% CI)", 
           cex.lab=2,
           cex.axis = 1.7#change font size of axis labels
           ) +
  polygon(c(rev(BigDataFrame$ordinal), BigDataFrame$ordinal), c(rev(BigDataFrame$burn_stat_UCI), BigDataFrame$burn_stat_LCI), col = 'darkseagreen1', border = NA) +
  lines(BigDataFrame$ordinal, y = BigDataFrame$burn_stat_UCI, lty = 'solid', col = 'darkgreen', lwd = 1.5) + 
  lines(BigDataFrame$ordinal, y = BigDataFrame$burn_stat_LCI, lty = 'solid', col = 'darkgreen', lwd = 1.5) +
  lines(BigDataFrame$ordinal, y = BigDataFrame$burn_stat, lty = 'solid', col = 'black', lwd = 2) +
        abline(h= 0, col = "darkblue", lty = "dashed", lwd = 3)


#### DTN Road ----  
par(mar = c(5, 5, 5, 5)) # Set the margin on all sides to 2

DTN_road_fig = plot(x = BigDataFrame$ordinal, y = BigDataFrame$DTN_road, xlim = c(0, 365), 
                     ylim = c(-1,0), type = 'l', xlab = "Ordinal Date", ylab = "DTN road (Beta +/- 95% CI)", 
                     cex.lab=2,
                     cex.axis = 1.7#change font size of axis labels
) +
  polygon(c(rev(BigDataFrame$ordinal), BigDataFrame$ordinal), c(rev(BigDataFrame$DTN_road_UCI), BigDataFrame$DTN_road_LCI), col = 'darkseagreen1', border = NA) +
  lines(BigDataFrame$ordinal, y = BigDataFrame$DTN_road_UCI, lty = 'solid', col = 'darkgreen', lwd = 1.5) + 
  lines(BigDataFrame$ordinal, y = BigDataFrame$DTN_road_LCI, lty = 'solid', col = 'darkgreen', lwd = 1.5) +
  lines(BigDataFrame$ordinal, y = BigDataFrame$DTN_road, lty = 'solid', col = 'black', lwd = 2) +
  abline(h= 0, col = "darkblue", lty = "dashed", lwd = 3)


#### DTN Fallow ----
DTN_bf_fig = plot(x = BigDataFrame$ordinal, y = BigDataFrame$DTN_bf, xlim = c(0, 365), 
                     ylim = c(-0.5,0.0), type = 'l', xlab = "Ordinal Date", ylab = "% BF (Beta +/- 95% CI)", 
                     cex.lab=2,
                     cex.axis = 1.7#change font size of axis labels
) +
  polygon(c(rev(BigDataFrame$ordinal), BigDataFrame$ordinal), c(rev(BigDataFrame$DTN_bf_UCI), BigDataFrame$DTN_bf_LCI), col = 'darkseagreen1', border = NA) +
  lines(BigDataFrame$ordinal, y = BigDataFrame$DTN_bf_UCI, lty = 'solid', col = 'darkgreen', lwd = 1.5) + 
  lines(BigDataFrame$ordinal, y = BigDataFrame$DTN_bf_LCI, lty = 'solid', col = 'darkgreen', lwd = 1.5) +
  lines(BigDataFrame$ordinal, y = BigDataFrame$DTN_bf, lty = 'solid', col = 'black', lwd = 2) +
  abline(h= 0, col = "darkblue", lty = "dashed", lwd = 3)


## Dot Whisker Plots ----

### Year---- 
summary(all_mod_fixedcourse)
summary(all_mod_randomcourse) 

# dot whisker plot with editing 
Fig1_all = dwplot(all_mod_fixedcourse, 
                  ci = 0.95, 
                  dodge_size = .5, # how far apart pts are frome eachother (0.4 = default) 
                  show_intercept = FALSE, 
                  model_order = NULL, 
                  dot_args = list(size = 2, alpha = 1),
                  whisker_args = list(size = .5, colour = "grey3"),
                  vline = geom_vline(xintercept = 0, linetype = 2, colour ="grey8", size = 3), 
                  vars_order = c("DTN_bf", "burn_stat","ndvi", "DTN_road")) %>% # plot line at zero _behind_coefs
  relabel_predictors(
    c(
      DTN_bf = "DTN Brood Field",
      DTN_road = "DTN Road",
      ndvi = "NDVI", 
      burn_stat = "Burn Status")
  ) +
  theme_bw() + xlab("β +/- 95% CI") + ylab("") + 
  scale_color_discrete(name = "Model", labels = c("Annual"), type = c('green4', 'slateblue4')) # labels the legend then the models, then assigns colors 

Fig1_all + xlim(c(-.8,.8)) + coord_flip() +
  theme(text = element_text(size=30),
        axis.text.x = element_text(angle=25, hjust=1), 
        legend.text = element_text(size = 25))



### Breeding/NonBreeding ----
summary(breeding_mod) 
summary(nonbreeding_mod)

# A basic dot whisker plot without editing anything 
fig_a = dwplot(list(breeding_mod, nonbreeding_mod))
fig_a
brVSnb_mods = list(breeding_mod, nonbreeding_mod)

# dot whisker plot with editing 
Figa_biannual = dwplot(brVSnb_mods,
                       ci = 0.95, 
                       dodge_size = .6, # how far apart pts are frome eachother (0.4 = default) 
                       show_intercept = FALSE, 
                       model_order = NULL, 
                       dot_args = list(size = 7),
                       whisker_args = list(size = 1, colour = "grey3"),
                       vline = geom_vline(xintercept = 0, linetype = 2, colour ="black", size = 3), 
                       vars_order = c("DTN_bf", "burn_stat","ndvi", "DTN_road")) %>% # plot line at zero _behind_coefs
  relabel_predictors(
    c(
      DTN_bf = "DTN Brood Field",
      DTN_road = "DTN Road",
      ndvi = "NDVI", 
      burn_stat = "Burn Status")
  ) +
  
  theme_bw() + xlab("β +/- 95% CI") + ylab("") + 
  scale_color_discrete(name = "Model", labels = c("Breeding", "Non-breeding"), type = c('green4', 'cornflowerblue'))# labels the legend then the models, then assigns colors 

Figa_biannual + xlim(c(-.9,.8)) + coord_flip()+
  theme(text = element_text(size=30),
        axis.text.x = element_text(angle=25, hjust=1), 
        legend.text = element_text(size = 25))



### Season ----
summary(spring_mod)
summary(summer_mod)
summary(fall_mod) 
summary(winter_mod)


## Figure 
season_mod = list(spring_mod, summer_mod, fall_mod, winter_mod)

# dot whisker plot with editing 

dwplot(list(spring_mod, summer_mod, fall_mod, winter_mod))


Fig2_seasonal = dwplot(list(spring_mod, summer_mod, fall_mod, winter_mod),
                       ci = 0.95, 
                       dodge_size = 0.4, # how far apart pts are frome eachother (0.4 = default) 
                       show_intercept = FALSE, 
                       model_order = NULL, 
                       dot_args = list(size = 7), 
                       whisker_args = list(size = 1, colour = "grey3"),
                       vline = geom_vline(xintercept = 0, linetype = 2, colour ="grey8", size = 3), 
                       vars_order = c("DTN_bf", "burn_stat","ndvi", "DTN_road")) %>% # plot line at zero _behind_coefs
  relabel_predictors(
    c(
      DTN_bf = "DTN Brood Field",
      DTN_road = "DTN Road",
      ndvi = "NDVI", 
      burn_stat = "Burned")
  ) +
  
  theme_bw() + xlab("Coefficient Estimate") + ylab("") + 
  scale_color_discrete(name = "Model", labels = c("Spring", "Summer", "Fall", "Winter"), type = c('orchid2', 'green4', 'darkorange', 'dodgerblue2'))# labels the legend then the models, then assigns colors 


Fig2_seasonal + xlim(c(-.8,.8)) + coord_flip() +
  theme(text = element_text(size=30),
        axis.text.x = element_text(angle=25, hjust=1), 
        legend.text = element_text(size = 25))


### Bi-Monthly ---- 
# Fixed 
summary(jf_mod)
summary(ma_mod)
summary(mj_mod)
summary(ja_mod)
summary(so_mod)
summary(nd_mod) 

# Figure
month_2interv_mods = list(jf_mod, ma_mod, mj_mod, ja_mod, so_mod, nd_mod)

Fig3_month_2interv = dwplot(month_2interv_mods,
                            ci = 0.95, 
                            dodge_size = 0.4, # how far apart pts are frome eachother (0.4 = default) 
                            show_intercept = FALSE, 
                            model_order = NULL, 
                            dot_args = list(size = 7), 
                            whisker_args = list(size = 1, colour = "grey3"),
                            vline = geom_vline(xintercept = 0, linetype = 2, colour ="grey8", size = 3), 
                            vars_order = c("DTN_bf", "burn_stat","ndvi", "DTN_road")) %>% # plot line at zero _behind_coefs
  relabel_predictors(
    c(
      DTN_bf = "DTN Brood Field",
      DTN_road = "DTN Road",
      ndvi = "NDVI", 
      burn_stat = "Burned")
  ) +
  
  theme_bw() + xlab("Coefficient Estimate") + ylab("") + 
  scale_color_discrete(name = "Model", labels = c("Jan-Feb", "Mar-Apr", "May-Jun", "Jul-Aug", 
                                                  "Sep-Oct", "Nov-Dec"), type = c('lightskyblue', 'lightgreen', 'orchid3', 'darkgreen', 'darkorange', 'red4'))# labels the legend then the models, then assigns colors 


Fig3_month_2interv + xlim(c(-1,1)) + coord_flip() +
  theme(text = element_text(size=30),
        axis.text.x = element_text(angle=25, hjust=1), 
        legend.text = element_text(size = 25))


### Monthly ----

# Fixed  
summary(jan_mod)
summary(feb_mod) 
summary(mar_mod)
summary(apr_mod)
summary(may_mod)
summary(jun_mod)
summary(jul_mod)
summary(aug_mod)
summary(sept_mod) 
summary(oct_mod)
summary(nov_mod)
summary(dec_mod)

# make a list to hold the mods 
month_mod = list(jan_mod, feb_mod, mar_mod, apr_mod, may_mod, jun_mod, jul_mod, aug_mod, sept_mod, oct_mod, nov_mod, dec_mod)

# Figure
Fig4_month = dwplot(month_mod,
                    ci = 0.95, 
                    dodge_size = 0.8, # how far apart pts are frome eachother (0.4 = default) 
                    show_intercept = FALSE, 
                    model_order = NULL, 
                    dot_args = list(size = 7), 
                    whisker_args = list(size = 1, colour = "grey3"),
                    vline = geom_vline(xintercept = 0, linetype = 2, colour ="grey8", size = 3), 
                    vars_order = c("DTN_bf", "burn_stat","ndvi", "DTN_road")) %>% # plot line at zero _behind_coefs
  relabel_predictors(
    c(
      DTN_bf = "DTN Brood Field",
      DTN_road = "DTN Road",
      ndvi = "NDVI", 
      burn_stat = "Burned")
  ) +
  guides(color = guide_legend(reverse=TRUE)) +
  theme_bw() + xlab("Coefficient Estimate") + ylab("") + 
  scale_color_discrete(name = "Model", labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", 
                                                  "Sep", "Oct", "Nov", "Dec"), 
                       type = c('lightslateblue', 'blue', 'lightskyblue','lightseagreen', 'green3', 'darkgreen', 'goldenrod1', 'darkorange', 'chocolate4', 'red2', 'deeppink4', 'plum3'))# labels the legend then the models, then assigns colors 


Fig4_month + xlim(c(-1, .8)) + coord_flip() +
  theme(text = element_text(size=30),
        axis.text.x = element_text(angle=25, hjust=1), 
        legend.text = element_text(size = 25))







