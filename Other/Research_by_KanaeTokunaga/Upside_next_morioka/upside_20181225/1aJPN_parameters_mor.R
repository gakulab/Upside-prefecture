# Japan Upside Model - Clean Data to Run GUM Package 
## Note 1 : TS >= 1964 & Revised Stock Category & Queen crab stock category revised
## Note 2: Change price unit from JPY/Kg to USD/mt 
## Kanae Tokunaga
## July 27, 2018
## Updated on December 28, 2018

rm(list = ls(all=TRUE))

## STEP 1a: Install Libraries
library(plyr)
library(dplyr)
library(tidyverse)
library(GUM)
library(stringr)
library(stats)
library(rfishbase)

setwd("~/Dropbox/Upside_next_morioka/upside_20181225/GUM_KK/R")
file.sources = list.files(pattern="*.R")
sapply(file.sources,source,.GlobalEnv)
file.sources = list.files(pattern="*.rda")
sapply(file.sources,load,.GlobalEnv)
setwd("~/Dropbox/Upside_next_morioka/upside_20181225/GUM_KK/data")
data.sources = list.files(pattern="*.rda")
sapply(data.sources,load,.GlobalEnv)

## Read dataframe created from JFA production statistics & stock assessments
setwd("~/Dropbox/Upside_next_morioka/upside_20181225")
dt.prod <- read.csv("data/cleaned/0dataProductionValueByStock.csv")
#dt.prod$fish <- as.character(dt.prod$fish)
#dt.prod$fish <- ifelse(dt.prod$fish == "half.crenate_ark_shell", "half-crenate_ark_shell", dt.prod$fish)
#dt.prod$fish <- as.factor(dt.prod$fish)

## Add SpeciesCat, SpeciesCatName, and make sure SciName and CommName are correct.
fish.name <- read.csv("data/miscData/fishName.csv")
dt.prod2 <- merge(dt.prod, fish.name, all.x = TRUE)
dt.prod2 <- dt.prod2[order(dt.prod2$EngName, dt.prod2$SciName,dt.prod2$fish, dt.prod2$stock, dt.prod2$year),]
### Note: There are stock = <NA> entries even though the stock is identified as "single_stock".
### These are most likely created when I merged datasets, but should check again later. 
asfis <- read.csv("data/miscData/1ASFIS 6 languages_2017.csv")
asfis2 <- subset(asfis, select = c("ISSCAAP", "Scientific_name", "English_name"))
colnames(asfis2)[2] <- "SciName" 
        
dt.prod3 <- merge(dt.prod2, asfis2, by = c("SciName"), all.x = TRUE)
dt.prod3 <- dt.prod3[order(dt.prod3$EngName, dt.prod3$year),]
#dt.prod3 <- dt.prod3[order(dt.prod3$EngName, dt.prod3$SciName,dt.prod3$fish, dt.prod3$stock, dt.prod3$year),]

## Drop years before 1982 (Note: This is because large amount of DWF activity prior to 1982)
dt.prod4 <- subset(dt.prod3, dt.prod3$year >= 1964)
unique(dt.prod4$EngName) #77 species
## List of fish that do not have Scientific name assigned 
### Drop those without scientific name assigned (for now)
dt.prod5 <- subset(dt.prod4, is.na(dt.prod4$SciName) == FALSE)
unique(dt.prod5$EngName) #70 species
dt.prod5 <- dt.prod5[order(dt.prod5$EngName, dt.prod5$SciName, dt.prod5$stock, dt.prod5$year),]
dt.prod5 <- subset(dt.prod5, select = c("EngName", "SciName", "stock", "year", "production_vol_t",
                                        "production_val_millionJPY", "ave_price_perKg", "ISSCAAP"))

# Crimson seabream + Yellowback seabream  =  Crison seabream, Yellowback seabream
crim_ylb_seabream <- subset(dt.prod5, dt.prod5$EngName == "Crimson seabream"|
                                    dt.prod5$EngName == "ellowback seabream" | dt.prod5$EngName == "Crison seabream, Yellowback seabream" )
crim_ylb_seabream2 <- ddply(crim_ylb_seabream, .(year), summarise,
                            production_vol_t = sum(production_vol_t, na.rm = TRUE),
                            production_val_millionJPY = sum(production_val_millionJPY, na.rm = TRUE),
                            ave_price_perKg = mean(ave_price_perKg, na.rm = TRUE))
crim_ylb_seabream2$EngName <- rep("Crison seabream, Yellowback seabream", nrow(crim_ylb_seabream2))
crim_ylb_seabream2$SciName <- rep("Evynnis japonica, Dentex tumifrons", nrow(crim_ylb_seabream2))
crim_ylb_seabream2$stock <- rep("single_stock", nrow(crim_ylb_seabream2))
crim_ylb_seabream2$ISSCAAP <- rep(NA, nrow(crim_ylb_seabream2))
dt.prod5 <- subset(dt.prod5, dt.prod5$EngName != "Crimson seabream" &
                           dt.prod5$EngName != "Yellowback seabream" & dt.prod5$EngName != "Crison seabream, Yellowback seabream" )
dt.prod5 <- rbind(dt.prod5, crim_ylb_seabream2)

# Bluefin tuna --> separate the dataset before and after 1995 due to Y < 1995 including southern bluefin and excluding meji (i.e. small bluefin)
dt.prod5$stock <- as.character(dt.prod5$stock)
dt.prod5$stock <- ifelse( dt.prod5$EngName == "Pacific bluefin tuna" & dt.prod5$year <= 1994, "single_stock_inc_sbf_exc_sml", dt.prod5$stock)
dt.prod5$stock <- ifelse( dt.prod5$EngName == "Pacific bluefin tuna" & dt.prod5$year >= 1995, "single_stock_exc_sbf_inc_sml", dt.prod5$stock)

# Blackhead seabream, Goldlined seabream --> separate dataset before and after 1995 due to Y < 1995 excluding Goldlined seabream data
dt.prod5$EngName <- as.character(dt.prod5$EngName)
dt.prod5$EngName <- ifelse( dt.prod5$EngName == "Blackhead seabream, Goldlined seabream" & dt.prod5$year <= 1994, "Blackhead seabream", dt.prod5$EngName)

## Pull resilience data manually from rfishbase
### c.f. https://cran.r-project.org/web/packages/rfishbase/vignettes/tutorial.html
fish <- as.character(dt.prod5$SciName)
resil <- stocks(fish, fields="Resilience")
resil <- subset(resil, is.na(resil$Resilience) == FALSE)
resil <- subset(resil, duplicated(resil$SciName) == FALSE)
#write.csv(resil, file = "data/miscData/resilience.csv") #This is in miscData folder
#resil <- read.csv("data/miscData/resilience.csv")

## Merge the dataset with resilience info 
dt.prod6 <- merge(dt.prod5, resil, by.x = "SciName", by.y = "sciname", all.x = TRUE) 

## Check which species are lacking resilience data
no.resil <- subset(dt.prod6, is.na(dt.prod6$Resilience) == TRUE)
no.resil.list <- subset(no.resil, select = c("SciName", "EngName"))
no.resil.list <- unique(no.resil.list) #45 species category

yes.resil <- subset(dt.prod6, is.na(dt.prod6$Resilience) == FALSE)
yes.resil.list <- subset(yes.resil, select = c("SciName", "EngName"))
yes.resil.list <- unique(yes.resil.list) #30 species category

# If no resilience value, assign "Medium"
dt.prod6$Resilience <- ifelse(is.na(dt.prod6$Resilience)=="TRUE", "Medium", dt.prod6$Resilience)

## Create IdOrig value from species x stock combination
### First, identify species without EngName 
dt.prod6$EngName <- as.character(dt.prod6$EngName)
#### Lepidotrigla microptera; Marsupenaeus japonicus; Mugil spp; Parapristipoma trilineatum; Pennahia spp 
dt.prod6$EngName <- ifelse(dt.prod6$SciName == "Lepidotrigla microptera", "Redwing searobin", dt.prod6$EngName)
dt.prod6$EngName <- ifelse(dt.prod6$SciName == "Marsupenaeus japonicus", "Japanese shrimp", dt.prod6$EngName)
dt.prod6$EngName <- ifelse(dt.prod6$SciName == "Mugil spp", "Flathead mullet", dt.prod6$EngName)
dt.prod6$EngName <- ifelse(dt.prod6$SciName == "Parapristipoma trilineatum", "Threeline grunt", dt.prod6$EngName)
dt.prod6$EngName <- ifelse(dt.prod6$SciName == "Pennahia spp", "Other croaker", dt.prod6$EngName)

dt.prod6$Fishery <- paste(dt.prod6$EngName, dt.prod6$stock, sep = " ")
dt.prod6 <- dt.prod6[order(dt.prod6$Fishery, dt.prod6$year),]
dt.prod6$EngName <- as.factor(as.character(dt.prod6$EngName))
dt.prod6$Fishery <- as.factor(as.character(dt.prod6$Fishery))

dt.prod6$IdOrig <- as.numeric(dt.prod6$Fishery)

fishery.list <- subset(dt.prod6, select = c("IdOrig", "Fishery", "SciName", "stock", "EngName"))
fishery.list <- unique(fishery.list) #102 fisheries

write.csv(fishery.list, file = "data/miscData/fisheryList.csv")

### Create species category name
unique(dt.prod6$ISSCAAP) #[1] NA 23 33 34 36 42 35 37 46 32 52 76 24 91 56 57 43 31 44 55 38
dt.prod6$SpeciesCatName <- ifelse(dt.prod6$ISSCAAP == 23, "Salmons, trouts, smelts", "ph")
dt.prod6$SpeciesCatName <- ifelse(dt.prod6$ISSCAAP == 33, "Miscellaneous coastal fishes", dt.prod6$SpeciesCatName)
dt.prod6$SpeciesCatName <- ifelse(dt.prod6$ISSCAAP == 34, "Miscellaneous demersal fishes", dt.prod6$SpeciesCatName)
dt.prod6$SpeciesCatName <- ifelse(dt.prod6$ISSCAAP == 36, "Tunas, bonitos, billfishes", dt.prod6$SpeciesCatName)
dt.prod6$SpeciesCatName <- ifelse(dt.prod6$ISSCAAP == 42, "Crabs, sea-spiders", dt.prod6$SpeciesCatName)
dt.prod6$SpeciesCatName <- ifelse(dt.prod6$ISSCAAP == 35, "Herrings, sardines, anchovies", dt.prod6$SpeciesCatName)
dt.prod6$SpeciesCatName <- ifelse(dt.prod6$ISSCAAP == 37, "Miscellaneous pelagic fishes", dt.prod6$SpeciesCatName)
dt.prod6$SpeciesCatName <- ifelse(dt.prod6$ISSCAAP == 46, "Krill, planktonic crustaceans", dt.prod6$SpeciesCatName)
dt.prod6$SpeciesCatName <- ifelse(dt.prod6$ISSCAAP == 32, "Cods, hakes, haddocks", dt.prod6$SpeciesCatName)
dt.prod6$SpeciesCatName <- ifelse(dt.prod6$ISSCAAP == 52, "Abalones, winkles, conchs", dt.prod6$SpeciesCatName)
dt.prod6$SpeciesCatName <- ifelse(dt.prod6$ISSCAAP == 76, "Sea-urchins and other echinoderms", dt.prod6$SpeciesCatName)
dt.prod6$SpeciesCatName <- ifelse(dt.prod6$ISSCAAP == 24, "Shads", dt.prod6$SpeciesCatName)
dt.prod6$SpeciesCatName <- ifelse(dt.prod6$ISSCAAP == 91, "Brown seaweeds", dt.prod6$SpeciesCatName)
dt.prod6$SpeciesCatName <- ifelse(dt.prod6$ISSCAAP == 56, "Clams, cockles, arkshells", dt.prod6$SpeciesCatName)
dt.prod6$SpeciesCatName <- ifelse(dt.prod6$ISSCAAP == 57, "Squids, cuttlefishes, octopuses", dt.prod6$SpeciesCatName)
dt.prod6$SpeciesCatName <- ifelse(dt.prod6$ISSCAAP == 43, "Lobsters, spiny-rock lobsters", dt.prod6$SpeciesCatName)
dt.prod6$SpeciesCatName <- ifelse(dt.prod6$ISSCAAP == 31, "Flounders, halibuts, soles", dt.prod6$SpeciesCatName)
dt.prod6$SpeciesCatName <- ifelse(dt.prod6$ISSCAAP == 44, "King crabs, squat-lobsters", dt.prod6$SpeciesCatName)
dt.prod6$SpeciesCatName <- ifelse(dt.prod6$ISSCAAP == 55, "Scallops, pectens", dt.prod6$SpeciesCatName)
dt.prod6$SpeciesCatName <- ifelse(dt.prod6$ISSCAAP == 38, "Sharks, rays, chimaeras", dt.prod6$SpeciesCatName)

dt.prod6$SpeciesCatName <- ifelse(dt.prod6$EngName == "Blackhead seabream, Goldlined seabream", "Miscellaneous coastal fishes", dt.prod6$SpeciesCatName)
dt.prod6$SpeciesCatName <- ifelse(dt.prod6$EngName == "Blackhead seabream", "Miscellaneous coastal fishes", dt.prod6$SpeciesCatName)
dt.prod6$SpeciesCatName <- ifelse(dt.prod6$EngName == "Crison seabream, Yellowback seabream", "Miscellaneous coastal fishes", dt.prod6$SpeciesCatName)

dt.prod6$ISSCAAP <- ifelse(dt.prod6$SpeciesCatName == "Miscellaneous coastal fishes", 33, dt.prod6$ISSCAAP)


### Subset with the necessary variables
dt.prod6 <- subset(dt.prod6, select = c("IdOrig", "Fishery", "SciName", "EngName", "ISSCAAP", "SpeciesCatName", "Resilience", "year", "production_vol_t", "ave_price_perKg"))
colnames(dt.prod6) <- c("IdOrig","Fishery", "SciName", "CommName", "SpeciesCat", "SpeciesCatName", "Res1", "Year", "Catch", "Price")
unique(dt.prod6$CommName) #69 species

### For now, only include the ones with complete information (i.e. exclude ones with missing Resilience and other info)
dt.prod6.clean <- subset(dt.prod6, complete.cases(dt.prod6[,1:9]) == TRUE)
unique(dt.prod6.clean$CommName) #69 species
unique(dt.prod6.clean$Fishery) #102 fisheries
#write.csv(dt.prod6.clean, "data/cleanedData/0dataProdClean.csv")

library(reshape2)
dt.prod6.clean2 <- dt.prod6.clean
dt.prod6.clean2 <- dt.prod6.clean2[,-10]
dt.prod6.clean2 <- dcast(dt.prod6.clean2, IdOrig + Fishery + SciName + CommName + SpeciesCat + SpeciesCatName + Res1 ~ Year, value.var="Catch")
write.csv(dt.prod6.clean2, file = "data/cleaned/1japan_production.csv")

dt.prod6.clean3 <-  dt.prod6.clean
dt.prod6.clean3 <- subset(dt.prod6.clean3, dt.prod6.clean3$Year >= 2003)
dt.prod6.clean3$Price2 <- (dt.prod6.clean3$Price * 0.0090)*1000 # Convert price unit from JPY/Kg to USD/mt
dt.prod6.clean3 <- dt.prod6.clean3[,-c(9, 10)]
dt.prod6.clean3 <- dcast(dt.prod6.clean3, IdOrig + Fishery + SciName + CommName + SpeciesCat + SpeciesCatName + Res1 ~ Year, value.var="Price2")
#write.csv(dt.prod6.clean3, file = "data/cleaned/1japan_price.csv")


