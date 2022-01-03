# FAJ Fisheries Production Data Cleaning (part2) + Production Value Data Cleaning (Part2)
## Kanae Tokunaga
## Feb 28, 2018
## Last updated April 16, 2018

rm(list = ls(all=TRUE))
setwd("/Volumes/GoogleDrive/Shared drives/gakuLab_Research_Upside/Upside-U.Iwate/Other/Research_by_KanaeTokunaga/Upside_next_morioka/upside_20181225/")

library(plyr)
library(reshape2)
library(stringr)
library(zoo)
library(gdata)

# Read cleaned data file (c.f. cleanKaimenKenbetsu.R)
dt <- read.csv("data/CatchJPN1956_2019.csv",fileEncoding = "CP932")

unique(dt$fish)


#7/8ここまで==================

# Species Name
species <- read.csv("data/miscData/fishName.csv")

dt <- merge(dt, species, by.x = "variable", by.y = "fish", all.x = TRUE)
dt$EngName <- as.character(dt$EngName)
dt$variable <- as.character(dt$variable)

dt$EngName <- ifelse(dt$EngName == "", dt$variable, dt$EngName)
dt$EngName <- ifelse(is.na(dt$EngName) == TRUE, dt$variable, dt$EngName)
unique(dt$EngName)


#dt.pos <- subset(dt, dt$value > 0)
pref.fish <- unique(dt[c("prefecture","variable")])

# stock code
stock.list <- read.csv("data/miscData/stock_code_0530.csv")
num.stock <- as.data.frame(table(stock.list$Fish))
num.stock$multi.stock <- as.numeric(num.stock$Freq > 1)

pref.fish <- merge(pref.fish, num.stock, by.x = "variable", by.y = "Var1", all.x = TRUE)

pref.fish$stock <- ifelse(pref.fish$multi.stock == 0, 0, "ph")
pref.fish.multi <- subset(pref.fish, pref.fish$multi.stock != 0)

# Export the list of prefecture x fish list for the ones with multiple stocks
#con <- file('multiStock.csv',encoding="cp932")
#write.csv(pref.fish.multi, file = con)

# Read stock data
dt.stock <- read.csv("data/miscData/multiStock_complete_0530.csv", encoding = "cp932")

pref.fish.list <- subset(pref.fish, select = c("variable", "prefecture", "multi.stock"))
colnames(pref.fish.list) <- c("fish", "prefecture", "multi.stock")

dt.stock <- merge(pref.fish.list, dt.stock, all.x = TRUE)
dt.stock <- subset(dt.stock, select = c("fish","prefecture", "multi.stock", "stock"))
dt.stock$stock <- ifelse(dt.stock$multi.stock == 0, "single_stock", as.character(dt.stock$stock))
dt.stock$stock <- as.factor(dt.stock$stock)

# Merge stock data with the production data
dt <- subset(dt, select = c("year", "prefecture", "variable", "value", "pref_code", "SciName", "EngName"))
colnames(dt)[3:4] <- c("fish", "production_vol_t") 

dt.comp <- merge(dt, dt.stock, by = c("prefecture", "fish"), all.x = TRUE)

# Convert prefecture name to alphabet
pref.alph <- read.csv("data/miscData/japan-prefectures-roman-hepburn.csv", header = FALSE,encoding = "cp932")
colnames(pref.alph) <- c("prefecture", "hiragana", "alphabet")
dt.comp <- merge(dt.comp, pref.alph, by = "prefecture", all.x = TRUE)
dt.comp <- subset(dt.comp, select = c("fish", "EngName", "SciName", "year", "production_vol_t", "stock", "alphabet"))
colnames(dt.comp) <- c("fish", "EngName","SciName", "year", "production_vol_t", "stock", "prefecture")

# Exclude other_xxx: "other_crab", "other_croaker", "other_fish", "other_fish.1", "other_kajiki", "other_seaweed"                            
#"other_shellfish", "other_shrimp", "other_squid", "other_tuna"
dt.comp <- subset(dt.comp, dt.comp$fish != "other_crab" & dt.comp$fish != "other_croaker" &
                          dt.comp$fish != "other_fish" & dt.comp$fish != "other_fish.1" &
                          dt.comp$fish != "other_kajiki" & dt.comp$fish != "other_seaweed" &
                          dt.comp$fish != "other_shellfish" & dt.comp$fish != "other_shrimp" &
                          dt.comp$fish != "other_squid" & dt.comp$fish != "other_tuna")

# Exclude seaweed species: "funori_seaweed", "hijiki_seaweed", "kelp" , "tengusa_seaweed" , "wakame_seaweed"  
dt.comp <- subset(dt.comp, dt.comp$fish != "funori_seaweed" & dt.comp$fish != "hijiki_seaweed" &
                          dt.comp$fish != "kelp" & dt.comp$fish != "tengusa_seaweed" &
                          dt.comp$fish != "wakame_seaweed")

# Exclude datapoints with production_vol_t == 0
dt.comp <- subset(dt.comp, dt.comp$production_vol_t > 0)

# Set stock for the half.crenate_ark_shell as single_stock
dt.comp$stock <- as.character(dt.comp$stock)
dt.comp$EngName <- as.character(dt.comp$EngName)
dt.comp$stock <- ifelse(dt.comp$EngName == "half.crenate_ark_shell", "single_stock", dt.comp$stock)

write.csv(dt.comp, file = "data/cleaned/0dataProductionByPrefecture.csv")


# Summarize the prefecture-level data -> stock-level data
dt.prod.stock <- ddply(dt.comp, .(EngName, year, stock), summarise,
                       production_vol_t = sum(production_vol_t, na.rm = TRUE))
#write.csv(dt.prod.stock, file = "data/cleaned/0dataProductionByStock_0530.csv")

# Production value data (value data is only available after 2003)
dt.val <- read.csv("data/dataKaimenSanshutsugaku.csv")

dt.val <- merge(dt.val, dt.stock, by = c("prefecture", "fish"), all.x = TRUE)

dt.val <- merge(dt.val, pref.alph, by = "prefecture", all.x = TRUE)
dt.val <- subset(dt.val, select = c("year", "alphabet", "fish", "value_million_JPY", "stock"))
colnames(dt.val) <- c("year", "prefecture", "fish", "value_million_JPY", "stock")
dt.val <- dt.val[order(dt.val$prefecture, dt.val$year),]

dt.val <- merge(dt.val, species, by.x = "fish", by.y = "fish", all.x = TRUE)
dt.val$EngName <- as.character(dt.val$EngName)
dt.val$fish <- as.character(dt.val$fish)

dt.val$EngName <- ifelse(dt.val$EngName == "", dt.val$fish, dt.val$EngName)
dt.val$EngName <- ifelse(is.na(dt.val$EngName) == TRUE, dt.val$fish, dt.val$EngName)
unique(dt$EngName)

# Merge value data with the production data by prefecture
dt.comp2 <- merge(dt.comp, dt.val, all.x = TRUE)
dt.comp2 <- dt.comp2[order(dt.comp2$prefecture, dt.comp2$year),]

dt.comp2$price_perKg <- (dt.comp2$value_million_JPY * 1000) / dt.comp2$production_vol_t
dt.comp2$price_perKg[is.nan(dt.comp2$price_perKg) == TRUE] <- NA
#write.csv(dt.comp2, file = "data/cleaned/0dataProductionValueByPrefecture.csv")

# Production value data by stock
dt.prod.value <- ddply(dt.comp2, .(EngName, year, stock), summarise,
                       production_val_millionJPY = sum(value_million_JPY, na.rm = TRUE),
                       production_vol_t = sum(production_vol_t, na.rm = TRUE),
                       ave_price_perKg = mean(price_perKg, na.rm = TRUE))

dt.prod.value$ave_price_perKg <- ifelse(is.nan(dt.prod.value$ave_price_perKg) == TRUE, NA, dt.prod.value$ave_price_perKg)
dt.prod.value$production_val_millionJPY <- ifelse(dt.prod.value$year < 2003,  NA, dt.prod.value$production_val_millionJPY)

dt.value.stock <- ddply(dt.val, .(EngName, year, stock), summarise,
                       production_val_millionJPY = sum(value_million_JPY, na.rm = TRUE))
dt.value.stock$production_val_millionJPY <- ifelse(dt.value.stock$year < 2003, NA,
                                                   dt.value.stock$production_val_millionJPY)

#write.csv(dt.prod.value, file = "data/cleaned/0dataProductionValueByStock.csv")



