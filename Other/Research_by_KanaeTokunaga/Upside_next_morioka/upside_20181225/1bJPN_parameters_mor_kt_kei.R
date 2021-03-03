# Japan Upside Model - Run GUM Package (Queen crab stock revised)
## Kanae Tokunaga
## July 27, 2018
## Updated on December 28, 2018

#Kei Kawamuraが改訂(注釈を付加)した版
#2021/1/25

#環境を初期化
rm(list = ls(all=TRUE))

## STEP 1a: Install Libraries
library(plyr)
library(dplyr)
library(tidyverse)
library(GUM)
library(stringr)
library(stats)
library(rfishbase)

#ディレクトリの変更(佳奈恵さんのコードでやるとエラーが生じるので修正したKeiのコードで実行する)
setwd("/Volumes/GoogleDrive/Shared drives/gakuLab_Research_Upside/Upside-prefecture/Other/GUM-master-Kei/R_Kei")
file.sources = list.files(pattern="*.R")
sapply(file.sources,source,.GlobalEnv)
file.sources = list.files(pattern="*.rda")
sapply(file.sources,load,.GlobalEnv)

setwd("/Volumes/GoogleDrive/Shared drives/gakuLab_Research_Upside/Upside-prefecture/Other/GUM-master-Kei/data")
data.sources = list.files(pattern="*.rda")
sapply(data.sources,load,.GlobalEnv)

# Step 0: Read the data
setwd("/Volumes/GoogleDrive/Shared drives/gakuLab_Research_Upside/Upside-prefecture/Other/Research_by_KanaeTokunaga/Upside_next_morioka/upside_20181225/")


#漁獲量データと生産額データの読み込み
dt.catch <- read.csv("data/cleaned/1japan_production.old.csv")
dt.price <- read.csv("data/cleaned/1japan_price.csv")

## STEP 1: Format data so it can be run in GUM package
#ここでデフォルトで使っているデータでは年数ごとに列が作成されているので、tidyな縦型データにする
japan_catch1 <- dt.catch %>%
        gather(Year, Catch, X1964:X2015) %>%
        mutate(Year = as.integer(substring(Year, 2))) 

#魚種ごとに並べ替える
japan_catch1 <- japan_catch1[order(japan_catch1$IdOrig, japan_catch1$Year), ]

## Make final dataframe
#並べ替えた順番通りに番号を付与し直して、IdOrig(任意のid)を付け直して整える
japan_catch2 <- japan_catch1 %>%
        mutate(X = seq.int(nrow(japan_catch1))) %>%
        mutate(IdOrig = paste("Japan",IdOrig, sep = "-"))

japan_catch3 <- japan_catch2

#データ型を整える
japan_catch3$SciName <- as.character(japan_catch3$SciName)
#japan_catch3$sciname <- japan_catch3$SciName
japan_catch3$CommName <- as.character(japan_catch3$CommName)
japan_catch3$SpeciesCatName <- as.character(japan_catch3$SpeciesCatName)
japan_catch3$Res1 <- as.character(japan_catch3$Res1)



##### Run GUM assessment ######
#ここからが実質的なスタート。まずは生物モデル(資源動態モデル)に先ほど作成したデータを入れて実行する。
japan_results <- run_gum_assessment_Kei(japan_catch3) ## If this doesn't run, try restart R
##KK: This should be "Fishery" as this is the unique id for each stock:
unique(japan_results$Fishery) # 102 fisheries in the dataset

#必要な列(魚種系群名・魚種一般名・魚種学名)
japan.fishery <- subset(japan_results, select = c(Fishery, CommName, SciName))

#重複した列を除き、"0Upside_inputs_prep_181228.csv"として出力
japan.fishery <- japan.fishery[!duplicated(japan.fishery),]
write.csv(japan.fishery, file = "data/0Upside_inputs_prep_181228.csv")

### Add BMSY
BMSY = function(K,phi)
{
        bmsy = K / (phi + 1) ^ (1 / phi)
        return(bmsy)
}

###
Japan_results100 <- japan_results %>%
        mutate(BMSY = BMSY(k, phi),
               Biomass = BMSY * BvBmsy,
               B_over_K = Biomass / k)

## STEP 2: write csv of GUM results:
write.csv(Japan_results100, "data/2japan_GUM_results_K100_190116.csv", row.names = FALSE)

## STEP 3:
######################################################
##### Finish filling in the input sheet ##############
######################################################
## The file "3Upside_Input_...csv" is a file that the user should make themselves with the basic naming information
## for each stock and the column headers as in this file "3Upside_Input_Rom.csv". The columns of data are empty
## and will be filled in by the following code.

## 追加
#3Upside_Input_...csvを擬似作成
Input <- data.frame(
  Fishery = dt.catch$Fishery,
  Country = "Japan",
  Species = dt.catch$CommName,
  Scientific.x = dt.catch$SciName
)


japan_inputs <- Input #read.csv('data/3Upside_Input_0530.csv',stringsAsFactors=FALSE) #v3 is fine for this one.
japan_results <- read.csv('data/2japan_GUM_results_K100_190116.csv', header = T, stringsAsFactors = F)

# Fishery list
#japan_inputs <- subset(japan_inputs, is.element(japan_inputs$Scientific, japan_results$SciName) == TRUE)


# Changed to now use the thetas in the 3Upside_Input_Rom.csv file (user to input these, if not, use the settings
# below (theta_domestic, theta_legal1 = 1 and theta_legal2 = 0))
# Change discount rate as desired (here we are going to produce files with 0, 5 and 10%)
japan_inputs <- japan_inputs %>%
        mutate(#Fishery2 = Fishery,
               Time = 50,
               N = 1,
               phi_lower = 0.188,
               phi_expected = 0.188,
               phi_upper = 0.188,
               theta_domestic = 1,
               theta_legal1_lower = 1,
               theta_legal1_expected = 1,
               theta_legal1_upper = 1,
               theta_legal2_lower = 0,
               theta_legal2_expected = 0,
               theta_legal2_upper = 0,
               disc_lower = 0.01,
               disc_expected = 0.02,
               disc_upper = 0.03,
               beta_lower = 1,
               beta_expected = 1.3,
               beta_upper = 1.5,
               gamma_p1 = 1.31,
               gamma_p2 = 1.31,
               gamma_c1 = 0.77,
               gamma_c2 = 0.77,
               lambda = 0.1,
                split = 1
        )

## Change theta_domestic for international fisheries = 0.6
setwd("/Volumes/GoogleDrive/Shared drives/gakuLab_Research_Upside/Upside-U.Iwate/Other/Research_by_KanaeTokunaga/Upside_next_morioka")
int <- read.csv("data/international.csv")
japan_inputs <- merge(japan_inputs, int, by = "Fishery")
japan_inputs$theta_domestic <- ifelse(japan_inputs$International == 1, 0.6, japan_inputs$theta_domestic)
japan_inputs <- japan_inputs[, -c(54)]

### add inputs from GUM results
###KK: 2015 is not the most recent year for ALL species!! (This is why some species were removed)
japan_results_max<-japan_results%>%
  group_by(Fishery)%>%
  filter(year==max(year)) %>% 
  select(IdOrig, Fishery, SciName, year, BvBmsy, MSY, phi, g, k, FvFmsy) %>%
  rename(Scientific = SciName)

japan_results_max <- as.data.frame(japan_results_max)

japan_inputs1 <- left_join(japan_inputs, japan_results_max, by = c("Fishery"))

japan_inputs2 <- japan_inputs1 %>%
        mutate(g_lower = g,
               g_expected = g,
               g_upper = g,
               K_lower = k,
               K_expected = k,
               K_upper = k,
               b0_lower = BvBmsy,
               b0_expected = BvBmsy,
               b0_upper = BvBmsy,
               f0_total_lower = FvFmsy,
               f0_total_expected = FvFmsy,
               f0_total_upper = FvFmsy) # %>%
        #select(Fishery:split, MSY)


### Add prices: Only need this if you don't have user input on prices! This code will pull price data from the 
### 'lumped' upside data.

# Format price data
## STEP 1: Format data so it can be run in GUM package
japan_price1 <- dt.price %>%
        gather(Year, Price, X2003:X2015) %>%
        mutate(Year = as.integer(substring(Year, 2))) 

japan_price1 <- japan_price1[order(japan_price1$IdOrig, japan_price1$Year), ]

## Make final dataframe
japan_price2 <- japan_price1 %>%
        mutate(X = seq.int(nrow(japan_price1))) %>%
        mutate(IdOrig = paste("Japan",IdOrig, sep = "-"))
japan_price3 <- japan_price2
japan_price3$SciName <- as.character(japan_price3$SciName)
japan_price3$CommName <- as.character(japan_price3$CommName)
japan_price3$SpeciesCatName <- as.character(japan_price3$SpeciesCatName)
japan_price3$Res1 <- as.character(japan_price3$Res1)

pdata <- ddply(japan_price3, .(Fishery), summarise,
                p1_lower = min(Price, na.rm = TRUE),
                p1_expected = mean(Price, na.rm = TRUE),
                p1_upper = max(Price, na.rm = TRUE),
                p2_lower = min(Price, na.rm = TRUE),
                p2_expected = mean(Price, na.rm = TRUE),
                p2_upper = max(Price, na.rm = TRUE))

# Fill price information for Blackhead seabream single_stock & Pacific bluefin tuna single_stock_inc_sbf_exc_sml, Yellow croaker single_stock
pdata_bft <- subset(pdata, pdata$Fishery == "Pacific bluefin tuna single_stock_exc_sbf_inc_sml")
pdata$p1_lower <- ifelse(pdata$Fishery == "Pacific bluefin tuna single_stock_inc_sbf_exc_sml", pdata_bft$p1_lower, pdata$p1_lower)
pdata$p1_expected <- ifelse(pdata$Fishery == "Pacific bluefin tuna single_stock_inc_sbf_exc_sml", pdata_bft$p1_expected, pdata$p1_expected)
pdata$p1_upper <- ifelse(pdata$Fishery == "Pacific bluefin tuna single_stock_inc_sbf_exc_sml",  pdata_bft$p1_upper, pdata$p1_upper)
pdata$p2_lower <- ifelse(pdata$Fishery == "Pacific bluefin tuna single_stock_inc_sbf_exc_sml", pdata_bft$p1_lower, pdata$p2_lower)
pdata$p2_expected <- ifelse(pdata$Fishery == "Pacific bluefin tuna single_stock_inc_sbf_exc_sml", pdata_bft$p1_expected, pdata$p2_expected)
pdata$p2_upper <- ifelse(pdata$Fishery == "Pacific bluefin tuna single_stock_inc_sbf_exc_sml", pdata_bft$p1_upper, pdata$p2_upper)

pdata_bsb <- subset(pdata, pdata$Fishery == "Blackhead seabream, Goldlined seabream single_stock")
pdata$p1_lower <- ifelse(pdata$Fishery == "Blackhead seabream, Goldlined seabream single_stock", pdata_bsb$p1_lower, pdata$p1_lower)
pdata$p1_expected <- ifelse(pdata$Fishery == "Blackhead seabream, Goldlined seabream single_stock", pdata_bsb$p1_expected, pdata$p1_expected)
pdata$p1_upper <- ifelse(pdata$Fishery == "Blackhead seabream, Goldlined seabream single_stock", pdata_bsb$p1_upper, pdata$p1_upper)
pdata$p2_lower <- ifelse(pdata$Fishery == "Blackhead seabream, Goldlined seabream single_stock", pdata_bsb$p2_lower, pdata$p2_lower)
pdata$p2_expected <- ifelse(pdata$Fishery == "Blackhead seabream, Goldlined seabream single_stock", pdata_bsb$p2_expected, pdata$p2_expected)
pdata$p2_upper <- ifelse(pdata$Fishery == "Blackhead seabream, Goldlined seabream single_stock", pdata_bsb$p2_upper, pdata$p2_upper)




japan_inputs3 <- japan_inputs2 %>%
        mutate(p1_lower = pdata$p1_lower[match(Fishery,pdata$Fishery)],
               p1_expected = pdata$p1_expected[match(Fishery,pdata$Fishery)],
               p1_upper = pdata$p1_upper[match(Fishery,pdata$Fishery)],
               p2_lower = pdata$p2_lower[match(Fishery,pdata$Fishery)],
               p2_expected = pdata$p2_expected[match(Fishery,pdata$Fishery)],
               p2_upper = pdata$p2_upper[match(Fishery,pdata$Fishery)]
               )

#scinames <- as.vector(japan_inputs3$Scientific)
# pdata2 <- lumped %>%
#   filter(Year == 2012,
#          SciName %in% scinames) %>%
#   group_by(SciName) %>%
#   summarise(mean_price = mean(Price)) %>%
#   rename(Scientific = SciName)
# 
# pdata2 <- as.data.frame(pdata2)
# 
# japan_inputs3_x <- japan_inputs3 %>%
#   mutate(p1_lower = pdata$mean_price[match(Scientific,pdata$Scientific)],
#          p1_expected = pdata$mean_price[match(Scientific,pdata$Scientific)],
#          p1_upper = pdata$mean_price[match(Scientific,pdata$Scientific)],
#          p2_lower = pdata$mean_price[match(Scientific,pdata$Scientific)],
#          p2_expected = pdata$mean_price[match(Scientific,pdata$Scientific)],
#          p2_upper = pdata$mean_price[match(Scientific,pdata$Scientific)])

#########
# Harengula humeralis : speccat 35
# Harengula clupeola : 35
# Gerres cinereus : 33
# Archosargus rhomboidalis : 33
# Haemulon sciurus : 33
# Caranx latus : 37
# Trachinotus goodei : 37
# Lutjanus apodus : 33
# Calamus bajonado : 33

# pdata_spc <- lumped %>%
#   filter(Year == 2012,
#          Country == "Cuba") %>%
#   group_by(SpeciesCat) %>%
#   summarise(mean_price = mean(Price))
# 
# sci33 <- c("Gerres cinereus", "Archosargus rhomboidalis", "Haemulon sciurus", "Lutjanus apodus", "Calamus bajonado")
# sci35 <- c("Harengula humeralis", "Harengula clupeola")
# sci37 <- c("Caranx latus", "Trachinotus goodei")

## 33: 2934.518
## 35: 349.062

# cuba_inputs4 <- cuba_inputs3 %>%
#   mutate(p1_lower = ifelse(Scientific %in% sci33, 2934.518,
#                            ifelse(Scientific %in% sci35, 349.062, p1_lower)),
#          p1_expected = ifelse(Scientific %in% sci33, 2934.518,
#                                ifelse(Scientific %in% sci35, 349.062, p1_expected)),
#          p1_upper = ifelse(Scientific %in% sci33, 2934.518,
#                             ifelse(Scientific %in% sci35, 349.062, p1_upper)),
#          p2_lower = ifelse(Scientific %in% sci33, 2934.518,
#                             ifelse(Scientific %in% sci35, 349.062, p2_lower)),
#          p2_expected = ifelse(Scientific %in% sci33, 2934.518,
#                                ifelse(Scientific %in% sci35, 349.062, p2_expected)),
#          p2_upper =  ifelse(Scientific %in% sci33, 2934.518,
#                             ifelse(Scientific %in% sci35, 349.062, p2_upper)))

### species cat 37 is fao region 31

# pdata37 <- lumped %>%
#   filter(Year == 2012,
#          RegionFAO == "31",
#          SpeciesCat == 37) %>%
#   group_by(SpeciesCat) %>%
#   summarise(mean_price = mean(Price))

## mean price for species cat 37: 1563.375

# cuba_inputs5 <- cuba_inputs4 %>%
#   mutate(p1_lower = ifelse(Scientific %in% sci37, 1563.375, p1_lower),
#          p1_expected = ifelse(Scientific %in% sci37, 1563.375, p1_expected),
#          p1_upper = ifelse(Scientific %in% sci37, 1563.375, p1_upper),
#          p2_lower = ifelse(Scientific %in% sci37, 1563.375, p2_lower),
#          p2_expected = ifelse(Scientific %in% sci37, 1563.375, p2_expected),
#          p2_upper = ifelse(Scientific %in% sci37, 1563.375, p2_upper))

###########
### calculate cost

## Functions
## Cost of fishing parameter calculation
fishMortSS <- function(phi, bbar) {
        fbar <- (phi + 1) / phi * (1 - (bbar ^ phi) / (phi + 1))
        return(fbar) ## storing output of function
}

cost <- function(p_exp, phi, bbar, MSY, g_exp, beta) {
        fbar <- fishMortSS(phi, bbar)
        c_exp <-p_exp * fbar * bbar * MSY / ((g_exp * fbar) ^ beta)
        return(c_exp)
}

bbar <- 0.5 ##KK: used value from global paper rather than Cuba value

### Add COSTS to data frame
# RUN THE NEXT LINE IF YOU SUPPLY YOUR OWN COSTS:
#cuba_inputs5<-cuba_inputs2

japan_inputs4 <- japan_inputs3 %>%
        mutate(c1_lower = cost(p_exp = p1_lower, phi = phi_lower, bbar = bbar, MSY = MSY, g_exp = g_lower, beta = beta_lower),
               c1_expected = cost(p_exp = p1_expected, phi = phi_expected, bbar = bbar, MSY = MSY, g_exp = g_expected, beta = beta_expected),
               c1_upper = cost(p_exp = p1_upper, phi = phi_upper, bbar = bbar, MSY = MSY, g_exp = g_upper, beta = beta_upper),
               c2_lower = cost(p_exp = p2_lower, phi = phi_lower, bbar = bbar, MSY = MSY, g_exp = g_lower, beta = beta_lower),
               c2_expected = cost(p_exp = p2_expected, phi = phi_expected, bbar = bbar, MSY = MSY, g_exp = g_expected, beta = beta_expected),
               c2_upper = cost(p_exp = p2_upper, phi = phi_upper, bbar = bbar, MSY = MSY, g_exp = g_upper, beta = beta_upper)) %>%
        select(Fishery:split)


japan_inputs4 <- japan_inputs4[order(japan_inputs4$Fishery),] 

## Price info are missing for 5 species Drop those.
table(complete.cases(pdata) == FALSE)

japan_inputs5 <- subset(japan_inputs4, complete.cases(japan_inputs4) == TRUE)

col = names(dataInput)
japan_inputs5 <- select(japan_inputs5,
                        col)

#write.csv(japan_inputs5, "4Inputs_for_proj_disc5per_200917.csv", row.names = FALSE)



