###################################################
#### Surplus Production Model applied to Japan ####
###################################################

# Created by Raphael Roman -- 
# -- To be used in Gaku's presentation at the 2019 JSFS Conference in Tokyo
# March 2019
rm(list = ls())

# Note: Download non-loaded libraries if necessary (e.g. "rfishbase", "GGUM" and "pander")
library(devtools)
library(plyr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(stringr)
library(stats)
library(rfishbase)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(pander)
library(GUM)
library(tictoc)
tic()
# to install the 'GUM' package, we need to install the package 'devtools' and then
# download the GUM library from UCSB's Github repo, so that:
# devtools::install_github('DanOvando/GUM', build_vignettes = T)

# Set working directory
User = 3 # 1: Raphael 2: Gaku and 3: kei
if(User == 1){setwd("~/Dropbox/Upside_next_morioka_work/JapanCode")} else if
(User == 2) {setwd("/Users/gakugaku/Dropbox/Drop_REsearch/Upside_next_morioka_original/JapanCode/")}else if
(User == 3) {setwd("/Users/keikawamura/Dropbox/Upside_next_morioka_work/JapanCode")}

###################################################
# Let's first read and perform a quick scan/analysis of Japan's production/catch and price data

# Catch data
Japan_catch <- read.csv("data/1japan_production.csv", stringsAsFactors = F, header = T, sep=",", dec=".")
Japan_catch <-  Japan_catch[,c(2:60)]
# Using a loop to remove "X"s that appear in front of each year column
for(i in 8:ncol(Japan_catch)) {
  names(Japan_catch)[i] <- gsub("X","", names(Japan_catch)[i])
}
str(Japan_catch)
# Note: Catch data are expressed in metric tons (MT)

# Price data
Japan_price <- read.csv("data/1japan_price.csv", stringsAsFactors = F, header = T, sep=",", dec=".")
Japan_price <-  Japan_price[,c(2:21)]
for(i in 8:ncol(Japan_price)) {
  names(Japan_price)[i] <- gsub("X","", names(Japan_price)[i])
}
str(Japan_price)
# Note: price units are expressed in USD/MT (transformed from JPY/kg)

# Note: the column named "Res1" represents the resilience of a particular stock, with categorical values ranging from "very low" to "high".
table(Japan_catch$Res1)
table(Japan_price$Res1)
# More than 90% of stocks in this dataset are associated with "Medium" resilience
# Note: based on Kanae's code (cf. "1aJPN_parameters_18072.R"), if a stock did not have a resilience value, then she assigned the value "Medium" by default.

# Let's make sure we don't any duplicates or discrepancies in those two databases (quick scan)
table(Japan_catch$Fishery)
catches_dupl_catch <- Japan_catch %>% group_by(Fishery) %>% filter(n() >1)

table(Japan_price$Fishery)
catches_dupl_price <- Japan_price %>% group_by(Fishery) %>% filter(n() >1)
# No duplicates to report
rm(catches_dupl_catch, catches_dupl_price)

unique(Japan_catch$Fishery)
# 102 stocks with catch data
unique(Japan_price$Fishery)
# 97 stocks with price data

# Then, it would be interesting to know which stocks have catch data but no price information
fish_stocks_catch <- subset(Japan_catch, select = "Fishery")
fish_stocks_price <- subset(Japan_price, select = "Fishery")

stocks_not_merged <- anti_join(fish_stocks_catch, fish_stocks_price, by=c("Fishery"))
unique(stocks_not_merged)
# Those stocks comprise:
# (1) Blackhead seabream single_stock
# (2) Indo-Pacific gurnards single_stock
# (3) King crabs single_stock
# (4) Pacific bluefin tuna single_stock_inc_sbf_exc_sml
# (5) Yellow croaker single_stock
# To keep in mind for subsequent analyses 
# Note: all the above stocks do not present catch data for the years available in the price dataset (see Kanae's code "1aJPN_parameters_18072.R" for more details)

unique(Japan_catch$CommName)
# There are 69 common names (which include 102 stocks)

# Similar to what we did with stocks, let's investigate which species' common names are not present in the price dataset (for reference purposes)
CommName_catch <- subset(Japan_catch, select = "CommName")
CommName_price <- subset(Japan_price, select = "CommName")

CommName_not_merged <- anti_join(CommName_catch, CommName_price, by=c("CommName"))
unique(CommName_not_merged)
# (i) Blackhead seabream, (ii) Indo-Pacific gurnards, (iii) King crabs and (iv) Yellow croaker.

# Let's re-format the Japan catch and price data so that we can run the GUM assessment function later on, 
# Using the gather() function to re-format the datasets from a wide to a long format

Japan_catch_long <- Japan_catch %>%
  gather(Year, Catch, '1964':'2015')

Japan_catch_final <- Japan_catch_long %>%
  mutate(X = seq.int(nrow(Japan_catch_long))) %>%
  mutate(IdOrig = paste("Japan", IdOrig, sep = "-"))

str(Japan_catch_final)
Japan_catch_final$Year <- as.integer(Japan_catch_final$Year)

# Let's do the same with the price dataset
Japan_price_long <- Japan_price %>%
  gather(Year, Price, '2003':'2015')

Japan_price_final <- Japan_price_long %>%
  mutate(X = seq.int(nrow(Japan_price_long))) %>%
  mutate(IdOrig = paste("Japan", IdOrig, sep = "-"))

str(Japan_price_final)
Japan_price_final$Year <- as.integer(Japan_price_final$Year)

###################################################
# Let's merge the catch and price datasets (in a long format), so that we can estimate the
# volatility or variability of those two key variables
str(Japan_catch_long)
Japan_catch_long$Year <- as.integer(Japan_catch_long$Year)

str(Japan_price_long)
Japan_price_long$Year <- as.integer(Japan_price_long$Year)

# Let's re-structure both datasets accordingly
Japan_catch_long <- Japan_catch_long[, c(2:9)]
Japan_catch_long <- Japan_catch_long[c(7,1:6,8)]

Japan_price_long <- Japan_price_long[, c(2:9)]
Japan_price_long <- Japan_price_long[c(7,1:6,8)]

unique(Japan_catch_long$Fishery)
unique(Japan_price_long$Fishery)

# Merging
Japan_data_merged <- left_join(Japan_catch_long, Japan_price_long, by=c("Year","Fishery","SciName","CommName","SpeciesCat","SpeciesCatName","Res1"))

#Exporting as a CSVf file
#write.csv(Japan_data_merged, file= "data/Japan_catch_price.csv")

###################################################
# Let's now run the so-called "GUM assessment"
# To do so, we will download the associated function files (Dropbox source)
# We need to download files that have the following pattern (1) .R and (2) .rda
if(User == 1){setwd("~/Dropbox/Upside_next_morioka/GUM_KK/R/")} else if
(User == 2) {setwd("/Users/gakugaku/Dropbox/Drop_REsearch/Upside_next_morioka_original/GUM_KK/R")} else if
(User == 3) {setwd("/Users/keikawamura/Dropbox/Upside_next_morioka/GUM_KK/R")}


file.sources = list.files(pattern="*.R")
sapply(file.sources, source, .GlobalEnv)
file.sources = list.files(pattern="*.rda")
sapply(file.sources,load,.GlobalEnv)

if(User == 1){setwd("~/Dropbox/Upside_next_morioka/GUM_KK/data/")} else if
(User == 2) {setwd("/Users/gakugaku/Dropbox/Drop_REsearch/Upside_next_morioka_original/GUM_KK/data/")}else if
(User == 3) {setwd("/Users/keikawamura/Dropbox/Upside_next_morioka/GUM_KK/data")}

data.sources = list.files(pattern="*.rda")
sapply(data.sources,load,.GlobalEnv)

str(Japan_catch_final)
Japan_catch_final <- Japan_catch_final[c(10,1:9)]

Japan_catch_final$SpeciesCat <- as.numeric(Japan_catch_final$SpeciesCat)
Japan_catch_final$Year <- as.numeric(Japan_catch_final$Year)

# Running the GUM assessment
Japan_results <- run_gum_assessmentKK(Japan_catch_final)

# Note: (1) An output message said that there were too few g-k combinations,
# and that we should check input parameters.
# (2) Also one warning message said: "In min(k1[g1 < 1.1 * parbound$g[1]], na.rm = T) :
# no non-missing arguments to min; returning Inf".
# Such warning messages could be due to problems with the historical Catch-MSY data for one
# particular stocks, inducing NA values for K1 (the stock in question is the "Imperial surf clam")

# Let's check for NA values and see what columns/variables have been affected
apply(Japan_results, 2, function(x) any(is.na(x)))
# Variables that contain NA values:
# "MaxLenght", "AgeMat", "VonBertK", "Temp", "MSY", "phi", "g", "k",
# "MSYlogSd", "gLogSd", "KLogSd", "CatchMSYvBmsy", "CatchMSYvBmsy_LogSd", "FvFmsy"
Japan_result_NAs <- Japan_results %>%
  filter(is.na(MSY) & is.na(phi) & is.na(g) & is.na(k))

# Let's import Kanae's CSV file and compare to see if she any NA values that we do
if(User == 1){setwd("~/Dropbox/Upside_next_morioka/upside_20181227_try/data/")} else if
(User == 2) {setwd("/Users/gakugaku/Dropbox/Drop_REsearch/Upside_next_morioka_original/upside_20181227_try/data/")}else if
(User == 3) {setwd("/Users/keikawamura/Dropbox/Upside_next_morioka/upside_20181227_try/data")}

KT_GUM_results <- read.csv("2japan_GUM_results_K100_0727.csv", stringsAsFactors = F, header = T, sep=",", dec=".")
# Let's run a quick NA check to see if she encountered similar issues
apply(KT_GUM_results, 2, function(x) any(is.na(x)))
KT_GUM_results_NAs <- KT_GUM_results %>%
  filter(is.na(MSY) & is.na(phi) & is.na(g) & is.na(k))

# It seems that Kanae's dataset is quite similar when it comes to missing values. The only notable difference
# concerns the "Japanese anchovy seto_inland_sea_stock", which has NAs for "MSY", "g" and "k" parameters in
# our dataset, but not in Kanae's one.

unique(Japan_results$Fishery) # 102 fisheries
Japan_fishery <- subset(Japan_results, select=c("Fishery", "SciName", "CommName"))
Japan_fishery <- Japan_fishery[!duplicated(Japan_fishery),]
# see "data/0Upside_inputs_prep_180727.csv" in the upside model dropbox

# Let's now add the optimal biomass or biomass that enables a fish stock to deliver the MSY
# i.e. Bmsy
BMSY = function(K, phi)
{
  bmsy = K / (phi + 1) ^ (1/phi)
  return(bmsy)
}

# Applying it to the Japan_results dataset that we obtained from the GUM assessment
Japan_results_all <- Japan_results %>%
  mutate(BMSY = BMSY(k, phi),
         Biomass = BMSY * BvBmsy,
         B_over_K = Biomass / k)

# Exporting a CSV file that contains the GUM results
if(User == 1){setwd("~/Dropbox/Upside_next_morioka/JapanCode/data/")} else if
(User == 2) {setwd("/Users/gakugaku/Dropbox/Drop_REsearch/Upside_next_morioka_original/JapanCode/data/")}else if
(User == 3) {setwd("/Users/keikawamura/Dropbox/Upside_next_morioka/JapanCode/data")}
#write.csv(Japan_results_all, file= "Japan_GUM_results_2019.csv")

###################################################

# Filling in the input/parameter sheet
if(User == 1){setwd("~/Dropbox/Upside_next_morioka/upside_20181225/")} else if
(User == 2) {setwd("/Users/gakugaku/Dropbox/Drop_REsearch/Upside_next_morioka_original/upside_20181225/")}else if
(User == 3) {setwd("/Users/keikawamura/Dropbox/Upside_next_morioka/upside_20181225")}

Japan_inputs <- read.csv("data/3Upside_Input_0530.csv", stringsAsFactors = F, header = T)
Japan_inputs <- Japan_inputs[,c(1, 3:52)]
names(Japan_inputs) # overview of the 51 variables present in this input sheet

# Let's now fill in the input sheet accordingly (considering the Japanese context)
### Time = 50 years projection
### We can change the discount rate as desired (e.g. 0, 5 and 10%), in the case of Japan,
# expert opinion recommends to set the expected discount rate at around 2%.
### Beta corresponds to the scalar cost parameter that determines how non-linear costs are,
# based on Costello et al., its expected value is around 1.3 [it implies that increasing units of effort are increasingly costly to apply]
### Phi (=Pella-Tomlinson shape parameter) has been set with its default value of 0.188 (based on Costello et al.)
### Gamma_p is the price scalar that reflects economic incentives (estimated price increase when reforms are applied), expected value ~ 1.31 [i.e. 31% increase] (based on Costello et al.)
### Gamma_c is the cost scalar that reflects economic incentives (estimated cost decrease when reforms are applied), expected value ~ 0.77 [i.e. 23% decrease] (based on Costello et al.)
### lambda is an open-access adustment parameter and its value is set at 0.1 (based on Costello et al.)
Japan_inputs <- Japan_inputs %>%
  mutate(Time = 50,
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
         disc_lower = 0.02,
         disc_expected = 0.03,
         disc_upper = 0.04,
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

### Theta represents the proportion of harvest by the domestic fleet
# it equals one for species occurring in its EEZ or species that are not caught by other nations.
# We need to change theta_domestic for international fisheries, so that it equals ~ 0.6, according to Japanese experts
int <- read.csv("data/international.csv")
str(int)
int$Fishery <- as.character(int$Fishery)
Japan_inputs <- left_join(Japan_inputs, int, by=c("Fishery"))
Japan_inputs$theta_domestic <- ifelse(Japan_inputs$International == 1, 0.6, Japan_inputs$theta_domestic)


# Let's now add the remaining inputs from GUM results
### Note: 2015 is not the most recent year for all species
Japan_results_max <- Japan_results_all %>%
  group_by(Fishery) %>%
  filter(year==max(year)) %>%
  select(IdOrig, Fishery, SciName, year, BvBmsy, MSY, phi, g, k, FvFmsy) %>%
  rename(Scientific = SciName)

Japan_inputs_1 <- left_join(Japan_inputs, Japan_results_max, by=c("Fishery", "Scientific"))
table(Japan_inputs_1$year) # 16 species have their max year < 2015

Japan_inputs_2 <- Japan_inputs_1 %>%
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
         f0_total_upper = FvFmsy) %>%
  select(Fishery:split, MSY)

# We will now need to add our price data (cf. Japan_price_final)
str(Japan_price_final)
# Let's check for NA values and see what columns/variables have been affected
apply(Japan_price_final, 2, function(x) any(is.na(x)))

# Using ddply (split data frame, apply function, and return results in a data frame)
price_data <- ddply(Japan_price_final, .(Fishery), summarise,
                    p1_lower = min(Price, na.rm = TRUE),
                    p1_expected = mean(Price, na.rm = TRUE),
                    p1_upper = max(Price, na.rm = TRUE),
                    p2_lower = min(Price, na.rm = TRUE),
                    p2_expected = mean(Price, na.rm = TRUE),
                    p2_upper = max(Price, na.rm = TRUE))

# Let's now incorporate those price infos in our input data set
Japan_inputs_3 <- Japan_inputs_2 %>% 
  mutate(p1_lower = price_data$p1_lower[match(Fishery ,price_data$Fishery)],
         p1_expected = price_data$p1_expected[match(Fishery,price_data$Fishery)],
         p1_upper = price_data$p1_upper[match(Fishery,price_data$Fishery)],
         p2_lower = price_data$p2_lower[match(Fishery,price_data$Fishery)],
         p2_expected = price_data$p2_expected[match(Fishery,price_data$Fishery)],
         p2_upper = price_data$p2_upper[match(Fishery,price_data$Fishery)]
  )

# 5 species do not have price values
species_NA <- Japan_inputs_3 %>%
  filter(is.na(p1_lower))

# We can fill price information for the "Blackhead seabream single_stock" & "Pacific bluefin tuna single_stock_inc_sbf_exc_sml"
# Indeed we can assume that they are exhibiting a similar ex-vessel price than related or neighbouring stocks whose price data is available

# 1. Pacific bluefin tuna single_stock_inc_sbf_exc_sml (we will apply the price data of the stock "Pacific bluefin tuna single_stock_exc_sbf_inc_sml")
Japan_inputs_3_bft <- subset(Japan_inputs_3, Japan_inputs_3$Fishery == "Pacific bluefin tuna single_stock_exc_sbf_inc_sml")
Japan_inputs_3$p1_lower <- ifelse(Japan_inputs_3$Fishery == "Pacific bluefin tuna single_stock_inc_sbf_exc_sml", Japan_inputs_3_bft$p1_lower, Japan_inputs_3$p1_lower)
Japan_inputs_3$p1_expected <- ifelse(Japan_inputs_3$Fishery == "Pacific bluefin tuna single_stock_inc_sbf_exc_sml", Japan_inputs_3_bft$p1_expected, Japan_inputs_3$p1_expected)
Japan_inputs_3$p1_upper <- ifelse(Japan_inputs_3$Fishery == "Pacific bluefin tuna single_stock_inc_sbf_exc_sml",  Japan_inputs_3_bft$p1_upper, Japan_inputs_3$p1_upper)
Japan_inputs_3$p2_lower <- ifelse(Japan_inputs_3$Fishery == "Pacific bluefin tuna single_stock_inc_sbf_exc_sml", Japan_inputs_3_bft$p1_lower, Japan_inputs_3$p2_lower)
Japan_inputs_3$p2_expected <- ifelse(Japan_inputs_3$Fishery == "Pacific bluefin tuna single_stock_inc_sbf_exc_sml", Japan_inputs_3_bft$p1_expected, Japan_inputs_3$p2_expected)
Japan_inputs_3$p2_upper <- ifelse(Japan_inputs_3$Fishery == "Pacific bluefin tuna single_stock_inc_sbf_exc_sml", Japan_inputs_3_bft$p1_upper, Japan_inputs_3$p2_upper)

# 2. Blackhead seabream single_stock (we will apply the price data of the stock "Blackhead seabream, Goldlined seabream single_stock")
Japan_inputs_3_bsb <- subset(Japan_inputs_3, Japan_inputs_3$Fishery == "Blackhead seabream, Goldlined seabream single_stock")
Japan_inputs_3$p1_lower <- ifelse(Japan_inputs_3$Fishery == "Blackhead seabream single_stock", Japan_inputs_3_bsb$p1_lower, Japan_inputs_3$p1_lower)
Japan_inputs_3$p1_expected <- ifelse(Japan_inputs_3$Fishery == "Blackhead seabream single_stock", Japan_inputs_3_bsb$p1_expected, Japan_inputs_3$p1_expected)
Japan_inputs_3$p1_upper <- ifelse(Japan_inputs_3$Fishery == "Blackhead seabream single_stock", Japan_inputs_3_bsb$p1_upper, Japan_inputs_3$p1_upper)
Japan_inputs_3$p2_lower <- ifelse(Japan_inputs_3$Fishery == "Blackhead seabream single_stock", Japan_inputs_3_bsb$p2_lower, Japan_inputs_3$p2_lower)
Japan_inputs_3$p2_expected <- ifelse(Japan_inputs_3$Fishery == "Blackhead seabream single_stock", Japan_inputs_3_bsb$p2_expected, Japan_inputs_3$p2_expected)
Japan_inputs_3$p2_upper <- ifelse(Japan_inputs_3$Fishery == "Blackhead seabream single_stock", Japan_inputs_3_bsb$p2_upper, Japan_inputs_3$p2_upper)


# Finally, let's estimate the cost parameters

# Cost of fishing parameter calculation
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

# Now we can add the cost parameter to the input data frame
Japan_inputs_4 <- Japan_inputs_3 %>%
  mutate(c1_lower = cost(p_exp = p1_lower, phi = phi_lower, bbar = bbar, MSY = MSY, g_exp = g_lower, beta = beta_lower),
         c1_expected = cost(p_exp = p1_expected, phi = phi_expected, bbar = bbar, MSY = MSY, g_exp = g_expected, beta = beta_expected),
         c1_upper = cost(p_exp = p1_upper, phi = phi_upper, bbar = bbar, MSY = MSY, g_exp = g_upper, beta = beta_upper),
         c2_lower = cost(p_exp = p2_lower, phi = phi_lower, bbar = bbar, MSY = MSY, g_exp = g_lower, beta = beta_lower),
         c2_expected = cost(p_exp = p2_expected, phi = phi_expected, bbar = bbar, MSY = MSY, g_exp = g_expected, beta = beta_expected),
         c2_upper = cost(p_exp = p2_upper, phi = phi_upper, bbar = bbar, MSY = MSY, g_exp = g_upper, beta = beta_upper))

Japan_inputs_4 <- Japan_inputs_4 %>%
  arrange(Fishery)

# Some info is missing for 5 species -> Drop those.
table(complete.cases(Japan_inputs_4) == FALSE)

Japan_inputs_4 <- subset(Japan_inputs_4, complete.cases(Japan_inputs_4) == TRUE)

###################################################

# We can now run the Upside model simulations, considering the 3 different management scenarios of interest
# i.e. (1) Business-as-usual (BAU), (2) Fmsy and (3) Economic Optimal

# First let's import the outcome of the volatility analysis we performed on Japan's catch and price data.
# The data table contains the coefficients of variation (CV) pertaining to the catch of each fishery, which we want to employ
# as the SD of the normally distributed variable we will use to model process uncertainty associated with the population dynamics
if(User == 1){setwd("~/Dropbox/Upside_Portfolio_Phase1")} else if
(User == 2) {setwd("/Users/gakugaku/Dropbox/Drop_REsearch/#_0_Research_UPM_Portfolio/Upside_Portfolio_Phase1/")} else if
(User == 3) {setwd("/Users/keikawamura/Upside/Upside_Portfolio_Phase1_Kei1April19")}

CV_catch_price <- read.csv("Data/CV_catch_price.csv", stringsAsFactors = F, header = T)

# Let's just keep the variables "Fishery", "CV_catch" and "CV_catch_Log"
CV_catch <- CV_catch_price[,c(2,6,9)]

# Let's merge "CV_catch_price" with "Japan_inputs_4"
Japan_inputs_4 <- left_join(Japan_inputs_4, CV_catch, by=c("Fishery"))
Japan_inputs_4 <- Japan_inputs_4[,-c(52)]

if(User == 1){setwd("~/Dropbox/Upside_next_morioka/upside_20181225/")} else if
(User == 2) {setwd("/Users/gakugaku/Dropbox/Drop_REsearch/Upside_next_morioka_original/upside_20181225/")}else if
(User ==3) {setwd("/Users/keikawamura/Dropbox/Upside_next_morioka/upside_20181225/")}

# Read in functions file
source("2FunctionsJapan_update.R") # choose if running deterministic model
#source("2FunctionsJapan_update_stochastic.R") # choose if running stochastic model (i.e. with simulations)

## Set up model run
# Enter management scenarios to loop over - select 1 through 5
scenarios = c(1,2,3,4)#,5,6) # run EconOpt (#4) with SQ and FMSY (will remove #3 later as we do not need it)
#scenarios = c(1,2) # run only BAU and Fmsy

# Enter whether or not to loop over catch share cost and price scalars - put "yes" or "no"
catchShareLoop = "no"
# Enter whether or not to loop over eliminating illegal fishing - put "yes" or "no"
illegalLoop = "no"

## Implementation vector
delayVec <- c(2)

## Loop over all fisheries
for (i in 1:nrow(Japan_inputs_4))
{
  outputs = projectionModel(Japan_inputs_4[i,],scenarios,catchShareLoop,illegalLoop)
  
  masterOutputi = cbind(rep(Japan_inputs_4[i,]$Fishery,nrow(melt(outputs$BProjections))),
                        rep(Japan_inputs_4[i,]$Species,nrow(melt(outputs$BProjections))),
                        melt(outputs$BProjections),
                        melt(outputs$HInt1Projections)$value,
                        melt(outputs$HInt2Projections)$value,
                        melt(outputs$HNonIntProjections)$value,
                        melt(outputs$profit1Projections)$value,
                        melt(outputs$profit2Projections)$value,
                        melt(outputs$bProjections)$value,
                        melt(outputs$fInt1Projections)$value,
                        melt(outputs$fInt2Projections)$value,
                        melt(outputs$fNonIntProjections)$value,
                        melt(outputs$fTotalProjections)$value)
  
  colnames(masterOutputi) = c("fishery","species","management","MC", "catchShare","illegalFishing","implementYear", "time","biomass",
                              "harvest1", "harvest2", "harvest_ill_for","profit1","profit2", "BvBMSY","FvFMSY1",
                              "FvFMSY2", "FvFMSYill", "FvFMSYtotal")
  
  if (i == 1) {
    masterOutput = masterOutputi
  } else {
    masterOutput = rbind(masterOutput,masterOutputi)
  }
  
  masterOutput$management[masterOutput$management == 1] = "SQ"
  masterOutput$management[masterOutput$management == 2] = "FMSY"
  masterOutput$management[masterOutput$management == 3] = "minRec"
  masterOutput$management[masterOutput$management == 4] = "econOpt"
  #masterOutput$management[masterOutput$management == 5] = "close"
  #masterOutput$management[masterOutput$management == 6] = "openA"
  masterOutput$catchShare[masterOutput$catchShare == 1] = "no_CS"
  masterOutput$catchShare[masterOutput$catchShare == 2] = "CS"
  masterOutput$illegalFishing[masterOutput$illegalFishing == 1] = "illegal_fishing"
  masterOutput$illegalFishing[masterOutput$illegalFishing == 2] = "no_illegal_fishing"
  
  recoveryOutputi = cbind(rep(Japan_inputs_4[i,]$Fishery,nrow(melt(outputs$timeToRecovery))),
                          rep(Japan_inputs_4[i,]$Species,nrow(melt(outputs$timeToRecovery))),
                          melt(outputs$timeToRecovery))
  
  colnames(recoveryOutputi) = c("fishery","species","management", "MC", "catchShare","illegalFishing","implementYear", "recTime")
  
  if (i == 1) {
    recoveryOutput = recoveryOutputi
  } else {
    recoveryOutput = rbind(recoveryOutput,recoveryOutputi)
  }
  
  recoveryOutput$management[recoveryOutput$management == 1] = "SQ"
  recoveryOutput$management[recoveryOutput$management == 2] = "FMSY"
  recoveryOutput$management[recoveryOutput$management == 3] = "MinRec"
  recoveryOutput$management[recoveryOutput$management == 4] = "econOpt"
  #recoveryOutput$management[recoveryOutput$management == 5] = "close"
  #recoveryOutput$management[recoveryOutput$management == 6] = "openA"
  recoveryOutput$catchShare[recoveryOutput$catchShare == 1] = "no_CS"
  recoveryOutput$catchShare[recoveryOutput$catchShare == 2] = "CS"
  recoveryOutput$illegalFishing[recoveryOutput$illegalFishing == 1] = "illegal_fishing"
  recoveryOutput$illegalFishing[recoveryOutput$illegalFishing == 2] = "no_illegal_fishing"
  
  npvOutputi = cbind(rep(Japan_inputs_4[i,]$Fishery,nrow(melt(outputs$npv1))),
                     rep(Japan_inputs_4[i,]$Species,nrow(melt(outputs$npv1))),
                     melt(outputs$npv1),
                     melt(outputs$npv2)$value)
  
  colnames(npvOutputi) = c("fishery", "species","management", "MC", "catchShare","illegalFishing","implementYear", "npv1", "npv2")
  
  if (i == 1) {
    npvOutput = npvOutputi
  } else {
    npvOutput = rbind(npvOutput,npvOutputi)
  }
  
  npvOutput$management[npvOutput$management == 1] = "SQ"
  npvOutput$management[npvOutput$management == 2] = "FMSY"
  npvOutput$management[npvOutput$management == 3] = "MinRec"
  npvOutput$management[npvOutput$management == 4] = "econOpt"
  #npvOutput$management[npvOutput$management == 5] = "close"
  #npvOutput$management[npvOutput$management == 6] = "openA"
  npvOutput$catchShare[npvOutput$catchShare == 1] = "no_CS"
  npvOutput$catchShare[npvOutput$catchShare == 2] = "CS"
  npvOutput$illegalFishing[npvOutput$illegalFishing == 1] = "illegal_fishing"
  npvOutput$illegalFishing[npvOutput$illegalFishing == 2] = "no_illegal_fishing"
  
}


masterOutput$implementYear = (masterOutput$implementYear + 1)
#write.csv(masterOutput,file="output/Japan_master_output_2019_opt.csv")

recoveryOutput$implementYear = (recoveryOutput$implementYear + 1)
#write.csv(recoveryOutput,file="output/Japan_recovery_output_2019_opt.csv")

npvOutput$implementYear = (npvOutput$implementYear + 1)
#write.csv(npvOutput,file="output/Japan_NPV_output_2019_opt.csv")

## Removing the minRec scenario (#3), so that we only have SQ, Fmsy and EconOpt
  masterOutput <- masterOutput %>%
    filter(management != "minRec")

  recoveryOutput <- recoveryOutput %>%
    filter(management != "minRec")

  npvOutput <- npvOutput %>%
    filter(management != "minRec")

if(User == 1){setwd("~/Dropbox/Upside_next_morioka/upside_20181225/output")} else if
(User == 2) {setwd("/Users/gakugaku/Dropbox/Drop_REsearch/Upside_next_morioka_original/upside_20181225/output")}else if
(User ==3) {setwd("~/Upside/Surplus_Production_Japan_KK/output")}
  
  disc_rate <- Japan_inputs_4[1,"disc_expected"]
  file_name <- paste("disc_rate",disc_rate,"Japan_master_output_2019_opt.csv",sep = "_")
  write_csv(masterOutput,file_name)
## opt at the end of csv names indicates results from model with policy that is optimized once at beginning
  toc()
 
  
  
  
  
  
  
  
  
  
  
  
  
  
   
###################################################
### Simulations
## Set up model run
# Enter management scenarios to loop over - select 1 through 5
scenarios = c(1,2,3,4) # run EconOpt (#4) with SQ and FMSY (will remove #3 later, as do not need it)
#scenarios = c(1,2) # run only BAU and Fmsy

# Enter whether or not to loop over catch share cost and price scalars - put "yes" or "no"
catchShareLoop = "no"
# Enter whether or not to loop over eliminating illegal fishing - put "yes" or "no"
illegalLoop = "no"

## Implementation vector
delayVec <- c(2)

## We want to run a dynamic version of the Upside model of Japan
# We included a process error in the dynamic part of the biological model (which is lognormally distributed),
# now we want to create a loop that will run the projection m times, and save each run
# as a dataframe. The m created dataframes will be then merged together in order to estimate the final variables of interest
# i.e. biomass, harvest and profit.

#loop over the m number of simulations wanted
m = c(1:1000)
Master_output <- list()
Recovery_output <- list()
NPV_output <- list()

for (j in 1:length(m)){
  Master_output[[j]] = masterOutput
  Recovery_output[[j]] = recoveryOutput
  NPV_output[[j]] = npvOutput
  
  ## Loop over all fisheries
  for (i in 1:nrow(Japan_inputs_4))
  {
    outputs = projectionModel(Japan_inputs_4[i,],scenarios,catchShareLoop,illegalLoop)
    
    masterOutputi = cbind(rep(Japan_inputs_4[i,]$Fishery,nrow(melt(outputs$BProjections))),
                          rep(Japan_inputs_4[i,]$Species,nrow(melt(outputs$BProjections))),
                          melt(outputs$BProjections),
                          melt(outputs$HInt1Projections)$value,
                          melt(outputs$HInt2Projections)$value,
                          melt(outputs$HNonIntProjections)$value,
                          melt(outputs$profit1Projections)$value,
                          melt(outputs$profit2Projections)$value,
                          melt(outputs$bProjections)$value,
                          melt(outputs$fInt1Projections)$value,
                          melt(outputs$fInt2Projections)$value,
                          melt(outputs$fNonIntProjections)$value,
                          melt(outputs$fTotalProjections)$value)
    
    colnames(masterOutputi) = c("fishery","species","management","MC", "catchShare","illegalFishing","implementYear", "time","biomass",
                                "harvest1", "harvest2", "harvest_ill_for","profit1","profit2", "BvBMSY","FvFMSY1",
                                "FvFMSY2", "FvFMSYill", "FvFMSYtotal")
    
    if (i == 1) {
      masterOutput = masterOutputi
    } else {
      masterOutput = rbind(masterOutput,masterOutputi)
    }
    
    masterOutput$management[masterOutput$management == 1] = "SQ"
    masterOutput$management[masterOutput$management == 2] = "FMSY"
    masterOutput$management[masterOutput$management == 3] = "minRec"
    masterOutput$management[masterOutput$management == 4] = "econOpt"
    #masterOutput$management[masterOutput$management == 5] = "close"
    #masterOutput$management[masterOutput$management == 6] = "openA"
    masterOutput$catchShare[masterOutput$catchShare == 1] = "no_CS"
    masterOutput$catchShare[masterOutput$catchShare == 2] = "CS"
    masterOutput$illegalFishing[masterOutput$illegalFishing == 1] = "illegal_fishing"
    masterOutput$illegalFishing[masterOutput$illegalFishing == 2] = "no_illegal_fishing"
    
    recoveryOutputi = cbind(rep(Japan_inputs_4[i,]$Fishery,nrow(melt(outputs$timeToRecovery))),
                            rep(Japan_inputs_4[i,]$Species,nrow(melt(outputs$timeToRecovery))),
                            melt(outputs$timeToRecovery))
    
    colnames(recoveryOutputi) = c("fishery","species","management", "MC", "catchShare","illegalFishing","implementYear", "recTime")
    
    if (i == 1) {
      recoveryOutput = recoveryOutputi
    } else {
      recoveryOutput = rbind(recoveryOutput,recoveryOutputi)
    }
    
    recoveryOutput$management[recoveryOutput$management == 1] = "SQ"
    recoveryOutput$management[recoveryOutput$management == 2] = "FMSY"
    recoveryOutput$management[recoveryOutput$management == 3] = "minRec"
    recoveryOutput$management[recoveryOutput$management == 4] = "econOpt"
    #recoveryOutput$management[recoveryOutput$management == 5] = "close"
    #recoveryOutput$management[recoveryOutput$management == 6] = "openA"
    recoveryOutput$catchShare[recoveryOutput$catchShare == 1] = "no_CS"
    recoveryOutput$catchShare[recoveryOutput$catchShare == 2] = "CS"
    recoveryOutput$illegalFishing[recoveryOutput$illegalFishing == 1] = "illegal_fishing"
    recoveryOutput$illegalFishing[recoveryOutput$illegalFishing == 2] = "no_illegal_fishing"
    
    npvOutputi = cbind(rep(Japan_inputs_4[i,]$Fishery,nrow(melt(outputs$npv1))),
                       rep(Japan_inputs_4[i,]$Species,nrow(melt(outputs$npv1))),
                       melt(outputs$npv1),
                       melt(outputs$npv2)$value)
    
    colnames(npvOutputi) = c("fishery", "species","management", "MC", "catchShare","illegalFishing","implementYear", "npv1", "npv2")
    
    if (i == 1) {
      npvOutput = npvOutputi
    } else {
      npvOutput = rbind(npvOutput,npvOutputi)
    }
    
    npvOutput$management[npvOutput$management == 1] = "SQ"
    npvOutput$management[npvOutput$management == 2] = "FMSY"
    npvOutput$management[npvOutput$management == 3] = "minRec"
    npvOutput$management[npvOutput$management == 4] = "econOpt"
    #npvOutput$management[npvOutput$management == 5] = "close"
    #npvOutput$management[npvOutput$management == 6] = "openA"
    npvOutput$catchShare[npvOutput$catchShare == 1] = "no_CS"
    npvOutput$catchShare[npvOutput$catchShare == 2] = "CS"
    npvOutput$illegalFishing[npvOutput$illegalFishing == 1] = "illegal_fishing"
    npvOutput$illegalFishing[npvOutput$illegalFishing == 2] = "no_illegal_fishing"
    
  }
  
  
  masterOutput$implementYear = (masterOutput$implementYear + 1)
  #write.csv(masterOutput,file="output/Japan_master_output_2019_opt.csv")
  
  recoveryOutput$implementYear = (recoveryOutput$implementYear + 1)
  #write.csv(recoveryOutput,file="output/Japan_recovery_output_2019_opt.csv")
  
  npvOutput$implementYear = (npvOutput$implementYear + 1)
  #write.csv(npvOutput,file="output/Japan_NPV_output_2019_opt.csv")
  
  ## Removing the minRec scenario (#3), so that we only have SQ, Fmsy and EconOpt
  masterOutput <- masterOutput %>%
    filter(management != "minRec")
  
  recoveryOutput <- recoveryOutput %>%
    filter(management != "minRec")
  
  npvOutput <- npvOutput %>%
    filter(management != "minRec")
  
}

## Saving the 3 list of dataframes (i.e. "Master_output", "Recovery_output" and "NPV_output")
# in RData format
if(User == 1){setwd("~/Dropbox/Upside_Portfolio_Phase1/Data/")} else if
(User == 2) {setwd("/Users/gakugaku/Dropbox/Drop_REsearch/#_0_Research_UPM_Portfolio/Upside_Portfolio_Phase1/Data")}

save(Master_output, Recovery_output, NPV_output, file="Simulations/1000run_BAU_Fmsy.RData")

###################################################
## Let's now open the .RData file that contains the simulations
load("Simulations/1000run_BAU_Fmsy.RData")

# We need to sort/arrange the nested m dataframes in each of the 3 lists we obtained (i.e. "Master_output", "Recovery_output" and "NPV_output")
# and apply an index that we can refer to for future analyses

for(i in 1:length(m)) {
  Master_output[[i]] <- Master_output[[i]][,-c(5:6)]  # removing the columns/variables "catchShare" and "illegalFishing"
}
str(Master_output[[1]]) # species and fishery variables are considered as factors here

# Let's convert them to characters
for(i in 1:length(m)) {
  Master_output[[i]]$fishery <- as.character(Master_output[[i]]$fishery)
  Master_output[[i]]$species <- as.character(Master_output[[i]]$species)
}

# Let's try to convert each list of dataframes into one single large one
Master_output_df <- bind_rows(Master_output, .id = "df_ID") # the .id creates a unique identifier for each dataframe

# Changing the values of the time variable (from 1 to 50 sequences to actual predited years -> 2016 to 2065)
n = c(1:50)
for(i in 1:length(n)){
  
  Master_output_df$time <- ifelse(Master_output_df$time == n[i], 2015 + n[i], Master_output_df$time)
}

###################################################
## Let's estimate the inverse demand curve
# We do have price and catch/harvest data
# Furthermore, based on Delgado et al. (2003), we will use the same constant elasticity
# of demand than Costello et al. (2016), i.e. e ~ -1.15

# We will use and edit the Japan_data_merged dataset (which contains both the price and catch data)

# Knowing that price data are only available from 2003 to 2015, we will trim the dataset accordingly
Japan_price_catch <- Japan_data_merged %>%
  filter(Year >= 2003)

# Adding the constant elasticity of demand e = -1.15
Japan_price_catch <- Japan_price_catch %>%
  mutate(e = - 1.15)

Japan_price_catch <- Japan_price_catch[,-c(7)] # removing Resilience

# Based on the formula q(i,t) = alpha(i) * (p(i,t))^e
# where i represents each fishery and t is time
# we will be able to estimate the fishery specific parameter alpha
# and then assess the inverse demand curve

# To compute such parameter, we nee to select a year t for standardization purposes
# Which year is the most recent one for all species in our Japan's dataset
# Checking Japan_results_max

alpha <- Japan_price_catch %>%
  group_by(Fishery) %>% 
  filter(Year == max(Year))

# checking for duplicates
alpha_dupl <- alpha %>%
  group_by(Fishery) %>%
  filter(n() > 1) # no duplicates

table(alpha$Year)  # all the current fisheries ahve their latest data points in 2015
table(alpha$Fishery)

# But we want to make sure that each fishery presents both price and catch data in 2015
# Let's filter out fisheries that have NA values for either or both variables of interest
alpha <- alpha %>%
  group_by(Fishery) %>%
  filter(!is.na(Catch|Price))
# As a consequence, our alpha dataset now contains 86 fisheries/stocks

# We can now compute the alpha parameter, such that alpha = (Catch(i,t) / Price(i,t)^e)
# where i represents each fishery and t = 2015 in this case
Inverse_demand <- alpha %>%
  group_by(Fishery) %>%
  mutate(alpha = (Catch / (Price)^e))

Inverse_demand_2015 <- Inverse_demand
Inverse_demand_2015 <- Inverse_demand_2015[,-c(1)]  # removing the variable Year = 2015

# Let's use the outcome of our projections (notably future harvest values)
# and use our fishery specific parameter alpha to predict future prices
masterOutput_demand <- masterOutput[,c-(5:6)] # removing the catchShare and illegalFishing variables
masterOutput_demand <- masterOutput_demand[,-c(9:10,12, 15:16)] # trimming it further

# Let's merge the masterOutput_demand dataset with our inverse_demand_alpha one
# so that we can include our fishery specific parameter and estimate future inverse demand curves
inverse_demand_alpha <- Inverse_demand_2015 %>%
  select(Fishery, SciName, CommName,SpeciesCat, SpeciesCatName,e, alpha)

masterOutput_demand <- masterOutput_demand %>%
  select(fishery, species, management, MC, implementYear, time, harvest1) %>%
  rename(Fishery = fishery, CommName = species)

str(inverse_demand_alpha)
str(masterOutput_demand)
masterOutput_demand$Fishery <- as.character(masterOutput_demand$Fishery)
masterOutput_demand$CommName <- as.character(masterOutput_demand$CommName)

future_inverse_demand <- full_join(inverse_demand_alpha, masterOutput_demand, by=c("Fishery", "CommName"))

# removing species with NAs
future_inverse_demand <- future_inverse_demand %>%
  filter(!is.na(alpha))

apply(future_inverse_demand, 2, function(x) any(is.na(x))) # same NA(s) identified

NA_values <- future_inverse_demand %>%
  filter(is.na(management))
# appears that one row (species "Queen crab Pacific") has an alpha value but no
# outcome from the projections
future_inverse_demand <- future_inverse_demand %>%
  filter(!is.na(management))
apply(future_inverse_demand, 2, function(x) any(is.na(x))) # No more NAs

# Let's now multiply our fishery specific parameter by the harvest values from 2015 onwards
# we will thus get price values from 2015 onwards
future_inverse_demand <- future_inverse_demand %>%
  rowwise() %>%
  mutate(price1 = ((1 / alpha)^(1/e)) * ((harvest1)^(1/e)))

future_inverse_demand <- future_inverse_demand %>%
  rename(Catch = harvest1, Price = price1)

# let's change the implementYear and time variables' values into actual years
future_inverse_demand$implementYear <- ifelse(future_inverse_demand$implementYear == 2, 2017, future_inverse_demand$implementYear)
n = c(1:50)
for(i in 1:length(n)){
  
  future_inverse_demand$time <- ifelse(future_inverse_demand$time == n[i], 2015 + n[i], future_inverse_demand$time)
}
future_inverse_demand <- rename(future_inverse_demand, Year = time)

#write.csv(future_inverse_demand,file="output/future_inverse_demand.csv")


###################################################
##Plotting!!

SnamesAll = c("BAU","Fmsy","Econ Optimal")
legendLabels = SnamesAll[scenarios]
legendName = "Management Scenario"
# legendName1 = "Fishery"
# legendName2 = "Fishery (Coastal sp.)"
# legendName3 = "Fishery (Other sp.)"

###KK: problem here was that the variable is "fishery" not species that you want to aggregate on:
masterP<-data.frame(masterOutput)
#Fleet 1 = state (gets SQ for new BAU scenario):
head(masterP)
masterPLongH1<- dcast(masterP, fishery+species+catchShare+illegalFishing+time~management,
                      value.var = "harvest1")
masterPLongH1$fleet<-"State"
names(masterPLongH1)<-c("fishery", "species", "catchShare","illegalFishing","time","close1","econOpt1","FMSY1","minRec1","openA1","SQ1", "fleet")
#Fleet 2 = private (gets OA for new BAU scenario):
masterPLongH2<- dcast(masterP, fishery+species+catchShare+illegalFishing+time~management,
                      value.var = "harvest2")
masterPLongH2$fleet<-"Private"
names(masterPLongH2)<-c("fishery", "species","catchShare","illegalFishing","time","close2","econOpt2","FMSY2","minRec2","openA2","SQ2", "fleet")
#Fleet 3 = illegal (gets OA for new BAU scenario):
masterPLongHill<- dcast(masterP, fishery+species+catchShare+illegalFishing+time~management,
                        value.var = "harvest_ill_for")
masterPLongHill$fleet<-"Illegal"
names(masterPLongHill)<-c("fishery", "species","catchShare","illegalFishing","time","close3","econOpt3","FMSY3","minRec3","openA3","SQ3", "fleet")

masterPLong<-merge(masterPLongH1, masterPLongH2, by=c("fishery", "species","catchShare","illegalFishing","time"))
masterPLong<-merge(masterPLong, masterPLongHill, by=c("fishery", "species","catchShare","illegalFishing","time"))
masterPLong$BAU<-masterPLong$SQ1+masterPLong$SQ2+masterPLong$SQ3
masterPLong$FMSY<-masterPLong$FMSY1+masterPLong$FMSY2+masterPLong$FMSY3
masterPLong$econOpt<-masterPLong$econOpt1+masterPLong$econOpt2+masterPLong$econOpt3
names(masterPLong)
masterPLong<-masterPLong[,c(-6:-25)]
masterPWide_H<-melt(masterPLong, measure.vars = c("BAU", "FMSY", "econOpt"),
                    variable.name="management", value.name = "harvest")


mPH1<-masterPLongH1[,c(1:5, 7:8, 11:12)]
names(mPH1)<-c("fishery", "species","catchShare","illegalFishing","time","econOpt","FMSY","BAU", "fleet")
mPH2<-masterPLongH2[,c(1:5, 7:8, 11:12)]
names(mPH2)<-c("fishery", "species","catchShare","illegalFishing","time","econOpt","FMSY","BAU", "fleet")
mPH3<-masterPLongHill[,c(1:5, 7:8, 11:12)]
names(mPH3)<-c("fishery", "species","catchShare","illegalFishing","time","econOpt","FMSY","BAU", "fleet")
mPHx<-rbind(mPH1, mPH2, mPH3)

mPHxx<-melt(mPHx, measure.vars = c("BAU", "FMSY", "econOpt"),
            variable.name="management", value.name = "harvest")
mPHxx$Situation<-paste(mPHxx$catchShare,"_", mPHxx$illegalFishing, sep="")
mPHxx$Situation2<-ifelse(mPHxx$Situation=="no_CS_illegal_fishing", "Worst", ifelse(mPHxx$Situation=="CS_no_illegal_fishing", "Best", "Blah"))
mPHxx<-mPHxx[which(mPHxx$Situation2!="Blah"),]
mPHxx$ill<-ifelse(mPHxx$illegalFishing=="no_illegal_fishing", "No illegal", "Illegal")
mPHxx$CS<-ifelse(mPHxx$catchShare=="no_CS", "No CS", "CS")

#Fleet 1 = state:
masterPLongP1<- dcast(masterP, fishery+species+catchShare+illegalFishing+time~management,
                      value.var = "profit1")
masterPLongP1$fleet<-"State"
names(masterPLongP1)<-c("fishery", "species","catchShare","illegalFishing","time","close1","econOpt1","FMSY1","minRec1","openA1","SQ1", "fleet")
#Fleet 2 = private (gets OA for new BAU scenario):
masterPLongP2<- dcast(masterP, fishery+species+catchShare+illegalFishing+time~management,
                      value.var = "profit2")
masterPLongP2$fleet<-"Private"
names(masterPLongP2)<-c("fishery", "species","catchShare","illegalFishing","time","close2","econOpt2","FMSY2","minRec2","openA2","SQ2", "fleet")

masterPLong_P<-merge(masterPLongP1, masterPLongP2, by=c("fishery", "species","catchShare","illegalFishing","time"))
masterPLong_P$BAU<-masterPLong_P$SQ1+masterPLong_P$SQ2
masterPLong_P$FMSY<-masterPLong_P$FMSY1+masterPLong_P$FMSY2
masterPLong_P$econOpt<-masterPLong_P$econOpt1+masterPLong_P$econOpt2
names(masterPLong_P)
masterPLong_P<-masterPLong_P[,c(-6:-19)]
masterPWide_P<-melt(masterPLong_P, measure.vars = c("BAU", "FMSY", "econOpt"),
                    variable.name="management", value.name = "profit")
names(masterPLongP1)
mPP1<-masterPLongP1[,c(1:5, 7:8, 11:12)]
names(mPP1)<-c("fishery", "species","catchShare","illegalFishing","time","econOpt","FMSY","BAU", "fleet")
mPP2<-masterPLongP2[,c(1:5, 7:8, 11:12)]
names(mPP2)<-c("fishery", "species","catchShare","illegalFishing","time","econOpt","FMSY","BAU", "fleet")
mPPx<-rbind(mPP1, mPP2)
mPPxx<-melt(mPPx, measure.vars = c("BAU", "FMSY", "econOpt"),
            variable.name="management", value.name = "profit")
mPPxx$Situation<-paste(mPPxx$catchShare,"_", mPPxx$illegalFishing, sep="")
mPPxx$Situation2<-ifelse(mPPxx$Situation=="no_CS_illegal_fishing", "Worst", ifelse(mPPxx$Situation=="CS_no_illegal_fishing", "Best", "Blah"))
mPPxx<-mPPxx[which(mPPxx$Situation2!="Blah"),]
mPPxx$ill<-ifelse(mPPxx$illegalFishing=="no_illegal_fishing", "No illegal", "Illegal")
mPPxx$CS<-ifelse(mPPxx$catchShare=="no_CS", "No CS", "CS")


masterPsub<-masterP[masterP$management %in% c("SQ", "FMSY", "econOpt"),]
masterPsub$management<-ifelse(masterPsub$management=="SQ", "BAU", masterPsub$management)
masterPsub<-merge(masterPsub, masterPWide_H, by=c("fishery", "species","catchShare","illegalFishing","time", "management"))
masterPsub<-merge(masterPsub, masterPWide_P, by=c("fishery", "species","catchShare","illegalFishing","time", "management"))

masterPsub$Situation<-paste(masterPsub$catchShare,"_", masterPsub$illegalFishing, sep="")
masterPsub$Situation2<-ifelse(masterPsub$Situation=="no_CS_illegal_fishing", "Worst", ifelse(masterPsub$Situation=="CS_no_illegal_fishing", "Best", "Blah"))
masterPsub<-masterPsub[which(masterPsub$Situation2!="Blah"),]
masterPsub$ill<-ifelse(masterPsub$illegalFishing=="no_illegal_fishing", "No illegal", "Illegal")
masterPsub$CS<-ifelse(masterPsub$catchShare=="no_CS", "No CS", "CS")

npvP<-data.frame(npvOutput)
#Fleet 1 = state (gets SQ for new BAU scenario):
npvP1<- dcast(npvP, fishery+species+catchShare+illegalFishing~management,
              value.var = "npv1")
names(npvP1)<-c("fishery", "species","catchShare","illegalFishing","close1","econOpt1","FMSY1","minRec1","openA1","SQ1")
#Fleet 2 = private (gets OA for new BAU scenario):
npvP2<- dcast(npvP, fishery+species+catchShare+illegalFishing~management,
              value.var = "npv2")
names(npvP2)<-c("fishery", "species","catchShare","illegalFishing","close2","econOpt2","FMSY2","minRec2","openA2","SQ2")

npvP3<-merge(npvP1, npvP2, by=c("fishery", "species","catchShare","illegalFishing"))
npvP3$BAU<-npvP3$SQ1+npvP3$SQ2
npvP3$FMSY<-npvP3$FMSY1+npvP3$FMSY2
npvP3$econOpt<-npvP3$econOpt1+npvP3$econOpt2
names(npvP3)
npvP3<-npvP3[,c(-5:-16)]
npvP3Wide<-melt(npvP3, measure.vars = c("BAU", "FMSY", "econOpt"),
                variable.name="management", value.name = "npv")

npvP3Wide$Situation<-paste(npvP3Wide$catchShare,"_", npvP3Wide$illegalFishing, sep="")
npvP3Wide$Situation2<-ifelse(npvP3Wide$Situation=="no_CS_illegal_fishing", "Worst", ifelse(npvP3Wide$Situation=="CS_no_illegal_fishing", "Best", "Blah"))
npvP3Wide<-npvP3Wide[which(npvP3Wide$Situation2!="Blah"),]


### Plotting code
library("RColorBrewer")
library(directlabels)
library(ggrepel)

names(masterPsub)
quartz() # we called quartz() inn order to prevent the error in grid.Call() message we get

Totals<-masterPsub%>%
  group_by(time, management)%>%
  summarise(biomass=sum(biomass), harvest=sum(harvest), profit=sum(profit))%>%
  mutate(manag = if_else(management=="econOpt", "Econ. opt.", management))

write.csv(Totals, file = "output/Results_FishDisc2019_TotalsBAU.csv", row.names=FALSE)

##KK: the following code makes the Costello plot--I simplified so that it does't 
if(User == 1){setwd("~/Dropbox/Upside_next_morioka/upside_20181227_try/")} else if
(User == 2) {setwd("/Users/gakugaku/Dropbox/Drop_REsearch/Upside_next_morioka_original/upside_20181227_try/")}

pdf("output/Results_FishDisc0727(phi=0.188)_Final/CostelloPlot_Disc0727(phi=0.188).pdf",width=6,height=4,onefile=T) 

Totals1<-Totals
check<-ggplot(Totals1, aes(time, biomass, size=harvest, fill=profit, alpha=0.2), colour="black") + 
  geom_point(shape=21) + 
  scale_fill_gradient(name="Profit [USD]", 
                      low = "red", high = "blue",
                      na.value = "grey50", guide = "colourbar")+
  scale_size(name="Harvest [MT]")+
  scale_alpha(guide=F) +
  geom_dl(aes(label = manag), method = "last.qp", cex = 0.9) +
  xlim(c(0,60)) +
  xlab("Time") +
  ylab("Biomass [MT]") +
  ggtitle("") +
  theme_bw()
print(check)
#dev.off()

##KK: don't really need the "next"CostelloPlot_combo" plot as this is used only if you have different scenarios you are comparing.  
##KK: I deleted the CostelloPlot_combo code.

fisheryNames <- unique(as.factor(masterOutput$fishery))
#fisheryNames<-rep(fisheryNames, 2)
fisheryNames<-fisheryNames[order(fisheryNames)]
lineSize <- 1.5
## Grab legend from ggplot
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}

i=NULL
TIME=50
i= fisheryNames[16]
unique(masterPsub$fishery)

for (i in fisheryNames[1:97]){
  datadf <- masterPsub %>%
    filter(fishery == i)
  FishTitle<-datadf$fishery[1]
  ###Make Kobe plot with density and arrows from BaseYear to end year by management scenario:
  data1<-subset(datadf, subset=time==1)
  data2<-subset(datadf, subset=time==TIME)
  data1$FvFMSYtotal[data1$FvFMSYtotal>4]<- 4
  data1$BvBMSY[data1$BvBMSY>2.5]<- 2.5
  data2$FvFMSYtotal[data2$FvFMSYtotal>4]<- 4
  data2$BvBMSY[data2$BvBMSY>2.5]<- 2.5
  data1<-data1[c(-1:-2),]
  BaseEnd<-rbind(data1, data2)
  BaseEnd<-data.frame(BaseEnd)
  BaseEnd$management<-ifelse(BaseEnd$time==1, "BaseYear", BaseEnd$management)
  ENDS<-NULL
  for (k in 2:4){
    EndVal<-ifelse(BaseEnd[1,16]>BaseEnd[k, 16], "first", "last")
    ENDS<-rbind(ENDS, EndVal)
  }
  #"#D55E00"= red (BAU),  "#E69F00" = gold (econOpt), "#0072B2"= blue (FMSY)
  kobe<-ggplot(BaseEnd, aes(BvBMSY, FvFMSYtotal), col=management, shape=Year) +
    stat_density_2d(aes(fill = ..density..), geom = "tile", n = 100, alpha = 1, contour = F) +
    scale_fill_gradient2(guide = F, low = "lightskyblue",mid = "white", high = "gold", midpoint = 0.5) +
    geom_point(shape=c(1, 24, 24, 24), size = 6, alpha=0.75) +
    geom_point(data=BaseEnd,shape = c(1, 24, 24, 24), size = 6, fill = c("black", "#D55E00", "#E69F00",  "#0072B2" ), alpha = 0.75) +
    geom_line(data=BaseEnd[c(1, 2),],arrow = arrow(length=unit(0.25,"cm"),ends=ENDS[1], type = "closed"), alpha=0.5, lty=2, lwd=0.1) +
    geom_line(data=BaseEnd[c(1, 3),],arrow = arrow(length=unit(0.25,"cm"),ends=ENDS[2], type = "closed"), alpha=0.5, lty=2, lwd=0.1) +
    geom_line(data=BaseEnd[c(1, 4),],arrow = arrow(length=unit(0.25,"cm"),ends=ENDS[3], type = "closed"), alpha=0.5, lty=2, lwd=0.1) +
    scale_x_continuous(limits = c(-1, 4), breaks = seq(-1,4, by = 0.5), labels = c(seq(-1, 2, by = 0.5), expression(phantom(x) >= 2.5), seq(3,4, by = 0.5))) +
    scale_y_continuous(limits = c(-1,6), breaks = seq(-1, 6, by = 0.5), labels = c(seq(-1,3.5, by = 0.5), expression(phantom(x) >= 4), seq(4.5,6, by = 0.5))) +
    coord_cartesian(xlim = c(0, 2.5), ylim = c(0,4)) +
    geom_hline(aes(yintercept = 1), linetype = "longdash") +
    geom_vline(aes(xintercept = 1), linetype = "longdash") +
    labs(x=expression(B/B[msy]), y=expression(F/F[msy]))+
    theme(legend.position="none")+
    ggtitle(FishTitle)
  
  ########Make biomass, harvest, profit time series plots:
  datadf2 <- datadf %>%
    group_by(management, time) %>%
    summarise(meanb = mean(biomass),
              sdb = sd(biomass),
              meanh = mean(harvest),
              sdh = sd(harvest),
              meanp = mean(profit),
              sdp = sd(profit),
              meanBvBMSY = mean(BvBMSY),
              sdBvBMSY = sd(BvBMSY),
              meanFvFMSYtotal = mean(FvFMSYtotal),
              sdFvFMSYtotal = sd(FvFMSYtotal))
  ### Biomass plot:
  #"#D55E00"= red (BAU),  "#E69F00" = gold (econOpt), "#0072B2"= blue (FMSY)
  bplot = ggplot(datadf2, aes(x = time, y = meanb, group = management, colour = management, fill = management)) +
    # geom_ribbon(aes(ymin = meanb - 2 * sdb, ymax = meanb + 2 * sdb)) +
    geom_line(size = lineSize) +
    geom_hline(yintercept=0, lty=1, colour="black", size=0.25, alpha=0.8) +
    xlab("Time") +
    ylab("Biomass [MT]") +
    scale_colour_manual(name=legendName, 
                        labels = c("BAU" = "Business As Usual",
                                   "FMSY" = "FMSY", 
                                   "econOpt" = "Economically Optimal"),
                        values = c("BAU" = "#D55E00",
                                   "FMSY" = "#0072B2",
                                   "econOpt" = "#E69F00")) +
    theme_bw() +
    theme(legend.position="none")
  
  
  ### Harvest plot:
  hplot1 = ggplot(datadf2, aes(x = time, y = meanh, group = management, colour = management, fill = management)) +
    # geom_ribbon(aes(ymin = meanb - 2 * sdb, ymax = meanb + 2 * sdb)) +
    geom_line(size = lineSize) +
    geom_hline(yintercept=0, lty=1, colour="black", size=0.25, alpha=0.8) +
    xlab("Time") +
    ylab("Harvest [MT]") +
    scale_colour_manual(name=legendName, 
                        labels = c("BAU" = "Business As Usual",
                                   "FMSY" = "FMSY", 
                                   "econOpt" = "Economically Optimal"),
                        values = c("BAU" = "#D55E00",
                                   "FMSY" = "#0072B2",
                                   "econOpt" = "#E69F00")) +
    theme_bw() +
    theme(legend.position="none")
  
  ### Profit plot:
  pplot1 = ggplot(datadf2, aes(x = time, y = meanp, group = management, colour = management, fill = management)) +
    # geom_ribbon(aes(ymin = meanb - 2 * sdb, ymax = meanb + 2 * sdb)) +
    geom_line(size = lineSize) +
    geom_hline(yintercept=0, lty=1, colour="black", size=0.25, alpha=0.8) +
    xlab("Time") +
    ylab("Profit [JPY]") +
    scale_colour_manual(name=legendName, 
                        labels = c("BAU" = "Business As Usual",
                                   "FMSY" = "FMSY", 
                                   "econOpt" = "Economically Optimal"),
                        values = c("BAU" = "#D55E00",
                                   "FMSY" = "#0072B2",
                                   "econOpt" = "#E69F00")) +
    theme_bw() +
    theme(legend.position="none")
  
  ### NPV plot
  npvdf <- npvP3Wide %>%
    filter(fishery == i) 
  
  npvplot = ggplot(npvdf, aes(x = reorder(management, npv), y = npv, fill = management)) +
    geom_bar(stat = "identity") +
    geom_hline(yintercept=0, lty=1, colour="black", size=0.25, alpha=0.8) +
    xlab("Management Scenario") +
    ylab("Total NPV [JPY]") +
    scale_fill_manual(name="", 
                      labels = c("FMSY" = "FMSY",
                                 "BAU" = "Business As Usual", 
                                 "econOpt" = "Economically Optimal" 
                      ),
                      values = c("BAU" = "#D55E00",
                                 "FMSY" = "#0072B2",
                                 "econOpt" = "#E69F00")) +
    theme_bw() +
    theme(legend.position="bottom", legend.text = element_text(size = 7)) + guides(fill = guide_legend(nrow = 2, byrow = T))
  
  
  colorLineLegend = ggplot(datadf, aes(x = time, y = harvest, colour = management, fill = management, lty = fleet)) +
    # geom_ribbon(aes(ymin = meanh - 2 * sdh, ymax = meanh + 2 * sdh)) +
    geom_line(size = lineSize) +
    geom_hline(yintercept=0, lty=1, colour="black", size=0.25, alpha=0.8) +
    xlab("Time") +
    ylab("Harvest [MT]") +
    # scale_fill_manual(guide = "none") +
    scale_colour_manual(guide = F, name=legendName, 
                        labels = c("FMSY" = "FMSY",
                                   "BAU" = "Business As Usual", 
                                   "econOpt" = "Economically Optimal" 
                        ),
                        values = c("BAU" = "#D55E00",
                                   "FMSY" = "#0072B2",
                                   "econOpt" = "#E69F00")) +
    scale_linetype_manual(values = c("State" = 1, 
                                     "Private" = 2, 
                                     "Illegal" = 3),
                          labels = c("State" = "State",
                                     "Private" = "Private",
                                     "Illegal" = "Illegal")) +
    theme_bw() +
    theme(legend.key = element_rect(colour = "transparent", fill = "white"),
          legend.key.width = unit(5, "line"))
  
  
  leg<-g_legend(colorLineLegend)
  
  gs<-list(kobe,
           npvplot,
           bplot,
           hplot1,
           pplot1)
  lay <- rbind(c(1,1,2),
               c(3,4,5))
  GL<-grid.arrange(grobs = gs, layout_matrix = lay)
  
  ggsave(file=paste("output_2019/Results_FishDisc_2019/", FishTitle, "Disc2019.pdf"), GL, height=7,width=12)
  print(GL)
}
dev.off()

###################################################
