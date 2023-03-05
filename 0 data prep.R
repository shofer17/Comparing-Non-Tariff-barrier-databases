# Author: Silvan Hofer
# Date: 6. February 2023
# Purpose: 
# This script cleans and standardizes data
# from various sources (CEPII, GTA, UNCTAD, WB) for analysis in 1 data analysis.

# The data will be standardised in the following: 
# 1. All product codes will be translated into ISIC 3.
# 2. Countries are specified by 3-digits ISO codes. 
# 3. All measures are classified as liberalising or restrictive
# 4. Country-MAST chapter combinations will be aggregared to get number of interventions per combination
# 


setwd("..") # move up one
rm(list = ls())
#install.packages("gsubfn")
#install.packages("remotes")
#remotes::install_github("insongkim/concordance")
library(concordance)
library(gsubfn)
library(readxl)
library(tidyr)
library(magrittr)
library(dplyr)
library(lubridate)
library(splitstackshape)
library(gtalibrary)
library(stringr)
# old version to avoid memory bug 
# devtools::install_version("haven", version = "1.1.0") 
library(haven) #dta files

# Parameters

years <- 2009:2019

# Paths

path.data.raw <- "0 data raw/"
path.data.out <- "1 data processed/"




# 1. UN ESCAP (Trade Costs) ----------------------------------------------------

## Load 
sheets <- c("AB", "D", "GTT") # Sector: ISIC rev.3
trade.costs <- data.frame()

for(i in sheets){ # takes a while
  sheet <- read_xlsx(paste0(path.data.raw, 
                            "ESCAP-WB-tradecosts-dataset-20220509.xlsx"), 
                     sheet = i)
  
  trade.costs    <- rbind(trade.costs, sheet)
  
}


## Clean

trade.costs <- trade.costs %>% filter(year >= min(years) & year <= max(years)) %>%
  select(-c(reportername, partnername, sectorname))

#DROP HALF SAMPLE TO AVOID REPETITON (tariffs are bidirectional, so each tariff shows up twice)
#To compute efficiently, sort all ISO codes per row alphabetically and apply unique()
trade.costs$reporter.backup <- trade.costs$reporter

trade.costs$reporter <- ifelse(trade.costs$reporter < trade.costs$partner, 
                               trade.costs$reporter, 
                               trade.costs$partner)
trade.costs$partner <- ifelse(trade.costs$partner > trade.costs$reporter.backup, 
                              trade.costs$partner, 
                              trade.costs$reporter.backup)

trade.costs <- trade.costs %>%
  select(-reporter.backup) %>%
  unique()

# trade.costs[trade.costs == ".."] <- NA #.. denotes no value, change to NA
# names(trade.costs) <- gsub("\\[(.*)\\]","", names(trade.costs), perl = T)
# trade.costs <- pivot_longer(trade.costs, 
#                             7:ncol(trade.costs), 
#                             names_to  = "year", 
#                             values_to = "tij")

# Save
saveRDS(trade.costs, file = paste0(path.data.out, 
                                   "Trade Costs Processed.RData"))

writexl::write_xlsx(trade.costs, path = paste0(path.data.out, 
                                          "Trade Costs Processed.xlsx"))

rm(sheet, trade.costs)
# 2. TRAINS ------------------------------------------------------------------

## Load ---------

TRAINS <- read.csv(paste0(path.data.raw, "UNCTAD_TRAINS_database.csv"))
hs.to.isic <- readxl::read_xlsx(paste0(path.data.out, "ISIC to HS 2 digits conversion.xlsx"))
TRAINS.to.GTA.names <- readxl::read_xlsx(path = paste0(path.data.out, "UNCTAD_GTA_conversion.table.xlsx"))
TRAINS.to.GTA.names <- rbind(TRAINS.to.GTA.names, c("World" ,"World", NA))
hs.17.to.hs.12 <- readRDS(file = paste0(path.data.out, "hs.17.to.hs.12.RData"))


## Names ------------
names(TRAINS) <- c("implementing.jurisdiction","affected.jurisdiction",
                   "mast.chapter","description", "product.description",
                   "hs.code", "issuing.agency","regulation.title",
                   "regulation.symbol","date.implemented",
                   "official.regulation.document","official.title.original.language",     
                   "measure.description.original.language", "product.description.original.language",
                   "supporting/related.regulations","measure.objective",
                   "years.of.data.collection","date.removed")

TRAINS <- TRAINS %>% select(-c(official.title.original.language, # For explanation of cases, refer to Chapter 3.2: UNCTAD TRAINS in the Thesis. 
                               `supporting/related.regulations`, 
                               years.of.data.collection,
                               regulation.symbol,
                               issuing.agency,
                               product.description.original.language,    # 1 case
                               official.regulation.document,             # 1056 cases: e.g. https://trainsdataentry-api.unctad.org/regulations/get-file?fileName=VNM_01_2016_TT-BNNPTNT_VN.doc vs. https://trainsdataentry-api.unctad.org/regulations/get-file?fileName=VNM_01_2016_TT-BNNPTNT_EN.doc
                               measure.description.original.language,    # 2 cases
                               measure.objective,                        # 5 cases
                               #description                               # 1039 cases: e.g. Wood-based packaging materials and packages are subject to heat treatment vs. Wood-based packaging materials and packages shall be specifically stamped
                               ))
#test <- TRAINS[duplicated(TRAINS),]

TRAINS <- unique(TRAINS)


## Dates ------------

any(is.na(TRAINS$date.removed))
TRAINS$date.implemented[is.na(TRAINS$date.implemented)] <- "1900-01-01" #NAs come from dates before 1900
TRAINS <- TRAINS %>% 
  mutate(date.removed = as.numeric(year(date.removed)))%>%
  mutate(date.implemented = as.numeric(year(date.implemented)))%>%
  filter(is.na(date.removed) | date.removed > min(years)) %>%
  filter(is.na(date.implemented) | date.implemented < max(years)) %>% #remove interventions that are out of range
  mutate(date.removed = ifelse(is.na(date.removed) | date.removed > max(years),
                               max(years),
                               date.removed)) %>% #set everything to 2019 that is after that or has no removal date
  mutate(date.implemented = ifelse(date.implemented < min(years), 
                                   min(years), 
                                   date.implemented)) #set everything before 2009 to 2009

# get all in force years in a strint
TRAINS$years.in.force <- apply(TRAINS[, c("date.implemented", "date.removed")], 1, FUN = function(x) paste0(seq(x[1],max(x)), collapse = ","))
TRAINS <- TRAINS %>% select(-c(date.removed, date.implemented))

TRAINS <- TRAINS %>% mutate(measure.id = 1:nrow(TRAINS)) #add unique id


## Names ---------------------
trains <- TRAINS

# clean affected names and move exceptions to separate column
TRAINS <- TRAINS %>%
  mutate(exceptions =  gsub(".*(?<=except)", "", affected.jurisdiction, ignore.case = T, perl = T)) %>%
  mutate(affected.jurisdiction = ifelse(grepl("except", affected.jurisdiction), gsub("(?<=except).*", "", affected.jurisdiction, ignore.case = T, perl = T), affected.jurisdiction)) %>%
  mutate(exceptions = ifelse(grepl("except", affected.jurisdiction), exceptions, "")) %>%
  mutate(affected.jurisdiction = gsub("\\(except", "", affected.jurisdiction, perl = T))

TRAINS <- cSplit(TRAINS, "affected.jurisdiction", direction = "long")

world.conversion <- data.frame(TRAINS.to.GTA.names[, c("UNCTAD.name")])
world.conversion <- world.conversion %>%
  mutate(affected.jurisdiction = UNCTAD.name) 
world.conversion.2 <- world.conversion
world.conversion.2$affected.jurisdiction <- "World "
world.conversion <- rbind(world.conversion, world.conversion.2)
world.conversion <- world.conversion[world.conversion$UNCTAD.name != "World",]


TRAINS <- merge(TRAINS, world.conversion, by = "affected.jurisdiction", all.x = T, all.y = T)


TRAINS <- merge(TRAINS, TRAINS.to.GTA.names[, c("UNCTAD.name", "GTA.name")], by.x = "implementing.jurisdiction", by.y = "UNCTAD.name", all.x = T)
TRAINS <- TRAINS %>% 
  mutate(implementing.jurisdiction = GTA.name) %>%
  select(-GTA.name)

countries.iso <- country.names[, c("name", "iso_code")]
countries.iso <- rbind(countries.iso, c("European Union", "EU"))

# add ISO codes
TRAINS <- merge(TRAINS, countries.iso, by.x = "implementing.jurisdiction", by.y = "name")
names(TRAINS)[ncol(TRAINS)] <- "iso_implementer"

# In best case use GTA function to get proper affected countries


## HS codes ------------

TRAINS <- TRAINS %>%
  #mutate(mast.chapter =  gsub("[0-9]", "", mast.chapter)) %>% #remove MAST subchapters
  mutate(hs.code =  gsub("\\(.*?\\)", "", hs.code, ignore.case = T)) %>% #remove HS explenations
  mutate(hs.code =  gsub("\\)", "", hs.code, ignore.case = T)) %>%
  mutate(hs.code =  gsub("[^0-9,]", "", hs.code, ignore.case = T)) 


TRAINS <- unique(cSplit(TRAINS, "hs.code", direction = "long", type.convert = F)) #some HS codes are double
TRAINS <- TRAINS %>% filter(nchar(hs.code) > 1) #drop empty string and products specified as "0".
TRAINS$hs.code <- ifelse(nchar(TRAINS$hs.code) > 6, substr(TRAINS$hs.code, 1,6), TRAINS$hs.code) #some are specified in 10 digits


#take 2 digit codes, add all 6 digit codes to it, convert to HS2012, and sum it up to one string
TRAINS.2 <- merge(TRAINS[nchar(TRAINS$hs.code)== 2,], hs.17.to.hs.12[, c("hs17.dig.2", "hs12.dig.6")], by.x = "hs.code", by.y = "hs17.dig.2", all.x = T, allow.cartesian = T)
TRAINS.4 <- merge(TRAINS[nchar(TRAINS$hs.code)== 4,], hs.17.to.hs.12[, c("hs17.dig.4", "hs12.dig.6")], by.x = "hs.code", by.y = "hs17.dig.4", all.x = T, allow.cartesian = T)
TRAINS.6 <- merge(TRAINS[nchar(TRAINS$hs.code)== 6,], hs.17.to.hs.12[, c("hs17.dig.6", "hs12.dig.6")], by.x = "hs.code", by.y = "hs17.dig.6", all.x = T, allow.cartesian = T)

TRAINS <- rbind(TRAINS.2, TRAINS.4, TRAINS.6)
TRAINS <- TRAINS %>%
  select(-hs.code) %>%
  unique() #avoid the same HS codes twice

# aggregate all HS codes into a string and save it
converted.hs <- aggregate(data = TRAINS[, c("measure.id", "hs12.dig.6")],  hs12.dig.6 ~ measure.id , FUN = function(x) paste0(x, collapse = ","))
TRAINS.asymmetric.6dig.hs12 <- TRAINS %>%
  select(-hs12.dig.6) %>%
  unique() %>%
  left_join(y = converted.hs, by = "measure.id")

## INVESTIGATE LEFTOVERS (possible form HS2012 or smth.)
test <- TRAINS.asymmetric.6dig.hs12[is.na(TRAINS.asymmetric.6dig.hs12$hs12.dig.6),]


saveRDS(TRAINS.asymmetric.6dig.hs12, file = paste0(path.data.out, "TRAINS_asymmetric_6dig.hs12.RData"))
rm(TRAINS.asymmetric.6dig.hs12, TRAINS.2, TRAINS.4, TRAINS.6, converted.hs)


## ISIC -------------

TRAINS$hs.code <- substr(TRAINS$hs12.dig.6, 1,2)

# Indonesia has Measures affecting HS 98 and 99. They could not be matched to ISIC (see code "96 generate help files.R")
# Therefore, they are matched manually to chapter C ("Mining and quarrying") in ISIC 3. It only affects 10 interventions. 
hs.to.isic <- rbind(hs.to.isic, c(NA,98,"C"), c(NA,99,"C"))

#get proper names, isic chapter and reduce dataframe
TRAINS <- merge(TRAINS, hs.to.isic[, c("hs.code", "chapter")], by = "hs.code")
TRAINS <- TRAINS %>% 
  select(-c(hs.code, description, product.description, regulation.title, hs12.dig.6)) %>%
  unique()%>%
  filter(chapter %in% c("A", "D"))

converted.chapter <- unique(aggregate(data = TRAINS[, c("measure.id", "chapter")],  chapter ~ measure.id , FUN = function(x) paste0(x, collapse = ",")))

TRAINS <- TRAINS %>%
  select(-chapter) %>%
  unique() %>%
  left_join(y = converted.chapter, by = "measure.id")



## Save
writexl::write_xlsx(TRAINS, path = paste0(path.data.out, 
                                          "TRAINS_asymmetric_isic.xlsx"))
saveRDS(TRAINS, file = paste0(path.data.out, 
                                   "TRAINS_asymmetric_isic.RData"))

rm(countries.iso, TRAINS.to.GTA.names, converted.chapter, trains, TRAINS, test)



saveRDS(TRAINS, file = paste0(path.data.out, 
                              "TRAINS_asymmetric_isic.RData"))

## aggregate ------------

TRAINS_asymmetric_isic <- readRDS(file = paste0(path.data.out, "TRAINS_asymmetric_isic.RData"))






# 3. WTO TMDB ----------------------------------------------------------------
## Load -------------
WTO <- readxl::read_xlsx(path = paste0(path.data.raw, 
                                "WTO_NTMs_Trade_Monitoring_Database.xlsx"))
wto.names.to.iso <- readxl::read_xlsx(path = paste0(path.data.out, 
                                                    "WTO ISO conversions.xlsx"))
isic.chapters <- readxl::read_xlsx(path = paste0(path.data.out, 
                                                 "ISIC chapter codes.xlsx"))
hs.12 <- data.frame(hs.codes[, "hs.code"])
names(hs.12) <- "hs12.dig.6"
hs.12$hs12.dig.6 <- ifelse(nchar(hs.12$hs12.dig.6) == 5, paste0(0, hs.12$hs12.dig.6), hs.12$hs12.dig.6)
hs.12$hs12.dig.4 <- substr(hs.12$hs12.dig.6, 1,4)
hs.12$hs12.dig.2 <- substr(hs.12$hs12.dig.6, 1,2)


WTO$id <- 1:nrow(WTO)

names(WTO) <- c("implementing.jurisdiction", "date.implemented","mast.chapter","evaluation",
                "hs.chapters", "hs.code","affected.jurisdiction", 
                "date.removed","description","source", "status","measure.id" )

#restrict dataset
WTO <- WTO %>% 
  filter(evaluation %in% c("Restrictive", "Trade remedy") & #only get restrictive measures
           !is.na(hs.code))%>% #only get measures that have product codes
  select(-c(evaluation, source, description))

## Dates -------------------
# split trading partners and get partners w. individual termination date
WTO <- cSplit(WTO, "affected.jurisdiction", sep = ";", direction = "long")
WTO$termin.p.partner <- ifelse(grepl("\\((.*)\\)", WTO$affected.jurisdiction),
                               as.character(WTO$affected.jurisdiction),NA)


# get termination date
WTO$termin.p.partner <- as.character(strapplyc(WTO$termin.p.partner, 
                                               "[0-9]{2}/[0-9]{2}/[0-9]{4}", 
                                               simplify = TRUE))
WTO$termin.p.partner <- ifelse(WTO$termin.p.partner == "character(0)", NA, 
                               WTO$termin.p.partner)

#add individual termination date to general termination date
WTO <- WTO %>%  
  mutate(date.removed = as.Date(date.removed))%>%
  mutate(termin.p.partner = as_date(termin.p.partner, format = '%d/%m/%Y'))%>%
  mutate(date.removed = if_else(!is.na(termin.p.partner), 
                              termin.p.partner, 
                              date.removed))%>%
  mutate(affected.jurisdiction = gsub(pattern = " \\((.*)\\)",
                                   replacement = "",
                                   affected.jurisdiction))%>%
  select(-termin.p.partner)

# only get active measures during observation period
WTO <- WTO%>%filter((is.na(date.removed)|date.removed < as.Date(paste0(max(years), "-12-31"))) &
                      date.implemented > as.Date(paste0(min(years), "-01-01"))&
                      date.implemented < as.Date(paste0(max(years), "-12-31")))


WTO$date.implemented <- year(WTO$date.implemented)
WTO$date.removed <- ifelse(is.na(WTO$date.removed) | 
                             WTO$date.removed > max(years), max(years), WTO$date.removed)

WTO$years.in.force <- apply(WTO[, c("date.implemented", "date.removed")], 1, FUN = function(x) paste0(seq(x[1],max(x)), collapse = ","))
WTO <- WTO %>% select(-c(date.implemented, date.removed))
## Names ---------------

# add ISO codes
WTO <- merge(WTO, wto.names.to.iso, by.x = "implementing.jurisdiction", by.y = "Name")
names(WTO)[ncol(WTO)] <- "ISO_implementing.jurisdiction"

WTO <- merge(WTO, wto.names.to.iso, by.x = "affected.jurisdiction", by.y = "Name", all.x = T)
names(WTO)[ncol(WTO)] <- "ISO_affected.jurisdiction"


## HS codes ----------------------
#go trough all hs products and convert them to ISIC

WTO <- cSplit(WTO, "hs.code", direction = "long", type.convert = F)

#take 2 digit codes, add all 6 digit codes to it, convert to HS2012, and sum it up to one string
WTO.2 <- merge(WTO[nchar(WTO$hs.code)== 2,], hs.12[, c("hs12.dig.2", "hs12.dig.6")], by.x = "hs.code", by.y = "hs12.dig.2", all.x = T, allow.cartesian = T)
WTO.4 <- merge(WTO[nchar(WTO$hs.code)== 4,], hs.12[, c("hs12.dig.4", "hs12.dig.6")], by.x = "hs.code", by.y = "hs12.dig.4", all.x = T, allow.cartesian = T)
WTO.6 <- data.frame(WTO[nchar(WTO$hs.code)== 6, "hs.code"])
WTO.6$hs12.dig.6 <-  apply(WTO.6 <- WTO.6, 1, FUN = function(x) gta_hs_vintage_converter(x)) # Applying to each row separately that it guesses each code independantly to account for potentially different HS vintages.
WTO.6 <- cbind(WTO.6, WTO[nchar(WTO$hs.code)== 6, -"hs.code"])

WTO <- rbind(WTO.2, WTO.4, WTO.6)
WTO$hs12.dig.6 <- as.character(WTO$hs12.dig.6)
WTO <- WTO %>%
  select(-hs.code)%>%
  unique()

# aggregate all HS codes into a string and save it
converted.hs <- aggregate(data = WTO[, c("measure.id", "hs12.dig.6")],  hs12.dig.6 ~ measure.id , FUN = function(x) paste0(x, collapse = ","))
WTO.asymmetric.6dig.hs12 <- WTO %>%
  select(-hs12.dig.6) %>%
  unique() %>%
  left_join(y = converted.hs, by = "measure.id")

saveRDS(WTO.asymmetric.6dig.hs12, file = paste0(path.data.out, "WTO_asymmetric_6dig.hs12.RData"))
WTO.asymmetric.6dig.hs12 <- readRDS(file = paste0(path.data.out, "WTO_asymmetric_6dig.hs12.RData"))

## ISIC ----------------

WTO$hs12.dig.6 <- substr(WTO$hs12.dig.6, 1,2)


#get proper names, isic chapter and reduce dataframe
WTO <- merge(WTO, hs.to.isic[, c("hs.code", "chapter")], by.x = "hs12.dig.6", by.y = "hs.code")
WTO <- WTO %>% 
  select(-c(hs.chapters, hs12.dig.6, status)) %>%
  unique()%>%
  filter(chapter %in% c("A", "D"))

converted.chapter <- unique(aggregate(data = WTO[, c("measure.id", "chapter")],  chapter ~ measure.id , FUN = function(x) paste0(x, collapse = ",")))

WTO <- WTO %>%
  select(-chapter) %>%
  unique() %>%
  left_join(y = converted.chapter, by = "measure.id")



## Save
writexl::write_xlsx(WTO, path = paste0(path.data.out, 
                                          "WTO_asymmetric_isic.xlsx"))
saveRDS(WTO, file = paste0(path.data.out, 
                              "WTO_asymmetric_isic.RData"))

rm(countries.iso, hs.to.isic, WTO.to.GTA.names, converted.chapter, WTO, WTO, test, WTO.2, WTO.4, WTO.6, WTO.62)

WTO <- readRDS(paste0(path.data.out, 
               "WTO_asymmetric_isic.RData"))


## aggregate ------------
# use alphabet: sort all implementer-affected pair alphabetically
# this way, we can aggregate across pairs and get bilateral trade flows

## CHANGE IF POSSIBLE TO INCLUDE ALL MEASURES -------
WTO <- WTO[!is.na(WTO$affected.jurisdiction) & !is.na(WTO$implementing.jurisdiction),]

WTO$implementing.jurisdiction.backup <- WTO$implementing.jurisdiction

WTO$implementing.jurisdiction <- ifelse(WTO$implementing.jurisdiction < WTO$affected.jurisdiction, 
                       WTO$implementing.jurisdiction, 
                       WTO$affected.jurisdiction)
WTO$affected.jurisdiction <- ifelse(WTO$affected.jurisdiction > WTO$implementing.jurisdiction.backup, 
                      WTO$affected.jurisdiction, 
                      WTO$implementing.jurisdiction.backup)
WTO$implementing.jurisdiction.backup <- NULL

WTO <- cSplit(WTO, "chapter", direction = "long")
WTO <- cSplit(WTO, "years.in.force", direction = "long")

WTO <- unique(WTO)

WTO <- aggregate(data = WTO, measure.id ~ implementing.jurisdiction + affected.jurisdiction + years.in.force, FUN = function(x) length(unique(x)))
names(WTO) <- c("country.1", "country.2", "year", "number.of.interventions")

## Save
writexl::write_xlsx(WTO, path = paste0(path.data.out, 
                                       "WTO_symmetric_isic.xlsx"))
saveRDS(WTO, file = paste0(path.data.out, "WTO_symmetric_isic.RData"))


# 4. GTA -----------------------------------------------------------------------



# 5. CEPII Gravity control variables -------------------------------------------


# Gravity controls
controls <- readRDS(paste0(path.data.raw, "CEPII_Gravity_Variables.Rds")) 
controls <- controls %>% 
  filter(year %in% years & country_exists_o & country_exists_d)%>%
  select(c("year","country_id_o","country_id_d","iso3_o","iso3_d","iso3num_o", 
           "distw_harmonic_jh","contig","diplo_disagreement","scaled_sci_2021",
           "comlang_off","comlang_ethno","comcol","comrelig","heg_o","heg_d",
           "col_dep_ever", "gatt_o","gatt_d","wto_o","wto_d","eu_o","eu_d",
           "fta_wto","fta_wto_raw","entry_tp_o","entry_tp_d")) #check distance measure


saveRDS(controls, file = paste0(path.data.out, "Controls cleaned CEPII.RData"))



# General controls
# distance.variables <- read_dta(paste0(path.data.raw, 
#                                       "CEPII_Distance_Variables.dta"))
# 
# geo.variables <- read_dta(paste0(path.data.raw, 
#                                       "CEPII_Geo_Variables.dta"))







