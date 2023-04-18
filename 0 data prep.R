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


# Libraries
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
library(tidyverse)
# old version to avoid memory bug 
# devtools::install_version("haven", version = "1.1.0") 
library(haven) #dta files


setwd("..") # move up one (out of the code file)

rm(list = ls())
source("BA_Thesis_code/00 Terms and Definitions.R")



# 1. UN ESCAP (Trade Costs) ----------------------------------------------------

## Load  -----------------------------------------------------------------------
sheets <- c("AB", "D", "GTT") # Sector: ISIC rev.3
trade.costs <- data.frame()

for(i in sheets){ # takes a while
  sheet <- read_xlsx(paste0(path.data.raw, 
                            "ESCAP-WB-tradecosts-dataset-20220509.xlsx"), 
                     sheet = i)
  
  trade.costs    <- rbind(trade.costs, sheet)
}


## Clean -----------------------------------------------------------------------
#TRADE COST TO GRID
trade.costs <- trade.costs %>% 
  filter(year >= min(years) & year <= max(years)) %>%
  filter(reporter %in% selected.countries & partner %in% selected.countries) %>%
  select(-c(reportername, partnername, sectorname))

#rounding is unlikely to affect results
trade.costs$tij <- round(as.numeric(trade.costs$tij),3) #round from 4 to 3 digits after comma to avoid double counting values (unique() does not work otherwise)
trade.costs$geometric_avg_tariff <- round(as.numeric(trade.costs$geometric_avg_tariff),5) #round from 6 to 5 digits after comma to avoid double counting values (unique() does not work otherwise)
trade.costs$nontariff_tij <- round(as.numeric(trade.costs$nontariff_tij),3) #round from 4 to 3 digits after comma to avoid double counting values (unique() does not work otherwise)

#Drop half sample (tariffs are bidirectional, each obs. shows up twice, ij and ji)
trade.costs <- unique(to_alphabeta(trade.costs, "reporter", "partner"))

names(trade.costs) <- c("country.1", "country.2", "year", "chapter", "tij", "geometric_avg_tariff", "nontariff_tij")


nrow(trade.costs) == nrow(grid)

## Save -------------------------------------------------------------------------
saveRDS(trade.costs, file = paste0(path.data.out, "Trade Costs Processed.RData"))

writexl::write_xlsx(trade.costs, path = paste0(path.data.out, "Trade Costs Processed.xlsx"))

rm(sheet, trade.costs, sheets)

# 2. TRAINS ------------------------------------------------------------------

## Load ---------
hs.to.isic <- readxl::read_xlsx(paste0(path.data.out, "ISIC to HS 2 digits conversion.xlsx"))
TRAINS <- read.csv(paste0(path.data.raw, "UNCTAD_TRAINS_database.csv"))
TRAINS.to.GTA.names <- readxl::read_xlsx(path = paste0(path.data.out, "UNCTAD_GTA_conversion.table.xlsx"))
TRAINS.to.GTA.names <- rbind(TRAINS.to.GTA.names, c("World" ,"World", NA))
TRAINS.to.GTA.names <- TRAINS.to.GTA.names %>% 
  left_join(country.names[, c("name", "iso_code")], by = join_by(GTA.name == name)) %>%
  filter(iso_code %in% selected.countries)
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
TRAINS <- TRAINS %>% 
  mutate(mast.subchapter = mast.chapter)%>%
  mutate(mast.chapter = substr(mast.chapter, 1,1)) %>%
  filter(mast.chapter %in% selected.mast)

## Dates ------------

any(is.na(TRAINS$date.removed))
TRAINS$date.implemented[is.na(TRAINS$date.implemented)] <- "1900-01-01" #NAs come from dates before 1900
TRAINS <- TRAINS %>% 
  mutate(date.removed = as.numeric(year(date.removed)))%>%
  mutate(date.implemented = as.numeric(year(date.implemented)))%>%
  filter(is.na(date.removed) | date.removed > min(years)) %>%
  filter(!date.implemented < min(years)) %>%
  filter(is.na(date.implemented) | date.implemented <= max(years)) %>% #remove interventions that are out of range
  mutate(date.removed = ifelse(is.na(date.removed) | date.removed > max(years),
                               max(years),
                               date.removed)) %>% #set everything to 2019 that is after that or has no removal date
  mutate(date.implemented = ifelse(date.implemented < min(years), 
                                   min(years), 
                                   date.implemented)) #set everything before 2009 to 2009

# get all in force years in a string
TRAINS$years.in.force <- apply(TRAINS[, c("date.implemented", "date.removed")], 1, FUN = function(x) paste0(seq(x[1],max(x)), collapse = ","))
TRAINS <- TRAINS %>% select(-c(date.removed, date.implemented))

TRAINS <- TRAINS %>% mutate(measure.id = 1:nrow(TRAINS)) #add unique id


## Names ---------------------
trains <- TRAINS
TRAINS <- trains

# clean affected names and move exceptions to separate column
TRAINS <- TRAINS %>%
  mutate(exceptions =  gsub(".*(?<=except)", "", affected.jurisdiction, ignore.case = T, perl = T)) %>%
  mutate(affected.jurisdiction = ifelse(grepl("except", affected.jurisdiction), gsub("(?<=except).*", "", affected.jurisdiction, ignore.case = T, perl = T), affected.jurisdiction)) %>%
  mutate(exceptions = ifelse(grepl("except", affected.jurisdiction), exceptions, "")) %>%
  mutate(affected.jurisdiction = gsub(" \\(except", "", affected.jurisdiction, perl = T))

# add GTA names to implementer
TRAINS <- merge(TRAINS, TRAINS.to.GTA.names[, c("UNCTAD.name", "GTA.name", "iso_code")], by.x = "implementing.jurisdiction", by.y = "UNCTAD.name")
TRAINS <- TRAINS %>% 
  mutate(implementing.jurisdiction = GTA.name) %>%
  select(-GTA.name) %>% 
  filter(iso_code %in% selected.countries)

#get single affected and remove world affected
TRAINS <- cSplit(TRAINS, "affected.jurisdiction", direction = "long")
TRAINS$is.world <- ifelse(TRAINS$affected.jurisdiction == "World", 1, 0)
TRAINS.non.world <- TRAINS %>% filter(is.world == 0)
TRAINS.non.world <- unique(TRAINS.non.world$measure.id)

TRAINS$is.world <- ifelse(TRAINS$affected.jurisdiction == "World" & 
                            !TRAINS$measure.id %in% TRAINS.non.world,
                          1,0)
TRAINS$exceptions <- NULL

# remove world to merge GTA names
TRAINS.world <- TRAINS %>% filter(is.world == 1)
TRAINS.non.world <- TRAINS %>% 
  filter(is.world == 0) %>% 
  filter(affected.jurisdiction != "World") #remove World where already other countries affected


# The affected countries have a different format. So, ISO codes are looked up for the using GPT 4 (w.Bing). 
translate.affected <- read.csv(file = paste0(path.data.raw, "UNCTAD_affected_to_iso.csv"), encoding = "Latin-1")
translate.affected$Index <- NULL
names(translate.affected)[2] <- "ISO_affected"

#Add Eu countries
eu <- country.names[country.names$is.eu, c("iso_code", "name")]
eu <- eu %>% filter(name != "United Kingdom") %>%
  mutate("EU" = "EU")%>%
  rename("ISO_EU" = "iso_code")

#deal with special characters
TRAINS.non.world$affected.jurisdiction[TRAINS.non.world$affected.jurisdiction == "Türkiye"] <- "Turkiye"
TRAINS.non.world$affected.jurisdiction[TRAINS.non.world$affected.jurisdiction == "Turkey"] <- "Turkiye"
TRAINS.non.world$affected.jurisdiction[TRAINS.non.world$affected.jurisdiction == "Côte d'Ivoire"] <- "Cote d'Ivoire"
TRAINS.non.world$affected.jurisdiction[TRAINS.non.world$affected.jurisdiction == "Réunion"] <- "Reunion"
TRAINS.non.world$affected.jurisdiction[TRAINS.non.world$affected.jurisdiction == "Curaçao"] <- "Curacao"

#convert to iso
TRAINS.non.world <- merge(TRAINS.non.world, translate.affected,
                          by.x = "affected.jurisdiction", 
                          by.y = "Country")

TRAINS.non.world <- TRAINS.non.world %>% 
  filter(!is.na(ISO_affected))

#add eu
TRAINS.non.world <- merge(TRAINS.non.world, eu, by.x = "ISO_affected", by.y = "EU", all.x = T, allow.cartesian = T)
TRAINS.non.world$ISO_affected <- ifelse(TRAINS.non.world$ISO_affected == "EU", TRAINS.non.world$ISO_EU, TRAINS.non.world$ISO_affected)
TRAINS.non.world <- TRAINS.non.world %>% 
  select(-c(name, ISO_EU)) %>%
  filter(ISO_affected %in% selected.countries)%>%
  left_join(country.names[, c("name", "iso_code")], by = c("ISO_affected" =  "iso_code")) %>%
  mutate(affected.jurisdiction = name)%>%
  select(-name) #add GTA names


rm(eu, translate.affected)
TRAINS.non.world$ISO_affected <- NULL
TRAINS <- rbind(TRAINS.world, TRAINS.non.world)


# aggreagate affected countries
converted.affected <- aggregate(data = TRAINS[, c("measure.id", "affected.jurisdiction")],  affected.jurisdiction ~ measure.id , FUN = function(x) paste0(x, collapse = ","))
TRAINS <- TRAINS %>%
  select(-affected.jurisdiction) %>%
  unique() %>%
  left_join(y = converted.affected, by = "measure.id")


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

## Affected  --------------------

world.affected <- readRDS(file = paste0(path.data.out, "TRAINS.affected.jurisdictions.Rds"))
names(world.affected) <- c("measure.id", "affected.world")

TRAINS <- TRAINS %>% 
  left_join(world.affected, by = "measure.id")

TRAINS$affected.jurisdiction <- ifelse(TRAINS$affected.jurisdiction == "World", 
                                       TRAINS$affected.world, 
                                       TRAINS$affected.jurisdiction)

TRAINS <- TRAINS %>% 
  select(-affected.world) %>%
  filter(!is.na(affected.jurisdiction))

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
  filter(chapter %in% c("A","B", "D"))
TRAINS$chapter <- ifelse(TRAINS$chapter == "A", "AB", TRAINS$chapter)

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

rm(TRAINS.to.GTA.names, converted.chapter, trains, TRAINS, test, world.conversion, world.conversion.2, converted.affected)





## aggregate ------------

TRAINS <- readRDS(file = paste0(path.data.out, "TRAINS_asymmetric_isic.RData"))

TRAINS <- cSplit(TRAINS, "affected.jurisdiction", direction = "long")
TRAINS$iso_code <- NULL
TRAINS <- to_iso(TRAINS, "implementing.jurisdiction", "affected.jurisdiction")
TRAINS <- to_alphabeta(TRAINS, "implementing.jurisdiction", "affected.jurisdiction")

TRAINS <- unique(cSplit(TRAINS, "chapter", direction = "long"))
TRAINS <- unique(cSplit(TRAINS, "years.in.force", direction = "long"))

data.out <- data.frame()

for(i in years){ #aggregate by year to ease computational burden
  data.loop <- TRAINS %>% filter(years.in.force == i)
  
  data.loop <- aggregate(data = data.loop, measure.id ~ implementing.jurisdiction + affected.jurisdiction + years.in.force + chapter + mast.chapter, FUN = function(x) length(unique(x)))
  
  data.out <- rbind(data.out, data.loop)
}

#TRAINS <- aggregate(data = TRAINS, measure.id ~ implementing.jurisdiction + affected.jurisdiction + years.in.force + chapter, FUN = function(x) length(unique(x)))
names(data.out) <- c("country.1", "country.2", "year","chapter","mast.chapter",  "number.of.interventions")
data.out$chapter <- ifelse(data.out$chapter == "A", "AB", data.out$chapter)
data.out$number.of.interventions <- ifelse(is.na(data.out$number.of.interventions), 0, data.out$number.of.interventions)
data.out <- pivot_wider(data.out, id_cols = 1:4, names_from = "chapter", values_from = "number.of.interventions")
data.out$AB <- ifelse(is.na(data.out$AB), 0, data.out$AB)
data.out$D <- ifelse(is.na(data.out$D), 0, data.out$D)
data.out$GTT <- data.out$AB + data.out$D
data.out <- pivot_longer(data.out, cols = 5:ncol(data.out), names_to = "chapter", values_to = "number.of.interventions")
data.out <- pivot_wider(data.out, id_cols = c("country.1", "country.2", "year", "chapter"), names_from = "mast.chapter", values_from = "number.of.interventions")
data.out[is.na(data.out)] <- 0
data.out$total <- apply(data.out[,5:ncol(data.out)],1,FUN = sum)

# add pairs with 0 interventions associated
grid$chapter <- as.character(grid$chapter)
# test <- merge(grid, data.out, by = c("country.1", "country.2", "year","chapter"), all.x = T, all.y = T)
# nrow(grid)- nrow(data.out) + sum(is.na(data.out$number.of.interventions)) == sum(is.na(test$number.of.interventions))
data.out <- merge(grid, data.out, by = c("country.1", "country.2", "year","chapter"), all.x = T, all.y = T)
data.out[, 5:ncol(data.out)][is.na(data.out[, 5:ncol(data.out)])] <- 0


saveRDS(data.out, file = paste0(path.data.out, 
                              "TRAINS_symmetric_isic.RData"))


rm(TRAINS, data.loop, data.out)
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
  filter(chapter %in% c("A","B", "D"))

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

WTO <- aggregate(data = WTO, measure.id ~ implementing.jurisdiction + affected.jurisdiction + years.in.force + chapter, FUN = function(x) length(unique(x)))
names(WTO) <- c("country.1", "country.2", "year","chapter", "number.of.interventions")


## Names ---------------

# add ISO codes
WTO <- merge(WTO, wto.names.to.iso, by.x = "country.1", by.y = "Name")
names(WTO)[ncol(WTO)] <- "ISO_country.1"


WTO <- merge(WTO, wto.names.to.iso, by.x = "country.2", by.y = "Name", all.x = T)
names(WTO)[ncol(WTO)] <- "ISO_country.2"

WTO$country.2 <- NULL
WTO$country.1 <- NULL

## Save
writexl::write_xlsx(WTO, path = paste0(path.data.out, 
                                       "WTO_symmetric_isic.xlsx"))
saveRDS(WTO, file = paste0(path.data.out, "WTO_symmetric_isic.RData"))

#WTO <- readRDS(file = paste0(path.data.out, "WTO_symmetric_isic.RData"))

 # 4. GTA -----------------------------------------------------------------------

gta_data_slicer(data.path = paste0(path.data.raw, "master_plus.Rdata"))

GTA <- master.sliced %>% 
  filter(gta.evaluation != "Green" & !is.na(a.un))%>%
  mutate(mast.chapter = as.character(mast.chapter))%>%
  filter(mast.chapter %in% selected.mast) %>%
  select(-c(a.un, i.un, title, date.announced, affected.sector, i.atleastone.G20, a.atleastone.G20))

GTA <- to_iso(GTA, "implementing.jurisdiction", "affected.jurisdiction")
GTA <- GTA %>% filter(implementing.jurisdiction %in% selected.countries & affected.jurisdiction %in% selected.countries)
## Dates ------------------
GTA$date.implemented[is.na(GTA$date.implemented)] <- "1900-01-01" #NAs come from dates before 1900
GTA <- GTA %>% 
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
GTA$years.in.force <- apply(GTA[, c("date.implemented", "date.removed")], 1, FUN = function(x) paste0(seq(x[1],max(x)), collapse = ","))
GTA <- GTA %>% select(-c(date.removed, date.implemented))


## ISIC --------------------

GTA <- cSplit(GTA, "affected.product", direction = "long")
GTA$affected.product <- substr(GTA$affected.product, 1,2)

GTA <- unique(GTA)
# Indonesia has Measures affecting HS 98 and 99. They could not be matched to ISIC (see code "96 generate help files.R")
# Therefore, they are matched manually to chapter C ("Mining and quarrying") in ISIC 3. It only affects 10 interventions. 
#hs.to.isic <- rbind(hs.to.isic, c(NA,98,"C"), c(NA,99,"C"))

#get proper names, isic chapter and reduce dataframe
GTA <- merge(GTA, hs.to.isic[, c("hs.code", "chapter")], 
             by.x = "affected.product", 
             by.y = "hs.code")

GTA$chapter <- ifelse(GTA$chapter %in% c("A", "B"), "AB", GTA$chapter)

GTA <- GTA %>% 
  select(-c(affected.product)) %>%
  unique()%>%
  filter(chapter %in% c("AB","D"))

converted.chapter <- unique(aggregate(data = GTA[, c("intervention.id","implementing.jurisdiction", "affected.jurisdiction", "chapter")],  chapter ~ intervention.id + implementing.jurisdiction + affected.jurisdiction, FUN = function(x) paste0(unique(x), collapse = ",")))

GTA <- GTA %>%
  select(-chapter) %>%
  unique() %>%
  left_join(y = converted.chapter, by = c("intervention.id" ,"implementing.jurisdiction", "affected.jurisdiction"))


## Save
writexl::write_xlsx(GTA, path = paste0(path.data.out, 
                                          "GTA_asymmetric_isic.xlsx"))
saveRDS(GTA, file = paste0(path.data.out, 
                              "GTA_asymmetric_isic.RData"))


## aggregate --------
GTA <- readRDS(file = paste0(path.data.out, "GTA_asymmetric_isic.RData"))

GTA <- to_alphabeta(GTA, "implementing.jurisdiction", "affected.jurisdiction")


GTA <- unique(cSplit(GTA, "chapter", direction = "long"))
GTA <- unique(cSplit(GTA, "years.in.force", direction = "long"))

data.out <- data.frame()

for(i in years){
  data.loop <- GTA %>% filter(years.in.force == i)
  
  data.loop <- aggregate(data = data.loop, intervention.id ~ implementing.jurisdiction + affected.jurisdiction + years.in.force + chapter + mast.chapter, FUN = function(x) length(unique(x)))
  
  data.out <- rbind(data.out, data.loop)
}

names(data.out) <- c("country.1", "country.2", "year","chapter","mast.chapter",  "number.of.interventions")

data.out <- pivot_wider(data.out, id_cols = 1:4, names_from = "chapter", values_from = "number.of.interventions")
data.out$AB <- ifelse(is.na(data.out$AB), 0, data.out$AB)
data.out$D <- ifelse(is.na(data.out$D), 0, data.out$D)
data.out$GTT <- data.out$AB + data.out$D
data.out <- pivot_longer(data.out, cols = 5:ncol(data.out), names_to = "chapter", values_to = "number.of.interventions")
data.out <- pivot_wider(data.out, id_cols = c("country.1", "country.2", "year", "chapter"), names_from = "mast.chapter", values_from = "number.of.interventions")
data.out[is.na(data.out)] <- 0
data.out$total <- apply(data.out[,5:ncol(data.out)],1,FUN = sum)



# test <- merge(grid, data.out, by = c("country.1", "country.2", "year","chapter"), all.x = T, all.y = T)
# nrow(grid)- nrow(data.out) + sum(is.na(data.out$number.of.interventions)) == sum(is.na(test$number.of.interventions))
# add pairs with 0 interventions associated
data.out <- merge(grid, data.out, by = c("country.1", "country.2", "year","chapter"), all.x = T)
data.out[, 5:ncol(data.out)][is.na(data.out[, 5:ncol(data.out)])] <- 0

saveRDS(data.out, file = paste0(path.data.out, 
                                "GTA_symmetric_isic.RData"))








# 6. Control variables -------------------------------------------
rm(list = ls())
source("BA_Thesis_code/00 Terms and Definitions.R")

library(imputeTS)
library(cepiigeodist)
## WB LPI ----------------------------------------------------------------------
data.out <- data.frame()
num.sheets <- length(excel_sheets( paste0(path.data.raw, "WB_LPI.xlsx")))
names.sheets <- excel_sheets( paste0(path.data.raw, "WB_LPI.xlsx"))

for(i in 1:num.sheets){
  WB.lpi <- readxl::read_xlsx(paste0(path.data.raw, "wb_lpi.xlsx"), skip = 2, sheet = i)
  
  WB.lpi <- WB.lpi %>% 
    select(Code, score...3) %>%
    mutate(year = names.sheets[i])
  data.out <- rbind(WB.lpi, data.out)

}

WB.lpi <- pivot_wider(data.out, names_from = "year", values_from = "score...3")

for(i in 2007:max(years)){
  if(!((i + 1) %in% names(WB.lpi)))
    eval(parse(text = paste0("WB.lpi <- add_column(WB.lpi, '",i+1,"' = ",NA,", .after = '",i,"')")))
  
}
WB.lpi$`2006` <- WB.lpi$`2007`
WB.lpi$`2005` <- WB.lpi$`2007`


WB.lpi[WB.lpi$Code == "TMP", "2020"] <- WB.lpi[WB.lpi$Code == "TMP", "2007"]
WB.lpi  <- WB.lpi %>%
  filter(!Code %in% c("YUG")) %>%
  pivot_longer(cols = 2:ncol(WB.lpi), names_to = "year") %>%
  group_by(Code) %>%
  mutate(value = imputeTS::na_interpolation(value))


WB.lpi <- WB.lpi %>% 
  filter(Code %in% selected.countries) %>%
  filter(year %in% years)


names(WB.lpi) <- c("country.1", "year", "lpi")
rm(names.sheets, num.sheets, data.out,i)

## WB GDP cap ppp --------------------------------------------------------------
WB.gdp.cap.ppp <- read.csv(paste0(path.data.raw, "WB_GDP_cap_ppp.csv"))
WB.gdp.cap.ppp <- WB.gdp.cap.ppp[, c(2, 5:ncol(WB.gdp.cap.ppp))]
names(WB.gdp.cap.ppp) <- c("ISO", 2007:2021)
WB.gdp.cap.ppp <- pivot_longer(WB.gdp.cap.ppp, cols = 2:ncol(WB.gdp.cap.ppp), names_to = "year", values_to = "gdp.cap.ppp")

WB.gdp.cap.ppp <- WB.gdp.cap.ppp %>% 
  filter(ISO %in% selected.countries) %>%
  filter(year %in% years)
WB.gdp.cap.ppp$gdp.cap.ppp <- as.numeric(WB.gdp.cap.ppp$gdp.cap.ppp)

## UNCTAD Liner shipping index ---------------
TRAINS.to.GTA.names <- readxl::read_xlsx(path = paste0(path.data.out, "UNCTAD_GTA_conversion.table.xlsx"))
UNCTAD.LSCI <- read.csv(paste0(path.data.raw, "UNCTAD_LSCI_3.csv"), skip = 2)
UNCTAD.LSCI <- UNCTAD.LSCI[-1,]
UNCTAD.LSCI[, 2:ncol(UNCTAD.LSCI)] <- apply(UNCTAD.LSCI[, 2:ncol(UNCTAD.LSCI)],2, FUN = as.numeric)
UNCTAD.LSCI <- pivot_longer(UNCTAD.LSCI, cols = 2:ncol(UNCTAD.LSCI), names_to = "year_quarter", values_to = "LSI")
UNCTAD.LSCI$year <- substr(UNCTAD.LSCI$year_quarter, 4,8)
UNCTAD.LSCI <- aggregate(LSI ~ year + QUARTER, UNCTAD.LSCI, mean)

t <- expand.grid(years, TRAINS.to.GTA.names$GTA.name)
names(t) <- c("year", "GTA.name")
TRAINS.to.GTA.names <- merge(TRAINS.to.GTA.names, t, by = "GTA.name")
UNCTAD.LSCI <- merge(UNCTAD.LSCI, TRAINS.to.GTA.names, by.x = c("QUARTER", "year"), by.y = c("UNCTAD.name", "year"), all.y = T)
UNCTAD.LSCI$LSI <- ifelse(is.na(UNCTAD.LSCI$LSI), 0, UNCTAD.LSCI$LSI) # the shipping index is Na for landlocked countries, therefore set to 0

UNCTAD.LSCI$QUARTER <- NULL
UNCTAD.LSCI$un_code <- NULL

iso.conversion <- rbind(country.names[, c("name", "iso_code")], c("EU", "EU"))
UNCTAD.LSCI <- merge(UNCTAD.LSCI, iso.conversion[, c("name", "iso_code")], by.x = "GTA.name", by.y = "name", all.x = T)
UNCTAD.LSCI$GTA.name <- NULL


rm(t, TRAINS.to.GTA.names)

## CEPII Gravity controls ------------------------------------------------------
controls <- readRDS(paste0(path.data.raw, "CEPII_Gravity_Variables.Rds")) 
CEPII.landlocked <- cepiigeodist::geo_cepii[, c("iso3", "landlocked")] ## Landlocked

# reduce controls
controls <- controls %>% 
  filter(year %in% years & country_exists_o & country_exists_d)%>%
  select(c("year","country_id_o","country_id_d","iso3_o","iso3_d","iso3num_o", 
           "distw_harmonic","contig","diplo_disagreement","scaled_sci_2021",
           "comlang_off","comlang_ethno","comcol","comrelig","heg_o","heg_d",
           "col_dep_ever", "gatt_o","gatt_d","wto_o","wto_d","eu_o","eu_d",
           "fta_wto","fta_wto_raw","entry_tp_o","entry_tp_d", "gdp_o", "gdp_d", 
           "gdpcap_ppp_o", "gdpcap_ppp_d", "entry_cost_o", "entry_cost_d", "dist")) %>%#check distance measure
  filter(iso3_d != iso3_o) %>%
  filter(iso3_o %in% selected.countries & iso3_d %in% selected.countries)
  

# WB.lpi
names(WB.lpi) <- c("iso3_o", "year", "lpi_o")
controls <- merge(controls, WB.lpi, by = c("iso3_o", "year"), all.x = T)
names(WB.lpi) <- c("iso3_d", "year", "lpi_d")
controls <- merge(controls, WB.lpi, by = c("iso3_d", "year"), all.x = T)

# WB.gdp.cap.ppp
names(WB.gdp.cap.ppp) <- c("iso3_o", "year", "gdp.cap.ppp_o")
controls <- merge(controls, WB.gdp.cap.ppp, by = c("iso3_o", "year"), all.x = T)
names(WB.gdp.cap.ppp) <- c("iso3_d", "year", "gdp.cap.ppp_d")
controls <- merge(controls, WB.gdp.cap.ppp, by = c("iso3_d", "year"), all.x = T)

# UNCTAD LSCI
names(UNCTAD.LSCI) <- c("year", "lsci_o","iso3_o") ###CHECK FOR COLLINEARITY WITH LANDLOCKED
controls <- merge(controls, UNCTAD.LSCI, by = c("iso3_o", "year"), all.x = T)
names(UNCTAD.LSCI) <- c("year", "lsci_d","iso3_d")
controls <- merge(controls, UNCTAD.LSCI, by = c("iso3_d", "year"), all.x = T)

# Landlocked
names(CEPII.landlocked) <- c("iso3_o", "landlocked_o")
controls <- merge(controls, CEPII.landlocked, by = c("iso3_o"), all.x = T)
names(CEPII.landlocked) <- c("iso3_d", "landlocked_d")
controls <- merge(controls, CEPII.landlocked, by = c("iso3_d"), all.x = T)


# Intranational Trade flows
load(file = paste0(path.data.raw, "Final goods support table.Rdata"))
exports <- aggregate(data = final, Value  ~ Year + Reporter.jurisdiction, FUN = sum)
exports <- exports %>%
  filter(Year %in% years) %>%
  left_join(country.names[, c("name", "iso_code")], by = c("Reporter.jurisdiction" =  "name")) 
exports$Reporter.jurisdiction <- NULL


controls <- controls %>% 
  left_join(exports, by = c("country_id_o" = "iso_code", "year" = "Year")) %>%
  rename( "exports_o" = "Value") %>%
left_join(exports, by = c("country_id_d" = "iso_code", "year" = "Year")) %>%
  rename( "exports_d" = "Value")

controls <- controls %>% #get intranational trade flows by substracting exports from GDP
  mutate(xii_o = gdp_o - exports_o) %>% 
  mutate(xii_d = gdp_d - exports_d)

#only get necessary controlls
controls <- controls %>% 
  filter(substr(country_id_o, nchar(country_id_o), nchar(country_id_o)) != 1 & #old countries have missing values
           substr(country_id_d, nchar(country_id_d), nchar(country_id_d)) != 1 
           )

#make controlls bilateral
controls$landlocked <- ifelse(controls$landlocked_d == 1 | 
                                controls$landlocked_o == 1,1,0)

controls$gdp.cap.ppp <- apply(controls[ , c("gdp.cap.ppp_d", "gdp.cap.ppp_o")], 1, FUN = function(x) exp(mean(log(x))))
controls$lsci <- apply(controls[ , c("lsci_d", "lsci_o")], 1, FUN = function(x) exp(mean(log(x))))
controls$lpi <- apply(controls[ , c("lpi_d", "lpi_o")], 1, FUN = function(x) exp(mean(log(x))))

controls <- controls %>% select(-c(landlocked_o, landlocked_d, gdp.cap.ppp_d, gdpcap_ppp_o, lsci_o, lsci_d, lpi_d, lpi_o))
saveRDS(controls, file = paste0(path.data.out, "Controls cleaned CEPII.RData"))

controls <- controls %>% select(iso3_d, iso3_o, year, distw_harmonic, contig, diplo_disagreement, comlang_ethno, comlang_off, comcol, comrelig, fta_wto, dist, gdp.cap.ppp, landlocked, lsci, lpi, exports_o, exports_d, gdp_d, gdp_o)
controls <- unique(to_alphabeta(controls, "iso3_d", "iso3_o"))

controls <- merge(grid, controls, by.x = c("country.1", "country.2", "year"), by.y = c("iso3_d", "iso3_o", "year"))
saveRDS(controls, file = paste0(path.data.out, "Controls cleaned CEPII grid.RData"))



