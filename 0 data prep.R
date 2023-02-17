# Author: Silvan Hofer
# Date: 6. February 2023
# Purpose: 
# This script cleans and standardizes data
# from various sources (CEPII, GTA, UNCTAD, WB) for analysis in 1 data analysis.

# The data will be standardised in the following: 
# 1. All product codes will be translated into ISIC 3.1.
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

rm(sheet)

## Clean

trade.costs <- trade.costs %>% filter(year >= min(years) & year <= max(years))

#DROP HALF SAMPLE TO AVOID REPETITON

# trade.costs[trade.costs == ".."] <- NA #.. denotes no value, change to NA
# names(trade.costs) <- gsub("\\[(.*)\\]","", names(trade.costs), perl = T)
# trade.costs <- pivot_longer(trade.costs, 
#                             7:ncol(trade.costs), 
#                             names_to  = "year", 
#                             values_to = "tij")

# Save
saveRDS(trade.costs, file = paste0(path.data.out, 
                                   "Trade Costs Processed.RData"))

rm(sheet, trade.costs)
# 2. TRAINS ------------------------------------------------------------------

## Load
TRAINS <- read.csv(paste0(path.data.raw, "UNCTAD_TRAINS_database.csv"))
hs.to.isic <- readxl::read_xlsx(paste0(path.data.out, "ISIC to HS 2 digits conversion.xlsx"))
TRAINS.to.GTA.names <- readxl::read_xlsx(path = paste0(path.data.out, "UNCTAD_GTA_conversion.table.xlsx"))
TRAINS.to.GTA.names <- rbind(TRAINS.to.GTA.names, c("World" ,"World", NA))
## Clean 
names(TRAINS) <- c("implementing.jurisdiction","affected.jurisdiction",
                   "mast.chapter","description", "product.description",
                   "hs.code", "issuing.agency","regulation.title",
                   "regulation.symbol","date.implemented",
                   "official.regulation.document","official.title.original.language",     
                   "measure.description.original.language", "product.description.original.language",
                   "supporting/related.regulations","measure.objective",
                   "years.of.data.collection","date.removed")

TRAINS <- TRAINS %>% select(-c(product.description.original.language, 
                               official.title.original.language, 
                               `supporting/related.regulations`, 
                               years.of.data.collection, 
                               official.regulation.document,
                               measure.description.original.language,
                               regulation.symbol,
                               issuing.agency))



any(is.na(TRAINS$date.removed))
TRAINS <- TRAINS %>% 
  filter(is.na(date.removed) | as.Date(date.removed) > as.Date(paste0(years[1],"-01-01"))) %>%
  mutate(mast.chapter =  gsub("[0-9]", "", mast.chapter)) %>% #remove MAST subchapters
  mutate(hs.code =  gsub("[^0-9,]", "", hs.code, ignore.case = T))  #remove HS explenations

TRAINS <- TRAINS %>% mutate(measure.id = 1:nrow(TRAINS)) #add unique id
TRAINS <- unique(cSplit(TRAINS, "hs.code", direction = "long")) #some HS codes are double
TRAINS$hs.code <- substr(TRAINS$hs.code, 1,2)
TRAINS <- unique(TRAINS)


# Indonesia has Measures affecting HS 98 and 99. They could not be matched to ISIC (see code "96 generate help files.R")
# Therefore, they are matched manually to chapter C ("Mining and quarrying") in ISIC 3.1. It only affects 10 interventions. 
hs.to.isic <- rbind(hs.to.isic, c(NA,98,"C"), c(NA,99,"C"))


TRAINS <- merge(TRAINS, hs.to.isic[, c("hs.code", "chapter")], by = "hs.code")
TRAINS <- TRAINS %>% 
  select(-c(hs.code, description, product.description, regulation.title)) %>%
  unique()%>%
  filter(chapter %in% c("A", "D"))


TRAINS <- merge(TRAINS, TRAINS.to.GTA.names, by.x = "implementing.jurisdiction", by.y = "UNCTAD.name", all.x = T)
TRAINS <- cSplit(TRAINS, "affected.jurisdiction", direction = "long")


countries.iso <- country.names[, c("name", "iso_code")]
countries.iso <- rbind(countries.iso, c("European Union", "EU"))

test <- TRAINS %>% 
  select(implementing.jurisdiction)%>%
  unique()
test <- merge()

# add ISO codes
TRAINS <- merge(TRAINS, wto.names.to.iso, by.x = "Member/Observer", by.y = "Name")
names(TRAINS)[ncol(TRAINS)] <- "ISO_Observer/Member"

TRAINS <- merge(TRAINS, wto.names.to.iso, by.x = "Trading partners", by.y = "Name", all.x = T)
names(TRAINS)[ncol(TRAINS)] <- "ISO_Trading partner"



## Save
writexl::write_xlsx(TRAINS, path = paste0(path.data.out, 
                                          "TRAINS cleaned.xlsx"))
saveRDS(TRAINS, file = paste0(path.data.out, 
                                   "TRAINS cleaned.RData"))


# 3. WTO TMDB ----------------------------------------------------------------

## Load
WTO <- readxl::read_xlsx(path = paste0(path.data.raw, 
                                "WTO_NTMs_Trade_Monitoring_Database.xlsx"))

# Load in conversions
wto.names.to.iso <- readxl::read_xlsx(path = paste0(path.data.out, 
                                                    "WTO ISO conversions.xlsx"))
isic.chapters <- readxl::read_xlsx(path = paste0(path.data.out, 
                                                 "ISIC chapter codes.xlsx"))


## Clean
#restrict dataset
WTO <- WTO %>% 
  filter(`Measure class` == "Restrictive"& #only get restrictive measures
           !is.na(Products))%>% #only get measures that have product codes
  select(-`Measure class`) 


# split trading partners and get partners w. individual termination date
WTO <- cSplit(WTO, "Trading partners", sep = ";", direction = "long")
WTO$termin.p.partner <- ifelse(grepl("\\((.*)\\)", WTO$`Trading partners`),
                               as.character(WTO$`Trading partners`),NA)


# get termination date
WTO$termin.p.partner <- as.character(strapplyc(WTO$termin.p.partner, 
                                               "[0-9]{2}/[0-9]{2}/[0-9]{4}", 
                                               simplify = TRUE))
WTO$termin.p.partner <- ifelse(WTO$termin.p.partner == "character(0)", NA, 
                               WTO$termin.p.partner)

#add individual termination date to general termination date
WTO <- WTO %>%  
  mutate(Terminated = as.Date(Terminated))%>%
  mutate(termin.p.partner = as_date(termin.p.partner, format = '%d/%m/%Y'))%>%
  mutate(Terminated = if_else(!is.na(termin.p.partner), 
                              termin.p.partner, 
                              Terminated))%>%
  mutate(`Trading partners` = gsub(pattern = " \\((.*)\\)",
                                   replacement = "",
                                   `Trading partners`))%>%
  select(-termin.p.partner )


# add ISO codes
WTO <- merge(WTO, wto.names.to.iso, by.x = "Member/Observer", by.y = "Name")
names(WTO)[ncol(WTO)] <- "ISO_Observer/Member"

WTO <- merge(WTO, wto.names.to.iso, by.x = "Trading partners", by.y = "Name", all.x = T)
names(WTO)[ncol(WTO)] <- "ISO_Trading partner"


# only get active measures during observation period
WTO <- WTO%>%filter((is.na(Terminated)|Terminated < as.Date(paste0(max(years), "-12-31"))) &
                      `Implemented at` > as.Date(paste0(min(years), "-01-01"))&
                      `Implemented at` < as.Date(paste0(max(years), "-12-31")))


#go trough all hs products and convert them to ISIC
WTO$ISIC <- NA

for(i in 1:length(WTO$Products)){
  prod <- WTO$`Product chapters`[i]
  if(!is.na(prod)){
    t <- unlist(str_split(prod, pattern = ",")) #get single HS codes
    t <- gsub(" ", "", t)
    t <- ifelse(nchar(t) == 1, paste0("0",t), t)
    # t <- gta_hs_vintage_converter(t) #convert to HS2012
    # t <- ifelse(nchar(t) == 5, paste0(0, t),#add leading zeros
    #         ifelse(nchar(t) == 4, paste0("00", t),
    #         t))
    t <- data.frame(unique(concord_hs_isic(t, #convert to ISIC
                                           origin = "HS4", 
                                           destination = "ISIC3", 
                                           dest.digit = 2
    )))
    names(t) <- "code"
    t <- merge(t, isic.chapters, by = "code") #get ISIC chapters
    WTO$ISIC[i] <- paste0(unique(t$chapter), collapse = ",") #save
  }else{
    
  }
}

#remove unnecessary variables
WTO <- WTO %>% select(-c(Source, Status, Description, `Product chapters`, Products))


## Save
writexl::write_xlsx(WTO, path = paste0(path.data.out, 
                                       "WTO cleaned.xlsx"))

# 4. CEPII Gravity control variables ----------------------------------------


# Gravity controls
controls <- readRDS(paste0(path.data.raw, "CEPII_Gravity_Variables.Rds")) 
controls <- controls %>% filter(year %in% years &
                                country_exists_o&
                                country_exists_d)

# General controls
distance.variables <- read_dta(paste0(path.data.raw, 
                                      "CEPII_Distance_Variables.dta"))

geo.variables <- read_dta(paste0(path.data.raw, 
                                      "CEPII_Geo_Variables.dta"))







