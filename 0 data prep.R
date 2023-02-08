# Author: Silvan Hofer
# Date: 6. February 2023
# Purpose: 
# This script cleans,  standardizes, and combines data
# from various sources (CEPII, GTA, UNCTAD, WB) for analysis in 1 data analysis.

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

# 1. Load data -----------------------------------------------------------------
## 1.1 UN ESCAP Trade cost data ------------------------------------------------


sheets <- c("AB", "D", "GTT") # Sector: ISIC rev.3
trade.costs <- data.frame()

for(i in sheets){ # takes a while
  sheet <- read_xlsx(paste0(path.data.raw, 
                            "ESCAP-WB-tradecosts-dataset-20220509.xlsx"), 
                     sheet = i)
  
  trade.costs    <- rbind(trade.costs, sheet)
  
}

rm(sheet)


## 1.2 UNCTAD NTM (vgl. TRAINS) ------------------------------------------------


UNCTAD <- read_dta(paste0(path.data.raw, #takes a while
                          "UNCTAD_NTM_hs6_2010_2019_clean_v.12.dta"))
# see UNCTAD TRAINS Database manual 
# https://unctad.org/system/files/official-document/ditctab2017d3_en.pdf


## 1.3 WTO TMDB ----------------------------------------------------------------


WTO <- readxl::read_xlsx(path = paste0(path.data.raw, 
                                "WTO_NTMs_Trade_Monitoring_Database.xlsx"))


## 1.4 CEPII Gravity control variables ----------------------------------------


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


# 2. Prep data -----------------------------------------------------------------
# The data will be standardised in the following: 
# 1. All product codes will be translated into CPC 2.1
# 2. Countries are specified by 3-digits ISO codes. 
# 3. All measures are classified as liberalising or restrictive





## MAKE DATE COMON FOR ALL DATA

## 2.1 UN ESCAP Trade cost data ------------------------------------------------

trade.costs <- trade.costs %>% filter(year >= min(years) & year <= max(years))

#DROP HALF SAMPLE TO AVOID REPETITON

# trade.costs[trade.costs == ".."] <- NA #.. denotes no value, change to NA
# names(trade.costs) <- gsub("\\[(.*)\\]","", names(trade.costs), perl = T)
# trade.costs <- pivot_longer(trade.costs, 
#                             7:ncol(trade.costs), 
#                             names_to  = "year", 
#                             values_to = "tij")

## 2.2 UNCTAD NTM (vgl. TRAINS) ------------------------------------------------


test <- UNCTAD %>% filter("StartDate" >= min(years))


## 2.3 WTO ---------------------------------------------------------------------

# Load in conversions
wto.names.to.iso <- readxl::read_xlsx(path = paste0(path.data.out, 
                                                    "WTO ISO conversions.xlsx"))
isic.chapters <- readxl::read_xlsx(path = paste0(path.data.out, 
                                                    "ISIC chapter codes.xlsx"))

# restrict dataset 
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
    t <- unlist(str_split(prod, pattern = ",")) #get single HS coes
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

writexl::write_xlsx(WTO, path = paste0(path.data.out, 
                                                 "WTO cleaned.xlsx"))

## 2.4 CEPII Gravity control variables ----------------------------------------


# 3. save processed data -------------------------------------------------------


saveRDS(trade.costs, file = paste0(path.data.out, 
                                   "Trade Costs Processed.RData"))




