# Author: Silvan Hofer
# Date: 6. February 2023
# Purpose: 
# This script cleans,  standardizes, and combines data
# from various sources (CEPII, GTA, UNCTAD, WB) for analysis in 1 data analysis.

setwd("..") # move up one
rm(list = ls())

library(readxl)
library(tidyr)
library(magrittr)
library(dplyr)
library(xml2) #webscraping WTO names table
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

if(F){ # get WTO conversion table, only exectue if new version is necessary
  # Get WTO country names to ISO conversions
  # download html and get table
  wto.names.to.iso <- "https://docs.wto.org/gtd/Default.aspx?pagename=WTOIsocodes&langue=e"
  wto.names.to.iso <- xml2::read_html(wto.names.to.iso)
  wto.names.to.iso <- rvest::html_table(wto.names.to.iso)[[4]] %>% 
    tibble::as_tibble(.name_repair = "unique")
  
  # clean table
  wto.names.to.iso <- wto.names.to.iso[1:97, 1:4]
  names(wto.names.to.iso) <- c("Name", "ISO", "Name", "ISO")
  wto.names.to.iso <- rbind(wto.names.to.iso[, 1:2], wto.names.to.iso[, 3:4])
  wto.names.to.iso <- na.omit(wto.names.to.iso)
  wto.names.to.iso$Name <- gsub("(\r|\n|\t)", "", wto.names.to.iso$Name)
  wto.names.to.iso$ISO <- gsub("(\r|\n|\t)", "", wto.names.to.iso$ISO)
  
  # adjust some values manually
  wto.names.to.iso[wto.names.to.iso$Name == "Eswatini (formerly Swaziland)", "Name"] <- "Eswatini"
  wto.names.to.iso[wto.names.to.iso$Name == "European Union formerly European Communities", "Name"] <- "European Union"
  wto.names.to.iso[wto.names.to.iso$ISO == "EUEEC", "ISO"] <- "EU"
  wto.names.to.iso[wto.names.to.iso$Name == "Romania formerly Romania", "Name"] <- "Romania"
  wto.names.to.iso[wto.names.to.iso$ISO == "ROUROM", "ISO"] <- "ROU"
  wto.names.to.iso[wto.names.to.iso$ISO == "TPKM", "ISO"] <- "TWN"
  
  writexl::write_xlsx(wto.names.to.iso, path = paste0(path.data.out, "WTO ISO conversions.xlsx"))
}

wto.names.to.iso <- readxl::read_xlsx(path = paste0(path.data.out, 
                                                    "WTO ISO conversions.xlsx"))





## 2.4 CEPII Gravity control variables ----------------------------------------


# 3. save processed data -------------------------------------------------------


saveRDS(trade.costs, file = paste0(path.data.out, 
                                   "Trade Costs Processed.RData"))




