# Create the cleaned data for the regressions

setwd("..") #move up one
rm(list = ls())

# Libraries
library(readxl)
library(tidyr)
library(magrittr)
library(dplyr)

# old version to avoid bug with memory
# devtools::install_version("haven", version = "1.1.0") 
library(haven) #dta files

# Parameters

years <- c(2009, 2019)

# Paths

path.data.raw <- "0 data raw/"
path.data.out <- "1 data processed/"

# 1. Load data -----------------------------------------------------------------



#Missing values represent infinite trade costs, meaning countries do not trade
sheets <- c("AB", "D", "GTT")
trade.costs <- data.frame()

for(i in sheets){
  sheet <- read_xlsx(paste0(path.data.raw, 
                            "ESCAP-WB-tradecosts-dataset-20220509.xlsx"), 
                     sheet = i)
  
  trade.costs    <- rbind(trade.costs, sheet)
};rm(sheet)

# trade.costs: 
# Sector: ISIC rev. 3


UNCTAD <- read_dta(paste0(path.data.raw, 
                          "UNCTAD_NTM_hs6_2010_2019_clean_v.12.dta"))
#see UNCTAD TRAINS Database manual 
#(https://unctad.org/system/files/official-document/ditctab2017d3_en.pdf)


distance.variables <- read_dta(paste0(path.data.raw, 
                                      "CEPII_Distance_Variables.dta"))

geo.variables <- read_dta(paste0(path.data.raw, 
                                      "CEPII_Geo_Variables.dta"))


# 2. Prep data -----------------------------------------------------------------

#trade costs
trade.costs <- trade.costs %>% filter(year >= min(years) & year <= max(years))
test <- na.omit(trade.costs)

#DROP HALF SAMPLE TO AVOID REPETITON

# trade.costs[trade.costs == ".."] <- NA #.. denotes no value, change to NA
# names(trade.costs) <- gsub("\\[(.*)\\]","", names(trade.costs), perl = T)
# trade.costs <- pivot_longer(trade.costs, 
#                             7:ncol(trade.costs), 
#                             names_to  = "year", 
#                             values_to = "tij")

#TRAINS

test <- UNCTAD %>% filter("StartDate" >= min(years))


# 3. save processed data -------------------------------------------------------

saveRDS(trade.costs, file = paste0(path.data.out, 
                                   "Trade Costs Processed.RData"))




