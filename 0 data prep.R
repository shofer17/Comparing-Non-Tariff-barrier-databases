# Create the cleaned data for the regressions

setwd("..") #move up one

rm(list = ls())
# Libraries
library(readxl)
library(tidyverse)
library(haven) #dta files

# Parameters


# Paths

path.data.raw <- "data raw/"
path.data.out <- "data processed/"

# 1. Load data -----------------------------------------------------------------

trade.costs <- read_csv(paste0(path.data.raw, "ESCAP-WB-tradecosts-dataset-complete-2018.csv"))
UNCTAD <- read_dta(paste0(path.data.raw, "UNCTAD_NTM_hs6_2010_2019_clean_v.12.dta"))

# 2. Prep data -----------------------------------------------------------------

trade.costs[trade.costs == ".."] <- NA #.. denotes no value, change to NA
names(trade.costs) <- gsub("\\[(.*)\\]","", names(trade.costs), perl = T) # change weird year name format

trade.costs <- pivot_longer(trade.costs, 7:ncol(trade.costs), names_to = "year", values_to = "tij") #bring into longer format for matching




