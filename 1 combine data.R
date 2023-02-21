# Author: Silvan Hofer
# Date: 17. February 2023
# Purpose: 
# This script combines data and makes it ready for analysis.
# 
rm(list = ls())

years <- 2009:2019

# Paths

path.data.raw <- "0 data raw/"
path.data.out <- "1 data processed/"


# 1. Load data -----------------------------------------------------------------
# 
trade.costs <- readRDS(file = paste0(path.data.out, "Trade Costs Processed.RData"))
WTO <- readRDS(file = paste0(path.data.out, "WTO.RData"))
TRAINS <- readRDS(file = paste0(path.data.out, "TRAINS cleaned.RData"))
