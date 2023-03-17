# Author: Silvan Hofer
# Date: 17. February 2023
# Purpose: 
# This script combines data and makes it ready for analysis.
# 

setwd("..") # move up one

rm(list = ls())
install.packages("goft")
install.packages("fitdistrplus")
library(goft)
library(fitdistrplus)
library(gtalibrary)

years <- 2009:2019

# Paths

path.data.raw <- "0 data raw/"
path.data.out <- "1 data processed/"


# 1. Load data -----------------------------------------------------------------
# 
trade.costs <- readRDS(file = paste0(path.data.out, "Trade Costs Processed.RData"))
WTO.sym <- readRDS(file = paste0(path.data.out, "WTO_symmetric_isic.RData"))
TRAINS <- readRDS(file = paste0(path.data.out, "TRAINS_symmetric_isic.RData"))
controls <- readRDS(file = paste0(path.data.out, "Controls cleaned CEPII.RData"))




# add ISO codes
TRAINS <- merge(TRAINS, country.names[, c("name", "iso_code")], by.x = "country.1", by.y = "name", all.x = T)
names(TRAINS)[ncol(TRAINS)] <- "ISO_country.1"

TRAINS <- merge(TRAINS, country.names[, c("name", "iso_code")], by.x = "country.2", by.y = "name", all.x = T)
names(TRAINS)[ncol(TRAINS)] <- "ISO_country.2"

TRAINS$country.2 <- NULL
TRAINS$country.1 <- NULL



# 2. combine data (Symmetric) --------------------------------------------------

WTO.sym <- merge(WTO.sym, controls,
                 by.x = c("ISO_country.1", "ISO_country.2", "year"), 
                 by.y = c("country_id_o", "country_id_d", "year"))


any(!unique(WTO.sym$ISO_country.1) %in% unique(trade.costs$reporter))
test <- unique(WTO.sym$ISO_country.1)
test <- test[!unique(WTO.sym$ISO_country.1) %in% unique(trade.costs$reporter)]

test <- as.numeric(na.omit(trade.costs$tij))
hist(trade.costs$tij, breaks = 100)
ev_test(na.omit(trade.costs$tij))
descdist(test, discrete = F)


TRAINS.sym <- merge(TRAINS, controls,
                 by.x = c("ISO_country.1", "ISO_country.2", "year"), 
                 by.y = c("country_id_o", "country_id_d", "year"))

saveRDS(TRAINS.sym, file = paste0(path.data.out, 
                                "TRAINS_symmetric_w_controls.RData"))
saveRDS(WTO.sym, file = paste0(path.data.out, 
                                "WTO_symmetric_w_controls.RData"))

