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
GTA <- readRDS(file = paste0(path.data.out, "GTA_symmetric_isic.RData"))
controls <- readRDS(file = paste0(path.data.out, "Controls cleaned CEPII.RData"))

iso.conversion <- rbind(country.names[, c("name", "iso_code")], c("EU", "EU"))


# add ISO codes
TRAINS <- merge(TRAINS, iso.conversion[, c("name", "iso_code")], by.x = "country.1", by.y = "name", all.x = T)
names(TRAINS)[ncol(TRAINS)] <- "ISO_country.1"

TRAINS <- merge(TRAINS, iso.conversion[, c("name", "iso_code")], by.x = "country.2", by.y = "name", all.x = T)
names(TRAINS)[ncol(TRAINS)] <- "ISO_country.2"

TRAINS$country.2 <- NULL
TRAINS$country.1 <- NULL

GTA <- merge(GTA, iso.conversion[, c("name", "iso_code")], by.x = "country.1", by.y = "name", all.x = T)
names(GTA)[ncol(GTA)] <- "ISO_country.1"

GTA <- merge(GTA, iso.conversion[, c("name", "iso_code")], by.x = "country.2", by.y = "name", all.x = T)
names(GTA)[ncol(GTA)] <- "ISO_country.2"

GTA$country.2 <- NULL
GTA$country.1 <- NULL



# 2. combine data (Symmetric) --------------------------------------------------

## WTO -----------------
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

## TRAINS -------------
TRAINS.sym <- merge(TRAINS, controls,
                 by.x = c("ISO_country.1", "ISO_country.2", "year"), 
                 by.y = c("country_id_o", "country_id_d", "year"), 
                 all.x = T)

TRAINS.sym$chapter <- as.character(TRAINS.sym$chapter)
TRAINS.sym$chapter <- ifelse(TRAINS.sym$chapter == "A", "AB", TRAINS.sym$chapter)

trade.costs$is.cost <- 1
TRAINS.sym <- merge(TRAINS.sym, trade.costs[, c("reporter", "partner", "year", "sector", "tij", "is.cost")], 
                    by.x = c("ISO_country.1", "ISO_country.2", "year", "chapter"),
                    by.y = c("reporter", "partner", "year", "sector"), 
                    all.x = T)


##GTA ------------

GTA.sym <- merge(GTA, controls,
                    by.x = c("ISO_country.1", "ISO_country.2", "year"), 
                    by.y = c("country_id_o", "country_id_d", "year"), 
                    all.x = T)

GTA.sym$chapter <- as.character(GTA.sym$chapter)
GTA.sym$chapter <- ifelse(GTA.sym$chapter == "A", "AB", GTA.sym$chapter)

trade.costs$is.cost <- 1
GTA.sym <- merge(GTA.sym, trade.costs[, c("reporter", "partner", "year", "sector", "tij", "is.cost")], 
                    by.x = c("ISO_country.1", "ISO_country.2", "year", "chapter"),
                    by.y = c("reporter", "partner", "year", "sector"), 
                    all.x = T)



saveRDS(GTA.sym, file = paste0(path.data.out, 
                                  "TRAINS_symmetric_w_controls.RData"))

saveRDS(TRAINS.sym, file = paste0(path.data.out, 
                                "TRAINS_symmetric_w_controls.RData"))
saveRDS(WTO.sym, file = paste0(path.data.out, 
                                "WTO_symmetric_w_controls.RData"))

# run regressions -----------------

## TRAINS -----------
TRAINS.sym <- readRDS(file = paste0(path.data.out, 
                      "TRAINS_symmetric_w_controls.RData"))

TRAINS.sym <- TRAINS.sym %>% 
  filter(chapter == "D")%>%
  mutate(combined.name = paste0(ISO_country.1, ISO_country.2))%>%
  mutate(help.col = 1)

to.keep <- aggregate(data = TRAINS.sym, help.col ~ combined.name, FUN = sum)
to.keep <- to.keep[to.keep$help.col == 11,]

TRAINS.sym <- TRAINS.sym %>%
  filter(combined.name %in% to.keep$combined.name)


linreg <- lm(data = TRAINS.sym, log(tij) ~ number.of.interventions + diplo_disagreement + log(distw_harmonic) + comlang_off + comcol + contig + comlang_ethno + fta_wto)
summary(linreg)

## GTA -------------------

GTA.sym <- readRDS(file = paste0(path.data.out, 
                                    "GTA_symmetric_w_controls.RData"))

GTA.sym <- GTA.sym %>% 
  filter(chapter == "D")%>%
  mutate(combined.name = paste0(ISO_country.1, ISO_country.2))%>%
  mutate(help.col = 1)

to.keep <- aggregate(data = GTA.sym, help.col ~ combined.name, FUN = sum)
to.keep <- to.keep[to.keep$help.col == 11,]

GTA.sym <- GTA.sym %>%
  filter(combined.name %in% to.keep$combined.name)


linreg <- lm(data = GTA.sym, log(tij) ~ number.of.interventions + diplo_disagreement + log(distw_harmonic) + comlang_off + comcol + contig + comlang_ethno + fta_wto)
summary(linreg)
