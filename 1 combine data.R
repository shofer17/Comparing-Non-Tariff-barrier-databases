# Author: Silvan Hofer
# Date: 17. February 2023
# Purpose: 
# This script combines data and makes it ready for analysis.
# 

setwd("..") # move up one

rm(list = ls())
# install.packages("goft")
# install.packages("fitdistrplus")
#install.packages("sampleSelection")
library(goft)
library(fitdistrplus)
library(gtalibrary)
library(sampleSelection)
library(tidyverse)

options(scipen = 999)
years <- 2009:2019

# Paths

path.data.raw <- "0 data raw/"
path.data.out <- "1 data processed/"

# 0. function ------------------------------------------------------------------

to_iso <- function(data, column.name.1, column.name.2){ # from GTA name to ISO
  
  iso.conversion <- rbind(country.names[, c("name", "iso_code")], c("EU", "EU"))
  
  eval(parse(text = paste0("data <- merge(data, iso.conversion, by.x = '",column.name.1,"', by.y = 'name', all.x = T)")))
  names(data)[ncol(data)] <- "ISO_country.1"
  
  eval(parse(text = paste0("data <- merge(data, iso.conversion, by.x = '",column.name.2,"', by.y = 'name', all.x = T)")))
  names(data)[ncol(data)] <- "ISO_country.2"
  
  data <- data[, 3:ncol(data)]
  names(data)[(ncol(data)-1):ncol(data)] <- c(column.name.1, column.name.2)
  
  return(data)
}





# 1. Load data -----------------------------------------------------------------
# 
trade.costs <- readRDS(file = paste0(path.data.out, "Trade Costs Processed.RData"))
WTO.sym <- readRDS(file = paste0(path.data.out, "WTO_symmetric_isic.RData"))
TRAINS.sym <- readRDS(file = paste0(path.data.out, "TRAINS_symmetric_isic.RData"))
GTA.sym <- readRDS(file = paste0(path.data.out, "GTA_symmetric_isic.RData"))
controls <- readRDS(file = paste0(path.data.out, "Controls cleaned CEPII.RData"))


TRAINS.sym <- to_iso(TRAINS.sym, "country.1", "country.2")
GTA.sym <- to_iso(GTA.sym, "country.1", "country.2")

# 2. combine data (Symmetric) --------------------------------------------------

## WTO -----------------
WTO.sym$chapter <- as.character(WTO.sym$chapter)
WTO.sym$chapter <- ifelse(WTO.sym$chapter == "A", "AB", WTO.sym$chapter)

WTO.sym <- merge(WTO.sym, controls,
                 by.x = c("ISO_country.1", "ISO_country.2", "year"), 
                 by.y = c("country_id_o", "country_id_d", "year"))


trade.costs$is.cost <- 1
WTO.sym <- merge(WTO.sym, trade.costs[, c("reporter", "partner", "year", "sector", "tij", "is.cost")], 
                    by.x = c("ISO_country.1", "ISO_country.2", "year", "chapter"),
                    by.y = c("reporter", "partner", "year", "sector"), 
                    all.x = T)

any(!unique(WTO.sym$ISO_country.1) %in% unique(trade.costs$reporter))
test <- unique(WTO.sym$ISO_country.1)
test <- test[!unique(WTO.sym$ISO_country.1) %in% unique(trade.costs$reporter)]

test <- as.numeric(na.omit(trade.costs$tij))
hist(trade.costs$tij, breaks = 100)
ev_test(na.omit(trade.costs$tij))
descdist(test, discrete = F)

## TRAINS -------------
TRAINS.sym <- merge(TRAINS.sym, controls,
                 by.x = c("country.1", "country.2", "year"), 
                 by.y = c("country_id_o", "country_id_d", "year"), 
                 all.x = T)

TRAINS.sym$chapter <- as.character(TRAINS.sym$chapter)
TRAINS.sym$chapter <- ifelse(TRAINS.sym$chapter == "A", "AB", TRAINS.sym$chapter)

trade.costs$is.cost <- 1
TRAINS.sym <- merge(TRAINS.sym, trade.costs[, c("reporter", "partner", "year", "sector", "tij", "is.cost")], 
                    by.x = c("country.1", "country.2", "year", "chapter"),
                    by.y = c("reporter", "partner", "year", "sector"), 
                    all.x = T)


##GTA ------------

GTA.sym <- merge(GTA.sym, controls,
                    by.x = c("country.1", "country.2", "year"), 
                    by.y = c("country_id_o", "country_id_d", "year"), 
                    all.x = T)

GTA.sym$chapter <- as.character(GTA.sym$chapter)
GTA.sym$chapter <- ifelse(GTA.sym$chapter == "A", "AB", GTA.sym$chapter)

trade.costs$is.cost <- 1
GTA.sym <- merge(GTA.sym, trade.costs[, c("reporter", "partner", "year", "sector", "tij","geometric_avg_tariff", "is.cost")], 
                    by.x = c("country.1", "country.2", "year", "chapter"),
                    by.y = c("reporter", "partner", "year", "sector"), 
                    all.x = T)



saveRDS(GTA.sym, file = paste0(path.data.out, 
                                  "GTA_symmetric_w_controls.RData"))

saveRDS(TRAINS.sym, file = paste0(path.data.out, 
                                "TRAINS_symmetric_w_controls.RData"))
saveRDS(WTO.sym, file = paste0(path.data.out, 
                                "WTO_symmetric_w_controls.RData"))

# 3. run regressions -----------------

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

cutoff <- readxl::read_xlsx(paste0(path.data.out, "GTA NTM coverage.xlsx"))
cutoff <- cutoff %>% filter(coverage.measure.log > 0)

GTA.sym <- readRDS(file = paste0(path.data.out, 
                                    "GTA_symmetric_w_controls.RData"))
GTA.sym <- GTA.sym %>% filter(country.1 != "na" & country.2 != "na")%>%
  filter(!is.na(iso3_d))

GTA.sym <- unique(GTA.sym)

GTA.sym <- GTA.sym %>% 
  select(-scaled_sci_2021) %>%
  filter(chapter == "D")%>%
  #filter(ISO_country.1 %in% cutoff$iso_code & ISO_country.2 %in% cutoff$iso_code) %>%
  mutate(combined.name = paste0(country.1, country.2))
  #mutate(help.col = ifelse(!is.na(tij), 1,0))
GTA.sym$tij <- round(as.numeric(GTA.sym$tij),3)
GTA.sym <- unique(GTA.sym)
    

to.keep <- aggregate(data = GTA.sym, help.col ~ combined.name, FUN = sum)
to.keep <- to.keep[to.keep$help.col == 11,]

GTA.sym <- GTA.sym %>%
  filter(combined.name %in% to.keep$combined.name)

cutoff.merge <- cutoff[, c("iso_code", "coverage.measure.log")]
names(cutoff.merge) <- c("country.1", "coverage.1")
GTA.sym <- merge(GTA.sym, cutoff.merge, by.x = "country.1")
names(cutoff.merge) <- c("country.2", "coverage.2")
GTA.sym <- merge(GTA.sym, cutoff.merge, by.x = "country.2")
GTA.sym$coverage <- apply(GTA.sym[, c("coverage.1", "coverage.2")], 1, FUN = function(x) exp(mean(log(x))))



saveRDS(GTA.sym, file = paste0(path.data.out, "GTA_symmetric_w_controls_reg.RData"))
GTA.sym <- readRDS(file = paste0(path.data.out, "GTA_symmetric_w_controls_reg.RData"))

## ADD TARIFFS
linreg <- lm(data = GTA.sym, tij ~ number.of.interventions  + log(distw_harmonic) + comlang_off + comcol + contig + comlang_ethno + fta_wto + lsci + lpi + landlocked  + geometric_avg_tariff + coverage)
summary(linreg)



GTA.sym$is.available <- ifelse(is.na(GTA.sym$tij), 0, 1)
GTA.sym$gdp <- apply(GTA.sym[, c("gdp_o", "gdp_d")], 1, FUN = function(x) mean(x))

library(sampleSelection)
heckit <- selection(is.available ~ log(distw_harmonic) + contig + fta_wto + lpi + landlocked, 
                 log(tij) ~ number.of.interventions + log(distw_harmonic) + comlang_off + comcol + contig + comlang_ethno + fta_wto + lsci + lpi + landlocked + geometric_avg_tariff + log(gdp)+ coverage,
                 method = "2step",
                 data = GTA.sym)

summary(heckit)





# DO REG WITH REDUCED DATA

