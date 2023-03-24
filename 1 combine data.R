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
install.packages("fastDummies")
library(goft)
library(fitdistrplus)
library(gtalibrary)
library(sampleSelection)
library(tidyverse)
library(fastDummies)

options(scipen = 999)
source("BA_Thesis_code/00 Terms and Definitions.R")




# 1. Load data -----------------------------------------------------------------

trade.costs <- readRDS(file = paste0(path.data.out, "Trade Costs Processed.RData"))
WTO.sym <- readRDS(file = paste0(path.data.out, "WTO_symmetric_isic.RData"))
TRAINS.sym <- readRDS(file = paste0(path.data.out, "TRAINS_symmetric_isic.RData"))
GTA.sym <- readRDS(file = paste0(path.data.out, "GTA_symmetric_isic.RData"))
#controls <- readRDS(file = paste0(path.data.out, "Controls cleaned CEPII.RData"))
controls <- readRDS(file = paste0(path.data.out, "Controls cleaned CEPII grid.RData"))

# 2. combine data (Symmetric) --------------------------------------------------

## WTO -----------------
WTO.sym$chapter <- as.character(WTO.sym$chapter)
WTO.sym$chapter <- ifelse(WTO.sym$chapter == "A", "AB", WTO.sym$chapter)

WTO.sym <- merge(WTO.sym, controls,
                 by.x = c("ISO_country.1", "ISO_country.2", "year"), 
                 by.y = c("country_id_o", "country_id_d", "year"))


trade.costs$is.cost <- 1
WTO.sym <- merge(WTO.sym, trade.costs[, c("country.1", "country.2", "year", "chapter", "tij", "is.cost")], 
                    by.x = c("ISO_country.1", "ISO_country.2", "year", "chapter"),
                    by.y = c("country.1", "country.2", "year", "chapter"), 
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
                 by = c("country.1", "country.2", "year", "chapter"), 
                 all.x = T)


trade.costs$is.cost <- 1
TRAINS.sym <- merge(TRAINS.sym, trade.costs, 
                    by = c("country.1", "country.2", "year", "chapter"),
                    all.x = T)


##GTA ------------

GTA.sym <- merge(GTA.sym, controls,
                    by = c("country.1", "country.2", "year", "chapter"), 
                    all.x = T)

trade.costs$is.cost <- 1
GTA.sym <- merge(GTA.sym, trade.costs, 
                 by = c("country.1", "country.2", "year", "chapter"),
                 all.x = T)



saveRDS(GTA.sym, file = paste0(path.data.out, 
                                  "GTA_symmetric_w_controls.RData"))

saveRDS(TRAINS.sym, file = paste0(path.data.out, 
                                "TRAINS_symmetric_w_controls.RData"))
saveRDS(WTO.sym, file = paste0(path.data.out, 
                                "WTO_symmetric_w_controls.RData"))

# 3. run regressions -----------------

## TRAINS -----------
TRAINS.sym <- readRDS(file = paste0(path.data.out, "TRAINS_symmetric_w_controls.RData"))
TRAINS.zero.countries <- read.csv(paste0(path.data.reg, "TRAINS_non_zero_countries.csv"))

TRAINS.measurement <- readxl::read_xlsx(path = paste0(path.data.out, "TRAINS_Measurement_index.xlsx"))
TRAINS.sym <- merge(TRAINS.sym, TRAINS.measurement, by = c("country.1", "country.2", "year", "chapter"))


TRAINS.sym <- TRAINS.sym %>% 
  filter(chapter == "D")%>%
  #filter(number.of.interventions > 3) %>%
  # filter(coverage.mean > 2) %>%
  filter(country.1 %in% TRAINS.zero.countries$x &
           country.2 %in% TRAINS.zero.countries$x)


linreg <- lm(data = TRAINS.sym, log(tij) ~ number.of.interventions + diplo_disagreement + distw_harmonic+ comlang_off + comcol + contig + comlang_ethno + fta_wto + coverage.geom.mean)
summary(linreg)





## GTA -------------------------------------------------------------------------

GTA.sym <- readRDS(file = paste0(path.data.out, "GTA_symmetric_w_controls.RData"))
GTA.measurement <- readxl::read_xlsx(path = paste0(path.data.out, "GTA_Measurement_index.xlsx"))
GTA.sym <- merge(GTA.sym, GTA.measurement, by = c("country.1", "country.2", "year", "chapter"))
GTA.zero.countries <- read.csv(paste0(path.data.reg, "GTA_non_zero_countries.csv"))

GTA.sym <- GTA.sym %>% filter(country.1 %in% GTA.zero.countries$x &
                              country.2 %in% GTA.zero.countries$x)

GTA.sym <- dummy_cols(GTA.sym, select_columns = "country.1")

names(GTA.sym)[25:ncol(GTA.sym)] <- substr(names(GTA.sym)[25:ncol(GTA.sym)], 11,13)
#GTA.sym$ZWE <- 0 #correct for last country in country.2

for(i in 25:ncol(GTA.sym)){ # create dummies and add both countries
  GTA.sym[,i] <- ifelse((GTA.sym[, "country.1"] == names(GTA.sym)[i]) |
                          (GTA.sym[, "country.2"] == names(GTA.sym)[i]), 
                        1,0)
}


### Linreg -------------------------------------------------------------------------

linreg <- lm(data = GTA.sym, tij ~ number.of.interventions  + log(distw_harmonic) + comlang_off + comcol + contig + comlang_ethno + fta_wto + lsci + lpi + landlocked  + geometric_avg_tariff + coverage.mean)
summary(linreg)


linreg.fixed <- "linreg.fixed <- lm(data = GTA.sym, tij ~ number.of.interventions + log(distw_harmonic) + comlang_off + comcol + contig + comlang_ethno + fta_wto + lsci + lpi + landlocked  + geometric_avg_tariff + coverage.mean "
linreg.fixed <- paste0(linreg.fixed,"+", paste0(names(GTA.sym)[26:ncol(GTA.sym)], collapse = "+" ),")")
eval(parse(text = linreg.fixed))
summary(linreg.fixed)


### Heckman -------------------------------------------------------------------------

GTA.sym$is.available <- ifelse(is.na(GTA.sym$tij), 0, 1)
GTA.sym <- relocate(GTA.sym, is.available, .before = number.of.interventions)
library(sampleSelection)
heckit <- selection(is.available ~ log(distw_harmonic) + contig + fta_wto + lpi + landlocked, 
                 tij ~ number.of.interventions + log(distw_harmonic) + comlang_off + comcol + contig + comlang_ethno + fta_wto + lsci + lpi + landlocked + geometric_avg_tariff + coverage.mean,
                 method = "2step",
                 data = GTA.sym)
summary(heckit)



heckman.fixed <- "heckman.fixed <- selection(is.available ~ log(distw_harmonic) + contig + fta_wto + lpi + landlocked, tij ~ number.of.interventions + log(distw_harmonic) +  contig + comlang_ethno + fta_wto + lsci + lpi + landlocked + geometric_avg_tariff + coverage.mean"
heckman.fixed <- paste0(heckman.fixed,"+", paste0(names(GTA.sym)[60:ncol(GTA.sym)], collapse = "+" ),',method = "2step",data = GTA.sym)')
eval(parse(text = heckman.fixed))
summary(heckman.fixed)


detach("package:goft")
detach("package:fitdistrplus")
detach("package:MASS")


### PPML -------------------------------------------------------------------------
library(gravity)

ppml <- ppml(data = GTA.sym, 
     dependent_variable = "tij", 
     distance = "distw_harmonic", 
     additional_regressors = c("number.of.interventions","comlang_off", "comcol", 
                               "contig", "comlang_ethno", "fta_wto", "lsci", 
                                 "lpi", "landlocked", "geometric_avg_tariff", "coverage.mean"))
summary(ppml)


ppml.fixed <- "ppml.fixed <- ppml(data = GTA.sym, dependent_variable = 'tij', distance = 'distw_harmonic', additional_regressors = c('number.of.interventions','comlang_off', 'comcol', 'contig', 'comlang_ethno', 'fta_wto', 'lsci', 'lpi', 'landlocked', 'geometric_avg_tariff', 'coverage.mean',"
ppml.fixed <- paste0(ppml.fixed,"'", paste0(names(GTA.sym)[26:ncol(GTA.sym)], collapse = "','" ),"'", "))")
eval(parse(text = ppml.fixed))
summary(ppml.fixed)

### Bind together

install.packages("texreg")
library(texreg)

texreg(list(linreg, linreg.fixed, heckit, heckman.fixed, ppml, ppml.fixed), custom.coef.names = c("number.of.interventions", "log(distw_harmonic)" ,"comlang_off", "comcol", 
                                                                                                  "contig", "comlang_ethno", "fta_wto", "lsci", 
                                                                                                  "lpi", "landlocked", "geometric_avg_tariff", "coverage.mean"))
