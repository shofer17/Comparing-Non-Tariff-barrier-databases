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
#install.packages("fastDummies")
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


base.years <- controls %>%
  filter(year %in% base)%>%
  select(-gdp.cap.ppp)
base.years <- aggregate(data = base.years, . ~ country.1 + country.2 + chapter, FUN = mean)

interventions <- data.frame(matrix(ncol = length(selected.mast)+1, nrow = nrow(base.years), rep(0, (length(selected.mast)+1)*nrow(base.years))))
names(interventions) <- c(selected.mast, "total")

base.years <- cbind(base.years, interventions); rm(interventions)

GTA.sym.delta <- GTA.sym %>% 
  pivot_longer(cols = 5:ncol(GTA.sym), names_to = "variable", values_to = "value")
base.years <- base.years %>% 
  pivot_longer(cols = 5:ncol(base.years), names_to = "variable", values_to = "value") %>% 
  select(-year)

GTA.sym.delta <- GTA.sym.delta %>% left_join(base.years, by = c("country.1", "country.2", "chapter", "variable"))
GTA.sym.delta$delta <- GTA.sym.delta$value.x - GTA.sym.delta$value.y


saveRDS(GTA.sym.delta, file = paste0(path.data.out, 
                               "GTA_delta_symmetric_w_controls.RData"))

saveRDS(GTA.sym, file = paste0(path.data.out, 
                                  "GTA_symmetric_w_controls.RData"))

saveRDS(TRAINS.sym, file = paste0(path.data.out, 
                                "TRAINS_symmetric_w_controls.RData"))
saveRDS(WTO.sym, file = paste0(path.data.out, 
                                "WTO_symmetric_w_controls.RData"))

# 3. run regressions -----------------
sigma = 8
## TRAINS -----------
TRAINS.sym <- readRDS(file = paste0(path.data.out, "TRAINS_symmetric_w_controls.RData"))
TRAINS.zero.countries <- read.csv(paste0(path.data.reg, "TRAINS_non_zero_countries.csv"))
TRAINS.measurement <- readxl::read_xlsx(path = paste0(path.data.out, "TRAINS_Measurement_index.xlsx"))
TRAINS.sym <- merge(TRAINS.sym, TRAINS.measurement, by = c("country.1", "country.2", "year", "chapter"))


TRAINS.sym <- TRAINS.sym %>% 
  filter(chapter == "D")






TRAINS.sym <- dummy_cols(TRAINS.sym, select_columns = "country.1")

column.dummy.start <- min(grep("country.1.", names(TRAINS.sym)))

names(TRAINS.sym)[column.dummy.start:ncol(TRAINS.sym)] <- substr(names(TRAINS.sym)[column.dummy.start:ncol(TRAINS.sym)], 11,13)
#TRAINS.sym$ZWE <- 0 #correct for last country in country.2

for(i in column.dummy.start:ncol(TRAINS.sym)){ # create dummies and add both countries
  TRAINS.sym[,i] <- ifelse((TRAINS.sym[, "country.1"] == names(TRAINS.sym)[i]) |
                          (TRAINS.sym[, "country.2"] == names(TRAINS.sym)[i]), 
                        1,0)
}





### Linreg -------------------------------------------------------------------------
# ADD TIME FIXED EFFECTS -------------------------
#normal
linreg <- lm(data = TRAINS.sym, tij ~ total  + log(distw_harmonic) + comlang_off + comcol + contig + comlang_ethno + fta_wto + lsci + lpi + landlocked  + geometric_avg_tariff + coverage.mean)
summary(linreg)

#fixed effects
linreg.fixed <- "linreg.fixed <- lm(data = TRAINS.sym, tij ~ total + log(distw_harmonic) + comlang_off + comcol + contig + comlang_ethno + fta_wto + lsci + lpi + landlocked  + geometric_avg_tariff + coverage.mean "
linreg.fixed <- paste0(linreg.fixed,"+", paste0(names(TRAINS.sym)[column.dummy.start:ncol(TRAINS.sym)], collapse = "+" ),")")
eval(parse(text = linreg.fixed))
summary(linreg.fixed)


#FE + interventions per chapter
linreg.fixed <- "linreg.fixed <- lm(data = TRAINS.sym, tij ~ B + C + D + E + F + G + P + N + I + L + log(distw_harmonic) + comlang_off + comcol + contig + comlang_ethno + fta_wto + lsci + lpi + landlocked  + geometric_avg_tariff + coverage.mean "
linreg.fixed <- paste0(linreg.fixed,"+", paste0(names(TRAINS.sym)[column.dummy.start:ncol(TRAINS.sym)], collapse = "+" ),")")
eval(parse(text = linreg.fixed))
summary(linreg.fixed)

### Linreg (weighted) -------------------------------------------------------------------------

#geom mean
linreg.weighted.geom <- lm(data = TRAINS.sym, 
                           tij ~ total  + log(distw_harmonic) + comlang_off + comcol + contig + comlang_ethno + fta_wto + lsci + lpi + landlocked  + geometric_avg_tariff, 
                           weights = coverage.geom.mean)
summary(linreg.weighted.geom)


linreg.weighted.fixed.geom <- "linreg.weighted.fixed.geom <- lm(data = TRAINS.sym, tij ~ total + log(distw_harmonic) + comlang_off + comcol + contig + comlang_ethno + fta_wto + lsci + lpi + landlocked  + geometric_avg_tariff "
linreg.weighted.fixed.geom <- paste0(linreg.weighted.fixed.geom,"+", paste0(names(TRAINS.sym)[column.dummy.start:ncol(TRAINS.sym)], collapse = "+" ),", weights = coverage.geom.mean)")
eval(parse(text = linreg.weighted.fixed.geom))
summary(linreg.weighted.fixed.geom)


#arith mean
linreg.weighted.mean <- lm(data = TRAINS.sym, 
                           tij ~ total  + log(distw_harmonic) + comlang_off + comcol + contig + comlang_ethno + fta_wto + lsci + lpi + landlocked  + geometric_avg_tariff, 
                           weights = coverage.mean)
summary(linreg.weighted.mean)


linreg.weighted.fixed.mean <- "linreg.weighted.fixed.mean <- lm(data = TRAINS.sym, tij ~ total + log(distw_harmonic) + comlang_off + comcol + contig + comlang_ethno + fta_wto + lsci + lpi + landlocked  + geometric_avg_tariff "
linreg.weighted.fixed.mean <- paste0(linreg.weighted.fixed.mean,"+", paste0(names(TRAINS.sym)[column.dummy.start:ncol(TRAINS.sym)], collapse = "+" ),", weights = coverage.mean)")
eval(parse(text = linreg.weighted.fixed.mean))
summary(linreg.weighted.fixed.mean)


### Heckman -------------------------------------------------------------------------

TRAINS.sym$is.available <- ifelse(is.na(TRAINS.sym$tij), 0, 1)
TRAINS.sym <- relocate(TRAINS.sym, is.available, .before = total)
TRAINS.sym$exports <- ((TRAINS.sym$gdp_d - TRAINS.sym$exports_d) * ( TRAINS.sym$gdp_o - TRAINS.sym$exports_o))^(1/(2*(sigma-1)))
TRAINS.sym$tij.heck <- ifelse(is.na(TRAINS.sym$tij), 0, TRAINS.sym$tij)

library(sampleSelection)
heckit.trains <- selection(is.available ~ log(distw_harmonic) + comlang_off + comcol + contig + comlang_ethno + fta_wto + lsci + lpi + landlocked + geometric_avg_tariff + exports, 
                           tij.heck ~ total + log(distw_harmonic) + comlang_off + comcol + contig + comlang_ethno + fta_wto + lsci + lpi + landlocked + geometric_avg_tariff + coverage.geom.mean,
                           method = "2step",
                           data = TRAINS.sym)
summary(heckit.trains)


exclude.colinearity <- c()
heckman.fixed <- "heckman.fixed <- selection(is.available ~ log(distw_harmonic) + contig + fta_wto + lpi + landlocked, tij ~ total + log(distw_harmonic) +  contig + comlang_ethno + fta_wto + lsci + lpi + landlocked + geometric_avg_tariff + exports"
heckman.fixed <- paste0(heckman.fixed,"+", paste0(names(TRAINS.sym)[116:ncol(TRAINS.sym)], collapse = "+" ),',method = "2step",data = TRAINS.sym)')
eval(parse(text = heckman.fixed))
summary(heckman.fixed)


detach("package:goft")
detach("package:fitdistrplus")
detach("package:MASS")


### PPML -------------------------------------------------------------------------
library(gravity)

ppml <- ppml(data = TRAINS.sym, 
             dependent_variable = "tij", 
             distance = "distw_harmonic", 
             additional_regressors = c("total","comlang_off", "comcol", 
                                       "contig", "comlang_ethno", "fta_wto", "lsci", 
                                       "lpi", "landlocked", "geometric_avg_tariff", "coverage.mean"))
summary(ppml)


ppml.fixed <- "ppml.fixed <- ppml(data = TRAINS.sym, dependent_variable = 'tij', distance = 'distw_harmonic', additional_regressors = c('total','comlang_off', 'comcol', 'contig', 'comlang_ethno', 'fta_wto', 'lsci', 'lpi', 'landlocked', 'geometric_avg_tariff', 'coverage.mean',"
ppml.fixed <- paste0(ppml.fixed,"'", paste0(names(TRAINS.sym)[column.dummy.start:ncol(TRAINS.sym)], collapse = "','" ),"'", "))")
eval(parse(text = ppml.fixed))
summary(ppml.fixed)

### Bind together
names(heckit$lm$coefficients) <- gsub("XO", "", names(heckit$lm$coefficients))
names(heckit$lm$qr) <- gsub("XO", "", names(heckit$qr$coefficients))

library(texreg)
texreg(list(linreg, linreg.fixed, heckit, heckman.fixed, ppml, ppml.fixed) )

## GTA -------------------------------------------------------------------------

GTA.sym <- readRDS(file = paste0(path.data.out, "GTA_symmetric_w_controls.RData"))
GTA.measurement <- readxl::read_xlsx(path = paste0(path.data.out, "GTA_Measurement_index.xlsx"))
GTA.sym <- merge(GTA.sym, GTA.measurement, by = c("country.1", "country.2", "year", "chapter"))
GTA.zero.countries <- read.csv(paste0(path.data.reg, "GTA_non_zero_countries.csv"))

# GTA.sym <- GTA.sym %>% filter(country.1 %in% GTA.zero.countries$x &
#                               country.2 %in% GTA.zero.countries$x)

GTA.sym <- dummy_cols(GTA.sym, select_columns = "country.1")

column.dummy.start <- min(grep("country.1.", names(GTA.sym)))
names(GTA.sym)[column.dummy.start:ncol(GTA.sym)] <- substr(names(GTA.sym)[column.dummy.start:ncol(GTA.sym)], 11,13)
#GTA.sym$ZWE <- 0 #correct for last country in country.2

for(i in column.dummy.start:ncol(GTA.sym)){ # create dummies and add both countries
  GTA.sym[,i] <- ifelse((GTA.sym[, "country.1"] == names(GTA.sym)[i]) |
                          (GTA.sym[, "country.2"] == names(GTA.sym)[i]), 
                        1,0)
}

GTA.sym <- dummy_cols(GTA.sym, select_columns = "year")
### Linreg -------------------------------------------------------------------------

linreg <- lm(data = GTA.sym, tij ~ total  + log(distw_harmonic) + comlang_off + comcol + contig + comlang_ethno + fta_wto + lsci + lpi + landlocked  + geometric_avg_tariff + coverage.mean)
summary(linreg)


linreg.fixed <- "linreg.fixed <- lm(data = GTA.sym, tij ~ total + log(distw_harmonic) + comlang_off + comcol + contig + comlang_ethno + fta_wto + lsci + lpi + landlocked  + geometric_avg_tariff + coverage.mean "
linreg.fixed <- paste0(linreg.fixed,"+", paste0(names(GTA.sym)[column.dummy.start:ncol(GTA.sym)], collapse = "+" ),", weights = coverage.geom.mean)")
eval(parse(text = linreg.fixed))
summary(linreg.fixed)

### Linreg (weighted) -------------------------------------------------------------------------

#geom mean
linreg.weighted.geom <- lm(data = GTA.sym, 
                           tij ~ total  + log(distw_harmonic) + comlang_off + comcol + contig + comlang_ethno + fta_wto + lsci + lpi + landlocked  + geometric_avg_tariff, 
                           weights = coverage.geom.mean)
summary(linreg.weighted.geom)


linreg.weighted.fixed.geom <- "linreg.weighted.fixed.geom <- lm(data = GTA.sym, tij ~ total + log(distw_harmonic) + comlang_off + comcol + contig + comlang_ethno + fta_wto + lsci + lpi + landlocked  + geometric_avg_tariff "
linreg.weighted.fixed.geom <- paste0(linreg.weighted.fixed.geom,"+", paste0(names(GTA.sym)[column.dummy.start:ncol(GTA.sym)], collapse = "+" ),", weights = coverage.geom.mean)")
eval(parse(text = linreg.weighted.fixed.geom))
summary(linreg.weighted.fixed.geom)


#arith mean
linreg.weighted.mean <- lm(data = GTA.sym, 
                           tij ~ total  + log(distw_harmonic) + comlang_off + comcol + contig + comlang_ethno + fta_wto + lsci + lpi + landlocked  + geometric_avg_tariff, 
                           weights = coverage.mean)
summary(linreg.weighted.mean)


linreg.weighted.fixed.mean <- "linreg.weighted.fixed.mean <- lm(data = GTA.sym, tij ~ total + log(distw_harmonic) + comlang_off + comcol + contig + comlang_ethno + fta_wto + lsci + lpi + landlocked  + geometric_avg_tariff "
linreg.weighted.fixed.mean <- paste0(linreg.weighted.fixed.mean,"+", paste0(names(GTA.sym)[column.dummy.start:ncol(GTA.sym)], collapse = "+" ),", weights = coverage.mean)")
eval(parse(text = linreg.weighted.fixed.mean))
summary(linreg.weighted.fixed.mean)



library(texreg)
texreg(list(linreg, linreg.fixed, linreg.weighted.mean, linreg.weighted.fixed.mean,  linreg.weighted.geom, linreg.weighted.fixed.geom) )



### Heckman -------------------------------------------------------------------------

GTA.sym$is.available <- ifelse(is.na(GTA.sym$tij), 0, 1)
GTA.sym <- relocate(GTA.sym, is.available, .before = total)

GTA.sym$exports <- ((GTA.sym$gdp_d - GTA.sym$exports_d) * ( GTA.sym$gdp_o - GTA.sym$exports_o))^(1/(2*(sigma-1)))
GTA.sym$tij.heck <- ifelse(is.na(GTA.sym$tij), 0, GTA.sym$tij)

library(sampleSelection)
heckit.gta <- selection(is.available ~ log(distw_harmonic) + comlang_off + comcol + contig + comlang_ethno + fta_wto + lsci + lpi + landlocked + geometric_avg_tariff + exports, 
                        tij.heck ~ total + log(distw_harmonic) + comlang_off + comcol + contig + comlang_ethno + fta_wto + lsci + lpi + landlocked + geometric_avg_tariff + coverage.geom.mean,
                 method = "2step",
                 data = GTA.sym)
summary(heckit.gta)



heckman.fixed <- "heckman.fixed <- selection(is.available ~ log(distw_harmonic) + contig + fta_wto + lpi + landlocked, tij ~ total + log(distw_harmonic) +  contig + comlang_ethno + fta_wto + lsci + lpi + landlocked + geometric_avg_tariff + coverage.mean"
heckman.fixed <- paste0(heckman.fixed,"+", paste0(names(GTA.sym)[80:ncol(GTA.sym)], collapse = "+" ),',method = "2step",data = GTA.sym)')
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
     additional_regressors = c("total","comlang_off", "comcol", 
                               "contig", "comlang_ethno", "fta_wto", "lsci", 
                                 "lpi", "landlocked", "geometric_avg_tariff", "coverage.mean"))
summary(ppml)


ppml.fixed <- "ppml.fixed <- ppml(data = GTA.sym, dependent_variable = 'tij', distance = 'distw_harmonic', additional_regressors = c('total','comlang_off', 'comcol', 'contig', 'comlang_ethno', 'fta_wto', 'lsci', 'lpi', 'landlocked', 'geometric_avg_tariff', 'coverage.mean',"
ppml.fixed <- paste0(ppml.fixed,"'", paste0(names(GTA.sym)[column.dummy.start:ncol(GTA.sym)], collapse = "','" ),"'", "))")
eval(parse(text = ppml.fixed))
summary(ppml.fixed)

### Bind together
names(heckit$lm$coefficients) <- gsub("XO", "", names(heckit$lm$coefficients))
names(heckit$lm$qr) <- gsub("XO", "", names(heckit$qr$coefficients))

library(texreg)
texreg(list(linreg, linreg.fixed, heckit, heckman.fixed, ppml, ppml.fixed) )
