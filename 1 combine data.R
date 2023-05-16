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
#library(goft)
#library(fitdistrplus)
library(gtalibrary)
#library(sampleSelection)
library(tidyverse)
library(fastDummies)
library(moments)

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

# create base years
base.years <- controls %>%
  filter(year %in% base)%>%
  select(-gdp.cap.ppp) %>%
  left_join(trade.costs, by = c("country.1", "country.2", "chapter", "year"))
base.years <- aggregate(data = base.years, . ~ country.1 + country.2 + chapter, FUN = mean, na.action = na.pass)

#make 0 data frame for interventions
interventions <- data.frame(matrix(ncol = 2*(length(selected.mast.red)+1), nrow = nrow(base.years), rep(0, (2*(length(selected.mast.red)+1))*nrow(base.years))))
names(interventions) <- c(paste0(c(selected.mast.red, "total"), "_harmful"),paste0(c(selected.mast.red, "total"), "_liberalising") )
base.years <- cbind(base.years, interventions); rm(interventions)
base.years <- base.years %>% 
  pivot_longer(cols = 5:ncol(base.years), names_to = "variable", values_to = "value") %>% 
  select(-year)

#saveRDS(base.years, file = paste0(path.data.out, "base.year.data.Rds"))

## TRAINS ----------------------------------------------------------------------

selected.mast[!selected.mast %in% names(TRAINS.sym)]
TRAINS.sym <- TRAINS.sym %>% 
  select(names(TRAINS.sym)[names(TRAINS.sym) %in% c(names(grid),"total", selected.mast)])%>%
  mutate(M = 0) %>%
  relocate(any_of(c(names(grid), selected.mast, "total")))
names(TRAINS.sym) <- c(names(grid), paste0( c(selected.mast, "total"), "_harmful"))

liberalising <- data.frame(matrix(nrow = nrow(TRAINS.sym), ncol = length(selected.mast.red)+1, data = 0))
names(liberalising) <- c(paste0(c(selected.mast, "total"), "_liberalising"))
TRAINS.sym <- cbind(TRAINS.sym, liberalising)


TRAINS.sym <- merge(TRAINS.sym, controls,
                    by = c("country.1", "country.2", "year", "chapter"), 
                    all.x = T)


trade.costs$is.cost <- 1
TRAINS.sym <- merge(TRAINS.sym, trade.costs, 
                    by = c("country.1", "country.2", "year", "chapter"),
                    all.x = T)


# pivot longer to get delta
TRAINS.sym.delta <- TRAINS.sym %>% 
  pivot_longer(cols = 5:ncol(TRAINS.sym), names_to = "variable", values_to = "value")

# join values and get delta
TRAINS.sym.delta <- TRAINS.sym.delta %>% left_join(base.years, by = c("country.1", "country.2", "chapter", "variable"))
TRAINS.sym.delta <- TRAINS.sym.delta %>%
  mutate(delta = value.x - value.y)%>%
  select(-c(value.x, value.y)) %>%
  pivot_wider(values_from = "delta", names_from = "variable")


##GTA --------------------------------------------------------------------------

names(TRAINS.sym)[!names(TRAINS.sym) %in% names(GTA.sym)]
names(GTA.sym)[!names(GTA.sym) %in% names(TRAINS.sym)]

GTA.sym <- merge(GTA.sym, controls,
                 by = c("country.1", "country.2", "year", "chapter"), 
                 all.x = T)

trade.costs$is.cost <- 1
GTA.sym.delta <- GTA.sym
GTA.sym <- merge(GTA.sym, trade.costs, 
                 by = c("country.1", "country.2", "year", "chapter"),
                 all.x = T)


# pivot longer to get delta
GTA.sym.delta <- GTA.sym %>% 
  pivot_longer(cols = 5:ncol(GTA.sym), names_to = "variable", values_to = "value")

GTA.sym.delta <- GTA.sym.delta %>%
  filter(!variable %in% c("L_harmful", "L_liberalising")) %>%
  unique()

# join values and get delta
GTA.sym.delta <- GTA.sym.delta %>% 
  left_join(base.years, by = c("country.1", "country.2", "chapter", "variable")) %>%
  mutate(delta = value.x - value.y)
GTA.sym.delta.analysis <- GTA.sym.delta
test <- GTA.sym.delta %>%
  select(-c(value.x, value.y)) %>%
  pivot_wider(values_from = "delta", names_from = "variable")


# GTA.sym.delta  %>%
#   dplyr::group_by(country.1, country.2, year, chapter, variable) %>%
#   dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
#   dplyr::filter(n > 1L)


GTA.sym.delta.analysis <- GTA.sym.delta.analysis %>%
  filter(variable == "tij")%>%
  filter(chapter == "D") %>%
  rename("value.base" = "value.y", "value.year" = "value.x")
#hist(GTA.sym.delta.analysis$delta, 500)
#skewness(na.omit(GTA.sym.delta.analysis$delta))

sum(is.na(GTA.sym.delta.analysis$delta))/nrow(GTA.sym.delta.analysis) # 0.42
sum(!is.na(GTA.sym.delta.analysis$value.base)  & is.na(GTA.sym.delta.analysis$value.year))/nrow(GTA.sym.delta.analysis) #0.11
sum(is.na(GTA.sym.delta.analysis$value.base)  & !is.na(GTA.sym.delta.analysis$value.year))/nrow(GTA.sym.delta.analysis) #0.06
sum(is.na(GTA.sym.delta.analysis$value.base)  & is.na(GTA.sym.delta.analysis$value.year))/nrow(GTA.sym.delta.analysis) #0.25


saveRDS(GTA.sym.delta, file = paste0(path.data.out, "GTA_delta_symmetric_w_controls.RData"))
saveRDS(TRAINS.sym.delta, file = paste0(path.data.out, "TRAINS_delta_symmetric_w_controls.RData"))

saveRDS(GTA.sym, file = paste0(path.data.out, "GTA_symmetric_w_controls.RData"))
saveRDS(TRAINS.sym, file = paste0(path.data.out, "TRAINS_symmetric_w_controls.RData"))
saveRDS(WTO.sym, file = paste0(path.data.out, "WTO_symmetric_w_controls.RData"))


rm(base.years, base)
# 3. Add Dummies ---------------------------------------------------------------

create_dummies <- function(data, both = T){
  
  
  # create 
  data <- dummy_cols(data, select_columns = "country.1")
  column.dummy.start <<- min(grep("country.1_", names(data)))
  
  names(data)[column.dummy.start:ncol(data)] <- substr(names(data)[column.dummy.start:ncol(data)], 11,14)
  #TRAINS.sym$ZWE <- 0 #correct for last country in country.2
  for(i in column.dummy.start:ncol(data)){ # create dummies and add both countries
    data[,i] <- ifelse((data[, "country.1"] == names(data)[i]) |
                         (data[, "country.2"] == names(data)[i]), 1,0)
  }
  
  data <- dummy_cols(data, select_columns = "year")
    return(data)
}


## TRAINS -----------
TRA <- readRDS(file = paste0(path.data.out, "TRAINS_symmetric_w_controls.RData"))
TRA.d <- readRDS(file = paste0(path.data.out, "TRAINS_delta_symmetric_w_controls.RData"))

#TRA.zero.countries <- read.csv(paste0(path.data.reg, "TRAINS_non_zero_countries.csv"))
TRA.measurement <- readxl::read_xlsx(path = paste0(path.data.out, "TRAINS_Measurement_index.xlsx"))
TRA <- merge(TRA, TRA.measurement, by = c("country.1", "country.2", "year", "chapter"), all.x = T)
TRA.d <- merge(TRA.d, TRA.measurement, by = c("country.1", "country.2", "year", "chapter"), all.x = T)

## GTA -----------
GTA <- readRDS(file = paste0(path.data.out, "GTA_symmetric_w_controls.RData"))
GTA.d <- readRDS(file = paste0(path.data.out, "GTA_delta_symmetric_w_controls.RData"))

GTA.measurement <- readxl::read_xlsx(path = paste0(path.data.out, "GTA_Measurement_index.xlsx"))
GTA.measurement <- GTA.measurement %>%
  pivot_wider(id_cols = c("country.1", "country.2", "year", "chapter"), 
              names_from = "gta.evaluation", 
              values_from = c("CRI_sqrt_gm","CRI_sqrt","CRI_gm","CRI"))

GTA <- merge(GTA, GTA.measurement, by = c("country.1", "country.2", "year", "chapter"))
GTA.d <- merge(GTA.d, GTA.measurement, by = c("country.1", "country.2", "year", "chapter"))

#GTA.zero.countries <- read.csv(paste0(path.data.reg, "GTA_non_zero_countries.csv"))

GTA <- GTA %>%
  mutate(is.available = ifelse(is.na(tij), 0, 1))%>% # get 0 and 1s for heckman
  mutate(intranat.trade = ((gdp_d - exports_d) * (gdp_o - exports_o))^(1/(2*(sigma-1)))) %>% # calculate Internal trade flows
  mutate(tij.heck = ifelse(is.na(tij), 0, tij)) # make all elements that are not availabe 0


GTA.d <- GTA.d %>%
  select(-c(value.x, value.y))%>%
  pivot_wider(names_from = "variable", values_from = "delta")

GTA.d <- GTA.d %>%
  mutate(is.available = ifelse(is.na(tij), 0, 1))%>% # get 0 and 1s for heckman
  mutate(intranat.trade = ((gdp_d - exports_d) * (gdp_o - exports_o))^(1/(2*(sigma-1)))) %>% # calculate Internal trade flows
  mutate(tij.heck = ifelse(is.na(tij), 0, tij)) # make all elements that are not availabe 0


TRA <- TRA %>%
  mutate(is.available = ifelse(is.na(tij), 0, 1))%>% # get 0 and 1s for heckman
  mutate(intranat.trade = ((gdp_d - exports_d) * (gdp_o - exports_o))^(1/(2*(sigma-1)))) %>% # calculate Internal trade flows
  mutate(tij.heck = ifelse(is.na(tij), 0, tij)) # make all elements that are not availabe 0


TRA.d <- TRA.d %>%
  mutate(is.available = ifelse(is.na(tij), 0, 1))%>% # get 0 and 1s for heckman
  mutate(intranat.trade = ((gdp_d - exports_d) * (gdp_o - exports_o))^(1/(2*(sigma-1)))) %>% # calculate Internal trade flows
  mutate(tij.heck = ifelse(is.na(tij), 0, tij)) # make all elements that are not availabe 0

TRA <- create_dummies(TRA)
TRA.d <- create_dummies(TRA.d)
GTA <- create_dummies(GTA)
GTA.d <- create_dummies(GTA.d)

GTA$gdp <- apply(GTA[, c("gdp_d", "gdp_o")], 1, FUN = function(x) log(mean(x)))


saveRDS(GTA.d, file = paste0(path.data.out, "GTA_delta_final.RData"))
saveRDS(TRA.d, file = paste0(path.data.out, "TRAINS_delta_final.RData"))

saveRDS(GTA, file = paste0(path.data.out, "GTA_final.RData"))
saveRDS(TRA, file = paste0(path.data.out, "TRAINS_final.RData"))

