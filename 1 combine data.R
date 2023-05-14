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

# 4. Regressions ---------------------------------------------------------------
GTA.d <- readRDS(file = paste0(path.data.out, "GTA_delta_final.RData"))
TRA.d <- readRDS(file = paste0(path.data.out, "TRAINS_delta_final.RData"))

GTA <- readRDS(file = paste0(path.data.out, "GTA_final.RData"))
TRA <- readRDS(file = paste0(path.data.out, "TRAINS_final.RData"))

GTA <- GTA %>% filter(chapter == "D")
TRA <- TRA %>% filter(chapter == "D")
GTA.d <- GTA.d %>% filter(chapter == "D")
TRA.d <- TRA.d %>% filter(chapter == "D")
column.dummy.start <- 49
## 4.1 regression components  ---------------------------------------------------
fe.vec <- names(GTA)[(column.dummy.start+11):(ncol(GTA)-2)]
fe <- paste0(fe.vec, collapse = "+")

controls.vec <- c("log(distw_harmonic)", "comlang_off", "comcol", "contig", "comlang_ethno", 
                  "fta_wto", "lsci", "lpi", "landlocked", "geometric_avg_tariff", "FXV")
controls <- paste0(controls.vec, collapse = "+")
controls.vec.fe <- c("log(distw_harmonic)", "comlang_off", "comcol", "contig", "comlang_ethno", 
                     "fta_wto", "geometric_avg_tariff", "FXV")
controls.fe <- paste0(controls.vec.fe, collapse = "+")


controls.d.vec <- c("fta_wto", "lsci", "lpi", "geometric_avg_tariff", "FXV") #exclude stuff which should not change over time
controls.d <- paste0(controls.d.vec, collapse = "+")
controls.d.vec.fe <- c("fta_wto", "geometric_avg_tariff", "FXV") #exclude stuff which should not change over time
controls.d.fe <- paste0(controls.d.vec.fe, collapse = "+")


GTA.NTM <- "total_harmful + total_liberalising"
TRA.NTM <- "total_harmful"

GTA.CRI <- "CRI_sqrt_gm_harmful + CRI_sqrt_gm_liberalising"
TRA.CRI <- "CRI_sqrt_gm"

mast.chap <- paste0(mast.names, collapse = "+") # Interventions disaggregated
mast.chap.TRA <- paste0(mast.names[!grepl("liberalising", mast.names)], collapse = "+")
## 4.2 OLS -------------------------------------------------------------------------
### GTA ---------------------------------------------------------------------------

ols           <- reg(GTA,   cont = paste0(GTA.NTM,  "+", controls)); summary(ols)
eval(parse(text = paste0("tt <- ivreg(data = GTA, tij ~ ", GTA.NTM, "+", controls, "+",fe, "|", controls,"+",fe, "+ ",GTA.CRI, ")")))
summary(tt)
ols.fe        <- reg(GTA,   cont = paste0(GTA.NTM,  "+", controls.fe,"+", fe));   summary(ols.fe)
ols.chap      <- reg(GTA,   cont = paste0(mast.chap,"+", controls)); summary(ols.chap)
ols.fe.chap   <- reg(GTA,   cont = paste0(mast.chap,"+", controls.fe,"+", fe));   summary(ols.fe.chap)

ols.d         <- reg(GTA.d, cont = paste0(GTA.NTM,  "+", controls.d));                summary(ols.d)
ols.d.fe      <- reg(GTA.d, cont = paste0(GTA.NTM,  "+", controls.d.vec.fe,"+", fe)); summary(ols.d.fe)
ols.d.chap    <- reg(GTA.d, cont = paste0(mast.chap,"+", controls.d));                summary(ols.d.chap)
ols.d.fe.chap <- reg(GTA.d, cont = paste0(mast.chap,"+", controls.d.vec.fe,"+", fe)); summary(ols.d.fe.chap)
eval(parse(text = paste0("tt <- ivreg(data = GTA.d, tij ~ ", GTA.NTM, "+", controls.d.vec.fe, "+",fe, "|", controls.d.vec.fe,"+",fe, "+ ",GTA.CRI, ")")))
summary(tt)

results.ols.gta <- list(ols, ols.fe, ols.chap, ols.fe.chap, ols.d, ols.d.fe, ols.d.chap, ols.d.fe.chap)
saveRDS(results.ols.gta, file = paste0(path.data.out, "GTA_OLS_Results.Rds"))
### TRAINS ------------------------------------------------------------------------

ols.TRA                <- reg(TRA, cont = paste0(TRA.NTM,"+", controls)); summary(ols.TRA)
ols.fe.TRA             <- reg(TRA, cont = paste0(TRA.NTM,"+", fe));                     summary(ols.fe.TRA)
ols.chap.TRA    <- reg(TRA, cont = paste0(mast.chap.TRA,"+", controls));   summary(ols.chap.TRA)
ols.fe.chap.TRA <- reg(TRA, cont = paste0(mast.chap.TRA,"+", controls,"+", fe));        summary(ols.fe.chap.TRA)

ols.d.TRA <-                reg(TRA.d, cont = paste0(TRA.NTM,"+", controls.d));         summary(ols.d.TRA)
ols.d.fe.TRA <-             reg(TRA.d, cont = paste0(TRA.NTM,"+", controls.d.vec.fe,"+", fe)); summary(ols.d.fe.TRA)
ols.d.chap.TRA <-    reg(TRA.d, cont = paste0(mast.chap.TRA,"+", controls.d));          summary(ols.d.chap.TRA)
ols.d.fe.chap.TRA <- reg(TRA.d, cont = paste0(mast.chap.TRA,"+", controls.d.vec.fe,"+", fe));  summary(ols.d.fe.chap.TRA)

results.ols.TRA <- list(ols.TRA, ols.fe.TRA, ols.chap.TRA, ols.fe.chap.TRA, ols.d.TRA, ols.d.fe.TRA, ols.d.chap.TRA, ols.d.fe.chap.TRA)
saveRDS(results.ols.TRA, file = paste0(path.data.out, "TRAINS_OLS_Results.Rds"))

### Linreg (weighted) -------------------------------------------------------------------------

# Good R2 -->investigate
ols.w    <- reg(GTA, cont = paste0(GTA.NTM,"+", controls),         weights = "CRI_sqrt_gm_harmful"); summary(ols.w) # geom mean
ols.fe.w <- reg(GTA, cont = paste0(GTA.NTM,"+", controls,"+", fe), weights = "CRI_sqrt_gm_harmful"); summary(ols.fe.w)
ols.chap.w    <- reg(GTA, cont = paste0(mast.chap,"+", controls),         weights = "CRI_sqrt_gm_harmful"); summary(ols.chap.w) # geom mean
ols.chap.fe.w <- reg(GTA, cont = paste0(mast.chap,"+", controls,"+", fe), weights = "CRI_sqrt_gm_harmful"); summary(ols.chap.fe.w)

ols.d.w    <- reg(GTA.d, cont = paste0(GTA.NTM,"+", controls.d),         weights = "CRI_sqrt_gm_harmful"); summary(ols.d.w) # geom mean
ols.d.fe.w <- reg(GTA.d, cont = paste0(GTA.NTM,"+", controls.d.fe,"+", fe), weights = "CRI_sqrt_gm_harmful"); summary(ols.d.fe.w)
ols.d.chap.w    <- reg(GTA.d, cont = paste0(mast.chap,"+", controls.d),         weights = "CRI_sqrt_gm_harmful"); summary(ols.d.chap.w) # geom mean
ols.d.chap.fe.w <- reg(GTA.d, cont = paste0(mast.chap,"+", controls.d.fe,"+", fe), weights = "CRI_sqrt_gm_harmful"); summary(ols.d.chap.fe.w)

results.ols.gta.w <- list(ols.w, ols.fe.w, ols.chap.w, ols.chap.fe.w, ols.d.w, ols.d.fe.w, ols.d.chap.w, ols.d.chap.fe.w)
saveRDS(results.ols.gta.w, file = paste0(path.data.out, "GTA_OLS_Weighted_Results.Rds"))
# 4.3 Heckman -------------------------------------------------------------------------

library(sampleSelection)

# Baseline Heckit
h.GTA <- reg(GTA, 
             type = "heckman",
             #weights = "GTA$CRI_sqrt_gm_harmful",
             cont =           paste0(GTA.NTM,"+", controls) , 
             cont.selection = paste0(GTA.NTM,"+", controls, "+ intranat.trade")); summary(h.GTA)
h.GTA.w <- reg(GTA, 
             type = "heckman",
             weights = "GTA$CRI_sqrt_gm_harmful",
             cont =           paste0(GTA.NTM,"+", controls) , 
             cont.selection = paste0(GTA.NTM,"+", controls, "+ intranat.trade")); summary(h.GTA)

h.TRA <- reg(TRA, 
             type = "heckman", 
             cont =           paste0(controls,"+", TRA.NTM), 
             cont.selection = paste0(controls,"+", TRA.NTM,"+", "+ intranat.trade")); summary(h.TRA)
h.TRA.w <- reg(TRA, 
             type = "heckman", 
             weights = "TRA$CRI_sqrt_gm",
             cont =           paste0(controls,"+", TRA.NTM), 
             cont.selection = paste0(controls,"+", TRA.NTM,"+", "+ intranat.trade")); summary(h.TRA)

# FE Heckit
fe.adjust.GTA <- paste0(fe.vec[!fe.vec %in% c("ARE", "CUB", "MMR","BRB","CPV","LBR","SVK", "year_2019")], collapse = "+")
h.fe.GTA <- reg(GTA, 
                type = "heckman", 
                #weights = "GTA$CRI_sqrt_gm_harmful",
                cont =           paste0(GTA.NTM,"+", fe.adjust.GTA), 
                cont.selection = paste0(GTA.NTM,"+", fe.adjust.GTA, "+ intranat.trade")); summary(h.fe.GTA)

fe.adjust.TRA <- paste0(fe.vec[! fe.vec %in% c("CUB","LBR", "CRI","SVK", "year_2019")], collapse = "+")
h.fe.TRA <- reg(TRA, 
                type = "heckman", 
                cont =           paste0(TRA.NTM,"+", fe.adjust.TRA, "+ log(distw_harmonic) + geometric_avg_tariff"), 
                cont.selection = paste0(TRA.NTM,"+", fe.adjust.TRA, "+ log(distw_harmonic) + geometric_avg_tariff + intranat.trade")); summary(h.fe.TRA)


# Heckit per Chapter
mast.names.adj <- paste0(mast.names[! mast.names %in% c("C_liberalising", "G_liberalising")], collapse = "+")
h.fe.chap.GTA <- reg(GTA, 
                     type = "heckman", 
                     cont =           paste0(mast.names.adj,"+", fe.adjust.GTA," + log(distw_harmonic) + geometric_avg_tariff"), 
                     cont.selection = paste0(mast.names.adj,"+", fe.adjust.GTA," + log(distw_harmonic) + geometric_avg_tariff + intranat.trade")); summary(h.fe.chap.GTA)

h.fe.chap.TRA <- reg(TRA, 
                     type = "heckman", 
                     cont =           paste0(mast.names.adj,"+", fe.adjust.TRA, " + log(distw_harmonic) + geometric_avg_tariff"), 
                     cont.selection = paste0(mast.names.adj,"+", fe.adjust.TRA, " + log(distw_harmonic) + geometric_avg_tariff + intranat.trade")); summary(h.fe.chap.TRA)

results.heckit <- list(h.GTA, h.TRA, h.GTA.w, h.TRA.w, h.fe.GTA, h.fe.TRA)
saveRDS(results.heckit, file = paste0(path.data.out, "Heckit_Results.Rds"))


# for (i in 14:length(mast.names.adj)) {
# 
#   t <- try(eval(parse(text = paste0("selection(data = GTA, selection = is.available ~ total_harmful + total_liberalising +", paste0(paste0(mast.names.adj[1:i], collapse = "+"), "+ log(distw_harmonic) + geometric_avg_tariff","+",paste0(fe.vec, collapse = "+")), 
#                                     "+ intranat.trade, tij ~", paste0(paste0(fe.vec, collapse = "+"),"+",mast.names.adj[1:i], "+ total_harmful + total_liberalising + log(distw_harmonic) + geometric_avg_tariff "), ", method = '2step')"))))
# 
#   if(!inherits(t, "try-error")){
#     print(i)
# 
#   } else{
#     cat("Error occurred when removing variable ", mast.names.adj[i], "\n")
#     beep(sound = 2)
#   }
# }

library(beepr)
fe.adjust.TRA.vec <- fe.vec[! fe.vec %in% c("CUB","LBR", "CRI","SVK", "year_2019")]

for (i in 1:length(fe.adjust.TRA.vec)) {

  t <- try(eval(parse(text = paste0("selection(data = TRA, selection = is.available ~ ",paste0(fe.adjust.TRA.vec[1:i], collapse = "+"), "+ intranat.trade, tij ~ total_harmful +", paste0(fe.adjust.TRA.vec[1:i], collapse = "+"), ", method = '2step')"))))

  if(!inherits(t, "try-error")){
    print(i)

  } else{
    cat("Error occurred when removing variable ", fe.adjust.TRA.vec[i], "\n")
    beep(sound = 2)
}
}

# remove libraries causing trouble
detach("package:goft")
detach("package:fitdistrplus")
detach("package:MASS")


## 4.4 Make table --------------------------------------------------------------
library(texreg)
out_stock_gta <- texreg(list(ols, ols.fe, heckit, heckit.fe), omit.coef = paste0(fe.vec,collapse = "|"), 
                    custom.model.names = c("OLS", "OLS-FE", "Heckit", "Heckit-FE"))

out_stock_TRA <- texreg(list(ols.TRA, ols.fe.TRA, heckit.TRA, heckit.fe.TRA), omit.coef = paste0(fe.vec,collapse = "|"), 
                        custom.model.names = c("OLS", "OLS-FE", "Heckit", "Heckit-FE"))


out_OLS_GTA_TRA <- texreg(list(ols, ols.fe, ols.TRA, ols.fe.TRA), #omit.coef = paste0(fe.vec,collapse = "|"),
                             custom.model.names = c("GTA", "GTA-FE", "TRA", "TRA-FE"))


out_Heckit_GTA_TRA <- texreg(list(heckit, heckit.fe, heckit.TRA, heckit.fe.TRA), #omit.coef = paste0(fe.vec,collapse = "|"),
                             custom.model.names = c("GTA", "GTA-FE", "TRA", "TRA-FE"))


write.table(out_stock_gta, file = paste0(path.data.out, "regression_results_GTA_stock.txt"))
write.table(out_stock_TRA, file = paste0(path.data.out, "regression_results_TRA_stock.txt"))


# 5. Other tests ----------------------------------------------------------------
## 5.1 NTMs vs export share ----------------------------------------------------
#not exports but share of exports (or exports vs intranational)
#Also, not bilateral
export.share <- readRDS(file = paste0(path.data.out, "Export_share.Rds"))
ntms <- readRDS(file = paste0(path.data.out, "GTA_interventions.Rds"))
ntms <- ntms %>% 
  filter(chapter == "GTT") %>%
  left_join(export.share, by = c("years.in.force" = "year", "implementing.jurisdiction" = "Country.1"))
ntms <- ntms %>%
  select(years.in.force, implementing.jurisdiction, intervention.id, Share)%>%
  rename("Country.1" = "implementing.jurisdiction")
t <- dummy_cols(ntms, "Country.1")
names(t)[5:ncol(t)] <- gsub("Country.1_", "", names(t)[5:ncol(t)])

fe.t <- paste0(fe.vec[!fe.vec %in% c("CMR", "COM", "CPV", "CUB", "GAB", "GEO", "GIN", "LAO", "LBN", "LBR", "MOZ", "MUS")], collapse = "+")
fe.t <- fe.vec[fe.vec %in% selected.countries]
fe.t <- paste0(fe.t[!fe.t %in% c("CMR", "COM", "CPV", "CUB", "GAB", "GEO", "GIN", "LAO", "LBN", "LBR", "MOZ", "MUS", "MWI", "NER", "TCD", "TGO")], collapse = "+")

reg <- reg(data = t,  dependant = "intervention.id", controls = paste0("Share +", fe.t));summary(reg)
reg <- reg(data = t,  dependant = "intervention.id", controls = "Share");summary(reg)



## 5.2 Trade flow threshold ----------------------------------------------------


GTA.t <- GTA
GTA.t$is.available.2 <- ifelse(GTA$is.available == 1, 1, -1)
ols.t <- reg(GTA.t, dependant = "is.available.2", controls = paste0(controls, "+ intranat.trade"))
beta_intranat.trade <- ols.t$coefficients["intranat.trade"]
sigma = 8

beta_intranat.trade <- 0.04456
threshold <- exp(2*(sigma-1))/beta_intranat.trade
threshold_sqrt <- sqrt(threshold)
