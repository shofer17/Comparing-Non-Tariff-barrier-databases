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
  filter(!variable %in% c("L_harmful", "L_liberalising"))

# join values and get delta
GTA.sym.delta <- GTA.sym.delta %>% 
  left_join(base.years, by = c("country.1", "country.2", "chapter", "variable")) %>%
  mutate(delta = value.x - value.y)
GTA.sym.delta.analysis <- GTA.sym.delta
GTA.sym.delta <- GTA.sym.delta %>%
  select(-c(value.x, value.y)) %>%
  pivot_wider(values_from = "delta", names_from = "variable")

GTA.sym.delta.analysis <- GTA.sym.delta.analysis %>%
  filter(variable == "tij")%>%
  filter(chapter == "D") %>%
  rename("value.base" = "value.y", "value.year" = "value.x")
hist(GTA.sym.delta.analysis$delta, 500)
skewness(na.omit(GTA.sym.delta.analysis$delta))

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

create_dummies <- function(data){
  data <- dummy_cols(data, select_columns = "country.1")
  column.dummy.start <<- min(grep("country.1.", names(data)))
  
  names(data)[column.dummy.start:ncol(data)] <- substr(names(data)[column.dummy.start:ncol(data)], 11,13)
  #TRAINS.sym$ZWE <- 0 #correct for last country in country.2
  for(i in column.dummy.start:ncol(data)){ # create dummies and add both countries
    data[,i] <- ifelse((data[, "country.1"] == names(data)[i]) |
                         (data[, "country.2"] == names(data)[i]), 1,0)
  }
  data <- dummy_cols(data, select_columns = "year")
  return(data)
}


## TRAINS -----------
TRAINS.sym <- readRDS(file = paste0(path.data.out, "TRAINS_symmetric_w_controls.RData"))
TRAINS.zero.countries <- read.csv(paste0(path.data.reg, "TRAINS_non_zero_countries.csv"))
TRAINS.measurement <- readxl::read_xlsx(path = paste0(path.data.out, "TRAINS_Measurement_index.xlsx"))
TRAINS.sym <- merge(TRAINS.sym, TRAINS.measurement, by = c("country.1", "country.2", "year", "chapter"), all.x = T)
TRAINS.sym <- TRAINS.sym %>% 
  filter(chapter == "D")

## GTA -----------
GTA.sym <- readRDS(file = paste0(path.data.out, "GTA_symmetric_w_controls.RData"))
GTA.measurement <- readxl::read_xlsx(path = paste0(path.data.out, "GTA_Measurement_index.xlsx"))
GTA.measurement <- GTA.measurement %>%
  pivot_wider(id_cols = c("country.1", "country.2", "year", "chapter"), 
              names_from = "gta.evaluation", 
              values_from = c("CRI_sqrt_gm","CRI_sqrt","CRI_gm","CRI"))

GTA.sym <- merge(GTA.sym, GTA.measurement, by = c("country.1", "country.2", "year", "chapter"))
GTA.zero.countries <- read.csv(paste0(path.data.reg, "GTA_non_zero_countries.csv"))
GTA.sym <- GTA.sym %>% 
  filter(chapter == "D")


GTA.sym <- GTA.sym %>%
  mutate(is.available = ifelse(is.na(tij), 0, 1))%>% # get 0 and 1s for heckman
  mutate(intranat.trade = ((gdp_d - exports_d) * (gdp_o - exports_o))^(1/(2*(sigma-1)))) %>% # calculate Internal trade flows
  
  mutate(tij.heck = ifelse(is.na(tij), 0, tij)) # make all elements that are not availabe 0


GTA.sym.delta <- GTA.sym.delta %>%
  mutate(is.available = ifelse(is.na(tij), 0, 1))%>% # get 0 and 1s for heckman
  mutate(exports = ((gdp_d - exports_d) * (gdp_o - exports_o))^(1/(2*(sigma-1)))) %>% # calculate Internal trade flows
  mutate(tij.heck = ifelse(is.na(tij), 0, tij)) # make all elements that are not availabe 0

controls.vec <- c("total_harmful", "total_liberalising", "distw_harmonic", "comlang_off", "comcol", "contig", "comlang_ethno", "fta_wto", "lsci", "lpi", "landlocked", "geometric_avg_tariff", "exports")
GTA.sym.heck <- GTA.sym %>% 
  select(controls.vec)

names(GTA.sym.heck) <- paste0("base_", names(GTA.sym.heck))



TRAINS.sym <- TRAINS.sym %>%
  mutate(is.available = ifelse(is.na(tij), 0, 1))%>% # get 0 and 1s for heckman
  mutate(intranat..trade = ((gdp_d - exports_d) * (gdp_o - exports_o))^(1/(2*(sigma-1)))) %>% # calculate Internal trade flows
  mutate(tij.heck = ifelse(is.na(tij), 0, tij)) # make all elements that are not availabe 0


TRAINS.sym.delta <- TRAINS.sym.delta %>%
  mutate(is.available = ifelse(is.na(tij), 0, 1))%>% # get 0 and 1s for heckman
  mutate(intranat.trade = ((gdp_d - exports_d) * (gdp_o - exports_o))^(1/(2*(sigma-1)))) %>% # calculate Internal trade flows
  mutate(tij.heck = ifelse(is.na(tij), 0, tij)) # make all elements that are not availabe 0

TRAINS.sym.heck <- TRAINS.sym %>% 
  filter(chapter == "D")%>%
  select(controls.vec)


names(TRAINS.sym.heck) <- paste0("base_", names(TRAINS.sym.heck))
TRAINS.sym.delta.heck <- cbind(TRAINS.sym.delta,TRAINS.sym.heck )





TRAINS.sym <- create_dummies(TRAINS.sym)
TRAINS.sym.delta <- create_dummies(TRAINS.sym.delta)
GTA.sym <- create_dummies(GTA.sym)
GTA.sym.delta <- create_dummies(GTA.sym.delta)

GTA.sym$gdp <- apply(GTA.sym[, c("gdp_d", "gdp_o")], 1, FUN = function(x) log(mean(x)))


# 4. Regressions ---------------------------------------------------------------

detach("package:goft")
detach("package:fitdistrplus")
detach("package:MASS")

run_regression <- function(data,type = "lm", 
                           dependant = "tij", 
                           controls, 
                           dependant.selection = "is.available", 
                           controls.selection,
                           weights = NULL){
  
  if(type == "lm"){
    reg <- paste0("lm(data = data,", dependant, " ~", controls, ", weights = ",weights,")")
  }
  
  if(type == "heckman"){
    
    reg <- paste0("selection(", dependant.selection, "~", controls.selection, ",", 
                  dependant,           "~", controls, ",",
                  "method = '2step', data = data)")
  }
  
  reg <- paste0("output <- ", reg)
  eval(parse(text = reg))
  return(output)
}

## 4.1 regression components  ---------------------------------------------------
fe <- paste0(names(GTA.sym)[(column.dummy.start+12):(ncol(GTA.sym)-1)], collapse = "+" ) # fixed effects
fe.vec <- names(GTA.sym)[(column.dummy.start+10):ncol(GTA.sym)]
controls <- "total_harmful + total_liberalising + log(distw_harmonic) + comlang_off + comcol + contig + comlang_ethno + fta_wto + lsci + lpi + landlocked  + geometric_avg_tariff" # controls
controls.trains <- "total_harmful  + log(distw_harmonic) + comlang_off + comcol + contig + comlang_ethno + fta_wto + lsci + lpi + landlocked  + geometric_avg_tariff" # controls

controls.vec <- c( "distw_harmonic", "comlang_off", "comcol", "contig", "comlang_ethno", "fta_wto", "lsci", "lpi", "landlocked", "geometric_avg_tariff")
controls.heck <- paste0(controls.vec, collapse = "+")
CRI <- "coverage.mean" #CRI
mast.chapters <- paste0(mast.names, collapse = "+") # Interventions disaggregated
controls.per.chapter <- paste0( mast.chapters, "+", "log(distw_harmonic) + comlang_off + comcol + contig + comlang_ethno + fta_wto + lsci + lpi + landlocked  + geometric_avg_tariff") # controls for Heckman
controls.delta <- "total_harmful + total_liberalising + fta_wto + lsci + lpi +  geometric_avg_tariff" # controls

## 4.2 Linreg -------------------------------------------------------------------------

### GTA ---------------------------------------------------------------------------
ols <- run_regression(GTA.sym, controls = paste0(controls, "+ CRI_sqrt_gm_harmful + CRI_sqrt_gm_liberalising")); summary(ols)
ols.fe <- run_regression(GTA.sym, controls = paste0("total_harmful + total_liberalising +", fe)); summary(ols.fe)
ols.per.chapter <- run_regression(GTA.sym, controls = paste0(controls.per.chapter, "+ CRI_sqrt_gm_harmful + CRI_sqrt_gm_liberalising")); summary(ols.per.chapter)
ols.fe.per.chapter <- run_regression(GTA.sym, controls = paste0(controls.per.chapter,"+", fe)); summary(ols.fe.per.chapter)

ols.delta <- run_regression(GTA.sym.delta, controls = paste0(controls.delta)); summary(ols.delta)
ols.delta.fe <- run_regression(GTA.sym.delta, controls = paste0("total_harmful + total_liberalising +", fe)); summary(ols.delta.fe)
ols.delta.per.chapter <- run_regression(GTA.sym.delta, controls = controls.per.chapter); summary(ols.delta.per.chapter)
ols.delta.fe.per.chapter <- run_regression(GTA.sym.delta, controls = paste0(controls.per.chapter,"+", fe)); summary(ols.delta.fe.per.chapter)


### TRAINS ------------------------------------------------------------------------
ols.trains <- run_regression(TRAINS.sym, controls = paste0(controls, "+ CRI_sqrt_gm")); summary(ols.trains)
ols.fe.trains <- run_regression(TRAINS.sym, controls = paste0("total_harmful +", fe)); summary(ols.fe.trains)
ols.per.chapter.trains <- run_regression(TRAINS.sym, controls = paste0(controls.per.chapter, "+ CRI_sqrt_gm")); summary(ols.per.chapter.trains)
ols.fe.per.chapter.trains <- run_regression(TRAINS.sym, controls = paste0(controls.per.chapter,"+", fe)); summary(ols.fe.per.chapter.trains)

ols.delta.trains <- run_regression(TRAINS.sym.delta, controls = paste0(controls.delta)); summary(ols.delta.trains)
ols.delta.fe.trains <- run_regression(TRAINS.sym.delta, controls = paste0("total_harmful + total_liberalising +", fe)); summary(ols.delta.fe.trains)
ols.delta.per.chapter.trains <- run_regression(TRAINS.sym.delta, controls = controls.per.chapter); summary(ols.delta.per.chapter.trains)
ols.delta.fe.per.chapter.trains <- run_regression(TRAINS.sym.delta, controls = paste0(controls.per.chapter,"+", fe)); summary(ols.delta.fe.per.chapter.trains)


### Linreg (weighted) -------------------------------------------------------------------------
ols.w <- run_regression(GTA.sym, controls = controls, weights = "CRI_sqrt_gm_harmful"); summary(ols.w) # geom mean
ols.fe.w <- run_regression(GTA.sym, controls = paste0(controls,"+", fe), weights = "CRI_sqrt_gm_harmful"); summary(ols.fe.w)
# ols.w.a <- run_regression(GTA.sym, controls = controls, weights = "coverage.mean"); summary(ols.w.a) # arithm mean
# ols.fe.w.a <- run_regression(GTA.sym, controls = paste0(controls,"+", fe), weights = "coverage.mean"); summary(ols.fe.w.a)


# 4.3 Heckman -------------------------------------------------------------------------
library(sampleSelection)


heckit <- run_regression(GTA.sym, type = "heckman", controls = controls, controls.selection = paste0(controls,  " + gdp")); summary(heckit)
heckit.trains <- run_regression(TRAINS.sym, type = "heckman", controls = controls.trains, controls.selection = paste0(controls.trains,  " + intranat.trade")); summary(heckit.trains)


fe.vec.adjust.gta <- fe.vec[!fe.vec %in% c("CUB", "MMR","BRB","CPV","LBR","SVK", "year_2019")]
heckit.fe <- run_regression(GTA.sym, 
                            type = "heckman", 
                            controls = paste0("total_harmful + total_liberalising","+",paste0(fe.vec, collapse = "+")), 
                            controls.selection = paste0(paste0(fe.vec, collapse = "+"),"+total_harmful + total_liberalising  + intranat.trade")); summary(heckit.fe)

fe.vec.adjust.trains <- fe.vec[! fe.vec %in% c("CRI", "year_2019")]
heckit.fe.trains <- run_regression(TRAINS.sym, 
                            type = "heckman", 
                            controls = paste0("total_harmful  + log(distw_harmonic) + geometric_avg_tariff","+",paste0(fe.vec.adjust.trains, collapse = "+")), 
                            controls.selection = paste0(paste0(fe.vec.adjust.trains, collapse = "+"),"+total_harmful + log(distw_harmonic) + geometric_avg_tariff + intranat.trade")); summary(heckit.fe.trains)



mast.names.adj.GTA <- mast.names[! mast.names %in% c("C_liberalising", "G_liberalising")]
heckit.fe.chapter <- run_regression(GTA.sym, 
                            type = "heckman", 
                            controls = paste0(paste0(mast.names.adj.GTA, collapse = "+") , " +  log(distw_harmonic) + geometric_avg_tariff","+",paste0(fe.vec.adjust.gta, collapse = "+")), 
                            controls.selection = paste0(paste0(fe.vec.adjust.gta, collapse = "+"),"+",paste0(paste0(mast.names.adj.GTA, collapse = "+"), " + log(distw_harmonic) + geometric_avg_tariff + intranat.trade"))); summary(heckit.fe.chapter)


mast.names.adj.TRAINS <- mast.names[! mast.names %in% c("C_liberalising", "G_liberalising")]
heckit.fe.chapter <- run_regression(GTA.sym, 
                                    type = "heckman", 
                                    controls = paste0(paste0(mast.names.adj.TRAINS, collapse = "+") , " +  log(distw_harmonic) + geometric_avg_tariff","+",paste0(fe.vec.adjust.trains, collapse = "+")), 
                                    controls.selection = paste0(paste0(fe.vec.adjust.trains, collapse = "+"),"+",paste0(paste0(mast.names.adj.TRAINS, collapse = "+"), " + log(distw_harmonic) + geometric_avg_tariff + intranat.trade"))); summary(heckit.fe.chapter)



# for (i in 14:length(mast.names.adj)) {
# 
#   t <- try(eval(parse(text = paste0("selection(data = GTA.sym, selection = is.available ~ total_harmful + total_liberalising +", paste0(paste0(mast.names.adj[1:i], collapse = "+"), "+ log(distw_harmonic) + geometric_avg_tariff","+",paste0(fe.vec, collapse = "+")), 
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

# library(beepr)
# for (i in 1:length(fe.vec)) {
# 
#   t <- try(eval(parse(text = paste0("selection(data = TRAINS.sym, selection = is.available ~ ", controls.heck, "+ intranat.trade, tij ~total_harmful +", paste0(fe.vec.adjust.trains[1:i], collapse = "+"), ", method = '2step')"))))
# 
#   if(!inherits(t, "try-error")){
#     print(i)
# 
#   } else{
#     cat("Error occurred when removing variable ", fe.vec.adjust.trains[i], "\n")
#     beep(sound = 2)
# }
# }

# remove libraries causing trouble
detach("package:goft")
detach("package:fitdistrplus")
detach("package:MASS")


## 4.4 Make table --------------------------------------------------------------
library(texreg)
out_stock_gta <- texreg(list(ols, ols.fe, heckit, heckit.fe), omit.coef = paste0(fe.vec,collapse = "|"), 
                    custom.model.names = c("OLS", "OLS-FE", "Heckit"))

out_stock_trains <- texreg(list(ols.trains, ols.fe.trains, heckit.trains, heckit.fe.trains), omit.coef = paste0(fe.vec,collapse = "|"), 
                        custom.model.names = c("OLS", "OLS-FE", "Heckit", "Heckit-FE"))


out_OLS_GTA_TRAINS <- texreg(list(ols, ols.fe, ols.trains, ols.fe.trains), omit.coef = paste0(fe.vec,collapse = "|"),
                             custom.model.names = c("GTA", "GTA-FE", "TRAINS", "TRAINS-FE"))


write.table(out_stock, file = paste0(path.data.out, "regression_results_GTA_stock.txt"))


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

reg <- run_regression(data = t,  dependant = "intervention.id", controls = paste0("Share +", fe.t));summary(reg)
reg <- run_regression(data = t,  dependant = "intervention.id", controls = "Share");summary(reg)



## 5.2 Trade flow threshold ----------------------------------------------------
sigma = 8

beta_intranat.trade <- 0.04456
threshold <- exp(2*(sigma-1))/beta_intranat.trade
threshold_sqrt <- sqrt(threshold)


### PPML -------------------------------------------------------------------------
# library(gravity)
# 
# ppml <- ppml(data = GTA.sym, 
#      dependent_variable = "tij", 
#      distance = "distw_harmonic", 
#      additional_regressors = c("total","comlang_off", "comcol", 
#                                "contig", "comlang_ethno", "fta_wto", "lsci", 
#                                  "lpi", "landlocked", "geometric_avg_tariff", "coverage.mean"))
# summary(ppml)
# 
# 
# ppml.fixed <- "ppml.fixed <- ppml(data = GTA.sym, dependent_variable = 'tij', distance = 'distw_harmonic', additional_regressors = c('total','comlang_off', 'comcol', 'contig', 'comlang_ethno', 'fta_wto', 'lsci', 'lpi', 'landlocked', 'geometric_avg_tariff', 'coverage.mean',"
# ppml.fixed <- paste0(ppml.fixed,"'", paste0(names(GTA.sym)[column.dummy.start:ncol(GTA.sym)], collapse = "','" ),"'", "))")
# eval(parse(text = ppml.fixed))
# summary(ppml.fixed)
# 
# ### Bind together
# names(heckit$lm$coefficients) <- gsub("XO", "", names(heckit$lm$coefficients))
# names(heckit$lm$qr) <- gsub("XO", "", names(heckit$qr$coefficients))
# 
