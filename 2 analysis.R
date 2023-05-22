
rm(list = ls())

library(gtalibrary)
library(tidyverse)

options(scipen = 999)
source("BA_Thesis_code/00 Terms and Definitions.R")



# 1. Load data -----------------------------------------------------------------
GTA.d <- readRDS(file = paste0(path.data.out, "GTA_delta_final.RData"))
TRA.d <- readRDS(file = paste0(path.data.out, "TRAINS_delta_final.RData"))

GTA <- readRDS(file = paste0(path.data.out, "GTA_final.RData"))
TRA <- readRDS(file = paste0(path.data.out, "TRAINS_final.RData"))

CRI.GTA <- readxl::read_xlsx(path = paste0(path.data.out, "GTA_Measurement_index.xlsx"))
CRI.TRA <- readxl::read_xlsx(path = paste0(path.data.out, "TRAINS_Measurement_index.xlsx"))

GTA <- GTA %>%
  select(-c(CRI_sqrt_gm_harmful, CRI_sqrt_gm_liberalising, CRI_harmful, CRI_liberalising, CRI_gm_harmful, CRI_gm_liberalising, CRI_harmful, CRI_liberalising, CRI_sqrt_harmful, CRI_sqrt_liberalising, L_harmful, L_liberalising, CRI))%>%
  left_join(CRI.GTA, by = c("country.1", "country.2", "year", "chapter"))
GTA.d <- GTA.d %>%
  select(-c(CRI_sqrt_gm_harmful, CRI_sqrt_gm_liberalising, CRI_harmful, CRI_liberalising, CRI_gm_harmful, CRI_gm_liberalising, CRI_harmful, CRI_liberalising, CRI_sqrt_harmful, CRI_sqrt_liberalising, CRI))%>%
  left_join(CRI.GTA, by = c("country.1", "country.2", "year", "chapter"))


TRA <- TRA %>%
  select(-c(CRI_sqrt, CRI_sqrt_gm, CRI, CRI_gm, CRI.1))%>%
  left_join(CRI.TRA, by = c("country.1", "country.2", "year", "chapter"))
TRA.d <- TRA.d %>%
  select(-c(CRI_sqrt, CRI_sqrt_gm, CRI, CRI_gm, CRI.1))%>%
  left_join(CRI.TRA, by = c("country.1", "country.2", "year", "chapter"))


GTA <- GTA %>% filter(chapter == "D")
TRA <- TRA %>% filter(chapter == "D")
GTA.d <- GTA.d %>% filter(chapter == "D")
TRA.d <- TRA.d %>% filter(chapter == "D")
column.dummy.start <- 50


rename_df <- function(df){
  df <- df %>%
    rename(c("Tariff" = "geometric_avg_tariff",
             "Dist" = "distw_harmonic",
             "L_off" = "comlang_off",
             "Comcol" = "comcol", 
             "Contig" = "contig", 
             "L_eth" = "comlang_ethno", 
             "FTA" = "fta_wto", 
             "LSCI" = "lsci", 
             "LPI" = "lpi", 
             "LandL" = "landlocked",
             "NTM_H" = "total_harmful",
             "NTM_L" = "total_liberalising",
    ))
  
  return(df)
}

GTA <- rename_df(GTA)
TRA <- rename_df(TRA)
GTA.d <- rename_df(GTA.d)
TRA.d <- rename_df(TRA.d)

# 2 Regression components  ---------------------------------------------------
fe.vec <- names(GTA)[(column.dummy.start+1):(ncol(GTA)-8)]
fe <- paste0(fe.vec, collapse = "+")

controls.vec <- c("log(Dist)", "L_off", "Comcol", "Contig", "L_eth", 
                  "FTA", "LSCI", "LPI", "LandL", "Tariff", "FXV")
controls <- paste0(controls.vec, collapse = "+")
controls.vec.fe <- c("log(Dist)", "L_off", "Comcol", "Contig", "L_eth", 
                     "FTA", "Tariff", "FXV")
controls.fe <- paste0(controls.vec.fe, collapse = "+")


controls.d.vec <- c("FTA", "LSCI", "LPI", "Tariff", "FXV") #exclude stuff which should not change over time
controls.d <- paste0(controls.d.vec, collapse = "+")
controls.d.vec.fe <- c("FTA", "Tariff", "FXV") #exclude stuff which should not change over time
controls.d.fe <- paste0(controls.d.vec.fe, collapse = "+")


GTA.NTM <- "NTM_H + NTM_L"
TRA.NTM <- "NTM_H"

GTA.CRI <- "CRI_sqrt_gm"
TRA.CRI <- "CRI_sqrt_gm"


mast.chap <- paste0(mast.names, collapse = "+") # Interventions disaggregated
mast.chap.TRA <- paste0(mast.names[!grepl("liberalising", mast.names)], collapse = "+")



# 3. OLS -------------------------------------------------------------------------
### GTA ---------------------------------------------------------------------------

eval(parse(text = paste0("tt <- ivreg(data = GTA, tij ~ ", GTA.NTM, "+", controls, "+",fe, "|", controls,"+",fe, "+ ",GTA.CRI, ")"))); summary(tt)
ols           <- reg(GTA,   cont = paste0(GTA.NTM,  "+", controls)); summary(ols)
ols.fe        <- reg(GTA,   cont = paste0(GTA.NTM,  "+", controls.fe,"+", fe));   summary(ols.fe)

# Add weights
ols.w.stand <- reg(GTA, cont = paste0(GTA.NTM,"+", controls),         weights = "CRI_gm"); summary(ols.w.stand) # geom mean
ols.w.sqrt  <- reg(GTA, cont = paste0(GTA.NTM,"+", controls),         weights = "CRI_sqrt_gm"); summary(ols.w.sqrt) # geom mean
ols.w.log   <- reg(GTA, cont = paste0(GTA.NTM,"+", controls),         weights = "CRI_log_gm"); summary(ols.w.log) # geom mean

ols.w.fe      <- reg(GTA, cont = paste0(GTA.NTM,"+", controls.fe,"+", fe), weights = "CRI_gm"); summary(ols.w.fe)
ols.w.sqrt.fe <- reg(GTA, cont = paste0(GTA.NTM,"+", controls.fe,"+", fe), weights = "CRI_sqrt_gm"); summary(ols.w.sqrt.fe)
ols.w.log.fe  <- reg(GTA, cont = paste0(GTA.NTM,"+", controls.fe,"+", fe), weights = "CRI_log_gm"); summary(ols.w.log.fe)
#coeftest(ols.w.sqrt.fe, vcov = vcov(ols.w.sqrt.fe, type = "HC0")) #test robust standard errors

texreg(list(ols.w.fe, ols.w.sqrt.fe, ols.w.log.fe), omit.coef = paste0(fe.vec,collapse = "|"), single.row = T)

texreg(list(ols.fe,ols.w.fe,ols.w.sqrt.fe,ols.w.log.fe), omit.coef = paste0(fe.vec,collapse = "|"), single.row = T)

results.ols.gta <- list(ols, ols.fe, ols.w.stand, ols.w.sqrt, ols.w.log, ols.w.fe, ols.w.sqrt.fe, ols.w.log.fe)
saveRDS(results.ols.gta, file = paste0(path.data.reg , "GTA_OLS_Results_D.Rds"))
results.ols.gta <- readRDS(file = paste0(path.data.reg , "GTA_OLS_Results_D.Rds"))

### TRA ---------------------------------------------------------------------------

#eval(parse(text = paste0("tt <- ivreg(data = TRA, tij ~ ", TRA.NTM, "+", controls, "+",fe, "|", controls,"+",fe, "+ ",TRA.CRI, ")"))); summary(tt)
TRA.ols           <- reg(TRA,   cont = paste0(TRA.NTM,  "+", controls)); summary(TRA.ols)
TRA.ols.fe        <- reg(TRA,   cont = paste0(TRA.NTM,  "+", controls.fe,"+", fe));   summary(TRA.ols.fe)

# Add weights
TRA.ols.w.stand <- reg(TRA, cont = paste0(TRA.NTM,"+", controls),         weights = "CRI_gm"); summary(TRA.ols.w.stand) # geom mean
TRA.ols.w.sqrt  <- reg(TRA, cont = paste0(TRA.NTM,"+", controls),         weights = "CRI_sqrt_gm"); summary(TRA.ols.w.sqrt) # geom mean
TRA.ols.w.log   <- reg(TRA, cont = paste0(TRA.NTM,"+", controls),         weights = "CRI_log_gm"); summary(TRA.ols.w.log) # geom mean

TRA.ols.w.fe      <- reg(TRA, cont = paste0(TRA.NTM,"+", controls.fe,"+", fe), weights = "CRI_gm"); summary(TRA.ols.w.fe)
TRA.ols.w.sqrt.fe <- reg(TRA, cont = paste0(TRA.NTM,"+", controls.fe,"+", fe), weights = "CRI_sqrt_gm"); summary(TRA.ols.w.sqrt.fe)
TRA.ols.w.log.fe  <- reg(TRA, cont = paste0(TRA.NTM,"+", controls.fe,"+", fe), weights = "CRI_log_gm"); summary(TRA.ols.w.log.fe)

results.ols.tra <- list(TRA.ols, TRA.ols.fe, TRA.ols.w.stand, TRA.ols.w.sqrt, TRA.ols.w.log, TRA.ols.w.fe, TRA.ols.w.sqrt.fe, TRA.ols.w.log.fe)
saveRDS(results.ols.tra, file = paste0(path.data.reg, "TRA_OLS_Results_D.Rds"))
results.ols.tra <- readRDS(file = paste0(path.data.reg , "TRA_OLS_Results_D.Rds"))

# 4. Heckman -------------------------------------------------------------------------

library(sampleSelection)

# Baseline Heckit
h.GTA <- reg(GTA, 
             type = "heckman",
             #weights = "GTA$CRI_sqrt_gm",
             cont =           paste0(GTA.NTM,"+", controls) , 
             cont.selection = paste0(GTA.NTM,"+", controls, "+ intranat.trade")); summary(h.GTA)

h.GTA.w <- reg(GTA, 
             type = "heckman",
             weights = "GTA$CRI_sqrt_gm",
             cont =           paste0(GTA.NTM,"+", controls) , 
             cont.selection = paste0(GTA.NTM,"+", controls, "+ intranat.trade")); summary(h.GTA)


h.TRA <- reg(TRA, 
             type = "heckman",
             #weights = "TRA$CRI_sqrt_gm",
             cont =           paste0(TRA.NTM, "+",controls), 
             cont.selection = paste0(TRA.NTM, "+",controls,"+", "+ intranat.trade")); summary(h.TRA)

h.TRA.w <- reg(TRA, 
               type = "heckman", 
               weights = "TRA$CRI_sqrt_gm",
               cont =           paste0(TRA.NTM, "+",controls), 
               cont.selection = paste0(TRA.NTM, "+",controls,"+", "+ intranat.trade")); summary(h.TRA.w)




# FE Heckit
GTA.heck.exclude <- c("CMR", "COM", "GAB", "GEO","GIN", "HKG", "ISL","LAO", "LBN", "BRN", "BFA", "BEN", "ARE", "CUB","MOZ", "MMR","BRB","CPV","LBR","SVK", "year_2019", "VEN", "MUS", "MWI", "NER", "TCD", "TGO", "TTO")
fe.adjust.GTA.vec <- fe.vec[!fe.vec %in% GTA.heck.exclude]
fe.adjust.GTA <- paste0(fe.adjust.GTA.vec, collapse = "+")
GTA.heck <- GTA %>% select(-c(all_of(GTA.heck.exclude)))
h.GTA.fe.w <- reg(GTA.heck, 
                type = "heckman", 
                weights = "GTA.heck$CRI_sqrt_gm",
                cont =           paste0(GTA.NTM,"+",controls.fe,"+", fe.adjust.GTA), 
                cont.selection = paste0(GTA.NTM,"+",controls.fe,"+", fe.adjust.GTA, "+ intranat.trade")); summary(h.GTA.fe.w)

h.GTA.fe.w.log <- reg(GTA.heck, 
                  type = "heckman", 
                  weights = "GTA.heck$CRI_log_gm",
                  cont =           paste0(GTA.NTM,"+",controls.fe,"+", fe.adjust.GTA), 
                  cont.selection = paste0(GTA.NTM,"+",controls.fe,"+", fe.adjust.GTA, "+ intranat.trade")); summary(h.GTA.fe.w)


texreg(list(h.GTA.fe.w, h.GTA.fe.w.log))

TRA.heck.exclude <- c("CUB","LBR", "CRI","SVK", "year_2019", "VEN", "AUT", "BFA", "BGR", "DNK", "ESP", "FIN", "FRA", "GAB", "GRC", "HUN", "IRL", "ITA", "LTU", "MDA", "MLT", "PRT","SVN", "SWE", "ZMB")
fe.adjust.TRA.vec <- fe.vec[!fe.vec %in% TRA.heck.exclude]
fe.adjust.TRA <- paste0(fe.adjust.TRA.vec, collapse = "+")
TRA.heck <- TRA %>% select(-c(all_of(TRA.heck.exclude)))
h.TRA.w.fe <- reg(TRA.heck, 
                type = "heckman", 
                weights = "TRA.heck$CRI_sqrt_gm",
                cont =           paste0(TRA.NTM,"+",controls.fe,"+", fe.adjust.TRA), #, "+ log(distw_harmonic) + geometric_avg_tariff"
                cont.selection = paste0(TRA.NTM,"+",controls.fe,"+", fe.adjust.TRA,"+", "intranat.trade")); summary(h.TRA.w.fe) #+ log(distw_harmonic) + geometric_avg_tariff + 

h.TRA.w.fe.log <- reg(TRA.heck, 
                  type = "heckman", 
                  weights = "TRA.heck$CRI_log_gm",
                  cont =           paste0(TRA.NTM,"+",controls.fe,"+", fe.adjust.TRA), #, "+ log(distw_harmonic) + geometric_avg_tariff"
                  cont.selection = paste0(TRA.NTM,"+",controls.fe,"+", fe.adjust.TRA,"+", "intranat.trade")); summary(h.TRA.w.fe) #+ log(distw_harmonic) + geometric_avg_tariff + 



results.heckit <- list(h.GTA, h.TRA, h.GTA.w, h.TRA.w, h.GTA.fe.w, h.TRA.w.fe)
saveRDS(results.heckit, file = paste0(path.data.out, "Heckit_Results_D.Rds"))
results.heckit <- readRDS(file = paste0(path.data.out , "Heckit_Results_D.Rds"))


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
fe.adjust.TRA.vec <- fe.vec[! fe.vec %in% c("CUB","LBR", "CRI","SVK", "year_2019", "VEN")]
fe.adjust.GTA <- fe.vec[!fe.vec %in% c("CMR", "COM", "GAB", "GEO","GIN", "HKG", "ISL","LAO", "LBN", "BRN", "BFA", "BEN", "ARE", "CUB","MOZ", "MMR","BRB","CPV","LBR","SVK", "year_2019", "VEN", "MUS", "MWI", "NER", "TCD", "TGO", "TTO")]

names(h.fe.TRA$coefficients[is.na(h.fe.TRA$coefficients)])

for (i in 1:length(fe.adjust.GTA.vec)) {
  
  t <- try(eval(parse(text = paste0("selection(data = GTA.heck.chap, selection = is.available ~ ",paste0(fe.adjust.GTA.vec[1:i], collapse = "+"), "+ intranat.trade, tij ~ ",mast.chap," +", paste0(fe.adjust.GTA.vec[1:i], collapse = "+"), ", method = '2step', weights = GTA.heck.chap$CRI_sqrt_gm)"))))
  h.fe.TRA$coefficients[is.na(h.fe.TRA$coefficients)]
  summary(t)
  if(!inherits(t, "try-error")){
  #if(!inherits(t, "try-error")){
    print(i)
    
  } else{
    cat("Error occurred when removing variable ", fe.adjust.GTA[i], "\n")
    beep(sound = 2)
  }
}

# remove libraries causing trouble
detach("package:goft")
detach("package:fitdistrplus")
detach("package:MASS")




# 5. Delta -------------------------------------------------------------------------

ols.d    <- reg(GTA.d, cont = paste0(GTA.NTM,  "+", controls.d),                  ); summary(ols.d)
ols.d.fe <- reg(GTA.d, cont = paste0(GTA.NTM,  "+", controls.d.vec.fe,"+", fe),   ); summary(ols.d.fe)
ols.d.TRA <-    reg(TRA.d, cont = paste0(TRA.NTM,"+", controls.d),                ); summary(ols.d.TRA)
ols.d.fe.TRA <- reg(TRA.d, cont = paste0(TRA.NTM,"+", controls.d.vec.fe,"+", fe), ); summary(ols.d.fe.TRA)


ols.d.w    <- reg(GTA.d, cont = paste0(GTA.NTM,  "+", controls.d),                  weights = "CRI_sqrt_gm"); summary(ols.d.w)
ols.d.w    <- reg(GTA.d, cont = paste0(GTA.NTM,  "+", controls.d),                  weights = "CRI_log_gm"); summary(ols.d.w)

ols.d.w.fe <- reg(GTA.d, cont = paste0(GTA.NTM,  "+", controls.d.vec.fe,"+", fe),   weights = "CRI_sqrt_gm"); summary(ols.d.w.fe)
ols.d.w.TRA <-    reg(TRA.d, cont = paste0(TRA.NTM,"+", controls.d),                weights = "CRI_log_gm"); summary(ols.d.w.TRA)
ols.d.w.fe.TRA <- reg(TRA.d, cont = paste0(TRA.NTM,"+", controls.d.vec.fe,"+", fe), weights = "CRI_sqrt_gm"); summary(ols.d.w.fe.TRA)

results.delta <- list(ols.d, ols.d.fe, ols.d.TRA, ols.d.fe.TRA)
results.delta.w <- list(ols.d.w, ols.d.w.fe, ols.d.w.TRA, ols.d.w.fe.TRA)

saveRDS(results.delta.w, file = paste0(path.data.out, "Delta_Results_D.Rds"))
results.delta.w <- readRDS(file = paste0(path.data.out , "Delta_Results_D.Rds"))
library(texreg)

# 6. Chapters -------------------------------------------------------------------------


ols.chap      <- reg(GTA, cont = paste0(mast.chap,"+", controls));                                     summary(ols.chap)
ols.chap.w    <- reg(GTA, cont = paste0(mast.chap,"+", controls),            weights = "CRI_sqrt_gm"); summary(ols.chap.w)
ols.fe.chap   <- reg(GTA, cont = paste0(mast.chap,"+", controls.fe,"+", fe));                          summary(ols.fe.chap)
ols.fe.chap.w <- reg(GTA, cont = paste0(mast.chap,"+", controls.fe,"+", fe), weights = "CRI_sqrt_gm"); summary(ols.fe.chap.w)

TRA.ols.chap      <- reg(TRA, cont = paste0(mast.chap,"+", controls));                                     summary(TRA.ols.chap)
TRA.ols.chap.w    <- reg(TRA, cont = paste0(mast.chap,"+", controls),            weights = "CRI_sqrt_gm"); summary(TRA.ols.chap.w)
TRA.ols.fe.chap   <- reg(TRA, cont = paste0(mast.chap,"+", controls.fe,"+", fe));                          summary(TRA.ols.fe.chap)
TRA.ols.fe.chap.w <- reg(TRA, cont = paste0(mast.chap,"+", controls.fe,"+", fe), weights = "CRI_sqrt_gm"); summary(TRA.ols.fe.chap.w)



t <- try(eval(parse(text = paste0("selection(data = GTA.heck.chap, selection = is.available ~ ",paste0(fe.adjust.GTA.vec[1:i], collapse = "+"), "+ intranat.trade, tij ~ ",mast.names.adj," +", paste0(fe.adjust.GTA.vec[1:i], collapse = "+"), ", method = '2step', weights = GTA.heck.chap$CRI_sqrt_gm)"))))
h.fe.TRA$coefficients[is.na(h.fe.TRA$coefficients)]
summary(t)

mast.names.adj <- paste0(mast.names[! mast.names %in% c("C_liberalising", "G_liberalising")], collapse = "+")
GTA.heck.exclude <- c("ARG","AUS", "CMR", "COM", "GAB", "GEO","GIN", "HKG", "ISL","LAO", "LBN", "BRN", "BFA", "BEN", "ARE", "CUB","MOZ", "MMR","BRB","CPV","LBR","SVK", "year_2019", "VEN", "MUS", "MWI", "NER", "TCD", "TGO", "TTO")
fe.adjust.GTA.vec <- fe.vec[!fe.vec %in% GTA.heck.exclude]
fe.adjust.GTA <- paste0(fe.adjust.GTA.vec, collapse = "+")
GTA.heck.chap <- GTA %>% select(-c(all_of(GTA.heck.exclude)))
h.GTA.fe.w.chap <- reg(GTA.heck.chap, 
                  type = "heckman", 
                  weights = "GTA.heck.chap$CRI_sqrt_gm",
                  cont =           paste0(mast.names.adj,"+",controls.fe,"+", fe.adjust.GTA), 
                  cont.selection = paste0(mast.names.adj,"+",controls.fe,"+", fe.adjust.GTA, "+ intranat.trade")); summary(h.GTA.fe.w.chap)



t <- try(eval(parse(text = paste0("selection(data = TRA.heck, selection = is.available ~ ",paste0(controls.vec.fe[1:i], collapse = "+") , "+",paste0(fe.adjust.TRA.vec, collapse = "+"), "+ intranat.trade, tij ~ ",paste0(controls.vec.fe[1:i], collapse = "+"),"+",paste0(mast.names.adj.TRA, collapse = "+")," +", paste0(fe.adjust.TRA.vec, collapse = "+"), ", method = '2step', weights = TRA.heck$CRI_sqrt_gm)"))))
h.fe.TRA$coefficients[is.na(h.fe.TRA$coefficients)]
summary(t)

TRA.heck.exclude <- c("ARG", "CUB","LBR", "CRI","SVK", "year_2019", "VEN", "AUT", "BFA", "BGR", "DNK", "ESP", "FIN", "FRA", "GAB", "GRC", "HUN", "IRL", "ITA", "LTU", "MDA", "MLT", "PRT","SVN", "SWE", "ZMB")
fe.adjust.TRA.vec <- fe.vec[!fe.vec %in% TRA.heck.exclude]
fe.adjust.TRA <- paste0(fe.adjust.TRA.vec, collapse = "+")
TRA.heck <- TRA %>% select(-c(all_of(TRA.heck.exclude)))
mast.names.adj.TRA <- paste0(mast.names[!grepl("liberalising", mast.names)], collapse = "+")

mast.names.adj.TRA <- mast.names[!grepl("liberalising", mast.names)]
test <- mast.names.adj.TRA[mast.names.adj.TRA != "M_harmful"]


h.TRA.w.fe.chap <- reg(TRA.heck, 
                  type = "heckman", 
                  #weights = "TRA.heck$CRI_sqrt_gm",
                  cont =           paste0(test,"+",controls.fe,"+", fe.adjust.TRA), #, "+ log(distw_harmonic) + geometric_avg_tariff"
                  cont.selection = paste0(test,"+",controls.fe,"+", fe.adjust.TRA,"+", "intranat.trade")); summary(h.TRA.w.fe.chap) #+ log(distw_harmonic) + geometric_avg_tariff + 



results_chapter <- list(ols.fe.chap.w, TRA.ols.fe.chap.w, h.GTA.fe.w.chap, h.TRA.w.fe.chap)
saveRDS(results_chapter, file = paste0(path.data.out, "Chapter_Results_D.Rds"))

# Delta
ols.d.chap      <- reg(GTA.d, cont = paste0(mast.chap,"+", controls.d));                         summary(ols.d.chap)
ols.d.chap.w    <- reg(GTA.d, cont = paste0(mast.chap,"+", controls.d), weights = "CRI_sqrt_gm");summary(ols.d.chap)
ols.d.fe.chap <- reg(GTA.d, cont = paste0(mast.chap,"+", controls.d.vec.fe,"+", fe), weights = "CRI_sqrt_gm"); summary(ols.d.fe.chap)
ols.d.chap.TRA <-    reg(TRA.d, cont = paste0(mast.chap.TRA,"+", controls.d),        weights = "CRI_sqrt_gm"); summary(ols.d.chap.TRA)
ols.d.fe.chap.TRA <- reg(TRA.d, cont = paste0(mast.chap.TRA,"+", controls.d.vec.fe,"+", fe), weights = "CRI_sqrt_gm");  summary(ols.d.fe.chap.TRA)


baseline_all_delta <- texreg(list(ols.d.chap, ols.d.chap.w, ols.d.fe.chap, ols.d.chap.TRA, ols.d.fe.chap.TRA), omit.coef = paste0(fe.vec,collapse = "|"), single.row = T)


results_chapter_delta <- list(ols.fe.chap.w, TRA.ols.fe.chap.w, h.GTA.fe.w.chap, h.TRA.w.fe.chap)
saveRDS(results_chapter_delta, file = paste0(path.data.out, "Chapter_Delta_Results_D.Rds"))


# 7. Make table --------------------------------------------------------------
library(texreg)


# Baseline: OLS GTA, OLS TRA, Heckit OLS, Heckit TRA
baseline_all <- texreg(list(ols, TRA.ols, ols.w.sqrt.fe, TRA.ols.w.sqrt.fe, h.GTA.fe.w, h.TRA.w.fe), omit.coef = paste0(fe.vec,collapse = "|"), single.row = T)
saveRDS(baseline_all, file = paste0(path.data.reg, "Latex_Table_Baseline_Stock_D.Rds"))

table_baseline_all <- make_table(baseline_all)



baseline_all_delta.w <- texreg(results.delta.w, omit.coef = paste0(fe.vec,collapse = "|"), single.row = T)
saveRDS(baseline_all_delta.w, file = paste0(path.data.reg, "Latex_Table_Baseline_Delta_D.Rds"))
all_delta <- texreg(results.delta, omit.coef = paste0(fe.vec,collapse = "|"), single.row = T)



chapter_all <- texreg(results_chapter, omit.coef = paste0(fe.vec,collapse = "|"), single.row = T)
saveRDS(chapter_all, file = paste0(path.data.reg, "Latex_Table_Chapter_D.Rds"))

chapter_all_delta <- texreg(results_chapter_delta, omit.coef = paste0(fe.vec,collapse = "|"), single.row = T)
saveRDS(chapter_all_delta, file = paste0(path.data.reg, "Latex_Table_Chapter_Delta_D.Rds"))








# Load required libraries
library(tidyverse)
library(kableExtra)

make_table <- function(regressions){
  
  # Extract the data from the string
  data <- read.csv(text = regressions, sep = "&", skip = 7, header = F)
  cols <- ncol(data)
  names(data) <- c("Variable", paste0("Model ", 1:(cols-1)))
  data <- data %>% 
    filter(!grepl("^S: ", Variable))%>%
    mutate(Variable = gsub("O: ", "", Variable))
  data[, cols] <- gsub("\\", "", as.vector(data[,cols]), fixed = T)
  
  
  data <- data %>% 
    pivot_longer(cols = 2:cols, names_to = "Model", values_to = "Value")%>%
    filter(grepl("[0-9]|[a-zA-Z]", Value))%>%
    mutate(Variable = gsub(" ", "", Variable))%>%
    pivot_wider(id_cols = 1, names_from = "Model", values_from = "Value")
  
  
  data[is.na(data)] <- ""
  # Format the output table as LaTeX code
  output_latex <- data %>%
    knitr::kable(format = "latex", booktabs = TRUE, escape = FALSE, align = "c")
  # Print the output LaTeX code
  
  return(output_latex)
}




















out_stock_gta <- texreg(results.ols.gta, omit.coef = paste0(fe.vec,collapse = "|"), 
                        custom.model.names = c("OLS", "OLS-FE",))

out_stock_TRA <- texreg(list(ols.TRA, ols.fe.TRA, heckit.TRA, heckit.fe.TRA), omit.coef = paste0(fe.vec,collapse = "|"), 
                        custom.model.names = c("OLS", "OLS-FE", "Heckit", "Heckit-FE"))


out_OLS_GTA_TRA <- texreg(list(ols, ols.fe, ols.TRA, ols.fe.TRA), #omit.coef = paste0(fe.vec,collapse = "|"),
                          custom.model.names = c("GTA", "GTA-FE", "TRA", "TRA-FE"))


out_Heckit_GTA_TRA <- texreg(list(heckit, heckit.fe, heckit.TRA, heckit.fe.TRA), #omit.coef = paste0(fe.vec,collapse = "|"),
                             custom.model.names = c("GTA", "GTA-FE", "TRA", "TRA-FE"))


write.table(out_stock_gta, file = paste0(path.data.out, "regression_results_GTA_stock.txt"))
write.table(out_stock_TRA, file = paste0(path.data.out, "regression_results_TRA_stock.txt"))


# 8. Other tests ----------------------------------------------------------------
## 8.1 NTMs vs export share ----------------------------------------------------
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

reg.t <- reg(data = t,  dependant = "intervention.id", controls = paste0("Share +", fe.t));summary(reg)
reg.t <- reg(data = t,  dependant = "intervention.id", controls = "Share");summary(reg)



## 8.2 Trade flow threshold ----------------------------------------------------


GTA.t <- GTA
GTA.t$is.available.2 <- ifelse(GTA$is.available == 1, 1, -1)
ols.t <- reg(GTA.t, dependant = "is.available.2", controls = paste0(controls, "+ intranat.trade"))
beta_intranat.trade <- ols.t$coefficients["intranat.trade"]
sigma = 8

beta_intranat.trade <- 0.04456
threshold <- exp(2*(sigma-1))/beta_intranat.trade
threshold_sqrt <- sqrt(threshold)

## 8.3 MAST chapter distribution -----------------------------------------------

GTA.MAST <- readRDS(file = paste0(path.data.out, "GTA_asymmetric_isic.RData")) 
GTA.MAST <- aggregate(data = GTA.MAST, intervention.id ~ mast.chapter, FUN = function(x) length(unique(x)))
GTA.MAST <- aggregate(data = GTA.MAST, intervention.id ~ implementing.jurisdiction, FUN = function(x) length(unique(x)))


hist(GTA.MAST$chapter)


TRAINS.MAST <- readRDS(file = paste0(path.data.out, "TRAINS_asymmetric_isic.RData"))
TRAINS.MAST <- aggregate(data = TRAINS.MAST, measure.id ~ mast.chapter, FUN = function(x) length(unique(x)))
TRAINS.MAST <- aggregate(data = TRAINS.MAST, measure.id ~ implementing.jurisdiction, FUN = function(x) length(unique(x)))
hist(TRAINS.MAST$measure.id, breaks = 100)
