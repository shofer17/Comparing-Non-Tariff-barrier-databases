
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

TRA <- TRA %>%
  select(-c(CRI_sqrt, CRI_sqrt_gm, CRI, CRI_gm, CRI.1))%>%
  left_join(CRI.TRA, by = c("country.1", "country.2", "year", "chapter"))


GTA <- GTA %>% filter(chapter == "D")
TRA <- TRA %>% filter(chapter == "D")
GTA.d <- GTA.d %>% filter(chapter == "D")
TRA.d <- TRA.d %>% filter(chapter == "D")
column.dummy.start <- 50


# 2 Regression components  ---------------------------------------------------
fe.vec <- names(GTA)[(column.dummy.start+1):(ncol(GTA)-8)]
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

ols.w.fe      <- reg(GTA, cont = paste0(GTA.NTM,"+", controls,"+", fe), weights = "CRI_gm"); summary(ols.w.fe)
ols.w.sqrt.fe <- reg(GTA, cont = paste0(GTA.NTM,"+", controls,"+", fe), weights = "CRI_sqrt_gm"); summary(ols.w.sqrt.fe)
ols.w.log.fe  <- reg(GTA, cont = paste0(GTA.NTM,"+", controls,"+", fe), weights = "CRI_log_gm"); summary(ols.w.log.fe)


results.ols.gta <- list(ols, ols.fe, ols.w.stand, ols.w.sqrt, ols.w.log, ols.w.fe, ols.w.sqrt.fe, ols.w.log.fe)
saveRDS(results.ols.gta, file = paste0(path.data.reg , "GTA_OLS_Results.Rds"))

### TRA ---------------------------------------------------------------------------

#eval(parse(text = paste0("tt <- ivreg(data = TRA, tij ~ ", TRA.NTM, "+", controls, "+",fe, "|", controls,"+",fe, "+ ",TRA.CRI, ")"))); summary(tt)
TRA.ols           <- reg(TRA,   cont = paste0(TRA.NTM,  "+", controls)); summary(TRA.ols)
TRA.ols.fe        <- reg(TRA,   cont = paste0(TRA.NTM,  "+", controls.fe,"+", fe));   summary(TRA.ols.fe)

# Add weights
TRA.ols.w.stand <- reg(TRA, cont = paste0(TRA.NTM,"+", controls),         weights = "CRI_gm"); summary(TRA.ols.w.stand) # geom mean
TRA.ols.w.sqrt  <- reg(TRA, cont = paste0(TRA.NTM,"+", controls),         weights = "CRI_sqrt_gm"); summary(TRA.ols.w.sqrt) # geom mean
TRA.ols.w.log   <- reg(TRA, cont = paste0(TRA.NTM,"+", controls),         weights = "CRI_log_gm"); summary(TRA.ols.w.log) # geom mean

TRA.ols.w.fe      <- reg(TRA, cont = paste0(TRA.NTM,"+", controls,"+", fe), weights = "CRI_gm"); summary(TRA.ols.w.fe)
TRA.ols.w.sqrt.fe <- reg(TRA, cont = paste0(TRA.NTM,"+", controls,"+", fe), weights = "CRI_sqrt_gm"); summary(TRA.ols.w.sqrt.fe)
TRA.ols.w.log.fe  <- reg(TRA, cont = paste0(TRA.NTM,"+", controls,"+", fe), weights = "CRI_log_gm"); summary(TRA.ols.w.log.fe)


results.ols.gta <- list(TRA.ols, TRA.ols.fe, TRA.ols.w.stand, TRA.ols.w.sqrt, TRA.ols.w.log, TRA.ols.w.fe, TRA.ols.w.sqrt.fe, TRA.ols.w.log.fe)
saveRDS(results.ols.gta, file = paste0(path.data.reg, "TRA_OLS_Results.Rds"))

# 4. Heckman -------------------------------------------------------------------------

library(sampleSelection)

# Baseline Heckit
h.GTA <- reg(GTA, 
             type = "heckman",
             #weights = "GTA$CRI_sqrt_gm",
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
fe.adjust.GTA <- paste0(fe.vec[!fe.vec %in% c("ARE", "CUB", "MMR","BRB","CPV","LBR","SVK", "year_2019", "VEN")], collapse = "+")
h.fe.GTA <- reg(GTA, 
                type = "heckman", 
                weights = "GTA$CRI_sqrt",
                cont =           paste0(GTA.NTM,"+", fe.adjust.GTA), 
                cont.selection = paste0(GTA.NTM,"+", fe.adjust.GTA, "+ intranat.trade")); summary(h.fe.GTA)

fe.adjust.TRA <- paste0(fe.vec[! fe.vec %in% c("CUB","LBR", "CRI","SVK", "year_2019", "VEN")], collapse = "+")
h.fe.TRA <- reg(TRA, 
                type = "heckman", 
                cont =           paste0(TRA.NTM,"+", fe.adjust.TRA, "+ log(distw_harmonic) + geometric_avg_tariff"), 
                cont.selection = paste0(TRA.NTM,"+", fe.adjust.TRA, "+ log(distw_harmonic) + geometric_avg_tariff + intranat.trade")); summary(h.fe.TRA)



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
fe.adjust.TRA.vec <- fe.vec[! fe.vec %in% c("CUB","LBR", "CRI","SVK", "year_2019", "VEN")]
fe.adjust.GTA <- fe.vec[!fe.vec %in% c("ARE", "CUB", "MMR","BRB","CPV","LBR","SVK", "year_2019", "VEN")]

for (i in 94:length(fe.adjust.GTA)) {
  
  t <- try(eval(parse(text = paste0("selection(data = GTA, selection = is.available ~ ",paste0(fe.adjust.GTA[1:i], collapse = "+"), "+ intranat.trade, tij ~ total_harmful +", paste0(fe.adjust.GTA[1:i], collapse = "+"), ", method = '2step', weights = GTA$CRI_sqrt_gm)"))))
  
  if(!inherits(t, "try-error")){
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

ols.d    <- reg(GTA.d, cont = paste0(GTA.NTM,  "+", controls.d),                  weights = "CRI_sqrt_gm"); summary(ols.d)
ols.d.fe <- reg(GTA.d, cont = paste0(GTA.NTM,  "+", controls.d.vec.fe,"+", fe),   weights = "CRI_sqrt_gm"); summary(ols.d.fe)
ols.d.TRA <-    reg(TRA.d, cont = paste0(TRA.NTM,"+", controls.d),                weights = "CRI_sqrt_gm"); summary(ols.d.TRA)
ols.d.fe.TRA <- reg(TRA.d, cont = paste0(TRA.NTM,"+", controls.d.vec.fe,"+", fe), weights = "CRI_sqrt_gm"); summary(ols.d.fe.TRA)

results.delta <- list(ols.d, ols.d.fe, ols.d.TRA, ols.d.fe.TRA)
saveRDS(results.ols.TRA, file = paste0(path.data.out, "Delta_Results.Rds"))


# 6. Chapters -------------------------------------------------------------------------


ols.chap      <- reg(GTA, cont = paste0(mast.chap,"+", controls));                                     summary(ols.chap)
ols.chap.w    <- reg(GTA, cont = paste0(mast.chap,"+", controls),            weights = "CRI_sqrt_gm"); summary(ols.chap.w)
ols.fe.chap   <- reg(GTA, cont = paste0(mast.chap,"+", controls.fe,"+", fe));                          summary(ols.fe.chap)
ols.fe.chap.w <- reg(GTA, cont = paste0(mast.chap,"+", controls.fe,"+", fe), weights = "CRI_sqrt_gm"); summary(ols.fe.chap.w)

TRA.ols.chap      <- reg(TRA, cont = paste0(mast.chap,"+", controls));                                     summary(TRA.ols.chap)
TRA.ols.chap.w    <- reg(TRA, cont = paste0(mast.chap,"+", controls),            weights = "CRI_sqrt_gm"); summary(TRA.ols.chap.w)
TRA.ols.fe.chap   <- reg(TRA, cont = paste0(mast.chap,"+", controls.fe,"+", fe));                          summary(TRA.ols.fe.chap)
TRA.ols.fe.chap.w <- reg(TRA, cont = paste0(mast.chap,"+", controls.fe,"+", fe), weights = "CRI_sqrt_gm"); summary(TRA.ols.fe.chap.w)

mast.names.adj <- paste0(mast.names[! mast.names %in% c("C_liberalising", "G_liberalising")], collapse = "+")
h.fe.chap.GTA <- reg(GTA, type = "heckman", weights = "CRI_sqrt_gm",
                     cont =           paste0(mast.names.adj,"+", fe.adjust.GTA," + log(distw_harmonic) + geometric_avg_tariff"), 
                     cont.selection = paste0(mast.names.adj,"+", fe.adjust.GTA," + log(distw_harmonic) + geometric_avg_tariff + intranat.trade")); summary(h.fe.chap.GTA)

h.fe.chap.TRA <- reg(TRA, type = "heckman", weights = "CRI_sqrt_gm", 
                     cont =           paste0(mast.names.adj,"+", fe.adjust.TRA, " + log(distw_harmonic) + geometric_avg_tariff"), 
                     cont.selection = paste0(mast.names.adj,"+", fe.adjust.TRA, " + log(distw_harmonic) + geometric_avg_tariff + intranat.trade")); summary(h.fe.chap.TRA)



# Delta
ols.d.chap      <- reg(GTA.d, cont = paste0(mast.chap,"+", controls.d));                         summary(ols.d.chap)
ols.d.chap.w    <- reg(GTA.d, cont = paste0(mast.chap,"+", controls.d), weights = "CRI_sqrt_gm");summary(ols.d.chap)
ols.d.fe.chap <- reg(GTA.d, cont = paste0(mast.chap,"+", controls.d.vec.fe,"+", fe), weights = "CRI_sqrt_gm"); summary(ols.d.fe.chap)
ols.d.chap.TRA <-    reg(TRA.d, cont = paste0(mast.chap.TRA,"+", controls.d),        weights = "CRI_sqrt_gm"); summary(ols.d.chap.TRA)
ols.d.fe.chap.TRA <- reg(TRA.d, cont = paste0(mast.chap.TRA,"+", controls.d.vec.fe,"+", fe), weights = "CRI_sqrt_gm");  summary(ols.d.fe.chap.TRA)



## 6. Make table --------------------------------------------------------------
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
