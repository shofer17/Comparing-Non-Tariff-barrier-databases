# This file creates the measurement index

rm(list = ls())
source("BA_Thesis_code/00 Terms and Definitions.R")

# 0. Load data -----------------------------------------------------------------
GTA <- readRDS(file = paste0(path.data.out, "GTA_asymmetric_isic.RData"))
TRAINS <- readRDS(file = paste0(path.data.out, "TRAINS_asymmetric_isic.RData"))
controls <- readRDS(paste0(path.data.raw, "CEPII_Gravity_Variables.Rds")) 

grid.coverage <- expand.grid(selected.countries, years.observation, c("AB", "D", "GTT"))
names(grid.coverage) <- c("country", "year", "chapter")

# 1. data prep -----------------------------------------------------------------
## GTA -------------------------------------------------------------------------
# Aggregate intervetions
# 
GTA.chapter <- GTA %>%
  filter(mast.chapter %in% selected.mast)%>%
  filter(implementing.jurisdiction %in% selected.countries)%>%
  select(c(implementing.jurisdiction, years.in.force, chapter, intervention.id))%>%
  cSplit("years.in.force", direction = "long") %>%
  cSplit("chapter", direction = "long")%>%
  aggregate(intervention.id ~ years.in.force + chapter + implementing.jurisdiction, FUN = function(x) length(unique(x)))

GTA <- GTA %>%
  filter(mast.chapter %in% selected.mast)%>%
  filter(implementing.jurisdiction %in% selected.countries)%>%
  select(c(implementing.jurisdiction, years.in.force, chapter, intervention.id))%>%
  cSplit("years.in.force", direction = "long") %>%
  aggregate(intervention.id ~ years.in.force + implementing.jurisdiction, FUN = function(x) length(unique(x)))%>%
  mutate(chapter = "GTT")
GTA <- rbind(GTA, GTA.chapter)


#OLD Split to harmful and Lib
# GTA <- GTA %>% 
#   filter(mast.chapter %in% selected.mast)%>%
#   filter(implementing.jurisdiction %in% selected.countries)%>%
#   select(c(implementing.jurisdiction, years.in.force, chapter, intervention.id, gta.evaluation))%>%
#   cSplit("years.in.force", direction = "long") %>%
#   cSplit("chapter", direction = "long")%>%
#   aggregate(intervention.id ~ years.in.force + chapter + implementing.jurisdiction, FUN = function(x) length(unique(x)))
# 
# write.csv(unique(GTA$implementing.jurisdiction), paste0(path.data.reg, "GTA_non_zero_countries.csv"),row.names = F)
# 
# # add 0s for combinations without intervention
# GTA <- GTA %>%
#   pivot_wider(id_cols = c("years.in.force", "implementing.jurisdiction"), 
#               names_from = "chapter", 
#               values_from = "intervention.id")%>%
#   mutate(AB = ifelse(is.na(AB), 0, AB))%>%
#   mutate(D = ifelse(is.na(D), 0, D))%>%
#   mutate(GTT = AB + D)%>%
#   pivot_longer(cols = 3:ncol(.), names_to = "chapter", values_to = "intervention.id")


## TRAINS -------------------------------------------------------------------------
#aggregate interventions
TRAINS <- TRAINS %>% select(c(implementing.jurisdiction, years.in.force, chapter, measure.id))%>%
  cSplit(splitCols = "years.in.force", direction = "long", sep = ",") %>%
  cSplit(splitCols = "chapter", direction = "long")%>%
  aggregate(measure.id ~ years.in.force + chapter + implementing.jurisdiction, FUN = function(x) length(unique(x)))

TRAINS <- TRAINS %>% 
  pivot_wider(id_cols = c("years.in.force", "implementing.jurisdiction"), 
              names_from = "chapter", 
              values_from = "measure.id")%>%
  mutate(AB = ifelse(is.na(AB), 0, AB))%>%
  mutate(D = ifelse(is.na(D), 0, D))%>%
  mutate(GTT = AB + D) %>%
  pivot_longer(cols = 3:ncol(.), names_to = "chapter", values_to = "measure.id")

TRAINS <- TRAINS %>%
  left_join(country.names[, c("name", "iso_code")], 
            by = c("implementing.jurisdiction" = "name")) %>%
  select(-implementing.jurisdiction)
write.csv(unique(TRAINS$iso_code), paste0(path.data.reg, "TRAINS_non_zero_countries.csv"),row.names = F)

## Controls ---------------------------------------------------------------------
# get GDP
controls <- controls %>% 
  select(iso3_o,year, gdp_o) %>% 
  filter(year %in% years) %>%
  unique()%>%
  filter(iso3_o %in% selected.countries)


# 2. Combine data --------------------------------------------------------------
# combine
GTA <- grid.coverage %>%
  left_join(GTA, by = c("country" ="implementing.jurisdiction" , "year" = "years.in.force", "chapter"))%>%
  mutate(intervention.id = ifelse(is.na(intervention.id), 0, intervention.id))%>%
  left_join(controls, by = c("country" = "iso3_o", "year"))

saveRDS(GTA, file = paste0(path.data.out, "GTA_interventions_total.Rds"))
GTA <- readRDS(file = paste0(path.data.out, "GTA_interventions_total.Rds"))

TRAINS <- grid.coverage %>%
  unique()%>%
  left_join(TRAINS, by = c("country" ="iso_code" , "year" = "years.in.force", "chapter"))%>%
  mutate(measure.id = ifelse(is.na(measure.id), 0, measure.id))%>%
  left_join(controls, by = c("country" = "iso3_o", "year"))


# get measures
GTA <- GTA %>%
  mutate(CRI = intervention.id/gdp_o)%>%
  mutate(CRI_sqrt = intervention.id/sqrt(gdp_o))%>% 
  mutate(CRI_log = intervention.id/log(gdp_o))%>% 
  select(-c(gdp_o))
  
TRAINS <- TRAINS %>%
  mutate(CRI = measure.id/gdp_o)%>%
  mutate(CRI_sqrt = measure.id/sqrt(gdp_o))%>%
  mutate(CRI_log = measure.id/log(gdp_o))%>% 
  select(-c(measure.id, gdp_o))


writexl::write_xlsx(GTA, path = paste0(path.data.out, "Country measurement index total.xlsx")) #For MC Sim
GTA <- readxl::read_xlsx(path = paste0(path.data.out, "Country measurement index total.xlsx"))


#make bilateral
help <- merge(grid, GTA, by.x = c("country.1", "year", "chapter"), 
              by.y = c("country", "year", "chapter"), all.x = T)
GTA <- merge(help, GTA, by.x = c("country.2", "year", "chapter"), 
             by.y = c("country", "year", "chapter"),all.x = T)
GTA <- GTA %>% 
  filter(year %in% years.observation)

GTA$CRI_sqrt_gm <- apply(GTA[, c("CRI_sqrt.x", "CRI_sqrt.y")], 1, FUN = function(x) exp(mean(log(x))))
GTA$CRI_sqrt <- apply(GTA[, c("CRI_sqrt.x", "CRI_sqrt.y")], 1, FUN = function(x) mean(x))
GTA$CRI_log_gm <- apply(GTA[, c("CRI_log.x", "CRI_log.y")], 1, FUN = function(x) exp(mean(log(x))))
GTA$CRI_log <- apply(GTA[, c("CRI_log.x", "CRI_log.y")], 1, FUN = function(x) mean(x))
GTA$CRI_gm <- apply(GTA[, c("CRI.x", "CRI.y")], 1, FUN = function(x) exp(mean(log(x))))
GTA$CRI <- apply(GTA[, c("CRI.x", "CRI.y")], 1, FUN = function(x) mean(x))
#GTA$intervention.geom.mean <- apply(GTA[, c("intervention.id.x", "intervention.id.y")], 1, FUN = function(x) exp(mean(log(x))))
#GTA$intervention.mean <- apply(GTA[, c("intervention.id.x", "intervention.id.y")], 1, FUN = function(x) mean(x))



GTA <- GTA %>% select(-c(CRI.x, CRI.y,CRI_sqrt.x, CRI_sqrt.y, CRI_log.x, CRI_log.y, intervention.id.x, intervention.id.y))


help <- merge(grid, TRAINS, by.x = c("country.1", "year", "chapter"), 
              by.y = c("country", "year", "chapter"), all.x = T)
TRAINS <- merge(help, TRAINS, by.x = c("country.2", "year", "chapter"), 
                by.y = c("country", "year", "chapter"),all.x = T)
TRAINS <- TRAINS %>% 
  filter(year %in% years.observation)


TRAINS$CRI_sqrt_gm <- apply(TRAINS[, c("CRI_sqrt.x", "CRI_sqrt.y")], 1, FUN = function(x) exp(mean(log(x))))
TRAINS$CRI_sqrt <- apply(TRAINS[, c("CRI_sqrt.x", "CRI_sqrt.y")], 1, FUN = function(x) mean(x))
TRAINS$CRI_log_gm <- apply(TRAINS[, c("CRI_log.x", "CRI_log.y")], 1, FUN = function(x) exp(mean(log(x))))
TRAINS$CRI_log <- apply(TRAINS[, c("CRI_log.x", "CRI_log.y")], 1, FUN = function(x) mean(x))
TRAINS$CRI_gm <- apply(TRAINS[, c("CRI.x", "CRI.y")], 1, FUN = function(x) exp(mean(log(x))))
TRAINS$CRI <- apply(TRAINS[, c("CRI.x", "CRI.y")], 1, FUN = function(x) mean(x))
TRAINS <- TRAINS %>% select(-c(CRI.x, CRI.y,CRI_sqrt.x, CRI_sqrt.y, CRI_log.x, CRI_log.y))




writexl::write_xlsx(GTA, path = paste0(path.data.out, "GTA_Measurement_index.xlsx"))
writexl::write_xlsx(TRAINS, path = paste0(path.data.out, "TRAINS_Measurement_index.xlsx"))
