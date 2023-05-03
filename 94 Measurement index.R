# This file creates the measurement index

rm(list = ls())
source("BA_Thesis_code/00 Terms and Definitions.R")

# 0. Load data -----------------------------------------------------------------
GTA <- readRDS(file = paste0(path.data.out, "GTA_asymmetric_isic.RData"))
TRAINS <- readRDS(file = paste0(path.data.out, "TRAINS_asymmetric_isic.RData"))
controls <- readRDS(paste0(path.data.raw, "CEPII_Gravity_Variables.Rds")) 

grid.coverage <- expand.grid(selected.countries, years.observation, c("AB", "D", "GTT"), c("harmful", "liberalising"))
names(grid.coverage) <- c("country", "year", "chapter", "gta.evaluation")

# 1. data prep -----------------------------------------------------------------
## GTA -------------------------------------------------------------------------
# Aggregate intervetions
GTA <- GTA %>% 
  filter(mast.chapter %in% selected.mast)%>%
  filter(implementing.jurisdiction %in% selected.countries)%>%
  select(c(implementing.jurisdiction, years.in.force, chapter, intervention.id, gta.evaluation))%>%
  cSplit("years.in.force", direction = "long") %>%
  cSplit("chapter", direction = "long")%>%
  aggregate(intervention.id ~ years.in.force + chapter + implementing.jurisdiction + gta.evaluation, FUN = function(x) length(unique(x)))

write.csv(unique(GTA$implementing.jurisdiction), paste0(path.data.reg, "GTA_non_zero_countries.csv"),row.names = F)

# add 0s for combinations without intervention
GTA <- GTA %>%
  pivot_wider(id_cols = c("years.in.force", "implementing.jurisdiction", "gta.evaluation"), 
              names_from = "chapter", 
              values_from = "intervention.id")%>%
  mutate(AB = ifelse(is.na(AB), 0, AB))%>%
  mutate(D = ifelse(is.na(D), 0, D))%>%
  mutate(GTT = AB + D)%>%
  pivot_longer(cols = 4:ncol(.), names_to = "chapter", values_to = "intervention.id")


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
  left_join(GTA, by = c("country" ="implementing.jurisdiction" , "year" = "years.in.force", "chapter", "gta.evaluation"))%>%
  mutate(intervention.id = ifelse(is.na(intervention.id), 0, intervention.id))%>%
  left_join(controls, by = c("country" = "iso3_o", "year"))

saveRDS(GTA, file = paste0(path.data.out, "GTA_interventions.Rds"))

TRAINS <- grid.coverage %>%
  select(-gta.evaluation)%>%
  unique()%>%
  left_join(TRAINS, by = c("country" ="iso_code" , "year" = "years.in.force", "chapter"))%>%
  mutate(measure.id = ifelse(is.na(measure.id), 0, measure.id))%>%
  left_join(controls, by = c("country" = "iso3_o", "year"))


# get measures
GTA <- GTA %>%
  mutate(coverage.measure = intervention.id/gdp_o)%>%
  mutate(coverage.measure.sqrt = intervention.id/sqrt(gdp_o))
  
TRAINS <- TRAINS %>%
  mutate(coverage.measure = measure.id/gdp_o)%>%
  mutate(coverage.measure.sqrt = measure.id/sqrt(gdp_o))


writexl::write_xlsx(GTA, path = paste0(path.data.out, "Country measurement index.xlsx"))

GTA <- GTA %>% select(-c(gdp_o))
TRAINS <- TRAINS %>% select(-c(measure.id, gdp_o))


#make bilateral
help <- merge(grid, GTA, by.x = c("country.1", "year", "chapter"), 
              by.y = c("country", "year", "chapter"), all.x = T)
GTA <- merge(help, GTA, by.x = c("country.2", "year", "chapter", "gta.evaluation"), 
             by.y = c("country", "year", "chapter", "gta.evaluation"),all.x = T)
GTA <- GTA %>% 
  filter(year %in% years.observation)

GTA$coverage.geom.mean.log <- apply(GTA[, c("coverage.measure.sqrt.x", "coverage.measure.sqrt.y")], 1, FUN = function(x) exp(mean(log(x))))
GTA$coverage.mean.log <- apply(GTA[, c("coverage.measure.sqrt.x", "coverage.measure.sqrt.y")], 1, FUN = function(x) mean(x))
GTA$coverage.geom.mean <- apply(GTA[, c("coverage.measure.x", "coverage.measure.y")], 1, FUN = function(x) exp(mean(log(x))))
GTA$coverage.mean <- apply(GTA[, c("coverage.measure.x", "coverage.measure.y")], 1, FUN = function(x) mean(x))
GTA$intervention.geom.mean <- apply(GTA[, c("intervention.id.x", "intervention.id.y")], 1, FUN = function(x) exp(mean(log(x))))
GTA$intervention.mean <- apply(GTA[, c("intervention.id.x", "intervention.id.y")], 1, FUN = function(x) mean(x))



GTA <- GTA %>% select(-c(coverage.measure.x, coverage.measure.y, intervention.id.x, intervention.id.y))

help <- merge(grid, TRAINS, by.x = c("country.1", "year", "chapter"), 
              by.y = c("country", "year", "chapter"), all.x = T)
TRAINS <- merge(help, TRAINS, by.x = c("country.2", "year", "chapter"), 
                by.y = c("country", "year", "chapter"),all.x = T)
TRAINS <- TRAINS %>% 
  filter(year %in% years.observation)

TRAINS$coverage.geom.mean <- apply(TRAINS[, c("coverage.measure.x", "coverage.measure.y")], 1, FUN = function(x) exp(mean(log(x))))
TRAINS$coverage.geom.mean.sqrt <- apply(TRAINS[, c("coverage.measure.sqrt.x", "coverage.measure.sqrt.y")], 1, FUN = function(x) exp(mean(log(x))))
TRAINS$coverage.mean <- apply(TRAINS[, c("coverage.measure.x", "coverage.measure.y")], 1, FUN = function(x) mean(x))
TRAINS$coverage.mean.sqrt <- apply(TRAINS[, c("coverage.measure.sqrt.x", "coverage.measure.sqrt.y")], 1, FUN = function(x) mean(x))

TRAINS <- TRAINS %>% select(-c(coverage.measure.x, coverage.measure.y))




writexl::write_xlsx(GTA, path = paste0(path.data.out, "GTA_Measurement_index.xlsx"))
writexl::write_xlsx(TRAINS, path = paste0(path.data.out, "TRAINS_Measurement_index.xlsx"))
