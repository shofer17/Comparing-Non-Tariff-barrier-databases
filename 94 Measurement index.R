# This file creates the measurement index

rm(list = ls())

library(ggplot2)
source("BA_Thesis_code/00 Terms and Definitions.R")

GTA <- readRDS(file = paste0(path.data.out, "GTA_asymmetric_isic.RData"))
TRAINS <- readRDS(file = paste0(path.data.out, "TRAINS_asymmetric_isic.RData"))
controls <- readRDS(paste0(path.data.raw, "CEPII_Gravity_Variables.Rds")) 

grid.coverage <- expand.grid(selected.countries, years, c("AB", "D", "GTT"))
names(grid.coverage) <- c("country", "year", "chapter")

# Aggregate intervetions
GTA <- GTA %>% select(c(implementing.jurisdiction, years.in.force, chapter, intervention.id))
GTA <- cSplit(GTA, "years.in.force", direction = "long")
GTA <- cSplit(GTA, "chapter", direction = "long")
GTA <- aggregate(data = GTA, intervention.id ~ years.in.force + chapter + implementing.jurisdiction, FUN = function(x) length(unique(x)))

write.csv(unique(GTA$implementing.jurisdiction), paste0(path.data.reg, "GTA_non_zero_countries.csv"),row.names = F)

# add 0s for combinations without intervention
GTA <- pivot_wider(GTA, id_cols = c("years.in.force", "implementing.jurisdiction"), names_from = "chapter", values_from = "intervention.id")
GTA$AB <- ifelse(is.na(GTA$AB), 0, GTA$AB)
GTA$D <- ifelse(is.na(GTA$D), 0, GTA$D)
GTA$GTT <- GTA$AB + GTA$D
GTA <- pivot_longer(GTA, cols = 3:ncol(GTA), names_to = "chapter", values_to = "intervention.id")


TRAINS <- TRAINS %>% select(c(measure.id, years.in.force, chapter, implementing.jurisdiction))
TRAINS <- cSplit(TRAINS, "years.in.force", direction = "long")
TRAINS <- cSplit(TRAINS, "chapter", direction = "long")
TRAINS <- aggregate(data = TRAINS, measure.id ~ years.in.force + chapter + implementing.jurisdiction, FUN = function(x) length(unique(x)))
TRAINS <- merge(TRAINS, country.names[, c("name", "iso_code")], by.x = "implementing.jurisdiction", by.y = "name")
TRAINS$implementing.jurisdiction <- NULL

write.csv(unique(TRAINS$iso_code), paste0(path.data.reg, "TRAINS_non_zero_countries.csv"),row.names = F)

#add 0s 
names(TRAINS) <- c( "years.in.force", "chapter","measure.id","implementing.jurisdiction")
TRAINS <- pivot_wider(TRAINS, id_cols = c("years.in.force", "implementing.jurisdiction"), names_from = "chapter", values_from = "measure.id")
TRAINS$AB <- ifelse(is.na(TRAINS$AB), 0, TRAINS$AB)
TRAINS$D <- ifelse(is.na(TRAINS$D), 0, TRAINS$D)
TRAINS$GTT <- TRAINS$AB + TRAINS$D
TRAINS <- pivot_longer(TRAINS, cols = 3:ncol(TRAINS), names_to = "chapter", values_to = "measure.id")




# get GDP
controls <- controls %>% 
  select(iso3_o,year, gdp_o) %>% 
  filter(year %in% years) %>%
  unique()%>%
  filter(iso3_o %in% selected.countries)


# combine
GTA <- merge(GTA, grid.coverage, by.x = c("implementing.jurisdiction", "years.in.force", "chapter"), by.y = c("country", "year", "chapter"), all.y = T)
GTA <- merge(GTA, controls, by.x = c("implementing.jurisdiction", "years.in.force"), by.y = c("iso3_o", "year"))
GTA$intervention.id <- ifelse(is.na(GTA$intervention.id), 0, GTA$intervention.id)

TRAINS <- merge(TRAINS, grid.coverage, by.x = c("implementing.jurisdiction", "years.in.force", "chapter"), by.y = c("country", "year", "chapter"), all.y = T)
TRAINS <- merge(TRAINS, controls, by.x = c("implementing.jurisdiction", "years.in.force"), by.y = c("iso3_o", "year"))
TRAINS$measure.id <- ifelse(is.na(TRAINS$measure.id), 0, TRAINS$measure.id)

# get measures
GTA$coverage.measure <- GTA$intervention.id / log(GTA$gdp_o )
TRAINS$coverage.measure <- TRAINS$measure.id / log(TRAINS$gdp_o)

GTA <- GTA %>% select(-c(intervention.id, gdp_o))
TRAINS <- TRAINS %>% select(-c(measure.id, gdp_o))


#make bilateral
help <- merge(grid, GTA, by.x = c("country.1", "year", "chapter"), 
             by.y = c("implementing.jurisdiction", "years.in.force", "chapter"), all.x = T)
GTA <- merge(help, GTA, by.x = c("country.2", "year", "chapter"), 
             by.y = c("implementing.jurisdiction", "years.in.force", "chapter"),all.x = T)

GTA$coverage.geom.mean <- apply(GTA[, c("coverage.measure.x", "coverage.measure.y")], 1, FUN = function(x) exp(mean(log(x))))
GTA$coverage.mean <- apply(GTA[, c("coverage.measure.x", "coverage.measure.y")], 1, FUN = function(x) mean(x))
GTA <- GTA %>% select(-c(coverage.measure.x, coverage.measure.y))

help <- merge(grid, TRAINS, by.x = c("country.1", "year", "chapter"), 
              by.y = c("implementing.jurisdiction", "years.in.force", "chapter"), all.x = T)
TRAINS <- merge(help, TRAINS, by.x = c("country.2", "year", "chapter"), 
             by.y = c("implementing.jurisdiction", "years.in.force", "chapter"),all.x = T)

TRAINS$coverage.geom.mean <- apply(TRAINS[, c("coverage.measure.x", "coverage.measure.y")], 1, FUN = function(x) exp(mean(log(x))))
TRAINS$coverage.mean <- apply(TRAINS[, c("coverage.measure.x", "coverage.measure.y")], 1, FUN = function(x) mean(x))
TRAINS <- TRAINS %>% select(-c(coverage.measure.x, coverage.measure.y))




writexl::write_xlsx(GTA, path = paste0(path.data.out, "GTA_Measurement_index.xlsx"))
writexl::write_xlsx(TRAINS, path = paste0(path.data.out, "TRAINS_Measurement_index.xlsx"))








# 
# gta_data_slicer(data.path = paste0(path.data.raw, "master_plus.Rdata"))
# 
# GTA <- master.sliced %>% 
#   filter(gta.evaluation != "Green" & !is.na(a.un))%>%
#   select(-c(a.un, i.un, title, date.announced, affected.sector, i.atleastone.G20, a.atleastone.G20))
# 
# 
# 
# 
# GTA <- GTA %>% select(implementing.jurisdiction, intervention.id)
# GTA <- merge(GTA, country.names[, c("name", "iso_code")], by.x = "implementing.jurisdiction", by.y = "name")
# 
# GTA <- aggregate(data = GTA, intervention.id ~ iso_code, FUN = function(x) length(unique(x)))
# 
# cont <- unique(controls[controls$year == 2019, c("iso3_d",  "gdp_d")])
# test <- merge(GTA,cont , 
#               by.x = c("iso_code"), 
#               by.y = "iso3_d")
# 
# test$coverage.measure.log <- test$intervention.id / log(test$gdp_d)
# test$coverage.measure <- test$intervention.id / test$gdp_d * 1000000
# 
# ggplot(data = test, aes(x = log(gdp_d), y = log(intervention.id)))+
#   geom_point()+
#   geom_smooth()
# 
# writexl::write_xlsx(test, paste0(path.data.out, "GTA NTM coverage.xlsx"))
