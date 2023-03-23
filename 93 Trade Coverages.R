rm(list = ls())

#detach("package:gtalibrary", unload=TRUE)
library(gtalibrary)
library(tidyverse)
detach("package:goft", unload=TRUE)
detach("package:fitdistrplus", unload=TRUE)
detach("package:systemfit", unload=TRUE)
detach("package:MASS", unload=TRUE)

setwd("..") 
years <- 2009:2019

# Paths

path.data.raw <- "0 data raw/"
path.data.out <- "1 data processed/"

GTA.sym <- readRDS(file = paste0(path.data.out, "GTA_symmetric_isic.RData"))
hs.to.isic <- readxl::read_xlsx(paste0(path.data.out, "ISIC to HS 2 digits conversion.xlsx"))
available.countries <- read.csv(file = paste0(path.data.out, "selected countries.xlsx"))
available.countries <- merge(available.countries, country.names[, c("name", "iso_code")], by.x = "x", by.y = "iso_code")



hs <- readxl::read_xlsx(paste0(path.data.raw, "GTA - Sectors and Products.xlsx"))
hs.to.isic <- hs.to.isic %>% filter(chapter == "D")
hs <- hs %>% filter(`Sector level 2 ID` %in% hs.to.isic$hs.code) %>%
  select(`Product level 6 ID`)
hs <- hs$`Product level 6 ID`

GTA.sym <- GTA.sym %>% filter(country.1 %in% available.countries$name & country.2 %in% available.countries$name)



GTA.sym <- unique(GTA.sym[, 1:2])
saveRDS(GTA.sym, file = paste0(path.data.out, "GTA symmetrical country pairs.Rds"))

#gta_setwd("G")
load(file =  paste0(path.data.raw,"gta_tuple (1).Rdata"))
gta_tuple$affected_products <- as.numeric(gta_tuple$affected_products)
gta_tuple$sector_code <- NULL

save(gta_tuple, file = paste0(path.data.raw,"gta_tuple.Rdata"))
library(gtalibrary)



data.out <- data.frame(matrix(nrow = nrow(GTA.sym), ncol = 3 + length(years)))
data.out[,1:2] <- GTA.sym[,3:4]
names(data.out) <- c("country.1", "country.2","Number of interventions affecting exported product" , paste0("Trade coverage estimate for ", years))

t_0 <- Sys.time()
for (i in 1: nrow(GTA.sym)){
  
  gta_trade_coverage(data.path = paste0(path.data.raw, "master_plus.Rdata"),
                     replica.path = paste0(path.data.raw, "database replica - parts - base.Rdata"),
                     replica.path.atl = paste0(path.data.raw, "gta_affected_tariff_line.Rdata"),
                     replica.path.tuple = paste0(path.data.raw,"gta_tuple.Rdata"),
                     coverage.period = c(min(years), max(years)),
                     hs.codes = as.integer(hs),
                     keep.hs = T,
                     affected.flows = c("inward", "outward"),
                     gta.evaluation = "red",
                     implementers = c(data.out$country.1[i], data.out$country.2[i]),
                     keep.implementer = T,
                     exporters = c(data.out$country.1[i], data.out$country.2[i]),
                     keep.exporters = T,
                     importers = c(data.out$country.1[i], data.out$country.2[i]),
                     keep.importers = T,
                     trade.data = "base"
                   )

  if(trade.coverage.estimates[1] != 0){
  data.out[i, 4:ncol(data.out)] <- trade.coverage.estimates[1,4:ncol(data.out)]
  trade.coverage.estimates <- c(0)
  }
    print(i)
    
  if(i %% 10 == 0){
    saveRDS(data.out, file = paste0(path.data.out, "GTA_symmetric_isic_coverage_d.Rds"))
  }
}
t_1 <- Sys.time()
t_1-t_0

