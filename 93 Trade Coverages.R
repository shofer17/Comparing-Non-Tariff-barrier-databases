rm(list = ls())

detach("package:gtalibrary", unload=TRUE)
library(gtalibrary)
library(tidyverse)

setwd("..") 
years <- 2009:2019

# Paths

path.data.raw <- "0 data raw/"
path.data.out <- "1 data processed/"

GTA.sym <- readRDS(file = paste0(path.data.out, "GTA_symmetric_w_controls_reg.RData"))
hs.to.isic <- readxl::read_xlsx(paste0(path.data.out, "ISIC to HS 2 digits conversion.xlsx"))
hs <- readxl::read_xlsx(paste0(path.data.raw, "GTA - Sectors and Products.xlsx"))
hs.to.isic <- hs.to.isic %>% filter(chapter == "D")
hs <- hs %>% filter(`Sector level 2 ID` %in% hs.to.isic$hs.code) %>%
  select(`Product level 6 ID`)
hs <- hs$`Product level 6 ID`


GTA.sym <- GTA.sym %>% select(ISO_country.1, ISO_country.2)%>%
  left_join(country.names[, c("name", "iso_code")], by = c("ISO_country.1" = "iso_code")) %>%
  left_join(country.names[, c("name", "iso_code")], by = c("ISO_country.2" = "iso_code"))

GTA.sym <- unique(GTA.sym)

gta_data_slicer(data.path = paste0(path.data.raw, "master_plus.Rdata"))


gta_setwd("G")

data.out <- data.frame()
for (i in 1: nrow(GTA.sym)){
  
  t_0 <- Sys.time()
  gta_trade_coverage(  #data.path = paste0(path.data.raw, "master_plus.Rdata"),
  #                    replica.path = paste0(path.data.raw, "database replica - parts - base.Rdata"),
  #                    replica.path.atl = paste0(path.data.raw, "gta_affected_tariff_line.Rdata"),
                    replica.path.tuple = "data/database replica/gta_tuple (1).Rdata",
                  coverage.period = c(min(years), max(years)),
                   hs.codes = as.integer(hs),
                   keep.hs = T,
                   affected.flows = c("inward", "outward", "outward subsidy"),
                   gta.evaluation = "red",
                   implementers = c(GTA.sym$name.x[i], GTA.sym$name.y[i]),
                   keep.implementer = T,
                   exporters = GTA.sym$name.x[i],
                   keep.exporters = T,
                   importers = GTA.sym$name.y[i],
                   keep.importers = T,
                  trade.data = "base"
                   )
  
  t_1 <- Sys.time()
  t_1-t_0
}

gta_trade_coverage(replica.path.tuple = "data/database replica/gta_tuple (1).Rdata",
                   importers = "China", 
                   keep.importers = T,
                   implementers = "United States of America", 
                   keep.implementer = T)
                   
                   