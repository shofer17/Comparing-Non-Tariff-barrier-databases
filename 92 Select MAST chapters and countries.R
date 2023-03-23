rm(list = ls())

path.data.raw <- "0 data raw/"
path.data.out <- "1 data processed/"


TRAINS <- read.csv(paste0(path.data.raw, "UNCTAD_TRAINS_database.csv"))
gta_data_slicer(data.path = paste0(path.data.raw, "master_plus.Rdata"))
trade.costs <- readRDS(file = paste0(path.data.out, "Trade Costs Processed.RData"))
TRAINS_ISO <- readRDS(file = paste0(path.data.out, "TRAINS_symmetric_w_controls.RData"))
WTO_ISO <- readRDS(file = paste0(path.data.out, "WTO_symmetric_w_controls.RData"))



# 1. MAST ------------------------------
mast.trains <- sort(unique(substr(TRAINS$NTM.code,1,1)))
mast.gta <- sort(as.character(unique(master.sliced$mast.chapter)))

selected.mast <- intersect(mast.gta, mast.trains)

write.csv(selected.mast, file = paste0(path.data.out, "selected MAST chapters.xlsx"))

# 2. Countries ---------------------------------
#define what countries to use: 
#countries that are in all three of the GTA, the TRAINS and the trade costs datasets are used. 

GTA.countries <- country.names$iso_code
trade.costs <- unique(c(trade.costs$reporter, trade.costs$partner))
TRAINS.countries <- unique(c(TRAINS_ISO$country.1, TRAINS_ISO$country.2))
WTO.countries <- unique(c(WTO_ISO$ISO_country.1, WTO_ISO$ISO_country.2))


available.countries <- intersect(GTA.countries, trade.costs)
available.countries <- intersect(available.countries, TRAINS.countries)
available.countries <- intersect(available.countries, WTO.countries)

write.csv(available.countries, file = paste0(path.data.out, "selected countries.xlsx"))
