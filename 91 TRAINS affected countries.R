library(sqldf)

rm(list = ls())
source("BA_Thesis_code/00 Terms and Definitions.R")
load(paste0(path.data.raw,"database replica/database replica - parts - base.Rdata"))
TRAINS <- readRDS(file = paste0(path.data.out, "TRAINS_asymmetric_6dig.hs12.RData"))

rm(ricardodev, ricardodev.readonly, ricardomain, ricardomain.readonly, mail, ft.api, devtwitter, bastiat, nyt.api, grid, gtaapi, gtadatagmail, gtadev, gtadev.readonly, gtamain, gtamain.readonly, pool, credentials, session.prefix, t, database, gta_jurisdiction, gta_jurisdiction_group, gta_jurisdiction_group_member, gta_state_act, gta_state_act_description, gta_tuple, gta_intervention_description)


#gta_it_revised <- read.csv(file = paste0(path.data.raw, "database replica/gta_it_revised.csv"))
#gta_tariff_line <- read.csv(file = paste0(path.data.raw, "database replica/gta_tariff_line.csv"))
#gta_support_fta <- read.csv(file = paste0(path.data.raw, "database replica/gta_support_fta.csv"))
load(paste0(path.data.raw, "database replica/Final goods support table.Rdata"))
gta_support_goods_trade <- final
rm(final)


# Prepare dataframe ------------------------------------------------------------
# Easy version --------------
# To standardise the frames
TRAINS.rest <- TRAINS %>% 
  filter(is.world != 1)%>%
  select(measure.id, affected.jurisdiction)
TRAINS.reduced <- TRAINS %>% 
  filter(is.world == 1)
  #if world and other countries are added as affected, only use specific countries
nrow(TRAINS.rest) + nrow(TRAINS.reduced) == nrow(TRAINS)

mast.flow.conversion <- data.frame("mast.chapter" = c("B", "C", "D", "E", "F", "G", "I", "L", "M","N", "P"
                                                      #,"P3", "P6","P9"
                                                      ),
                                   "affected.flow" = c("inward", "inward", "inward", "inward", "inward", "inward", "inward", "inward", "inward", "inward", "outward" #,"outward", "outward subsidy","outward subsidy
                                                       )
                                   )

names(TRAINS.reduced)[names(TRAINS.reduced) == "measure.id"] = "intervention_id"
names(TRAINS.reduced)[names(TRAINS.reduced) == "implementing.jurisdiction"] = "implementing_jurisdiction"

TRAINS.reduced <- TRAINS.reduced %>% left_join(mast.flow.conversion, by = "mast.chapter")
TRAINS.reduced <- TRAINS.reduced %>% select(intervention_id, implementing_jurisdiction, affected.flow, hs12.dig.6, years.in.force, iso_code)
TRAINS.reduced$inception.date <- sapply(strsplit(TRAINS.reduced$years.in.force, split = ","), FUN = min)

# INWARD ---------------------------------
rm(gta_affected_jurisdiction, gta_affected_sector, gta_affected_tariff_line, gta_distorted_market, gta_it_revised)

TRAINS.inward <- TRAINS.reduced %>% 
  filter(affected.flow == "inward") %>%
  select(-c(years.in.force, iso_code)) %>%
  mutate(inception.date = as.numeric(inception.date)-1) %>% 
  cSplit("hs12.dig.6", sep = ",", direction = "long") %>%
  left_join(gta_support_goods_trade[, c("Year", "Tariff.line", "Partner.jurisdiction", "Reporter.jurisdiction")], 
            by = c("inception.date" = "Year", "hs12.dig.6" = "Tariff.line", "implementing_jurisdiction" = "Reporter.jurisdiction"), 
            multiple = "all"
            )

TRAINS.inward <- TRAINS.inward %>% 
  filter(!is.na(Partner.jurisdiction)) %>%
  select(-c(hs12.dig.6, inception.date, affected.flow, implementing_jurisdiction)) %>% 
  unique()

TRAINS.inward <- aggregate(data = TRAINS.inward,`Partner.jurisdiction`  ~ intervention_id , FUN = function(x) paste0(x, collapse = ","))
names(TRAINS.inward) <- c("measure.id", "affected.jurisdiction")


# OUTWARD ---------------------------------

TRAINS.outward <- TRAINS.reduced %>% 
  filter(affected.flow == "outward") %>%
  select(-c(years.in.force, iso_code)) %>%
  mutate(inception.date = as.numeric(inception.date)-1) %>% 
  cSplit("hs12.dig.6", sep = ",", direction = "long") %>%
  left_join(gta_support_goods_trade[, c("Year", "Tariff.line", "Partner.jurisdiction", "Reporter.jurisdiction")], 
            by = c("inception.date" = "Year", "hs12.dig.6" = "Tariff.line", "implementing_jurisdiction" = "Partner.jurisdiction"), 
            multiple = "all")

TRAINS.outward <- TRAINS.outward %>% 
  filter(!is.na(Reporter.jurisdiction)) %>%
  select(-c(hs12.dig.6, inception.date, affected.flow, implementing_jurisdiction)) %>% 
  unique()

TRAINS.outward <- aggregate(data = TRAINS.outward,`Reporter.jurisdiction`  ~ intervention_id , FUN = function(x) paste0(x, collapse = ","))
names(TRAINS.outward) <- c("measure.id", "affected.jurisdiction")

# OUTWARD Subsidy ---------------------------------


TRAINS.outward.subsidy <- TRAINS %>% filter(mast.subchapter == "P6")


TRAINS.outward <- TRAINS.reduced %>% 
  filter(affected.flow == "outward") %>%
  select(-c(years.in.force, iso_code)) %>%
  mutate(inception.date = as.numeric(inception.date)-1) %>% 
  cSplit("hs12.dig.6", sep = ",", direction = "long") %>%
  left_join(gta_support_goods_trade[, c("Year", "Tariff.line", "Partner.jurisdiction", "Reporter.jurisdiction")], 
            by = c("inception.date" = "Year", "hs12.dig.6" = "Tariff.line", "implementing_jurisdiction" = "Partner.jurisdiction"), 
            multiple = "all")

TRAINS.outward <- TRAINS.outward %>% 
  filter(!is.na(Reporter.jurisdiction)) %>%
  select(-c(hs12.dig.6, inception.date, affected.flow, implementing_jurisdiction)) %>% 
  unique()

TRAINS.outward <- aggregate(data = TRAINS.outward,`Reporter.jurisdiction`  ~ intervention_id , FUN = function(x) paste0(x, collapse = ","))


# Combine -------------------------------------------------

TRAINS.out <- rbind(TRAINS.inward, TRAINS.outward, TRAINS.rest)

nrow(TRAINS.out) == nrow(TRAINS)

saveRDS(TRAINS.out, file = paste0(path.data.out, "TRAINS.affected.jurisdictions.Rds"))




# OLD -----------------------
# gta_data_slicer(data.path = paste0(path.data.raw, "master_plus.Rdata"))
# 
# 
# t <- master.sliced %>% select(mast.chapter, mast.id, affected.flow) %>% unique() %>% filter(mast.chapter %in% selected.mast)
# t_2 <- master.sliced %>% 
#   filter(mast.id == "P9") %>%
#   select(intervention.id, title, affected.flow) %>%
#   unique()
# 
# t_3 <- t_2 %>% filter(affected.flow == "outward")
# writexl::write_xlsx(t_3, path = paste0(path.data.out, "P9 cases.xlsx"))
# 
# t_Trains <- TRAINS %>% 
#   select(NTM.code) %>% 
#   mutate(NTM.1.dig = substr(NTM.code,1,1)) %>%
#   filter(NTM.1.dig %in% selected.mast) %>%
#   unique()
# 
# 
# 
# necessary.dataframes <- list(gta_it_revised, #j
#                              gta_affected_tariff_line, #j
#                              gta_tariff_line, #j
#                              gta_affected_jurisdiction, # to find
#                              gta_implementing_jurisdiction, #j
#                              gta_distorted_market,# to find
#                              gta_support_goods_trade, #j
#                              gta_support_fta) #j
# # To standardise the frames
# #TRAINS <- TRAINS %>% filter(is.world == 1)
# 
# mast.flow.conversion <- data.frame("mast.chapter" = c("B", "C", "D", "E", "F", "G", "I", "L", "M","N", "P"
#                                                       #,"P3", "P6","P9"
# ),
# "affected.flow" = c("inward", "inward", "inward", "inward", "inward", "inward", "inward", "inward", "inward", "inward", "outward" 
#                     #,"outward", "outward subsidy","outward subsidy"
# )
# )
# 
# names(TRAINS)[names(TRAINS) == "measure.id"] = "intervention_id"
# names(TRAINS)[names(TRAINS) == "implementing.jurisdiction"] = "implementing_jurisdiction"
# 
# test <- TRAINS %>% left_join(mast.flow.conversion, by = "mast.chapter")
# test <- test %>% select(intervention_id, implementing.jurisdiction, affected.flow, hs12.dig.6, years.in.force, iso_code)
# test$inception.date <- sapply(strsplit(test$years.in.force, split = ","), FUN = min)
# test$removel.date <- sapply(strsplit(test$years.in.force, split = ","), FUN = max)
# test$years.in.force <- NULL
# names(test) <- c("intervention_id","implementing_jurisdiction", "affected_flow", "affected_product","iso_code","inception_date","removel_date" )
# test$is.fta.included <- 0
# test$intervention_area <- 1
# test$dm_freeze <- 0
# test$aj_freeze <- 0
# 
# 
# gta_implementing.jurisdiction <- TRAINS %>% 
#   select(intervention_id, implementing_jurisdiction) %>% 
#   unique() %>%
#   left_join(country.names[, c("name", "un_code")], by = c("implementing_jurisdiction" = "name")) %>%
#   select(c(intervention_id, un_code)) %>%
#   rename("jurisdiction_id" = "un_code")
# 
# 
# gta_affected_tariff_line <- TRAINS %>% 
#   select(intervention_id, hs12.dig.6) %>%
#   cSplit("hs12.dig.6", sep = ",", direction = "long")%>%
#   rename("affected_products" = "hs12.dig.6")
# 
# gta_intervention <- TRAINS %>% select(intervention_id, affected.jurisdiction, years.in.force)
# gta_intervention$inception_date <- sapply(strsplit(gta_intervention$years.in.force, split = ","), FUN = min)
# gta_intervention$removel_date <- sapply(strsplit(gta_intervention$years.in.force, split = ","), FUN = max)
# gta_intervention$years.in.force <- NULL
# 
# gta_it_revised <- gta_it_revised[0,]
# gta_affected_jurisdiction <- gta_affected_jurisdiction[0,]

