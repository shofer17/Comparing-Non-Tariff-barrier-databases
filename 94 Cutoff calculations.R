library(ggplot2)

gta_data_slicer(data.path = paste0(path.data.raw, "master_plus.Rdata"))
GTA <- master.sliced %>% 
  filter(gta.evaluation != "Green" & !is.na(a.un))%>%
  select(-c(a.un, i.un, title, date.announced, affected.sector, i.atleastone.G20, a.atleastone.G20))

GTA <- GTA %>% select(implementing.jurisdiction, intervention.id)
GTA <- merge(GTA, country.names[, c("name", "iso_code")], by.x = "implementing.jurisdiction", by.y = "name")

GTA <- aggregate(data = GTA, intervention.id ~ iso_code, FUN = function(x) length(unique(x)))

cont <- unique(controls[controls$year == 2019, c("iso3_d",  "gdp_d")])
test <- merge(GTA,cont , 
              by.x = c("iso_code"), 
              by.y = "iso3_d")

test$coverage.measure.log <- test$intervention.id / log(test$gdp_d)
test$coverage.measure <- test$intervention.id / test$gdp_d * 1000000

ggplot(data = test, aes(x = log(gdp_d), y = log(intervention.id)))+
  geom_point()+
  geom_smooth()

writexl::write_xlsx(test, paste0(path.data.out, "GTA NTM coverage.xlsx"))
