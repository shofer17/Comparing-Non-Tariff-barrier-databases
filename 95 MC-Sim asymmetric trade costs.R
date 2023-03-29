# The simulations tries to understand the influence of modeling asymetric trade 
# costs with symmetrically. To do this, a linear model is assumed and generated.
# To keep the simulation to as close to reality as possible, the 

# install.packages("mixtools")
library(mixtools)
library(censReg)
library(plm)
library(ggplot2)
#detach("package:haven")
#install.packages("haven", version = "1.1.2")
library(ggpubr)
library(gtalibrary)
library(tidyverse)

options(scipen = 999)
rm(list = ls())

# Paths
source("BA_Thesis_code/00 Terms and Definitions.R")

# 1. get data --------------------------------------------------------------
trade.costs <- readRDS(file = paste0(path.data.out, "Trade Costs Processed.RData"))
#data <- readRDS(file = paste0(path.data.out, "TRAINS_symmetric_w_controls.RData"))
data <- readRDS(file = paste0(path.data.out, "GTA_symmetric_w_controls.RData"))
controls <- readRDS(paste0(path.data.raw, "CEPII_Gravity_Variables.Rds")) 
controls <- controls %>% select(iso3_o, iso3_d, year, gdp_o, gdp_d)
 
# 2. preliminary checks --------------------------------------------------------
# check if assumed relationship (higher share of NAs in trade costs --> higher trade costs, lower GDP)
trade.costs <- merge(trade.costs, controls[, c("iso3_o","iso3_d","gdp_o","gdp_d", "year")], by.x = c("country.1", "country.2", "year"), by.y = c("iso3_o", "iso3_d", "year"))
trade.costs$loggdp <- log(trade.costs$gdp_o) + log(trade.costs$gdp_d)

# check average trade costs of available data
check.perc.na <- trade.costs
check.perc.na$tij <- ifelse(is.na(check.perc.na$tij), 0, check.perc.na$tij)
avg.trade.costs <- check.perc.na %>% filter(tij > 0)
avg.trade.costs <- aggregate(data = avg.trade.costs, tij ~ country.1 , FUN = mean)

# check percentage of NA per country
check.perc.na$tij <- ifelse(check.perc.na$tij ==  0, 1 ,0)
check.perc.na <- aggregate(data = check.perc.na, tij ~ country.1, FUN = function(x) sum(x)/length(x))
names(check.perc.na) <- c("country.1", "perc.na")
check.perc.na <- merge(avg.trade.costs, check.perc.na, by = c("country.1"), all.y = T)

# avg GDP per country
avg.gdp <- aggregate(data = trade.costs, gdp_o ~ country.1 , FUN = mean)
check.perc.na <- merge(check.perc.na, avg.gdp, by = "country.1", all.x = T)
check.perc.na <- check.perc.na %>% filter(!is.na(tij))
# plot the results
p1 <- ggplot(data = check.perc.na, mapping = aes(x = tij, y = perc.na))+
  geom_point(color = gta_colour$qualitative[7])+
  geom_smooth(method=lm, color = gta_colour$blue[2])+
  xlim(c(0,500))+
  ylim(c(0,1))+
  xlab("Trade costs")+
  ylab("Share of trade costs are not available")+
  labs(caption = "Source: Author's calculation based on Novy (2012)")+
  theme_minimal()
show(p1)

p2 <- ggplot(data = check.perc.na, mapping = aes(x = log(gdp_o), y = perc.na))+
  geom_point(color = gta_colour$qualitative[7])+
  geom_smooth(method=lm, color = gta_colour$blue[2])+
  xlim(c(10,25))+
  ylim(c(0,1))+
  xlab("log(GDP)")+
  ylab("Share of trade costs are not available")+
  labs(caption = "Source: Author's calculation based Conte et al. (2022)")+
  theme_minimal()
p2

p3 <- ggarrange(p1, p2, labels = c("Available trade costs to trade costs", "Log(GDP) to trade costs"), hjust = 0)
p3

gta_plot_saver(p3, path = path.plot, 
               name = "Trade cost availability",
               png = T)


hist(trade.costs$tij, breaks = 100)
perc.na.real <- sum(is.na(trade.costs$tij))/nrow(trade.costs)

rm(p1,p2,p3,trade.cost.stats,check.perc.na, avg.gdp, avg.trade.costs, gta_colour)

# 2. simulate trade costs ------------------------------------------------------


# 2.1 Estimate empirical parameters --------------------------------------------
GTA.coverage <- readxl::read_excel(path = paste0(path.data.out, "Country measurement index.xlsx"))
GTA.coverage <- GTA.coverage %>% 
  filter(chapter == "D")

rho <- max(na.omit(GTA.coverage$coverage.measure))
GTA.coverage.year <- GTA.coverage %>% filter(coverage.measure == rho)
rho <- 0.9 * GTA.coverage.year$intervention.id/log(GTA.coverage.year$gdp_o)
rho * log(GTA.coverage.year$gdp_o)

GTA.coverage$simulated.interventions <- rho* log(GTA.coverage$gdp_o)
GTA.coverage$scaled.up <- GTA.coverage$simulated.interventions/GTA.coverage$intervention.id

#scale up
GTA <- readRDS(file = paste0(path.data.out, "GTA_asymmetric_isic.RData"))
GTA <- unique(cSplit(GTA, "chapter", direction = "long"))
GTA <- unique(cSplit(GTA, "years.in.force", direction = "long"))

data.out <- data.frame()

for(i in years){
  data.loop <- GTA %>% filter(years.in.force == i)
  
  data.loop <- aggregate(data = data.loop, intervention.id ~ implementing.jurisdiction + affected.jurisdiction + years.in.force + chapter + mast.chapter, FUN = function(x) length(unique(x)))
  
  data.out <- rbind(data.out, data.loop)
}

names(data.out) <- c("country.1", "country.2", "year","chapter","mast.chapter",  "number.of.interventions")

data.out <- pivot_wider(data.out, id_cols = 1:4, names_from = "chapter", values_from = "number.of.interventions")
data.out$AB <- ifelse(is.na(data.out$AB), 0, data.out$AB)
data.out$D <- ifelse(is.na(data.out$D), 0, data.out$D)
data.out$GTT <- data.out$AB + data.out$D
data.out <- pivot_longer(data.out, cols = 5:ncol(data.out), names_to = "chapter", values_to = "number.of.interventions")
data.out <- pivot_wider(data.out, id_cols = c("country.1", "country.2", "year", "chapter"), names_from = "mast.chapter", values_from = "number.of.interventions")
data.out[is.na(data.out)] <- 0
data.out$total <- apply(data.out[,5:ncol(data.out)],1,FUN = sum)

data.out <- data.out %>% 
  filter(chapter == "D") %>% 
  select(c(country.1, country.2, year, total)) %>% 
  left_join(GTA.coverage[, c("implementing.jurisdiction", "years.in.force", "scaled.up")], c("country.1" = "implementing.jurisdiction", "year" = "years.in.force"))

data.out$total <- data.out$total * data.out$scaled.up
data.out$scaled.up <- NULL

data.out <- to_alphabeta(data.out, "country.1", "country.2")
data.out <- aggregate(data = data.out, total ~country.1 + country.2 + year, FUN = sum)
data.out$total <- round(data.out$total)


data <- data %>% 
  select(-c(B:P)) %>% 
  filter(chapter == "D")
data$C <- NULL

data <- data %>%
  rename("total.revealed" = "total") %>%
  left_join(data.out, by = c("country.1", "country.2", "year") )
data$total <- ifelse(is.na(data$total), 0, data$total)

rm(GTA.coverage, GTA.coverage.year, grid, data.loop, GTA)
# 2.2 Create model -----------------------------------------------------------------

#values are roughly what Arvics et al. (2016) estimated
b_0 <- 0 #not from arivs
b_1 <- 0.1 # measures (not from arvis)
b_2 <- 20 # distance (not from arvis)
b_3 <- -40 # common border
b_6 <- -5 #common language ethno
b_7 <- -10 #common language official
b_8 <- -15 #common colony
b_9 <- -15 #common RTA
censor.value <- 35

sim.data <- data %>% filter(chapter == "D")
sim.data <- sim.data %>%
  left_join(controls, by = c( "country.1" = "iso3_d", "country.2" = "iso3_o", "year" = "year"))
# create model
sim.data$trade.costs <- b_0 + b_1 * sim.data$total + b_2 * log(sim.data$distw_harmonic) + b_3 * sim.data$contig + b_6 * sim.data$comlang_ethno  + b_8 * sim.data$fta_wto+ b_7 *sim.data$comlang_off + b_8 * sim.data$comcol + rnorm(nrow(sim.data), 0, 5) #
sim.data <- sim.data[!is.na(sim.data$trade.costs),]
sim.data$trade.costs <- ifelse(sim.data$trade.costs < 0, 0, sim.data$trade.costs) #set a few neg. values to 0
# hist(sim.data$trade.costs, breaks = 100)
# hist(trade.costs$tij, breaks = 100)


# 2.3. censor it --------------------------------------------------------------
test <- log(sim.data$gdp_d) + log(sim.data$gdp_o) - log(sim.data$trade.costs)
hist(test)
sim.data.lin <- sim.data
sim.data$trade.costs <- ifelse(log(sim.data$gdp_d) + log(sim.data$gdp_o ) - log(sim.data$trade.costs) > censor.value, 0, sim.data$trade.costs ) #USE SECTOR GDP
sim.data.lin$trade.costs <- ifelse(log(sim.data.lin$gdp_d) + log(sim.data.lin$gdp_o ) - log(sim.data.lin$trade.costs) > censor.value, NA, sim.data.lin$trade.costs ) #USE SECTOR GDP

perc.na.synth <- sum(na.omit(sim.data$trade.costs) == 0)/nrow(sim.data)

# 2.4 test if censored countries are correct -----------------------------------

censored.data <- merge(trade.costs[, c("country.1", "country.2", "year", "tij")], 
                       sim.data[, c("country.1", "country.2", "year", "trade.costs")], 
                       by =  c("country.1", "country.2", "year")
)

#get matrix to check if the correct values are censored
censored.data$correct <- ifelse(is.na(censored.data$tij) & censored.data$trade.costs == 0, "11", 
                                ifelse(is.na(censored.data$tij) & censored.data$trade.costs != 0, "10", 
                                       ifelse(!is.na(censored.data$tij) & censored.data$trade.costs == 0, "01",
                                              ifelse(!is.na(censored.data$tij) & censored.data$trade.costs != 0,"00", NA))))
censored.data$comparison <- 1
censored.data <- aggregate(data = censored.data, comparison ~ correct, sum)




# 2.5 regressions --------------------------------------------------------------
sim.data$id <- paste0(sim.data$country.1, sim.data$country.2)


linreg <- lm(data = sim.data.lin, trade.costs ~ total.revealed + log(distw_harmonic) + contig + comlang_ethno  + fta_wto + comcol + comlang_off)
summary(linreg)

