# The simulations tries to understand the influence of modeling asymetric trade 
# costs with symmetrically. To do this, a linear model is assumed and generated.
# To keep the simulation to as close to reality as possible, the 

install.packages("mixtools")
install.packages("censReg")
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

years <- 2009:2019

# Paths

path.data.raw <- "0 data raw/"
path.data.out <- "1 data processed/"
path.plot <- "2 plots/"

gta_colour_palette()

# 1. get data --------------------------------------------------------------
trade.costs <- readRDS(file = paste0(path.data.out, "Trade Costs Processed.RData"))
data <- readRDS(file = paste0(path.data.out, "TRAINS_symmetric_w_controls.RData"))
data <- readRDS(file = paste0(path.data.out, "GTA_symmetric_w_controls.RData"))
data <- data %>% select(-c(iso3_o, iso3_d, iso3num_o))
controls <- readRDS(paste0(path.data.raw, "CEPII_Gravity_Variables.Rds")) 
controls <- controls %>% select(iso3_o, iso3_d, year, gdp_o, gdp_d)
 
# 2. preliminary checks --------------------------------------------------------
# check if assumed relationship (higher share of NAs in trade costs --> higher trade costs, lower GDP)
trade.costs <- merge(trade.costs, controls[, c("iso3_o","iso3_d","gdp_o","gdp_d", "year")], by.x = c("reporter", "partner", "year"), by.y = c("iso3_o", "iso3_d", "year"))
trade.costs$loggdp <- log(trade.costs$gdp_o) + log(trade.costs$gdp_d)

# check average trade costs of available data
check.perc.na <- trade.costs
check.perc.na$tij <- ifelse(is.na(check.perc.na$tij), 0, check.perc.na$tij)
avg.trade.costs <- check.perc.na %>% filter(tij > 0)
avg.trade.costs <- aggregate(data = avg.trade.costs, tij ~ reporter , FUN = mean)

# check percentage of NA per country
check.perc.na$tij <- ifelse(check.perc.na$tij ==  0, 1 ,0)
check.perc.na <- aggregate(data = check.perc.na, tij ~ reporter, FUN = function(x) sum(x)/length(x))
names(check.perc.na) <- c("reporter", "perc.na")
check.perc.na <- merge(avg.trade.costs, check.perc.na, by = c("reporter"), all.y = T)

# avg GDP per country
avg.gdp <- aggregate(data = trade.costs, gdp_o ~ reporter , FUN = mean)
check.perc.na <- merge(check.perc.na, avg.gdp, by = "reporter", all.x = T)

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
p1

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

#values are roughly what Arvics et al. (2016) estimated
b_0 <- 100 #not from arivs
b_1 <- 0.25 # measures (not from arvis)
b_2 <- 20 # distance (not from arvis)
b_3 <- -40 # common border
b_6 <- -5 #common language ethno
b_7 <- -10 #common language official
b_8 <- -15 #common colony
b_9 <- -15 #common RTA
censor.value <- 30

sim.data <- data
# create model
sim.data$trade.costs <- b_0 + b_1 * sim.data$number.of.interventions + b_2 * log(sim.data$distw_harmonic) + b_3 * sim.data$contig + b_6 * sim.data$comlang_ethno  + b_8 * sim.data$fta_wto + rnorm(nrow(sim.data), 0, 5) #+ b_7 *sim.data$comlang_off + b_8 * sim.data$comcol
sim.data <- sim.data[!is.na(sim.data$trade.costs),]
sim.data$trade.costs <- ifelse(sim.data$trade.costs < 0, 0, sim.data$trade.costs) #set a few neg. values to 0
hist(sim.data$trade.costs, breaks = 100)
hist(trade.costs$tij, breaks = 100)
# censor it
test <- log(sim.data$gdp_d) + log(sim.data$gdp_o) - log(sim.data$trade.costs)
hist(test)
sim.data.lin <- sim.data
sim.data$trade.costs <- ifelse(log(sim.data$gdp_d) + log(sim.data$gdp_o ) - log(sim.data$trade.costs) > censor.value, 0, sim.data$trade.costs ) #USE SECTOR GDP
sim.data.lin$trade.costs <- ifelse(log(sim.data.lin$gdp_d) + log(sim.data.lin$gdp_o ) - log(sim.data.lin$trade.costs) > censor.value, NA, sim.data.lin$trade.costs ) #USE SECTOR GDP

perc.na.synth <- sum(sim.data$trade.costs == 0)/nrow(sim.data)


# regression
sim.data <- sim.data %>% filter(chapter == "D")
sim.data$id <- paste0(sim.data$ISO_country.1, sim.data$ISO_country.2)
sim.data <- pdata.frame(sim.data, c("id", "year"))
reg <- censReg(data = sim.data, trade.costs ~ number.of.interventions + log(distw_harmonic) + contig + comlang_ethno  + fta_wto, method = "BHHH", nGHQ = 64) #+ comlang_off + comcol
summary(reg)


linreg <- lm(data = sim.data.lin, trade.costs ~ number.of.interventions + log(distw_harmonic) + contig + comlang_ethno  + fta_wto)
summary(linreg)
# 2.2 test if censored countries are correct -----------------------------------

censored.data <- merge(trade.costs[, c("reporter", "partner", "year", "tij")], 
                       sim.data[, c("ISO_country.1", "ISO_country.2", "year", "trade.costs")], 
                       by.x =  c("reporter", "partner", "year"), 
                       by.y = c("ISO_country.1", "ISO_country.2", "year"))

#get matrix to check if the correct values are censored
censored.data$correct <- ifelse(is.na(censored.data$tij) & censored.data$trade.costs == 0, "11", 
                                ifelse(is.na(censored.data$tij) & censored.data$trade.costs != 0, "10", 
                                       ifelse(!is.na(censored.data$tij) & censored.data$trade.costs == 0, "01",
                                              ifelse(!is.na(censored.data$tij) & censored.data$trade.costs != 0,"00", NA))))
censored.data$comparison <- 1
censored.data <- aggregate(data = censored.data, comparison ~ correct, sum)
