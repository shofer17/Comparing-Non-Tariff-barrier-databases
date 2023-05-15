# The simulations tries to understand how the econometric approaches utilized, based on
# assumptions for the data generating process, influences the results. 
# To do this, a linear model is assumed and generated.

library(ggplot2)
library(ggpubr)
library(gtalibrary)
library(tidyverse)
setwd("..")
options(scipen = 999)
rm(list = ls())

# Paths
source("BA_Thesis_code/00 Terms and Definitions.R")

# 1. get data --------------------------------------------------------------

trade.costs <- readRDS(file = paste0(path.data.out, "Trade Costs Processed.RData"))
controls <- readRDS(file = paste0(path.data.out, "Controls cleaned CEPII grid.RData"))
GTA.coverage <- readxl::read_excel(path = paste0(path.data.out, "Country measurement index total.xlsx"))
GTA <- readRDS(file = paste0(path.data.out, "GTA_asymmetric_isic.RData"))

# 2. preliminary checks --------------------------------------------------------
# check if assumed relationship (higher share of NAs in trade costs --> higher trade costs, lower GDP)


if(F){
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
}
# 2. simulate trade costs ------------------------------------------------------
a <- 0.9

# 2.1 Estimate empirical parameters --------------------------------------------
# Get max empirical CRI --------------------------------------------------------

GTA.coverage <- GTA.coverage %>% 
  filter(chapter == "D") %>%
  left_join(unique(GTA[, c("country.1","year", "gdp_o")]), by = c("country" = "country.1", "year" = "year"))
  
CRI_max <- max(GTA.coverage$CRI_sqrt[GTA.coverage$country == "USA"])
GTA.coverage$simulated.interventions <- CRI_max * sqrt(GTA.coverage$gdp_o)/a


GTA.coverage <- GTA.coverage %>% 
  filter(intervention.id != 0)
#GTA.coverage$intervention.id <- ifelse(GTA.coverage$intervention.id == 0, 1, GTA.coverage$intervention.id) #if we know of know intervention, set to 1 for scaling up, otherwise infinite scale up
GTA.coverage$scaled.up <- GTA.coverage$simulated.interventions/GTA.coverage$intervention.id


#scale up

GTA <- unique(cSplit(GTA, "chapter", direction = "long"))
GTA <- unique(cSplit(GTA, "years.in.force", direction = "long"))
GTA <- GTA %>% filter(chapter == "D")

data.out <- data.frame()

for(i in years.observation){
  data.loop <- GTA %>% filter(years.in.force == i)
  
  data.loop <- aggregate(data = data.loop, intervention.id ~ implementing.jurisdiction + affected.jurisdiction + years.in.force, FUN = function(x) length(unique(x)))
  
  data.out <- rbind(data.out, data.loop)
}

names(data.out) <- c("country.1", "country.2", "year","number.of.interventions")
data.out <- data.out %>% 
  left_join(GTA.coverage[, c("country", "year", "scaled.up")], c("country.1" = "country", "year"))%>%
  mutate(number.of.interventions = number.of.interventions * scaled.up)%>%
  select(-scaled.up)

data.out <- to_alphabeta(data.out, "country.1", "country.2")
data.out <- aggregate(data = data.out, number.of.interventions ~country.1 + country.2 + year, FUN = sum)
data.out <- data.out %>%
  mutate(number.of.interventions = round(number.of.interventions))

rm(GTA.coverage, grid, data.loop, GTA)

data.out <- grid.observed %>%
  filter(chapter == "D")%>%
  select(-c(chapter))%>%
  left_join(data.out, by = c("country.1", "country.2", "year"))%>%
  mutate(int = ifelse(is.na(number.of.interventions), 0, number.of.interventions))


# 2.2 Create model -----------------------------------------------------------------
GTA.total <- readRDS(file = paste0(path.data.out, "GTA_final.RData"))
GTA.total <- GTA.total %>%
  filter(chapter == "D") %>%
  select(-c(mast.names, "tij", "L_harmful", "L_liberalising"))

GTA.total <- data.out %>%
  left_join(GTA.total, by = c("country.1", "country.2", "year"))

GTA.total$old.int.trade <- GTA.total$intranat.trade^(14)
GTA.total$old.int.trade <- GTA.total$old.int.trade^(1/10)
### Regresson ------------------------------------------------------------------
b_0 <- -100
b_1 <- 0.1 # measures 
b_2 <- 50 # distance 
b_3 <- -50 # common border
b_4 <- -10 #common language ethno
b_5 <- -20 #common RTA
b_6 <- -30 #common language official
b_7 <- -10 #common colony
b_8 <- -1 # LSCI
b_9 <- -100 # LPI
b_10 <- 40 #Landlocked
b_11 <- 50 #Tariffs
sigma <- 8
perc.censored.real <- sum(is.na(trade.costs$tij))/nrow(trade.costs)
gamma <- 450000000000000
names(sim)[names(sim) == "number.of.interventions"] <- "int"
#get error term
library(MASS)
bivariate_data <- as.data.frame(mvrnorm(n=nrow(GTA.total),
                                        mu=c(0, 0),
                                        Sigma=matrix(c(100, 10, 10,100), ncol=2)))

sim <- GTA.total
sim <- sim %>%
  mutate(tij = b_0+b_1*int +b_2*log(distw_harmonic)+b_3*contig+b_4*comlang_ethno+b_5*fta_wto+b_6*comlang_off+b_7*comcol+b_8*lsci+b_9*lpi+b_10*landlocked+b_11*geometric_avg_tariff+bivariate_data$V1)
sim$tij <- ifelse(sim$tij<0, 0, sim$tij)

sim$is.censored <- ifelse(0 < (1/gamma)^(1/(2*(sigma-1))) * sim$intranat.trade - sim$tij/100-1, 0, 1) #division by 100 is becauase the tij are measured in AVE, so in percent
sum(na.omit(sim$is.censored)/nrow(sim))
sim$tij.censored <- ifelse(sim$is.censored == 1, NA, sim$tij)




t_out <- sampleSelection::selection(data = sim.data,
                                    !is.censord ~ log(distw_harmonic) + contig + comlang_ethno  + fta_wto+ comlang_off + comcol + lsci + lpi + landlocked + geometric_avg_tariff + exports, 
                                    trade.costs.recorded ~ total.revealed + log(distw_harmonic) + contig + comlang_ethno  + fta_wto+ comlang_off + comcol + lsci + lpi + landlocked + geometric_avg_tariff)
summary(t_out)


hist(sim.data$trade.costs, breaks = 100, xlim = range(0,600))
hist(trade.costs$tij, breaks = 100, xlim = range(0,600))

ggplot(data= sim.data, aes(x = total.revealed, y = coverage.mean))+
  geom_point()

# 2.4 test if censored countries are correct -----------------------------------
sim.data$trade.costs.recorded.na <- ifelse(sim.data$trade.costs.recorded == 0, NA, sim.data$trade.costs.recorded)
censored.data <- merge(trade.costs[, c("country.1", "country.2", "year", "tij")],
                       sim.data[, c("country.1", "country.2", "year", "trade.costs.recorded.na")],
                       by =  c("country.1", "country.2", "year")
)

#get matrix to check if the correct values are censored
censored.data$correct <- ifelse(is.na(censored.data$tij) & is.na(censored.data$trade.costs.recorded.na) , "11",
                                ifelse(is.na(censored.data$tij) & !is.na(censored.data$trade.costs.recorded.na) , "10",
                                       ifelse(!is.na(censored.data$tij) & is.na(censored.data$trade.costs.recorded.na), "01",
                                              ifelse(!is.na(censored.data$tij) & !is.na(censored.data$trade.costs.recorded.na),"00", NA))))
censored.data$comparison <- 1
censored.data <- aggregate(data = censored.data, comparison ~ correct, sum)
sum(censored.data[c(1,4), 2])/sum(censored.data$comparison)
# could play around with variables to adjust and maximise that number.


# 2.5 regressions --------------------------------------------------------------
## Setup -----------------------------------------------------------------------

library(mixtools)
library(censReg)
library(plm)
library(fastDummies)

GTA.measurement <- readxl::read_xlsx(path = paste0(path.data.out, "GTA_Measurement_index.xlsx"))
sim.data.lin <- merge(sim.data, GTA.measurement, by = c("country.1", "country.2", "year", "chapter"))
sim.data.lin <- dummy_cols(sim.data.lin, select_columns = "country.1")

column.dummy.start <- min(grep("country.1.", names(sim.data.lin)))

names(sim.data.lin)[column.dummy.start:ncol(sim.data.lin)] <- substr(names(sim.data.lin)[column.dummy.start:ncol(sim.data.lin)], 11,13)
#sim.data.lin$ZWE <- 0 #correct for last country in country.2

for(i in column.dummy.start:ncol(sim.data.lin)){ # create dummies and add both countries
  sim.data.lin[,i] <- ifelse((sim.data.lin[, "country.1"] == names(sim.data.lin)[i]) |
                          (sim.data.lin[, "country.2"] == names(sim.data.lin)[i]), 
                        1,0)
}

#rm(controls, data.out, trade.costs, data, GTA.measurement)
backup <- sim.data.lin

sim.data$trade.costs.recorded.na <- ifelse(sim.data$trade.costs.recorded == 0, NA, sim.data$trade.costs.recorded)

lin <- "lm(data = sim.data,"
reg <- "trade.costs.recorded.na ~ total.revealed + log(distw_harmonic) + contig + comlang_ethno  + fta_wto + comlang_off + comcol + lsci + lpi +landlocked  + geometric_avg_tariff + coverage.mean"
reg.CRI.exl <- "trade.costs ~ total.revealed + log(distw_harmonic) + contig + comlang_ethno  + fta_wto + comlang_off + comcol + lsci + lpi +landlocked  + geometric_avg_tariff"
fe  <- paste0(names(sim.data.lin)[column.dummy.start:ncol(sim.data.lin)], collapse = "+" )

test <- sim.data %>% filter(!is.na(coverage.mean))
ggplot(test, aes(x = coverage.mean.log, y = total.revealed))+
  geom_point()+
  geom_smooth(method = "lm")

### Linreg -------------------------------------------------------------------------

linreg <- eval(parse(text = paste0(lin,reg,")")))
summary(linreg)

linreg.fixed <- eval(parse(text = paste0(lin,reg,"+",fe, ")")))
summary(linreg.fixed)


### Linreg (weighted) -------------------------------------------------------------------------

# Geometric mean weights
linreg.weighted.geom <- eval(parse(text = paste0(lin,reg.CRI.exl,",","weights = coverage.geom.mean",")")))
summary(linreg.weighted.geom)
linreg.weighted.fixed.geom <- eval(parse(text = paste0(lin,reg.CRI.exl,"+",fe,",","weights = coverage.geom.mean",")")))
summary(linreg.weighted.fixed.geom)

# arithm mean weights
linreg.weighted <- eval(parse(text = paste0(lin,reg.CRI.exl,",","weights = coverage.mean",")")))
summary(linreg.weighted)
linreg.weighted.fixed <- eval(parse(text = paste0(lin,reg.CRI.exl,"+",fe,",","weights = coverage.mean",")")))
summary(linreg.weighted.fixed)

library(texreg)
texreg(list(linreg, linreg.fixed, linreg.weighted.mean, linreg.weighted.fixed.mean,  linreg.weighted.geom, linreg.weighted.fixed.geom) )


### Heckman -------------------------------------------------------------------------

sim.data.lin$is.available <- ifelse(sim.data.lin$trade.costs == 0, 0, 1)
sim.data.lin <- relocate(sim.data.lin, is.available, .before = total.revealed)
library(sampleSelection)
heckit <- selection(is.available ~ total + log(distw_harmonic) + contig + comlang_ethno  + fta_wto + comlang_off + comcol + lsci + lpi +landlocked  + geometric_avg_tariff + log(gdp),
                    trade.costs ~ total + log(distw_harmonic) + contig + comlang_ethno  + fta_wto + comlang_off + comcol + lsci + lpi +landlocked  + geometric_avg_tariff,
                    method = "2step",
                    data = sim.data.lin)
summary(heckit)






#CHECK COVERAGE MEAN NA


heckman.fixed <- "heckman.fixed <- selection(is.available ~ log(distw_harmonic) + contig + fta_wto + lpi + landlocked, trade.costs ~ total.revealed + log(distw_harmonic) +  contig + comlang_ethno + fta_wto + lsci + lpi + landlocked + geometric_avg_tariff + coverage.mean"
heckman.fixed <- paste0(heckman.fixed,"+", paste0(names(sim.data.lin)[100:ncol(sim.data.lin)], collapse = "+" ),',method = "2step",data = sim.data.lin)')
eval(parse(text = heckman.fixed))
summary(heckman.fixed)


detach("package:goft")
detach("package:fitdistrplus")
detach("package:MASS")

names(heckit$lm$coefficients) <- gsub("XO", "", names(heckit$lm$coefficients))
names(heckit$lm$qr) <- gsub("XO", "", heckit$lm$qr[1])
names(heckit$lm$effects) <- gsub("XO", "", names(heckit$lm$effects))
names(heckit$lm$coefficients) <- gsub("XO", "", names(heckit$lm$coefficients))

t <- texreg(list(linreg, linreg.fixed, linreg.weighted.geom, linreg.weighted.fixed.geom, heckit))
colnames(heckit$lm$qr[[1]]) <- gsub("XO", "", colnames(heckit$lm$qr[[1]]))
test <- colnames(heckit$lm$qr[[1]])

### PPML -------------------------------------------------------------------------
library(gravity)

ppml <- ppml(data = sim.data.lin, 
             dependent_variable = "tij", 
             distance = "distw_harmonic", 
             additional_regressors = c("total.revealed","comlang_off", "comcol", 
                                       "contig", "comlang_ethno", "fta_wto", "lsci", 
                                       "lpi", "landlocked", "geometric_avg_tariff", "coverage.mean"))
summary(ppml)


ppml.fixed <- "ppml.fixed <- ppml(data = sim.data.lin, dependent_variable = 'tij', distance = 'distw_harmonic', additional_regressors = c('total.revealed','comlang_off', 'comcol', 'contig', 'comlang_ethno', 'fta_wto', 'lsci', 'lpi', 'landlocked', 'geometric_avg_tariff', 'coverage.mean',"
ppml.fixed <- paste0(ppml.fixed,"'", paste0(names(sim.data.lin)[column.dummy.start:ncol(sim.data.lin)], collapse = "','" ),"'", "))")
eval(parse(text = ppml.fixed))
summary(ppml.fixed)

### Bind together
names(heckit$lm$coefficients) <- gsub("XO", "", names(heckit$lm$coefficients))
names(heckit$lm$qr) <- gsub("XO", "", names(heckit$qr$coefficients))

library(texreg)
texreg(list(linreg, linreg.fixed, heckit, heckman.fixed, ppml, ppml.fixed) )
