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
  select(-c(mast.names, "tij", "L_harmful", "L_liberalising"))%>%
  mutate(interventions.revealed = total_harmful + total_liberalising)%>%
  select(-c("total_harmful", "total_liberalising"))

GTA.total <- data.out %>%
  left_join(GTA.total, by = c("country.1", "country.2", "year"))

GTA.total$old.int.trade <- GTA.total$intranat.trade^(14)
GTA.total$old.int.trade <- GTA.total$old.int.trade^(1/10)
### Regresson ------------------------------------------------------------------
b_0 <- 500
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
perc.not.censored.real <- sum(!is.na(trade.costs$tij))/nrow(trade.costs)

gamma <- 7000000000000
#get error term

sim <- GTA.total %>%
  filter(!is.na(geometric_avg_tariff))%>%
  filter(!is.na(lpi)) %>%
  select(-c(all_of(selected.countries[!selected.countries %in% "ZWE"])))%>%
  select(-c(all_of(paste0("year_",years.observation))))

names(sim)[names(sim) == "number.of.interventions"] <- "int"

bivariate_data <- as.data.frame(MASS::mvrnorm(n=nrow(sim),
                                        mu=c(0, 0),
                                        Sigma=matrix(c(200, 100, 100,200), ncol=2)))


sim <- sim %>%
  mutate(tij = b_0+b_1*int +b_4*comlang_ethno+b_5*fta_wto+b_6*comlang_off+b_7*comcol+b_8*lsci+b_9*lpi+b_11*geometric_avg_tariff+bivariate_data$V1)%>%
  mutate(tij_pure = b_0+b_1*int +b_4*comlang_ethno+b_5*fta_wto+b_6*comlang_off+b_7*comcol+b_8*lsci+b_9*lpi+b_11*geometric_avg_tariff)
sim$tij <- ifelse(sim$tij < 0, 0, sim$tij)

sim$is.not.censored <- 0 < (1/gamma)^(1/(2*(sigma-1))) * sim$intranat.trade*100 - sim$tij -100 + bivariate_data$V2 #division by 100 is because the tij are measured in AVE, so in percent
sum(na.omit(sim$is.not.censored)/nrow(sim))
sim$tij.censored <- sim$tij * (sim$is.not.censored  > 0) 
sim$tij.censored.na <- ifelse(sim$is.not.censored  > 0, sim$tij, NA)

mc.ols.pure <- lm(data = sim,                  tij_pure  ~ int + comlang_ethno  + fta_wto+ comlang_off + comcol + lsci + lpi + geometric_avg_tariff); summary(mc.ols.pure)

mc.ols <- lm(data = sim,                       tij.censored.na  ~ int + comlang_ethno  + fta_wto+ comlang_off + comcol + lsci + lpi + geometric_avg_tariff); summary(mc.ols)
mc.heckit <- sampleSelection::selection(data = sim,
                                    selection = is.not.censored ~ int + comlang_ethno  + fta_wto+ comlang_off + comcol + lsci + lpi + geometric_avg_tariff + intranat.trade, 
                                    outcome =   tij.censored.na ~ int + comlang_ethno  + fta_wto+ comlang_off + comcol + lsci + lpi + geometric_avg_tariff
                                    )
summary(mc.heckit)


saveRDS(list(mc.ols, mc.heckit, mc.ols.pure), file = paste0(path.data.out, "MC_results.Rds"))

library(texreg)
texreg(list(mc.ols, mc.heckit, mc.ols.pure))

ggplot(data= sim, aes(x = interventions.revealed, y = CRI_sqrt_gm_harmful))+
  geom_point()+
  geom_smooth(method = "lm")

# 2.4 test if censored countries are correct -----------------------------------
censored.data <- merge(trade.costs[trade.costs$chapter == "D", c("country.1", "country.2", "year", "tij")],
                       sim[, c("country.1", "country.2", "year", "is.censored")],
                       by =  c("country.1", "country.2", "year")
)

#get matrix to check if the correct values are censored
censored.data$correct <- ifelse(is.na(censored.data$tij) & censored.data$is.censored , "11",
                                ifelse(is.na(censored.data$tij) & !censored.data$is.censored , "10",
                                       ifelse(!is.na(censored.data$tij) & !censored.data$is.censored, "01",
                                              ifelse(!is.na(censored.data$tij) & censored.data$is.censored,"00", NA))))
censored.data$comparison <- 1
censored.data <- aggregate(data = censored.data, comparison ~ correct, sum)
sum(censored.data[c(1,4), 2])/sum(censored.data$comparison)
# could play around with variables to adjust and maximise that number.



detach("package:goft")
detach("package:fitdistrplus")
detach("package:MASS")

