# The simulations tries to understand the influence of modeling asymetric trade 
# costs with symmetrically. To do this, a linear model is assumed and generated.
# To keep the simulation to as close to reality as possible, the 

install.packages("mixtools")
install.packages("censReg")
library(mixtools)
library(censReg)
library(plm)

rm(list = ls())

years <- 2009:2019

# Paths

path.data.raw <- "0 data raw/"
path.data.out <- "1 data processed/"


# 1. get data --------------------------------------------------------------
trade.costs <- readRDS(file = paste0(path.data.out, "Trade Costs Processed.RData"))
data <- readRDS(file = paste0(path.data.out, "TRAINS_symmetric_w_controls.RData"))
data <- data %>% select(-c(iso3_o, iso3_d, iso3num_o))
controls <- readRDS(paste0(path.data.raw, "CEPII_Gravity_Variables.Rds")) 

# 2. simulate trade costs ------------------------------------------------------

trade.costs <- merge(trade.costs, controls[, c("iso3_o","iso3_d","gdp_o","gdp_d")], by.x = c("reporter", "partner"), by.y = c("iso3_o", "iso3_d"))
hist(trade.costs$tij, breaks = 100)



#values are roughly what Arvics et al. (2016) estimated
b_0 <- 100 #not from arivs
b_1 <- 0.25 # measures (not from arvis)
b_2 <- 0.5 # distance (not from arvis)
b_3 <- -40 # common border
b_6 <- -5 #common language ethno
b_7 <- -10 #common language official
b_8 <- -15 #common colony
b_9 <- -15 #cpmmon RTA

censor.value <- 35

# create model
data$trade.costs <- b_0 + b_1 * data$number.of.interventions + b_2 * log(data$distw_harmonic) + b_3 * data$contig + b_6 * data$comlang_ethno + b_7 *data$comlang_off + b_8 * data$comcol + b_8 * data$fta_wto + rnorm(nrow(data), 0, 0.1)
data$trade.costs <- ifelse(data$trade.costs < 0, 0, data$trade.costs) #set a few neg. values to 0
hist(data$trade.costs, breaks = 100)

sum(is.na(trade.costs$tij))/nrow(trade.costs)

# censor it
data$trade.costs <- ifelse(log(data$gdp_d) + log(data$gdp_o ) - log(data$trade.costs) > censor.value, 0, data$trade.costs ) #USE SECTOR GDP
hist(data$trade.costs, breaks = 100)
test <- log(data$gdp_d) + log(data$gdp_o) - log(data$trade.costs)
mean(na.omit(test))
hist(na.omit(test))

# regression
data <- data %>% filter(chapter == "D")
data$id <- paste0(data$ISO_country.1, data$ISO_country.2)
data <- pdata.frame(data, c("id", "year"))
reg <- censReg(data = data, trade.costs ~ number.of.interventions + log(distw_harmonic) + contig + comlang_ethno + comlang_off + comcol + fta_wto, method = "BHHH", nGHQ = 8)
summary(reg)

# Load mtcars dataset
data(mtcars)

# Censor data
mtcars$mpg[mtcars$mpg > 25] <- NA

# Estimate parameters using EM algorithm
result <- normalmixEM(mtcars$mpg, arbvar = T, epsilon = 1e-03)

# View results
result