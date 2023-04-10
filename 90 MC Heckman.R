set.seed(0)
library("sampleSelection")
library("mvtnorm")

n = 10000


eps <- rmvnorm(n, c(0,0), matrix(c(1,-0.7,-0.7,1), 2, 2))
xs <- -1 *runif(n)
ys <- xs + eps[,1] < 0
xo <- -1 *runif(n)
yoX <- xo + eps[,2]
yo <- yoX*(ys > 0)

summary( selection(ys~xs, yo ~xo))
use pnorm funciton ev. 

eps <- rmvnorm(n, c(0,0), matrix(c(1,-0.7,-0.7,1), 2, 2))
xs <-  runif(n)
ys <- xs + eps[,1] > 0
xo <-  runif(n)
yoX <- xo + eps[,2]
yo <- yoX*(ys > 0)

summary( selection(ys~xs, yo ~xo))



library(MASS)
library(caret)
library(dplyr)
bivariate_data <- as.data.frame(mvrnorm(n=nrow(sim.data),
                                        mu=c(0, 0),
                                        Sigma=matrix(c(10, 4, 4,10), ncol=2)))
detach("package:MASS")

t <- sim.data %>% select(c("total.revealed","distw_harmonic","contig","comlang_ethno",
                           "comlang_off","comcol","comrelig","fta_wto","landlocked",
                           "lsci","lpi","geometric_avg_tariff","total","gdp",
                           "trade.costs","censored"))
# t$total <- ifelse(t$total == 0, 20, t$total)
# t$total <- rnorm(nrow(t), 100, 20)
#t <- t %>% filter(total > 0)
library(MASS)
bivariate_data <- as.data.frame(mvrnorm(n=nrow(t),
                                        mu=c(0, 0),
                                        Sigma=matrix(c(10, 4, 4,10), ncol=2)))

cols <- nearZeroVar(t)
test <- t[apply(t, 1, var) >= 10000, ]


t$xs  <- b_0 + b_1 * t$total + b_2 * log(t$distw_harmonic) + b_3 * t$contig + b_4 * t$comlang_ethno  + b_5 * t$fta_wto+ b_6 *t$comlang_off + b_7 * t$comcol + b_8 * t$lsci + b_9 * t$lpi + b_10 * t$landlocked  + b_11 * t$geometric_avg_tariff + b_12 *log(t$gdp) + bivariate_data$V1
t$ys  <- t$xs  < 0
t$yoX <- b_0 + b_1 * t$total + b_2 * log(t$distw_harmonic) + b_3 * t$contig + b_4 * t$comlang_ethno  + b_5 * t$fta_wto+ b_6 *t$comlang_off + b_7 * t$comcol + b_8 * t$lsci + b_9 * t$lpi + b_10 * t$landlocked  + b_11 * t$geometric_avg_tariff+ bivariate_data$V2
t$yo  <- t$yoX * (t$ys > 0) # 


t_out <- sampleSelection::selection(data = t,
                           ys ~ log(distw_harmonic) + contig + comlang_ethno  + fta_wto+ comlang_off + comcol + lsci + lpi + landlocked + geometric_avg_tariff + log(gdp), 
                           yo ~ total +  log(distw_harmonic) + contig + comlang_ethno  + fta_wto+ comlang_off + comcol + lsci + lpi + landlocked + geometric_avg_tariff)


summary(t_out)








sim.data$censored <- b_0 + b_1 * sim.data$total + b_2 * log(sim.data$distw_harmonic) + b_3 * sim.data$contig + b_4 * sim.data$comlang_ethno  + b_5 * sim.data$fta_wto+ b_6 *sim.data$comlang_off + b_7 * sim.data$comcol + b_8 * sim.data$lsci + b_9 * sim.data$lpi + b_10 * sim.data$landlocked  + b_11 * sim.data$geometric_avg_tariff + b_12 *log(sim.data$gdp) + bivariate_data$V2
sim.data$trade.costs <- b_0 + b_1 * sim.data$total + b_2 * log(sim.data$distw_harmonic) + b_3 * sim.data$contig + b_4 * sim.data$comlang_ethno  + b_5 * sim.data$fta_wto+ b_6 *sim.data$comlang_off + b_7 * sim.data$comcol + b_8 * sim.data$lsci + b_9 * sim.data$lpi + b_10 * sim.data$landlocked  + b_11 * sim.data$geometric_avg_tariff + bivariate_data$V1 #
sim.data$is.censord <- ifelse(sim.data$censored > 0, 1, 0)





b_12 <- -9.52 # //b_12 <- -0.0000031523


sim.data$trade.costs <- ifelse(sim.data$censored > 0, 0, sim.data$trade.costs)

