rm(list =ls())

library(ggplot2)

# Parameters ------------

draws <- 10000
countries <- 10000
share_discovered <- 0.5
# Draw sample ---------------

id <- 1:countries
reduced <- id*(1/countries)
#is.g20 <- ifelse(id <= 2000, 1,0)
trade <- sample(1:5, countries, replace = T)
b_theoretical <- sample(1:10, countries, replace = T) # theoretical interventions
b_empirical <- b_theoretical * reduced
#b_empirical <- ifelse(is.g20 == 1, b_theoretical, share_discovered*b_theoretical) # empirically found interventions

u <- rnorm(draws)

y <- trade -0.5*b_theoretical + u

# Regression -------------

y_est <- lm(y ~ b_empirical)
y_estimates <- y_est$fitted.values
y_residuals <- y_est$residuals
#y_est$residuals


data <- data.frame(cbind(y, y_estimates,b_theoretical, b_empirical, is.g20))

ggplot(data, aes(y = y_residuals,x = id))+
  geom_point()+ 
  geom_smooth(method='lm', formula= y~x)
  #geom_point(aes(x = is.g20))

summary(lm(y_residuals ~ id))
