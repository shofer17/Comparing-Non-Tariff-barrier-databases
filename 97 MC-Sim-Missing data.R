# This file simulates data and serves as a decision aid for model specification

library(ggplot2)
library(gtalibrary)
setwd("..") #move up one

path.plots <- "2 plots"

gta_colour_palette()
# draw random numbers
rm(list = ls())

n <- 1000
b1 <- 1
b2 <- -1
x_1 <- rnorm(n)
x_2 <- rnorm(n)

y <- b1*x_1 + b2 * x_2 + rnorm(n)

index <- y < sample(c(1:3), n, replace = T)
y_t <- y[index]
y_t2 <- y
y_t2[!index] <- 3

lm(y ~ x_1 + x_2)
lm(y[index] ~ x_1[index] + x_2[index])
lm(y_t2 ~ x_1 + x_2)





# 1. plot threshold ------------------------------------------------------------
threshold <- 1.5

y_n <- b1 * x_1 + rnorm(n)
y <- b1 * x_1

data <- data.frame("x" = x_1,
                   "y_n" = y_n,
                   "y_t" = y,
                   "y" = y
                   )
data$y_nt <- ifelse(data$y_n > threshold, NA, data$y_n)
data$y_t <- ifelse(data$y > threshold, NA, data$y)

data <- pivot_longer(data, 2:5, values_to = "y", names_to = "key")
data$group <- ifelse(data$key %in% c("y", "y_t"), "Without noise", "With noise")
data$key <- ifelse(data$key %in% c("y_t", "y_nt"), "truncated", "not truncated")

p <- ggplot(data = data, aes(x = x, y = y, color = key))+
  geom_point()+
  geom_hline(yintercept = threshold)+
  geom_smooth(method = "lm")+
  scale_color_manual(values = c(gta_colour$qualitative[7], gta_colour$blue[2]),
                     labels = c("Without upper limit",paste0("With upper limit at ", threshold)))+
  labs(colour="")+
  gta_theme(legend.position = "bottom")+
  ggtitle("The effect of a threshold for missing values in parameter estimation")+
  facet_wrap(.~group)
p


gta_plot_saver(plot = p,
               path = path.plots,
               name = "effect of natural variance")
