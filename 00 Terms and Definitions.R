# This script gets loaded in at the start of every other script and gives
# the essential definitions and function. 
# Parameters

library(tidyverse)
library(splitstackshape)
library(gtalibrary)

sigma = 8 # elasticity of substitution
years <- 2005:2019
base <- 2005:2007
years.observation <- 2009:2019
gta_colour_palette()
standard.colors <- c(gta_colour$blue[2], gta_colour$qualitative[7], gta_colour$qualitative[5], gta_colour$grey[1])


evaluation <- c("harmful", "liberalising")
# Paths
path.data.raw <- "1 data/0 data raw/"
path.data.out <- "1 data/1 data processed/"
path.data.reg <- "1 data/2 data for regression/"
path.plot <- "2 plots/"

# available countries and MAST
selected.countries <- read.csv(file = paste0(path.data.out, "selected countries.csv"))$x
selected.countries <- selected.countries[!selected.countries %in% c("CUB", "VEN")]
selected.mast <- read.csv(file = paste0(path.data.out, "selected MAST chapters.csv"))$x
selected.mast.red <- selected.mast[!selected.mast %in% c("N")]
mast.names <- c(paste0(selected.mast[!selected.mast %in% "N"], "_harmful"), paste0(selected.mast[!selected.mast %in% "N"], "_liberalising"))




# 0. functions ------------------------------------------------------------------

#transform names to iso
to_iso <- function(data, column.name.1, column.name.2){ # from GTA name to ISO
  
  iso.conversion <- rbind(country.names[, c("name", "iso_code")], c("EU", "EU"))
  
  eval(parse(text = paste0("data <- merge(data, iso.conversion, by.x = '",column.name.1,"', by.y = 'name', all.x = T)")))
  names(data)[ncol(data)] <- "ISO_country.1"
  
  eval(parse(text = paste0("data <- merge(data, iso.conversion, by.x = '",column.name.2,"', by.y = 'name', all.x = T)")))
  names(data)[ncol(data)] <- "ISO_country.2"
  
  data <- data[, 3:ncol(data)]
  names(data)[(ncol(data)-1):ncol(data)] <- c(column.name.1, column.name.2)
  
  return(data)
}



# Make two columns bilateral, so sort them rowise alphabetically
# data = trade.costs
# column.name.1 <- "reporter"
# column.name.2 <- "partner"
to_alphabeta <- function(data, column.name.1, column.name.2){
  
  eval(parse(text = paste0("data$backup <- data$", column.name.1)))
  
  eval(parse(text = paste0("data$",column.name.1," <- ifelse(data$",column.name.1,"< data$", column.name.2,
                                            ",data$",column.name.1,
                                            ",data$",column.name.2,")")))
  
  eval(parse(text = paste0("data$",column.name.2," <- ifelse(data$",column.name.2,"> data$backup",
                           ",data$",column.name.2,
                           ",data$backup",")")))

  eval(parse(text = "data$backup <- NULL"))
  
  return(data)
}



# Run a regression based on string inputs that makes handling of many controls easier
# Also, have Heckman and lm option
reg <- function(data,type = "lm", 
                dependant = "tij", 
                cont, 
                dependant.selection = "is.available", 
                cont.selection,
                weights = NULL){
  
  if(type == "lm"){
    reg <- paste0("lm(data = data,", dependant, " ~", cont, ", weights = ",weights,")")
  }
  
  if(type == "heckman"){
    
    reg <- paste0("selection(", dependant.selection, "~", cont.selection, ",", 
                  dependant,           "~", cont, ",",
                  "method = '2step', data = data, weights = ",weights ,")")
  }
  
  reg <- paste0("output <- ", reg)
  eval(parse(text = reg))
  return(output)
}

# 1. standard frame -------------------------------------------

#add 0 measure pairs
grid <- expand.grid(selected.countries, selected.countries, years, c("AB", "D", "GTT"))
names(grid) <- c("country.1", "country.2", "year","chapter")
grid <- grid %>% 
  mutate(country.1 = as.character(country.1)) %>%
  mutate(country.2 = as.character(country.2)) %>%
  filter(country.2 != country.1)

grid <- unique(to_alphabeta(grid, "country.1", "country.2"))
grid.observed <- grid %>% 
  filter(year %in% years.observation)

