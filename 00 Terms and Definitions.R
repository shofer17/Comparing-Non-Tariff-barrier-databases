# This script gets loaded in at the start of every other script and gives
# the essential definitions and function. 
# Parameters

library(tidyverse)
library(splitstackshape)
library(gtalibrary)

years <- 2009:2019
gta_colour_palette()

# Paths
path.data.raw <- "1 data/0 data raw/"
path.data.out <- "1 data/1 data processed/"
path.data.reg <- "1 data/2 data for regression/"
path.plot <- "2 plots/"

# available countries and MAST
selected.countries <- read.csv(file = paste0(path.data.out, "selected countries.csv"))$x
selected.mast <- read.csv(file = paste0(path.data.out, "selected MAST chapters.csv"))$x




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


# 1. standard frame -------------------------------------------

#add 0 measure pairs
grid <- expand.grid(selected.countries, selected.countries, years, c("AB", "D", "GTT"))
names(grid) <- c("country.1", "country.2", "year","chapter")
grid <- grid %>% 
  mutate(country.1 = as.character(country.1)) %>%
  mutate(country.2 = as.character(country.2)) %>%
  filter(country.2 != country.1)

grid <- unique(to_alphabeta(grid, "country.1", "country.2"))
