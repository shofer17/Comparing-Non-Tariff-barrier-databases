# Author: Silvan Hofer
# Date: 6. February 2023
# Purpose: 
# This script create all help files needed for data cleaning and analysis. 


library(xml2) #webscraping WTO names table
library(readxl)
library(tidyr)
library(magrittr)
library(dplyr)

setwd("..") # move up one
rm(list = ls())

path.data.raw <- "0 data raw/"
path.data.out <- "1 data processed/"

# 1. WTO country name conversion to ISO-----------------------------------------

if(F){ # get WTO conversion table, only exectue if new version is necessary
  # Get WTO country names to ISO conversions
  # download html and get table
  wto.names.to.iso <- "https://docs.wto.org/gtd/Default.aspx?pagename=WTOIsocodes&langue=e"
  wto.names.to.iso <- xml2::read_html(wto.names.to.iso)
  wto.names.to.iso <- rvest::html_table(wto.names.to.iso)[[4]] %>% 
    tibble::as_tibble(.name_repair = "unique")
  
  # clean table
  wto.names.to.iso <- wto.names.to.iso[1:97, 1:4]
  names(wto.names.to.iso) <- c("Name", "ISO", "Name", "ISO")
  wto.names.to.iso <- rbind(wto.names.to.iso[, 1:2], wto.names.to.iso[, 3:4])
  wto.names.to.iso <- na.omit(wto.names.to.iso)
  wto.names.to.iso$Name <- gsub("(\r|\n|\t)", "", wto.names.to.iso$Name)
  wto.names.to.iso$ISO <- gsub("(\r|\n|\t)", "", wto.names.to.iso$ISO)
  
  # adjust some values manually
  wto.names.to.iso[wto.names.to.iso$Name == "Eswatini (formerly Swaziland)", "Name"] <- "Eswatini"
  wto.names.to.iso[wto.names.to.iso$Name == "European Union formerly European Communities", "Name"] <- "European Union"
  wto.names.to.iso[wto.names.to.iso$ISO == "EUEEC", "ISO"] <- "EU"
  wto.names.to.iso[wto.names.to.iso$Name == "Romania formerly Romania", "Name"] <- "Romania"
  wto.names.to.iso[wto.names.to.iso$ISO == "ROUROM", "ISO"] <- "ROU"
  wto.names.to.iso[wto.names.to.iso$ISO == "TPKM", "ISO"] <- "TWN"
  
  writexl::write_xlsx(wto.names.to.iso, path = paste0(path.data.out, 
                                                      "WTO ISO conversions.xlsx"))
}

# 2. Conversion table for HS codes ---------------------------------------------
# The trade costs are calculated based on ISIC Rev 3 and cover chapters A,B and D
# GTA data works on CPC 2.1 and HS 2012
# WTO data works on HS codes but I could not find the vintage
# TRAINS data works on HS codes but I could not find the vintage
# 
# Generally, since ISIC chapters are very broad, small differences in 
# classification between vintages should not influence the analysis since these
# become irrelevant when aggregating

install.packages("remotes")
remotes::install_github("insongkim/concordance")
# 
# Steven Liao, In Song Kim, Sayumi Miyano, Hao Zhang (2020). concordance: Product Concordance. 
# R package version 2.0.0. https://CRAN.R-project.org/package=concordance
# 
# 
# 

# 3. ISIC Chapters ---------------------------------------------
# Download the ISIC chapters from UNstats and clean it. Not the most elegant code
# but only used once
if(F){
  ISIC <- read.table(row.names = NULL,header = T,quote = "\"",sep = "\t", 
                   file = "https://unstats.un.org/unsd/classifications/Econ/Download/In%20Text/ISIC_Rev_3_english_structure.txt")
  ISIC <- data.frame(t(apply(ISIC, 
                             1, 
                             FUN = function(x) unlist(str_split(x, 
                                                                pattern = 
                                                                  "          ")))))
  names(ISIC) <- c("code", "description")
  ISIC$chapter <- ifelse(grepl(pattern = "[A-Z]", ISIC$code), ISIC$code, NA)
  for(i in 1:nrow(ISIC)){
    if(is.na(ISIC$chapter[i])){
      ISIC$chapter[i] <- ISIC$chapter[i-1]
    }
  }
  ISIC$code <- substr(ISIC$code, 1,2)
  ISIC <- unique(ISIC[!grepl(pattern = "[A-Z]", ISIC$code), c("code", "chapter")])
  
  writexl::write_xlsx(ISIC, path = paste0(path.data.out, 
                                          "ISIC chapter codes.xlsx"))
}
