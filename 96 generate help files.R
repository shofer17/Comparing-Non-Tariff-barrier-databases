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

# 3. HS to ISIC conversion -----------------------------------------------------
# write a file to convert 2 digit HS codes to ISIC
hs.2.dig <- as.character(1:99)
hs.2.dig <- ifelse(nchar(hs.2.dig) == 1, paste0(0, hs.2.dig), hs.2.dig)

hs.2.dig.to.isic <- data.frame("hs.code" = hs.2.dig,
                               "isic.code" = NA)
  
#loop to also catch cases where one hs code returns multiple isic
for(i in 1:nrow(hs.2.dig.to.isic)){
  
  codes <-  concord_hs_isic(hs.2.dig.to.isic$hs.code[i], #convert to ISIC
                            origin = "HS4", 
                            destination = "ISIC3", 
                            dest.digit = 2
                            )
  hs.2.dig.to.isic$isic.code[i] <- paste0(codes, collapse = ",")
  
}
  
hs.2.dig.to.isic <- hs.2.dig.to.isic[hs.2.dig.to.isic$isic.code != "NA", ] #chapters 77, 98, and 99 could not be matched
hs.2.dig.to.isic <- merge(hs.2.dig.to.isic, ISIC, by.x = "isic.code", by.y = "code", all.x = T, all.y = T)
hs.2.dig.to.isic <- hs.2.dig.to.isic[!is.na(hs.2.dig.to.isic$hs.code), ] #only needs codes for HS chapters

writexl::write_xlsx(hs.2.dig.to.isic, paste0(path.data.out, 
                                             "ISIC to HS 2 digits conversion.xlsx"))

# 4. Consistency of HS Chapters ------------------------------------------------
# Check if all 2 digit HS chapters have remaind constant over time. 

if(F){
  links <- data.frame("HS02" = "https://www.wcoomd.org/en/topics/nomenclature/instrument-and-tools/hs_nomenclature_previous_editions/hs_nomenclature_table_2002.aspx",
                      "HS07" = "https://www.wcoomd.org/en/topics/nomenclature/instrument-and-tools/hs_nomenclature_previous_editions/hs_nomenclature_table_2007.aspx",
                      "HS12" = c("https://www.wcoomd.org/en/topics/nomenclature/instrument-and-tools/hs_nomenclature_previous_editions/hs_nomenclature_table_2012.aspx"),
                      "HS17" = c("https://www.wcoomd.org/en/topics/nomenclature/instrument-and-tools/hs-nomenclature-2017-edition/hs-nomenclature-2017-edition.aspx"),
                      "HS22" = "https://www.wcoomd.org/en/topics/nomenclature/instrument-and-tools/hs-nomenclature-2022-edition/hs-nomenclature-2022-edition.aspx")
  
  hs.comparison <- data.frame("chapter" = 1:99)
  
  for(i in 1:ncol(links)){
    hs.tables <- links[1,i]
    hs.tables <- xml2::read_html(hs.tables)
    hs.tables <- rvest::html_table(hs.tables)[[2]] %>% 
      tibble::as_tibble(.name_repair = "unique")%>%
      filter(X1 !="")%>%
      select(-X3)
    names(hs.tables) <- c("chapter", paste0("description_",names(links)[i] ))
    
    hs.comparison <- merge(hs.comparison, hs.tables, by = "chapter", all.y = T)
  }
  writexl::write_xlsx(hs.comparison, path = paste0(path.data.out, 
                                          "HS Vintage comparison.xlsx"))
  hs.comparison$different_descriptions <- apply(hs.comparison, 1, FUN = function(x) length(unique(x)))-1
  hs.differences <- hs.comparison[ hs.comparison$different_descriptions >1, 2:(ncol(hs.comparison)-1)]
  test <- apply(hs.differences, 1, FUN = function(x) unique(x))
  #even remaining categories remain basically the same, just with minor adjustments.
  #Therefore, using the chapters for conversion to ISIC can be used without regard for the 
  #HS code version they are in. As I could not find the HS version the tmdb database used, 
  #I will assume two digit hs codes  given are equal or equivalent to two digit HS4 (2012) codes. 
}