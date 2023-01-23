# This script uses APIs to download the relevant data from several databases. 

library(httr)
library(jsonlite)


path.data.raw <- "data raw/"




# TRAINS (UNCTAD)
meta.url <- "http://wits.worldbank.org/API/V1/wits/datasource/trn/dataavailability/"
url      <- "http://wits.worldbank.org/API/V1"

availability <- GET(meta.url)
