# Author: Silvan Hofer
# Date: 10. February 2023
# Purpose:  
# This script scrapes and downloads data for analysis if a simpler way 
# was not possible.  
# Please note: The scraper does not work properly because of some peculiarities of the 
# TRAINS site (e.g. after Taiwan is selected in the dropdown, so is automatically China).
# Therefore, the TRAINS was downloaded by hand partially. 

# library(httr)
# library(jsonlite)
# library(RSelenium)
# library(rvest)
# library(xml2)
# library(XML)
library(tidyr)
library(dplyr)
library(gtalibrary)
library(stringr)
library(splitstackshape)

rm(list = ls())

path.data.raw <- "0 data raw/"
path.data.out <- "1 data processed/"

# TRAINS (UNCTAD) --------------------------------------------------------------
## 1. convert UNCTAD to GTA names --------------------------------------------------
UNCTAD.countries <- "Afghanistan
Algeria
Antigua and Barbuda
Argentina
Armenia
Australia
Austria
Azerbaijan
Bahamas
Bahrain
Bangladesh
Barbados
Belgium
Benin
Bolivia (Plurinational State of)
Botswana
Brazil
Brunei Darussalam
Bulgaria
Burkina Faso
Cabo Verde
Cambodia
Cameroon
Canada
Chad
Chile
China
Colombia
Comoros
Costa Rica
Croatia
Cuba
Czech Republic
Côte d'Ivoire
Denmark
Dominica
Dominican Republic
Ecuador
Egypt
El Salvador
Estonia
Ethiopia
European Union
Finland
France
Gabon
Georgia
Germany
Ghana
Greece
Grenada
Guatemala
Guinea
Guyana
Honduras
China, Hong Kong Special Administrative Region
Hungary
Iceland
India
Indonesia
Ireland
Israel
Italy
Jamaica
Japan
Jordan
Kazakhstan
Kenya
Kuwait
Kyrgyz Republic
Lao People's Democratic Republic
Latvia
Lebanon
Liberia
Lithuania
Luxembourg
Malawi
Malaysia
Malta
Mauritania
Mauritius
Mexico
Republic of Moldova
Morocco
Mozambique
Myanmar
Nepal
Netherlands
New Zealand
Nicaragua
Niger
Norway
Oman
Pakistan
State of Palestine
Panama
Papua New Guinea
Paraguay
Peru
Philippines
Poland
Portugal
Qatar
Romania
Russian Federation
Rwanda
Saudi Arabia
Senegal
Singapore
Slovakia
Slovenia
South Africa
Korea, Republic of
Spain
Sri Lanka
Suriname
Sweden
Switzerland
China, Taiwan Province of
Tajikistan
United Republic of Tanzania
Thailand
Togo
Trinidad and Tobago
Tunisia
Republic of Türkiye
Uganda
Ukraine
United Arab Emirates
United Kingdom of Great Britain and Northern Ireland
United States of America
Uruguay
Venezuela (Bolivarian Republic of)
Viet Nam
Zambia
Zimbabwe"

UNCTAD.countries <- data.frame("UNCTAD.name" = strsplit(UNCTAD.countries, "\n")[[1]],
                               "GTA.name" = strsplit(UNCTAD.countries, "\n")[[1]])

if(T){
  UNCTAD.countries$GTA.name[UNCTAD.countries$GTA.name == "Antigua and Barbuda"] <- "Antigua & Barbuda"
  UNCTAD.countries$GTA.name[UNCTAD.countries$GTA.name == "Bolivia (Plurinational State of)"] <- "Bolivia"
  UNCTAD.countries$GTA.name[UNCTAD.countries$GTA.name == "Bosnia and Herzegovina"] <- "Bosnia & Herzegovina"
  UNCTAD.countries$GTA.name[UNCTAD.countries$GTA.name == "Cabo Verde"] <- "Cape Verde"
  UNCTAD.countries$GTA.name[UNCTAD.countries$GTA.name == "China, Taiwan Province of"] <- "Chinese Taipei"
  UNCTAD.countries$GTA.name[UNCTAD.countries$GTA.name == "China, Hong Kong Special Administrative Region"] <- "Hong Kong"
  UNCTAD.countries$GTA.name[UNCTAD.countries$GTA.name == "China, Macao Special Administrative Region"] <- "Macao"
  UNCTAD.countries$GTA.name[UNCTAD.countries$GTA.name == "Côte d'Ivoire"] <- "Ivory Coast"
  UNCTAD.countries$GTA.name[UNCTAD.countries$GTA.name == "Czech Republic"] <- "Czechia"
  UNCTAD.countries$GTA.name[UNCTAD.countries$GTA.name == "Democratic Republic of the Congo"] <- "DR Congo"
  UNCTAD.countries$GTA.name[UNCTAD.countries$GTA.name == "Korea, Democratic People's Republic of"] <- "DPR Korea"
  UNCTAD.countries$GTA.name[UNCTAD.countries$GTA.name == "Korea, Republic of"] <- "Republic of Korea"
  UNCTAD.countries$GTA.name[UNCTAD.countries$GTA.name == "Kingdom of Eswatini"] <- "Eswatini"
  UNCTAD.countries$GTA.name[UNCTAD.countries$GTA.name == "Falkland Islands (Malvinas)"] <- "Falkland Islands"
  UNCTAD.countries$GTA.name[UNCTAD.countries$GTA.name == "Holy See"] <- "Vatican"
  UNCTAD.countries$GTA.name[UNCTAD.countries$GTA.name == "Iran (Islamic Republic of)"] <- "Iran"
  UNCTAD.countries$GTA.name[UNCTAD.countries$GTA.name == "Kyrgyz Republic"] <- "Kyrgyzstan"
  UNCTAD.countries$GTA.name[UNCTAD.countries$GTA.name == "Lao People's Democratic Republic"] <- "Lao"
  UNCTAD.countries$GTA.name[UNCTAD.countries$GTA.name == "Micronesia (Federated States of)"] <- "Micronesia"
  UNCTAD.countries$GTA.name[UNCTAD.countries$GTA.name == "Republic of Türkiye"] <- "Turkey"
  UNCTAD.countries$GTA.name[UNCTAD.countries$GTA.name == "Russian Federation"] <- "Russia"
  UNCTAD.countries$GTA.name[UNCTAD.countries$GTA.name == "Saint Barthélemy"] <- "Saint-Barthelemy"
  UNCTAD.countries$GTA.name[UNCTAD.countries$GTA.name == "Saint Helena, Ascension and Tristan da Cunha"] <- "Saint Helena"
  UNCTAD.countries$GTA.name[UNCTAD.countries$GTA.name == "Saint Kitts and Nevis"] <- "Saint Kitts & Nevis"
  UNCTAD.countries$GTA.name[UNCTAD.countries$GTA.name == "Saint Pierre and Miquelon"] <- "Saint Pierre & Miquelon"
  UNCTAD.countries$GTA.name[UNCTAD.countries$GTA.name == "Saint Vincent & the Grenadines"] <- "Saint Vincent and the Grenadines"
  UNCTAD.countries$GTA.name[UNCTAD.countries$GTA.name == "Sao Tome and Principe"] <- "Sao Tome & Principe"
  UNCTAD.countries$GTA.name[UNCTAD.countries$GTA.name == "State of Libya"] <- "Libya"
  UNCTAD.countries$GTA.name[UNCTAD.countries$GTA.name == "Sudan"] <- "Republic of the Sudan"
  UNCTAD.countries$GTA.name[UNCTAD.countries$GTA.name == "Svalbard and Jan Mayen Islands"] <- "Svalbard & Jan Mayen Islands"
  UNCTAD.countries$GTA.name[UNCTAD.countries$GTA.name == "Syrian Arab Republic"] <- "Syria"
  UNCTAD.countries$GTA.name[UNCTAD.countries$GTA.name == "Trinidad and Tobago"] <- "Trinidad & Tobago"
  UNCTAD.countries$GTA.name[UNCTAD.countries$GTA.name == "Turks and Caicos Islands"] <- "Turks & Caicos Islands"
  UNCTAD.countries$GTA.name[UNCTAD.countries$GTA.name == "United Kingdom of Great Britain and Northern Ireland"] <- "United Kingdom"
  UNCTAD.countries$GTA.name[UNCTAD.countries$GTA.name == "United Republic of Tanzania"] <- "Tanzania"
  UNCTAD.countries$GTA.name[UNCTAD.countries$GTA.name == "Venezuela (Bolivarian Republic of)"] <- "Venezuela"
  UNCTAD.countries$GTA.name[UNCTAD.countries$GTA.name == "Viet Nam"] <- "Vietnam"
  UNCTAD.countries$GTA.name[UNCTAD.countries$GTA.name == "Wallis and Futuna Islands"] <- "Wallis & Futuna Islands"
  UNCTAD.countries$GTA.name[UNCTAD.countries$GTA.name == "Saint Vincent and the Grenadines"] <- "Saint Vincent & the Grenadines"
  UNCTAD.countries$GTA.name[UNCTAD.countries$GTA.name == "Netherlands Antilles"]
  
}

UNCTAD.countries.conversion <- merge(UNCTAD.countries, country.names, by.x = "GTA.name", by.y = "name", all.x = T)
UNCTAD.countries.conversion <- UNCTAD.countries.conversion[!is.na(UNCTAD.countries.conversion$un_code), c("GTA.name", "UNCTAD.name", "un_code")]
UNCTAD.countries.conversion <- rbind(UNCTAD.countries.conversion, c("EU", "European Union", NA))


writexl::write_xlsx(UNCTAD.countries.conversion, path = paste0(path.data.out, 
                                                               "UNCTAD_GTA_conversion.table.xlsx"))

rm(UNCTAD.countries)

## 2. Try selenium -------------------------------------------------------------
# Selenium is quite unstable. Also, the website makes things complicated to scrape to the point 
# where it is faster (and safer) to do it manually. As I only do it once, that should not pose an issue. 
# Within the if(F){} statement are different attempts at automating it. At some point I had to cut my losses.
# Downloading manually takes like 2 hours. Way less than automating it...

  #if(F){
  # url = "https://trainsonline.unctad.org/detailedSearch?imposingCountries=1,4,8,10,16,11,12,9,13,14,15,17,18,59,21,23,25,30,31,242,38,35,36,37,42,43,44,48,54,55,56,58,110,60,61,62,63,234,64,68,66,279,73,75,80,81,84,85,88,90,93,94,95,99,100,101,102,103,104,107,108,109,111,112,114,113,115,118,119,120,123,121,124,127,128,131,132,135,137,138,139,142,145,146,32,150,151,158,159,160,164,147,170,83,171,172,173,174,175,177,178,182,184,185,186,197,198,202,203,205,207,117,209,41,213,216,217,45,219,239,220,221,224,226,227,231,232,225,235,240,243,245,204,249,208,999&imposingCountriesGroupSelection=&imposingCountriesAll=true&internationalStandardsImposing=false&imposingCountriesGroup=0&affectedCountries=1,2,4,5,6,7,190,3,8,10,16,153,11,12,9,13,14,15,17,34,18,26,59,19,20,21,155,22,23,24,25,273,27,29,30,31,242,33,38,35,36,37,39,40,42,43,44,46,47,48,49,51,53,54,55,56,152,57,58,250,110,52,60,79,61,62,63,234,64,65,67,68,215,66,279,69,70,72,73,75,77,78,80,82,81,84,85,86,88,89,90,93,236,94,179,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,111,112,237,274,114,113,115,87,277,118,119,120,123,121,122,124,125,127,128,129,233,130,131,132,133,134,135,168,137,138,139,167,275,142,141,143,144,145,146,32,148,149,150,151,254,256,156,158,159,160,161,162,163,116,164,147,257,170,169,83,171,259,172,173,174,175,176,177,178,182,184,185,186,187,188,189,191,192,193,194,247,195,196,278,197,198,199,272,200,201,202,154,203,205,28,206,207,71,117,210,209,41,211,264,213,214,216,217,218,45,219,239,220,180,221,222,223,224,226,228,229,230,227,265,231,232,225,235,240,166,269,243,244,157,245,204,276,246,212,248,263,271,249,208,74,999&affectedCountriesGroupSelection=&affectedCountriesAll=true&affectedCountriesGroup=0&products=&productsAll=true&productsGroup=1&ntmTypes=A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P&allNtmTypes=true&fromDate=&toDate=&excludeHorizontal=false&import=true&export=true&multilateral=true&unilateral=true&pageNumber=1&columnsVisibility=%7B%22countryImposingNTMsVisible%22:true,%22affectedCountriesNamesVisible%22:true,%22ntmCodeVisible%22:true,%22measureDescriptionVisible%22:true,%22productDescriptionVisible%22:true,%22hsCodeVisible%22:true,%22issuingAgencyVisible%22:true,%22regulationTitleVisible%22:true,%22regulationSymbolVisible%22:true,%22implementationDateVisible%22:true,%22regulationFileVisible%22:true,%22regulationOfficialTitleOriginalVisible%22:true,%22measureDescriptionOriginalVisible%22:true,%22measureProductDescriptionOriginalVisible%22:true,%22supportingRegulationsVisible%22:true,%22measureObjectivesOriginalVisible%22:true,%22yearsOfDataCollectionVisible%22:true,%22repealDateVisible%22:true%7D"
  # port <- 4575L
  # sel <- rsDriver(browser = "firefox",
  #                 chromever = NULL,
  #                 port = port)
  # remDr <- sel$client
  # #remDr$open()
  # 
  # # go to URL: needs a manual adjustments on page as it somehow cannot load
  # # the table directly: Select one random product, load, and then select all products. This solves
  # # a bug that just shows a subset of products instead of all. 
  # 
  # 
  # remDr$navigate(url)
  # site <- remDr$getPageSource() #get whole page
  # 
  # webElem <- remDr$findElement(using = 'xpath',"/html/body/app-root/app-detailed-search/div/div/div/div/div[2]/div[3]/select/option[2]")
  # webElem$clickElement() #set table to contain 50 rows (max)
  # 
  # 
  # pages <- site %>% .[[1]] %>% #get number of pages we need to scrape through
  #   read_html() %>% 
  #   html_elements(xpath = '/html/body/app-root/app-detailed-search/div/div/div/div/div[2]/div[3]/ngb-pagination/ul/li[9]') %>% #get num of pages
  #   html_text2()%>%
  #   as.numeric()
  # 
  # rows <- site %>% .[[1]] %>% #get number of rows we need to scrape per page
  #   read_html() %>% 
  #   html_elements(xpath = '/html/body/app-root/app-detailed-search/div/div/div/div/div[2]/div[3]/select/option[2]') %>% #get num of pages
  #   html_text2() 
  # rows <- as.numeric(gsub("[a-zA-Z]", "", rows))
  # rows = 50
  # 
  # # IMPORTANT: Scroll out to make whole table visible (not most elegant way
  # # but only needs to be done once or twice)
  # columns <- c("countryImposingNTMs", "affectedCountriesNames","ntmCode","hsCode",
  #              "regulationTitle","productDescription", "issuingAgency",   
  #              "regulationSymbol", "implementationDate",  
  #              "repealDate", "regulationFile", "measureDescription", "row", "page")
  # UNCTAD <- data.frame(matrix(ncol = length(columns)))
  # names(UNCTAD) <- columns
  # 
  # 
  # start <- Sys.time()
  # for(page in 1:3){ #takes a few hours
  # 
  #   site <- remDr$getPageSource() #get whole page
  #   
  #   webElem <- remDr$findElement(using = 'css selector','a[aria-label="Next"]')
  #   webElem$clickElement() #go to next page, the site takes a few seconds to load, so we already move while still going through old page
  #   
  #   table <- site %>% .[[1]] %>% #get table
  #     read_html() %>% 
  #     html_elements('ag-grid-angular') 
  #   
  #   for(i in 0:(rows-1)){
  #     
  #     # go through rows and grab data, not too efficient but only needs to be done once or twice
  #     row.data <- c()
  #     row <- table %>% html_elements(paste0("[row-index='",i,"']"))%>%
  #       .[[2]]
  #     
  #     for(col in 1:(length(columns)-2)){ # go trough columns
  #     
  #       cell <- row %>% html_elements(paste0("[col-id='",columns[col],"']")) %>%
  #         html_text2() 
  #       row.data <- c(row.data, cell)
  #     }
  #     
  #     row.data <- c(row.data, i, page)
  #     UNCTAD <- rbind(UNCTAD,row.data)
  #     
  #     if(i == 0){ #grab first row for comparison
  #       current.row <- row.data[1:(length(columns)-2)]
  #     }
  # 
  #   }
  # 
  #   loaded.row <- current.row
  #   sleeps <- 0
  #   
  #   while(all(current.row == loaded.row)){ #check if we are still at old page, if not, jump to next one
  #     
  #     site <- remDr$getPageSource()
  #     table <- site %>% .[[1]] %>% #get table
  #       read_html() %>% 
  #       html_elements('ag-grid-angular') 
  #     
  #     
  #     row.data <- c()
  #     row <- table %>% html_elements(paste0("[row-index='",0,"']"))%>%
  #       .[[2]]
  #     
  #     for(col in 1:(length(columns)-2)){
  #       cell <- row %>% html_elements(paste0("[col-id='",columns[col],"']")) %>%
  #         html_text2() 
  #       row.data <- c(row.data, cell)
  #     }
  #     
  #     loaded.row <- row.data
  #     sleeps <- sleeps + 1
  #     
  #     if(sleeps %% 3 == 0){
  #       print(loaded.row[1])
  #       print(paste0("sleeping..."))
  #       print(sleeps)
  #     }
  #     Sys.sleep(2)
  #     
  #   }
  # }
  # end <- Sys.time()
  # end-start
  # UNCTAD <- unique(UNCTAD)
  # 
  # 
  # # 1. autodownload data
  # un.codes <- readxl::read_xlsx(path = paste0(path.data.out, "UNCTAD_GTA_conversion.table.xlsx"))
  # un.codes <- un.codes[!is.na(un.codes$UNCTAD.name), ]
  # un.codes <- un.codes[order(un.codes$UNCTAD.name), ]
  # 
  # un.codes <- un.codes$un_code
  # un.codes <- ifelse(nchar(un.codes) == 1, 
  #                    paste0("00", un.codes), 
  #                    ifelse(nchar(un.codes) == 2, paste0("0", un.codes), un.codes))
  # t$un_code <- ifelse(nchar(t$un_code) == 1, 
  #                    paste0("00", t$un_code), 
  #                    ifelse(nchar(t$un_code) == 2, paste0("0", t$un_code), t$un_code))
  # un.codes
  # 
  # 
  # for(i in 19:40){
  #   
  #   open.selection <- remDr$findElement(using = 'xpath','/html/body/app-root/app-detailed-search/div/div/div/div/div[1]/div[1]/div/app-country-groups-selection/div/div/div/div/span[1]')
  #   open.selection$clickElement() #go to next page, the site takes a few seconds to load, so we already move while still going through old page
  #   
  #   tryCatch({
  #     deselect.country <- remDr$findElement(using = 'css selector',paste0('[value="chkCountry_',un.codes[i-1],'"]'))
  #     deselect.country$clickElement() #go to next page, the site takes a few seconds to load, so we already move while still going through old page
  #   }, error=function(e){})
  #   Sys.sleep(0.5)
  #   tryCatch({
  #     select.country <- remDr$findElement(using = 'css selector',paste0('[value="chkCountry_',un.codes[i],'"]'))
  #     select.country$clickElement() #go to next page, the site takes a few seconds to load, so we already move while still going through old page
  #   }, error=function(e){})
  #   Sys.sleep(0.5)
  #   tryCatch({
  #     close.selection <- remDr$findElement(using = 'xpath','/html/body/app-root/app-detailed-search/div/div/div/div/div[1]/div[1]/div/app-country-groups-selection/div/div[1]/div/div/span[2]')
  #     close.selection$clickElement() #go to next page, the site takes a few seconds to load, so we already move while still going through old page
  #   }, error=function(e){})
  #   Sys.sleep(0.5)
  #   tryCatch({
  #     execute.search <- remDr$findElement(using = 'xpath','/html/body/app-root/app-detailed-search/div/div/div/div/div[1]/div[9]/div/button[1]')
  #     execute.search$clickElement() #go to next page, the site takes a few seconds to load, so we already move while still going through old page
  #   }, error=function(e){})
  #   
  #   
  #   old.country <- as.character(t[t$un_code ==  un.codes[i-1], "UNCTAD.name"])
  #   actual.country <- site %>% .[[1]] %>% #get table
  #     read_html() %>%
  #     html_elements('ag-grid-angular') %>%
  #     html_elements(paste0("[row-index='",0,"']"))%>%
  #     .[[2]] %>%
  #     html_elements(paste0("[col-id='","countryImposingNTMs","']")) %>%
  #     html_text2()
  # 
  # 
  #   while(old.country == actual.country){
  #     Sys.sleep(4)
  #     site <- remDr$getPageSource()
  #     actual.country <- site %>% .[[1]] %>% #get table
  #       read_html() %>%
  #       html_elements('ag-grid-angular') %>%
  #       html_elements(paste0("[row-index='",0,"']"))%>%
  #       .[[2]] %>%
  #       html_elements(paste0("[col-id='","countryImposingNTMs","']")) %>%
  #       html_text2()
  #   }
  #    
  #   tryCatch({
  #     download <- remDr$findElement(using = 'xpath','/html/body/app-root/app-detailed-search/div/div/div/div/div[2]/div[1]/button[3]')
  #     download$clickElement() #go to next page, the site takes a few seconds to load, so we already move while still going through old page
  #   }, error=function(e){})
  #   Sys.sleep(4)
  #   
  # }
  # 
  # 
  # 
  # 
  # saveRDS(UNCTAD, file = paste0(path.data.raw, "TRAINS_database.RData"))
  # write.csv(UNCTAD, file = paste0(path.data.raw, "TRAINS_database.csv"))
  # test <- test[,3:ncol(test)]
  # test <- unique(test)
  # # close Selenium and close port
  # remDr$close()
  # system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
  # 
  # rD <- rsDriver(verbose = FALSE,port=port)
  # remDr <- rD$client
  # remDr$close()
  # rD$server$stop()
  # 
  # 
  # 
  # 
  # url.start <- "https://trainsonline.unctad.org/detailedSearch?imposingCountries="
  # url.end <- "&imposingCountriesGroupSelection=&imposingCountriesAll=false&internationalStandardsImposing=false&imposingCountriesGroup=0&affectedCountries=1,2,4,5,6,7,190,3,8,10,16,153,11,12,9,13,14,15,17,34,18,26,59,19,20,21,155,22,23,24,25,273,27,29,30,31,242,33,38,35,36,37,39,40,42,43,44,46,47,48,49,51,53,54,55,56,152,57,58,250,110,52,60,79,61,62,63,234,64,65,67,68,215,66,279,69,70,72,73,75,77,78,80,82,81,84,85,86,88,89,90,93,236,94,179,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,111,112,237,274,114,113,115,87,277,118,119,120,123,121,122,124,125,127,128,129,233,130,131,132,133,134,135,168,137,138,139,167,275,142,141,143,144,145,146,32,148,149,150,151,254,256,156,158,159,160,161,162,163,116,164,147,257,170,169,83,171,259,172,173,174,175,176,177,178,182,184,185,186,187,188,189,191,192,193,194,247,195,196,278,197,198,199,272,200,201,202,154,203,205,28,206,207,71,117,210,209,41,211,264,213,214,216,217,218,45,219,239,220,180,221,222,223,224,226,228,229,230,227,265,231,232,225,235,240,166,269,243,244,157,245,204,276,246,212,248,263,271,249,208,74,999&affectedCountriesGroupSelection=&affectedCountriesAll=true&affectedCountriesGroup=0&products=&productsAll=true&productsGroup=1&ntmTypes=A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P&allNtmTypes=true&fromDate=&toDate=&excludeHorizontal=false&import=true&export=true&multilateral=true&unilateral=true&pageNumber=1&columnsVisibility=%7B%22countryImposingNTMsVisible%22:true,%22affectedCountriesNamesVisible%22:true,%22ntmCodeVisible%22:true,%22measureDescriptionVisible%22:true,%22productDescriptionVisible%22:true,%22hsCodeVisible%22:true,%22issuingAgencyVisible%22:true,%22regulationTitleVisible%22:true,%22regulationSymbolVisible%22:true,%22implementationDateVisible%22:true,%22regulationFileVisible%22:true,%22regulationOfficialTitleOriginalVisible%22:true,%22measureDescriptionOriginalVisible%22:true,%22measureProductDescriptionOriginalVisible%22:true,%22supportingRegulationsVisible%22:true,%22measureObjectivesOriginalVisible%22:true,%22yearsOfDataCollectionVisible%22:true,%22repealDateVisible%22:true%7D"
  # 
  # url <- paste0(url.start,un.codes[i] ,url.end)
  # remDr$navigate(url)
  # site <- remDr$getPageSource() #get whole page
  # 
  # for(i in 1:length(un.codes)){
  #   
  #   
  #   open.selection <- remDr$findElement(using = 'xpath','/html/body/app-root/app-detailed-search/div/div/div/div/div[1]/div[1]/div/app-country-groups-selection/div/div/div/div/span[1]')
  #   open.selection$clickElement() #go to next page, the site takes a few seconds to load, so we already move while still going through old page
  #   
  #   tryCatch({
  #     deselect.country <- remDr$findElement(using = 'css selector',paste0('[value="chkCountry_',un.codes[i-1],'"]'))
  #     deselect.country$clickElement() #go to next page, the site takes a few seconds to load, so we already move while still going through old page
  #   }, error=function(e){})
  #   
  #   tryCatch({
  #     select.country <- remDr$findElement(using = 'css selector',paste0('[value="chkCountry_',un.codes[i],'"]'))
  #     select.country$clickElement() #go to next page, the site takes a few seconds to load, so we already move while still going through old page
  #   }, error=function(e){})
  #   
  #   tryCatch({
  #     close.selection <- remDr$findElement(using = 'xpath','/html/body/app-root/app-detailed-search/div/div/div/div/div[1]/div[1]/div/app-country-groups-selection/div/div[1]/div/div/span[2]')
  #     close.selection$clickElement() #go to next page, the site takes a few seconds to load, so we already move while still going through old page
  #   }, error=function(e){})
  #   
  #   tryCatch({
  #     execute.search <- remDr$findElement(using = 'xpath','/html/body/app-root/app-detailed-search/div/div/div/div/div[1]/div[9]/div/button[1]')
  #     execute.search$clickElement() #go to next page, the site takes a few seconds to load, so we already move while still going through old page
  #   }, error=function(e){})
  #   
  #   tryCatch({
  #     download <- remDr$findElement(using = 'xpath','/html/body/app-root/app-detailed-search/div/div/div/div/div[2]/div[1]/button[3]')
  #     download$clickElement() #go to next page, the site takes a few seconds to load, so we already move while still going through old page
  #   }, error=function(e){})
  #   
  #   
  #   
  #   
  #   Sys.sleep(8)
  # }
  #}

## 3. Manual approach  --------------------------------------------------------
### 3.1 Load data  ---------------------------------------------------------------


files <- list.files(path = paste0(path.data.raw,  #get all files in directory
                                  "UNCTAD_TRAINS_downloads"), 
                    all.files = T,
                    full.names = T)
files <- files[!files %in% c("0 data raw/UNCTAD_TRAINS_downloads/.",
                             "0 data raw/UNCTAD_TRAINS_downloads/..")]


TRAINS <- data.frame()

for(i in 1:length(files)){ # download all files from downloads folder
  
  data.loop <- readxl::read_xlsx(path = files[i],
                                 skip = 7)
  
  TRAINS <- rbind(TRAINS, data.loop)
}

TRAINS <- unique(TRAINS)
rm(data.loop, i, files)

write.csv(TRAINS, file = paste0(path.data.raw, "UNCTAD_TRAINS_database.csv"), row.names = F)

### 3.2 Check potential missing downloads  -------------------------------------

#check if all countries are downloaded
sum(unique(TRAINS$`Country imposing NTM(s)`) %in% UNCTAD.countries.conversion$UNCTAD.name)
missing.countries <- UNCTAD.countries.conversion$UNCTAD.name[!UNCTAD.countries.conversion$UNCTAD.name %in% unique(TRAINS$`Country imposing NTM(s)`)]
eu.countries <- c("Austria", "Belgium", "Denmark", "Finland", "France", "Germany", "Greece", "Ireland", "Italy", "Malta", "Luxembourg","Portugal", "Romania", "Spain", "Sweden", "Netherlands") # just have EU values if downloaded
missing.countries <- missing.countries[!missing.countries %in% eu.countries]


# check if all data got downloaded from countries where more than one download was needed
files <- list.files(path = paste0(path.data.raw,  #get all files in directory
                                  "UNCTAD_TRAINS_downloads"), 
                    all.files = T,
                    full.names = T)
files <- files[!files %in% c("0 data raw/UNCTAD_TRAINS_downloads/.",
                             "0 data raw/UNCTAD_TRAINS_downloads/..")]

download.info <- data.frame()
for(i in 1:length(files)){ # download all files from downloads folder
  
  data.loop <- readxl::read_xlsx(path = files[i],
                                 skip = 2, #just get columns with description
                                 n_max = 6,
                                 col_names = F)
  
  data.loop <- data.frame(t(data.loop))
  names(data.loop) <- as.character(unlist(data.loop[1,]))
  data.loop <- data.loop[-1,]
  
  download.info <- rbind(download.info, data.loop)
}

rm(data.loop, i, files)
row.names(download.info) <- 1:nrow(download.info)
names(download.info) <- c("measure.type", "implementing.country", "affected.country", "hs.code")

#check content of downloads using multiple files
download.info <- download.info[!(download.info$measure.type == "All" & #countries where more than one download was needed
                               download.info$affected.country == "All" &
                               download.info$hs.code == "All"),
                                 ]
checked.countries <- c("South Korea", "Indonesia","Brazil","Viet Nam") #only MAST chapter was varied
download.info <- download.info[!download.info$implementing.country %in% checked.countries,]

#download their content again separately to make sure everything is properly downloaded
checked.countries.2 <- c("Peru", "Panama", "New Zealand", "Thailand", "China", "United States of America")


# download.info$hs.code[download.info$hs.code == "All"] <- 999999 #set all to 999999 to not loose them when Csplitting
# download.info <- download.info %>% mutate(hs.code =  gsub("[^0-9,]", "", hs.code, ignore.case = T))
# download.info <- cSplit(download.info, "hs.code", direction = "long")
# download.info <- download.info %>% 
#   filter(!is.na(hs.code)) %>%
#   mutate(hs.code = as.numeric(substr(hs.code, 1,2)))%>%
#   unique()
#   
# test <- aggregate(data = download.info, hs.code ~ cbind(implementing.country) , FUN = function(x) paste0(x))
# hist(as.numeric(unique(test$hs.code[[1]])), breaks = 100)
# #hist(as.numeric(unique(test$hs.code[[2]])), breaks = 100)
# hist(as.numeric(unique(test$hs.code[[3]])), breaks = 100)
# hist(as.numeric(unique(test$hs.code[[4]])), breaks = 100)
# hist(as.numeric(unique(test$hs.code[[5]])), breaks = 100)
# hist(as.numeric(unique(test$hs.code[[6]])), breaks = 100)
# hist(as.numeric(unique(test$hs.code[[7]])), breaks = 100)


