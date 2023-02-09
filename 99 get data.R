# This script uses APIs to download the relevant data from several databases. 

library(httr)
library(jsonlite)
#install.packages("RSelenium")
library(RSelenium)
library(rvest)
library(xml2)
library(XML)
path.data.raw <- "data raw/"




# TRAINS (UNCTAD)
# As I TRAINS never replied to my email regarding access to the data, 
# I'm just going to scrape it

# start Selenium driver
url = "https://trainsonline.unctad.org/detailedSearch?imposingCountries=1,4,8,10,16,11,12,9,13,14,15,17,18,59,21,23,25,30,31,242,38,35,36,37,42,43,44,48,54,55,56,58,110,60,61,62,63,234,64,68,66,279,73,75,80,81,84,85,88,90,93,94,95,99,100,101,102,103,104,107,108,109,111,112,114,113,115,118,119,120,123,121,124,127,128,131,132,135,137,138,139,142,145,146,32,150,151,158,159,160,164,147,170,83,171,172,173,174,175,177,178,182,184,185,186,197,198,202,203,205,207,117,209,41,213,216,217,45,219,239,220,221,224,226,227,231,232,225,235,240,243,245,204,249,208,999&imposingCountriesGroupSelection=&imposingCountriesAll=true&internationalStandardsImposing=false&imposingCountriesGroup=0&affectedCountries=1,2,4,5,6,7,190,3,8,10,16,153,11,12,9,13,14,15,17,34,18,26,59,19,20,21,155,22,23,24,25,273,27,29,30,31,242,33,38,35,36,37,39,40,42,43,44,46,47,48,49,51,53,54,55,56,152,57,58,250,110,52,60,79,61,62,63,234,64,65,67,68,215,66,279,69,70,72,73,75,77,78,80,82,81,84,85,86,88,89,90,93,236,94,179,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,111,112,237,274,114,113,115,87,277,118,119,120,123,121,122,124,125,127,128,129,233,130,131,132,133,134,135,168,137,138,139,167,275,142,141,143,144,145,146,32,148,149,150,151,254,256,156,158,159,160,161,162,163,116,164,147,257,170,169,83,171,259,172,173,174,175,176,177,178,182,184,185,186,187,188,189,191,192,193,194,247,195,196,278,197,198,199,272,200,201,202,154,203,205,28,206,207,71,117,210,209,41,211,264,213,214,216,217,218,45,219,239,220,180,221,222,223,224,226,228,229,230,227,265,231,232,225,235,240,166,269,243,244,157,245,204,276,246,212,248,263,271,249,208,74,999&affectedCountriesGroupSelection=&affectedCountriesAll=true&affectedCountriesGroup=0&products=&productsAll=true&productsGroup=1&ntmTypes=A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P&allNtmTypes=true&fromDate=&toDate=&excludeHorizontal=false&import=true&export=true&multilateral=true&unilateral=true&pageNumber=1&columnsVisibility=%7B%22countryImposingNTMsVisible%22:true,%22affectedCountriesNamesVisible%22:true,%22ntmCodeVisible%22:true,%22measureDescriptionVisible%22:true,%22productDescriptionVisible%22:true,%22hsCodeVisible%22:true,%22issuingAgencyVisible%22:true,%22regulationTitleVisible%22:true,%22regulationSymbolVisible%22:true,%22implementationDateVisible%22:true,%22regulationFileVisible%22:true,%22regulationOfficialTitleOriginalVisible%22:true,%22measureDescriptionOriginalVisible%22:true,%22measureProductDescriptionOriginalVisible%22:true,%22supportingRegulationsVisible%22:true,%22measureObjectivesOriginalVisible%22:true,%22yearsOfDataCollectionVisible%22:true,%22repealDateVisible%22:true%7D"
sel <- rsDriver(browser = "firefox",
                chromever = NULL,
                port = 4570L)
remDr <- sel$client
remDr$open()

# go to URL: needs a manual adjustments on page as it somehow cannot load
# the table directly: Select one random product, load, and then select all products. This solves
# a bug that just shows a subset of products instead of all. 

remDr$navigate(url)
site <- remDr$getPageSource() #get whole page
pages <- site %>% .[[1]] %>% #get number of pages we need to scrape through
  read_html() %>% 
  html_elements(xpath = '/html/body/app-root/app-detailed-search/div/div/div/div/div[2]/div[3]/ngb-pagination/ul/li[9]/a') %>% #get num of pages
  html_text2()%>%
  as.numeric()

webElem <- remDr$findElement(using = 'xpath',"/html/body/app-root/app-detailed-search/div/div/div/div/div[2]/div[3]/select/option[2]")
webElem$clickElement() #set table to contain 50 rows (max)


# table <- remDr$findElement(using = "xpath", value = '/html/body/app-root/app-detailed-search/div/div/div/div/div[2]/ag-grid-angular/div/div[1]/div[2]/div[3]/div[2]')
# table$highlightElement() #check if right part of page selected




for(page in 1:pages){

  
  site <- remDr$getPageSource() #get whole page
  
  
  table <- site %>% .[[1]] %>% #get table
    read_html() %>% 
    html_elements('ag-grid-angular') 
  
  
  
  cols = 4
  UNCTAD <- data.frame(ncol = cols)
  
  for(i in 1:50){
    row <- table %>% html_elements(paste0("[row-index='",i,"']"))%>%
      .[[2]] %>%
      html_text2()
    
    row <- unlist(strsplit(row, split = "\n"))
    UNCTAD[i,1:cols] <- row
  
  }
}


remDr$close()

rD <- rsDriver(verbose = FALSE,port=4568L)
remDr <- rD$client
remDr$close()
rD$server$stop()

remDr$executeScript
remDr$getEval


url <- xml2::read_html(url)
test <- rvest::html_table(url)[[1]] %>% 
  tibble::as_tibble(.name_repair = "unique")

res <- GET("https://api-trains2.unctad.org/denormalisedMeasuresCount")

library(rjson)
url <- "https://trainsonline.unctad.org/detailedSearch?imposingCountries=1,4,8,10,16,11,12,9,13,14,15,17,18,59,21,23,25,30,31,242,38,35,36,37,42,43,44,48,54,55,56,58,110,60,61,62,63,234,64,68,66,279,73,75,80,81,84,85,88,90,93,94,95,99,100,101,102,103,104,107,108,109,111,112,114,113,115,118,119,120,123,121,124,127,128,131,132,135,137,138,139,142,145,146,32,150,151,158,159,160,164,147,170,83,171,172,173,174,175,177,178,182,184,185,186,197,198,202,203,205,207,117,209,41,213,216,217,45,219,239,220,221,224,226,227,231,232,225,235,240,243,245,204,249,208&imposingCountriesGroupSelection=&imposingCountriesAll=true&internationalStandardsImposing=false&imposingCountriesGroup=0&affectedCountries=1,2,4,5,6,7,190,3,8,10,16,153,11,12,9,13,14,15,17,34,18,26,59,19,20,21,155,22,23,24,25,273,27,29,30,31,242,33,38,35,36,37,39,40,42,43,44,46,47,48,49,51,53,54,55,56,152,57,58,250,110,52,60,79,61,62,63,234,64,65,67,68,215,66,279,69,70,72,73,75,77,78,80,82,81,84,85,86,88,89,90,93,236,94,179,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,111,112,237,274,114,113,115,87,277,118,119,120,123,121,122,124,125,127,128,129,233,130,131,132,133,134,135,168,137,138,139,167,275,142,141,143,144,145,146,32,148,149,150,151,254,256,156,158,159,160,161,162,163,116,164,147,257,170,169,83,171,259,172,173,174,175,176,177,178,182,184,185,186,187,188,189,191,192,193,194,247,195,196,278,197,198,199,272,200,201,202,154,203,205,28,206,207,71,117,210,209,41,211,264,213,214,216,217,218,45,219,239,220,180,221,222,223,224,226,228,229,230,227,265,231,232,225,235,240,166,269,243,244,157,245,204,276,246,212,248,263,271,249,208,74&affectedCountriesGroupSelection=&affectedCountriesAll=true&affectedCountriesGroup=0&products=&productsAll=true&productsGroup=1&ntmTypes=A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P&allNtmTypes=true&fromDate=&toDate=&import=true&export=true&multilateral=true&unilateral=true&pageNumber=1&columnsVisibility=%7B%22countryImposingNTMsVisible%22:true,%22affectedCountriesNamesVisible%22:true,%22ntmCodeVisible%22:true,%22measureDescriptionVisible%22:true,%22productDescriptionVisible%22:false,%22hsCodeVisible%22:true,%22issuingAgencyVisible%22:false,%22regulationTitleVisible%22:true,%22regulationSymbolVisible%22:false,%22implementationDateVisible%22:true,%22regulationFileVisible%22:true,%22regulationOfficialTitleOriginalVisible%22:false,%22measureDescriptionOriginalVisible%22:false,%22measureProductDescriptionOriginalVisible%22:false,%22supportingRegulationsVisible%22:false,%22measureObjectivesOriginalVisible%22:false,%22yearsOfDataCollectionVisible%22:false,%22repealDateVisible%22:false%7D"
text <- content(GET(url),type="text")
text <- sub("var statesData = ","",text)
text <- sub(";$","",text)
json <- fromJSON(text)
tab <- read_html(text)


r <- GET("https://api-trains2.unctad.org/denormalisedMeasures")
r
