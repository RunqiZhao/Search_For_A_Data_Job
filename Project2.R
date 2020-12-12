# Indeed job data list
# Data Scientist
# Statistican
# https://www.indeed.com/jobs?q=Statistician&start=0


library(rvest)
library(tidyverse)
library(data.table)
library(DT)
library(magrittr)
library(digest)
library(RPostgreSQL)
library(tidytext)
library(config)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(RCurl)
library(XML)
library(stringr)
library(zoo)

library(knitr)

searchlist <- function(listings_name,search_keyword,expected_numbers){
  for (i in seq(0, expected_numbers, 10)){
    url_ds <- paste0('https://www.indeed.com/jobs?q=',search_keyword,'&start=',i)
    var <- read_html(url_ds)
    
    #job title
    title <-  var %>% 
      html_nodes('#resultsCol .jobtitle') %>%
      html_text() %>%
      str_extract("(\\w+.+)+") 
    #company
    company <- var %>% 
      html_nodes('#resultsCol .company') %>%
      html_text() %>%
      str_extract("(\\w+).+") 
    #location
    location <- var %>%
      html_nodes('#resultsCol .location') %>%
      html_text() %>%
      # str_extract("(\\w+.)+,.[A-Z]{2}")
      str_extract("(\\w+.)+,.[A-Z]{2}(\\s)[0-9]{5}")
   
    #summary
    summary <- var %>%
      html_nodes('#resultsCol .summary') %>%
      html_text() %>%
      str_extract(".+")
    
    #link
    link <- var %>%
      html_nodes('#resultsCol .jobtitle .turnstileLink, #resultsCol a.jobtitle') %>%
      html_attr('href') 
    link <- paste0("https://www.indeed.com",link)
    
    listings_name <- rbind(listings_name, as.data.frame(cbind(title,
                                                          company,
                                                          location,
                                                          summary,
                                                          link)))
  }
  
  listings_name$uniqueid <- mapply(function(x, y, z) digest(paste0(x,y,z)), listings_name$title, listings_name$location, listings_name$company)
  #remove duplicate unique ids
  listings_name %<>%
    distinct(uniqueid, .keep_all = TRUE)
  #remove duplicate links
  listings_name %<>%
    distinct(link, .keep_all = TRUE)
  datatable(listings_name)
  
  for (i in (1:length(listings_name$link))){
    desciption <- tryCatch(
      html_text(html_node(read_html(as.character(listings_name$link[i])),'.jobsearch-JobComponent-description')),
      error=function(e){NA}
    )
    if (is.null(desciption)){
      desc <- NA
    }
    listings_name$description[i] <- desciption
  }
  
  return(listings_name)
}


search <- "data+scientist"
listings_DS <- data.frame(title=character(),
                          company=character(), 
                          location=character(), 
                          summary=character(), 
                          link=character(), 
                          description = character(),
                          stringsAsFactors=FALSE) 

listings_DS <- searchlist(listings_DS,search,500)

write.csv(listings_DS,file="listings_DS.csv",row.names = F)


search2 <- "statistician"
listings_ST <- data.frame(title=character(),
                          company=character(), 
                          location=character(), 
                          summary=character(), 
                          link=character(), 
                          description = character(),
                          stringsAsFactors=FALSE) 
listings_ST <- searchlist(listings_ST,search2,500)

write.csv(listings_ST,file="listings_ST.csv",row.names = F)
