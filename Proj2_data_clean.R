library(dplyr)
library(tidytext)
library(ggplot2)
library(tidyr)
library(wordcloud2)
library(ggmap)

# Read data
listings_ST <- read.csv("listings_ST.csv")
listings_DS <- read.csv("listings_DS.csv")
# listings_DS_ST <- read.csv("listings_ds_st.csv")

# Delete uniqueid
listings_ST <- listings_ST %>% select(-uniqueid)
listings_DS <- listings_DS %>% select(-uniqueid)

# Move summary to Descriptions for NAs
listings_ST1 <- filter(listings_ST,!is.na(listings_ST[,6]))
listings_DS1 <- filter(listings_DS,!is.na(listings_DS[,6]))

listings_ST2 <- filter(listings_ST,is.na(listings_ST[,6]))
listings_DS2 <- filter(listings_DS,is.na(listings_DS[,6]))
listings_ST2$description <- listings_ST2$summary
listings_DS2$description <- listings_DS2$summary

listings_ST <- rbind(listings_ST1,listings_ST2)
listings_DS <- rbind(listings_DS1, listings_DS2)

# Move Location to address, make location into city/state
listings_ST$address <- listings_ST$location
listings_DS$address <- listings_DS$location

listings_ST$location <- listings_ST$location %>% str_extract("(\\w+.)+,.[A-Z]{2}")
listings_DS$location <- listings_DS$location %>% str_extract("(\\w+.)+,.[A-Z]{2}")

listings_ST$address <- listings_ST$address %>% str_extract("(\\w+.)+,.+")
listings_DS$address <- listings_DS$address %>% str_extract("(\\w+.)+,.+")

# write.csv(listings_ST,file="listings_ST2.csv",row.names = F)
# write.csv(listings_DS,file="listings_DS2.csv",row.names = F)

# Grab longtitude and latitude
listings_ST$lon <- NA
listings_ST$lat <- NA
for (i in 1:dim(listings_ST)[1]){
  if(!is.na(listings_ST$address[i])){
    LL <- geocode(listings_ST$address[i])
    listings_ST$lon[i] <- LL[1]
    listings_ST$lat[i] <- LL[2]
  }
}

listings_DS$lon <- NA
listings_DS$lat <- NA
for (i in 1:dim(listings_DS)[1]){
  if(!is.na(listings_DS$address[i])){
    LL <- geocode(listings_DS$address[i])
    listings_DS$lon[i] <- LL[1]
    listings_DS$lat[i] <- LL[2]
  }
}

# Unlist lon&lat
listings_ST$lon <- unlist(listings_ST$lon)
listings_ST$lat <- unlist(listings_ST$lat)

listings_DS$lon <- unlist(listings_DS$lon)
listings_DS$lat <- unlist(listings_DS$lat)

# Save data
saveRDS(listings_ST,"listings_ST2.rds")
saveRDS(listings_DS,"listings_DS2.rds")