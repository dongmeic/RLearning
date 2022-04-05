# Source - https://github.com/s-titoo/Geocoding-in-R

# install.packages("ggmap")
# install.packages("tmaptools")
# install.packages("RCurl")
# install.packages("jsonlite")
# install.packages("tidyverse")
# install.packages("leaflet")
# install.packages("rjson")

# load packages
library(ggmap)
library(tmaptools)
library(RCurl)
library(jsonlite)
library(tidyverse)
library(leaflet)
library(rjson)

# replace "api_key" with your API key
setwd("T:/DCProjects/GitHub/BikeCounting")
api_key <- fromJSON(file = "config/keys.json")$google_maps$APIKEY
register_google(key = api_key)

################################################ ggmap ###############################################

# create a list of London pubs

pubs <- c("The Angel, Bermondsey", 
          "The Churchill Arms, Notting Hill", 
          "The Auld Shillelagh, Stoke Newington", 
          "The Sekforde, Clerkenwell", 
          "The Dove, Hammersmith", 
          "The Crown and Sugar Loaf, Fleet Street", 
          "The Lamb, Holborn", 
          "Prince of Greenwich, Greenwich", 
          "Ye Olde Mitre, Hatton Garden", 
          "The Glory, Haggerston", 
          "The Blue Posts, Soho",
          "The Old Bank of England, Fleet Street")

pubs_df <- data.frame(Pubs = pubs, stringsAsFactors = FALSE)

# run the geocode function from ggmap package

pubs_ggmap <- geocode(location = pubs, output = "more", source = "google")
pubs_ggmap <- cbind(pubs_df, pubs_ggmap)

# print the results

pubs_ggmap[, 1:6]

# extract the coordinates of London pubs

pubs_ggmap_crd <- list()

for (i in 1:dim(pubs_ggmap)[1]) {
  
  lon <- pubs_ggmap$lon[i]
  lat <- pubs_ggmap$lat[i]
  pubs_ggmap_crd[[i]] <- c(lon, lat)
  
}

# reverse geocode the coordinates and save them to the list

pubs_ggmap_address <- list()

for (i in 1:length(pubs_ggmap_crd)) {
  
  pub <- pubs[i]
  crd <- pubs_ggmap_crd[[i]]
  address <- revgeocode(location = crd, output = "address")
  pubs_ggmap_address[[i]] <- list(pub, crd, address)
  
}

# print the details of the first pub

pubs_ggmap_address[[1]]
pubs_ggmap_address[[1]][[3]]

################################################ tmaptools ###############################################
# modifying some search requests
# need to adjust pubs_m

pubs_m <- pubs
pubs_m[pubs_m=="The Crown and Sugar Loaf, Fleet Street"] <- "The Crown and Sugar Loaf"
pubs_m[pubs_m=="Ye Olde Mitre, Hatton Garden"] <- "Ye Olde Mitre"
pubs_m_df <- data.frame(Pubs = pubs_m, stringsAsFactors = FALSE)

# geocoding the London pubs
# "bar" is special phrase added to limit the search

pubs_tmaptools <- geocode_OSM(paste(pubs_m, "bar", sep = " "),
                              details = TRUE, as.data.frame = TRUE)

# extracting from the result only coordinates and address

pubs_tmaptools <- pubs_tmaptools[, c("lat", "lon", "display_name")]
pubs_tmaptools <- cbind(Pubs = pubs_m_df[-10, ], pubs_tmaptools)

# print the results

pubs_tmaptools

# remove The Glory pub from the list ("s" for "short")

pubs_s <- pubs_m[!pubs_m %in% "The Glory, Haggerston"]

# extract the coordinates of London pubs

pubs_tmaptools_crd <- list()

for (i in 1:dim(pubs_tmaptools)[1]) {
  
  lon <- pubs_tmaptools$lon[i]
  lat <- pubs_tmaptools$lat[i]
  pubs_tmaptools_crd[[i]] <- c(lon, lat)
  
}

# reverse geocode the coordinates and save them to a list

pubs_tmaptools_address <- list()

for (i in 1:length(pubs_tmaptools_crd)) {
  
  pub <- pubs_s[i]
  crd <- pubs_tmaptools_crd[[i]]
  address <- rev_geocode_OSM(x = crd[1], y = crd[2],as.data.frame = TRUE)[, c("name")]
  pubs_tmaptools_address[[i]] <- list(pub, crd, address)
  
}

# print the details about the first pub

pubs_tmaptools_address[[1]]
pubs_tmaptools_address[[1]][[3]]

# /////////////////////////////
# let's create our inputs first
# /////////////////////////////

search_query_1_1 <- pubs[1]
search_query_1_2 <- pubs
search_query_1_3 <- as.list(pubs)
search_query_1_4 <- as.matrix(pubs)
search_query_1_5 <- as.data.frame(pubs, stringsAsFactors = FALSE)

search_query_2_1 <- 1
search_query_2_2 <- c(1,2,3)
search_query_2_3 <- list(1,2,3)
search_query_2_4 <- as.matrix(c(1,2,3))
search_query_2_5 <- cbind(as.matrix(pubs), as.matrix(pubs))
search_query_2_6 <- as.data.frame(c(1,2,3))
search_query_2_7 <- data.frame(one = pubs, two = pubs, stringsAsFactors = FALSE)

fields_1_1 <- "all";
fields_1_2 <- "coordinates"
fields_1_3 <- c("all")
fields_1_4 <- c("coordinates", "address", "contacts")
fields_1_5 <- c("all", "all", "all")
fields_1_6 <- c("coordinates", "address", "address", "address")
fields_1_7 <- list("all")
fields_1_8 <- list("coordinates", "address", "contacts")
fields_1_9 <- list("all", "all", "all")
fields_1_10 <- list("contacts", "address", "address", "address")

fields_2_1 <- 1
fields_2_2 <- c(1,2,3)
fields_2_3 <- list(1,2,3)
fields_2_4 <- c("alls")
fields_2_5 <- c("coordinate", "address")
fields_2_6 <- list("alls")
fields_2_7 <- list("address", "contact")
fields_2_8 <- c("all", "coordinates")
fields_2_9 <- c("coordinates", "address", "x", "y")
fields_2_10 <- list("all", "coordinates")
fields_2_11 <- list("coordinates", "x", "y", "z")

key_1_1 <- "xyz"

key_2_1 <- 1
key_2_2 <- c("x", "y", "z")
key_2_3 <- list("x", "y", "z")

# ////////////////////////////////////
# now let's run some tests for SUCCESS
# ////////////////////////////////////

sq_1_1 <- check_arguments(search_query_1_1, "address", "key")
sq_1_2 <- check_arguments(search_query_1_2, "address", "key")
sq_1_3 <- check_arguments(search_query_1_3, "address", "key")
sq_1_4 <- check_arguments(search_query_1_4, "address", "key")
sq_1_5 <- check_arguments(search_query_1_5, "address", "key")

f_1_1 <- check_arguments("search", fields_1_1, "key")
f_1_2 <- check_arguments("search", fields_1_2, "key")
f_1_3 <- check_arguments("search", fields_1_3, "key")
f_1_4 <- check_arguments("search", fields_1_4, "key")
f_1_5 <- check_arguments("search", fields_1_5, "key")
f_1_6 <- check_arguments("search", fields_1_6, "key")
f_1_7 <- check_arguments("search", fields_1_7, "key")
f_1_8 <- check_arguments("search", fields_1_8, "key")
f_1_9 <- check_arguments("search", fields_1_9, "key")
f_1_10 <- check_arguments("search", fields_1_10, "key")

k_1_1 <- check_arguments("search", "address", key_1_1)

# let's check if all the elements in the array below are FALSE

success <- c(sq_1_1[[1]], sq_1_2[[1]], sq_1_3[[1]], sq_1_4[[1]], sq_1_5[[1]],
             f_1_1[[1]], f_1_2[[1]], f_1_3[[1]], f_1_4[[1]], f_1_5[[1]],
             f_1_6[[1]], f_1_7[[1]], f_1_8[[1]], f_1_9[[1]], f_1_10[[1]],
             k_1_1[[1]])

if(length(unique(success)) != 1) {
  FALSE
} else if(unique(success) == TRUE) {
  FALSE
} else {
  TRUE
}

# ////////////////////////////////////
# now let's run some tests for FAILURE
# ////////////////////////////////////

sq_2_1 <- check_arguments(search_query_2_1, "address", "key")
sq_2_2 <- check_arguments(search_query_2_2, "address", "key")
sq_2_3 <- check_arguments(search_query_2_3, "address", "key")
sq_2_4 <- check_arguments(search_query_2_4, "address", "key")
sq_2_5 <- check_arguments(search_query_2_5, "address", "key")
sq_2_6 <- check_arguments(search_query_2_6, "address", "key")
sq_2_7 <- check_arguments(search_query_2_7, "address", "key")

f_2_1 <- check_arguments("search", fields_2_1, "key")
f_2_2 <- check_arguments("search", fields_2_2, "key")
f_2_3 <- check_arguments("search", fields_2_3, "key")
f_2_4 <- check_arguments("search", fields_2_4, "key")
f_2_5 <- check_arguments("search", fields_2_5, "key")
f_2_6 <- check_arguments("search", fields_2_6, "key")
f_2_7 <- check_arguments("search", fields_2_7, "key")
f_2_8 <- check_arguments("search", fields_2_8, "key")
f_2_9 <- check_arguments("search", fields_2_9, "key")
f_2_10 <- check_arguments("search", fields_2_10, "key")
f_2_11 <- check_arguments("search", fields_2_11, "key")

k_2_1 <- check_arguments("search", "address", key_2_1)
k_2_2 <- check_arguments("search", "address", key_2_2)
k_2_3 <- check_arguments("search", "address", key_2_3)

# let's check if all the elements in the array below are TRUE

failure <- c(sq_2_1[[1]], sq_2_2[[1]], sq_2_3[[1]], sq_2_4[[1]], sq_2_5[[1]],
             sq_2_6[[1]], sq_2_7[[1]], f_2_1[[1]], f_2_2[[1]], f_2_3[[1]], f_2_4[[1]],
             f_2_5[[1]], f_2_6[[1]], f_2_7[[1]], f_2_8[[1]], f_2_9[[1]], f_2_10[[1]],
             f_2_11[[1]], k_2_1[[1]], k_2_2[[1]], k_2_3[[1]])

all(failure)

# let's see if error messages are generated correctly

failure_sq <- unique(c(sq_2_1[[2]], sq_2_2[[2]], sq_2_3[[2]], sq_2_4[[2]], sq_2_5[[2]],
                       sq_2_6[[2]], sq_2_7[[2]]))
failure_f <- unique(c(f_2_1[[2]], f_2_2[[2]], f_2_3[[2]], f_2_4[[2]], f_2_5[[2]],
                      f_2_6[[2]], f_2_7[[2]], f_2_8[[2]], f_2_9[[2]], f_2_10[[2]], f_2_11[[2]]))
failure_k <- unique(c(k_2_1[[2]], k_2_2[[2]], k_2_3[[2]]))

messages <- list(failure_sq, failure_f, failure_k)

messages

# and finally let's see how several error messages are displayed at once

cat(check_arguments(c(1,2,3), "contact", 1)[[2]])

############################# Geocoding Using Google Maps API ##########################
source("T:/DCProjects/GitHub/RLearning/geocoding_functions.R")
# replace "api_key" with your API key
pubs_google <- geocode_google(pubs, "all", api_key)

# check results
pubs_google

# extract coordinates from pubs_google
pubs_google_crd <- pubs_google[ , c("lat", "lng")]

# replace "api_key" with your API key
pubs_rev_google <- rev_geocode_google(pubs_google_crd, api_key)

# check results
pubs_rev_google <- cbind(pubs_df, pubs_rev_google)
pubs_rev_google

############################# Geocoding Using Nominatim API ##########################
# replace "email" with your email address

email <- "dongmeijane1107@gmail.com"
pubs_nominatim <- geocode_nominatim(pubs, country = "gb", fields = "all", email = email)

# let's now see the results

pubs_nominatim[, c(1:4)]
pubs_nominatim[, c(1, 5:10)]
pubs_nominatim[, c(1, 11:13)]
pubs_nominatim[, c(1, 14:17)]

############################# Building a Map With Leaflet Library ##########################
# copy the data from Nominatim API results
# pubs_map <- pubs_nominatim
pubs_map <- pubs_nominatim

# add city
pubs_map$city = "London"

# add details about The Glory pub
pubs_map[10, 2:11] <- c("51.536327", "-0.077021", "The Glory, 281 Kingsland Rd, Haggerston, London, Greater London, England, E2 8AS, United Kingdom", "The Glory", "Kingsland Rd", "281", "Haggerston", "E2 8AS", "Greater London", "London")

# ////////////////////////////
# CLEANSING OF CONTACT DETAILS
# ////////////////////////////

# remove emails
pubs_map <- pubs_map[, -c(16, 17)]

# clean phone number
pubs_map$phone <- NA
for (i in 1:dim(pubs_map)[1]) {
  pubs_phone <- pubs_map[i, c("phone_1", "phone_2")]
  match <- !is.na(pubs_phone)
  if (any(match)) {
    pubs_map[i, "phone"] <- pubs_phone[match][1]
  }
}

# remove unnecessary phone numbers
pubs_map <- pubs_map[, -c(14:15)]

# clean website
pubs_map$website <- NA
for (i in 1:dim(pubs_map)[1]) {
  pubs_website <- pubs_map[i, c("website_1", "website_2", "website_3")]
  match <- !is.na(pubs_website)
  if (any(match)) {
    pubs_map[i, "website"] <- pubs_website[match][1]
  }
}

# remove unnecessary websites
pubs_map <- pubs_map[, -c(11:13)]

# ///////////////////
# ADD MISSING DETAILS
# ///////////////////

# street name
pubs_map$street_name[11] <- "Rupert Street"

# house number
pubs_map$house_number[c(2,4,9,11)] <- c("119", "34", "1", "28")

# suburb
pubs_map$suburb[c(8,9)] <- c("Greenwich", "Holborn")

# phone
# for #9 phone number is different from the one Nominatim provided
# for #12 phone number is corrent but I want the phone number format to be unified
pubs_map$phone <- c("+44 20 7394 3214", "+44 20 7727 4242", "+44 20 7249 5951", "+44 20 7250 0010", "+44 20 8748 9474", "+44 20 7353 3693", "+44 20 7405 0713", "+44 79 4059 6381", "+44 20 7405 4751", "+44 20 7684 0794", "+44 79 2133 6010", "+44 20 7430 2255")

# website
# again for some pubs Nominatim provided a broken link
pubs_map$website[c(1,3,4,6,7,9,10,11)] <- c("https://website--7315122201677780705144-pub.business.site", "http://theauldshillelagh.co.uk/", "https://thesekforde.com/", "https://www.tripadvisor.co.uk/Attraction_Review-g186338-d12116922-Reviews-The_Crown_And_Sugarloaf-London_England.html", "https://www.thelamblondon.com/", "https://www.yeoldemitreholborn.co.uk/", "https://www.theglory.co/", "http://theblueposts.co.uk/")

# create address to be displayed on the pop-up
cols <- c("house_number", "street_name", "suburb", "city", "postcode")
pubs_map$address_display <- do.call(paste, c(pubs_map[cols], sep=", "))

# change lat-lng data type
pubs_map$lat <- as.numeric(pubs_map$lat)
pubs_map$lng <- as.numeric(pubs_map$lng)