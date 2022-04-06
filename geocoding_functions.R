
# load packages
library(ggmap)
library(tmaptools)
library(RCurl)
library(jsonlite)
library(tidyverse)
library(leaflet)
library(rjson)

# replace "api_key" with your API key
api_key <- fromJSON(file = "T:/DCProjects/GitHub/BikeCounting/config/keys.json")$google_maps$APIKEY
register_google(key = api_key)

# ///////////////////////////////////////////////
# SUPLEMENTARY FUNCTIONS
# ///////////////////////////////////////////////

# ///////////////////////////////////////////////
# 1. CHECK ARGUMENTS
# ///////////////////////////////////////////////

check_arguments <- function(search_query_arg, fields_arg, key_arg) {
  
  # create empty error messages
  msg_1 <- msg_2 <- msg_3 <- ""
  
  # check if "search_query" argument is a string or
  # a list/vector/data frame with string data
  
  if(is.data.frame(search_query_arg) || is.matrix(search_query_arg)) {
    is_df <- TRUE
  } else {
    is_df <- FALSE
  }
  
  if(is_df) {
    if(dim(search_query_arg)[2] > 1) {
      error_1 <- TRUE
    } else if(!is.character(search_query_arg[, 1])) {
      error_1 <- TRUE
    } else {
      error_1 <- FALSE
    }
  }
  
  if(!is_df && is.list(search_query_arg)) {
    match_1 <- c()
    for(i in 1:length(search_query_arg)) {
      match_1[i] <- !is.character(search_query_arg[[i]])
    }
    if(any(match_1)) {
      error_1 <- TRUE
    } else {
      error_1 <- FALSE
    }
  }
  
  if(!is_df && !is.list(search_query_arg)) {
    if(!is.character(search_query_arg)) {
      error_1 <- TRUE
    } else {
      error_1 <- FALSE
    }
  }
  
  if(error_1) {
    msg_1 <- "Error: search_query argument (or any of its elements) is not of a string type"
  }
  
  # check if "fields" argument:
  # consists of a single word only - "all"
  # OR
  # * is a string vector of length <=3
  # * and consists of words 'coordinates', 'address', 'contacts' only
  
  match_2 <- fields_arg %in% c("coordinates", "address", "contacts")
  
  if (!(fields_arg %in% "all" && length(unique(fields_arg)) == 1)) {
    if (!all(match_2) || length(unique(fields_arg)) > 3) {
      msg_2 <- paste0("Error: fields argument must be ",
                      "either a combination of 'coordinates', ",
                      "'address', and 'contacts' or a single ",
                      "word 'all'")
    }
  }
  
  # check if "key" argument is a string
  
  if(is.list(key_arg)) {
    if(length(key_arg) > 1) {
      error_3 <- TRUE
    } else if(!is.character(key_arg[[1]])) {
      error_3 <- TRUE
    } else {
      error_3 <- FALSE
    }
  } else if(!is.character(key_arg) || length(key_arg) > 1) {
    error_3 <- TRUE
  } else {
    error_3 <- FALSE
  }
  
  if(error_3) {
    msg_3 <- "Error: key argument is not of a string type"
  }
  
  # return error messages (if any)
  
  errors <- c(msg_1, msg_2, msg_3)
  
  if(any(errors!="")) {
    errors <- paste(errors[errors!=""], collapse = "\n")
    return(list(TRUE, errors))
  } else {
    return(list(FALSE))
  }
  
}

# ///////////////////////////////////////////////

# ///////////////////////////////////////////////
# SUPLEMENTARY FUNCTIONS
# ///////////////////////////////////////////////

# ///////////////////////////////////////////////
# 2. GENERATE API CALLS
# ///////////////////////////////////////////////

url_google_geocoding <- function(search_query_url, key_url) {
  
  # load libraries
  library(RCurl)
  
  # convert input into a list
  search_query_url <- sapply(search_query_url, as.list)
  
  # google gecoding api url
  url_geocoding_api <- "https://maps.googleapis.com/maps/api/geocode/"
  
  # percent-encode search request
  search_query_url <- sapply(search_query_url, URLencode)
  
  # construct search request for geocode
  url_geocoding_call <- paste0(url_geocoding_api, "json",
                               "?address=", search_query_url, "&key=", key_url)
  
  return(url_geocoding_call)
  
}

# ///////////////////////////////////////////////

url_google_place_search <- function(search_query_url, key_url) {
  
  # load libraries
  library(RCurl)
  
  # convert input into a list
  search_query_url <- sapply(search_query_url, as.list)
  
  # google places api url
  url_places_api <- "https://maps.googleapis.com/maps/api/place/"
  
  # percent-encode search request
  search_query_url <- sapply(search_query_url, URLencode)
  
  # construct search request for place id
  url_place_search_call <- paste0(url_places_api, "findplacefromtext/",
                                  "json", "?input=", search_query_url,
                                  "&inputtype=textquery","&fields=place_id",
                                  "&key=", key_url)
  
  return(url_place_search_call)
  
}

# ///////////////////////////////////////////////

url_google_place_details <- function(place_id_url, key_url) {
  
  # load libraries
  library(RCurl)
  
  # google places api url
  url_places_api <- "https://maps.googleapis.com/maps/api/place/"
  
  # in case you would want to add "fields" as an argument
  # fields_url <- paste(fields_url, collapse = ",")
  
  # construct search request for place details
  url_place_details_call <- paste0(url_places_api, "details/",
                                   "json", "?place_id=", place_id_url,
                                   "&fields=formatted_phone_number,website",
                                   "&key=", key_url)
  
  return(url_place_details_call)
  
}

# ///////////////////////////////////////////////

# ///////////////////////////////////////////////
# SUPLEMENTARY FUNCTIONS
# ///////////////////////////////////////////////

# ///////////////////////////////////////////////
# 3. EXTRACT DATA FROM JSON
# ///////////////////////////////////////////////

get_geodata_from_json_google <- function(geodata_json) {
  
  # load library
  library(jsonlite)
  
  # convert json output into r object
  geodata <- lapply(geodata_json, fromJSON, simplify = FALSE)
  
  # extract coordinates, address and city name
  
  lat_lng_a <- data.frame(lat = NA, lng = NA, address = NA, city = NA)
  
  for (i in 1:length(geodata)) {
    
    if (geodata[[i]]$status=="OK") {
      # 
      # extract coordinates and address
      
      lat <- geodata[[i]]$results[[1]]$geometry$location$lat
      lng <- geodata[[i]]$results[[1]]$geometry$location$lng
      address <- geodata[[i]]$results[[1]]$formatted_address
      
      # find out how many elements there are in "address_components"
      n <- length(geodata[[i]]$results[[1]]$address_components)           
      
      # extract city and country
      
      for (j in 1:n) {
        
        # extract the type of the "address_components"
        type <- geodata[[i]]$results[[1]]$address_components[[j]]$types[[1]]
        
        # extract the city name
        
        if (type == "postal_town") {
          city <- geodata[[i]]$results[[1]]$address_components[[j]]$long_name 
        }
        
      }                
      
      lat_lng_a[i, ] <- c(lat, lng, address, city)
      
    } else {
      lat_lng_a[i, ] <- NA
    }
    
  }
  
  return(lat_lng_a)
  
}

# ///////////////////////////////////////////////

get_place_id_from_json_google <- function(place_json) {
  
  # load library
  library(jsonlite)
  
  # convert json output into r object
  place_search <- lapply(place_json, fromJSON,simplify = FALSE)
  
  # extract place id
  
  place_id <- list()
  
  for (i in 1:length(place_search)) {
    
    if (place_search[[i]]$status=="OK") {
      place_id[[i]] <- place_search[[i]]$candidates[[1]]$place_id
    } else {
      place_id[[i]] <- NA
    }
  }
  
  return(place_id)
  
}

# ///////////////////////////////////////////////

get_contacts_from_json_google <- function(place_details_json) {
  
  # load library
  library(jsonlite)
  
  # convert json output into r object
  place_details <- lapply(place_details_json, fromJSON, simplify = FALSE)
  
  # extract phone number and website
  
  contacts <- data.frame("phone number" = NA, "website" = NA)
  
  for (i in 1:length(place_details)) {
    
    if (place_details[[i]]$status=="OK") {
      
      # get data
      
      phone_number <- place_details[[i]]$result$formatted_phone_number
      website <- place_details[[i]]$result$website
      
      # get rid of NULLs
      
      info <- list(phone_number, website)
      
      for (j in 1:length(info)) {
        if (is.null(info[[j]])) info[[j]] <- NA
      }
      
      # create output data frame
      contacts[i, ] <- info
      
    } else {
      contacts[i, ] <- NA
    }
  }
  
  return(contacts)
  
}

# ///////////////////////////////////////////////

# ///////////////////////////////////////////////
# MAIN FUNCTION
# ///////////////////////////////////////////////

geocode_google <- function(search_query, fields = "coordinates", key) {
  
  # STOP RUNNING THE FUNCTION IF ARGUMENTS ARE INCORRECT
  
  errors <- check_arguments(search_query, fields, key)
  
  if (errors[[1]]) {
    stop(errors[[2]])
  }
  
  # LOAD LIBRARIES
  
  library(RCurl)
  
  # EXTRACT COORDINATES
  
  if (any(c("coordinates", "address") %in% fields) || "all" %in% fields) {
    
    # construct url for geocoding
    url_geocode <- url_google_geocoding(search_query, key)
    
    # get data from google
    geodata_json <- getURL(url_geocode)
    
    # get data from json output
    
    geodata_df <- as.data.frame(sapply(search_query, as.character),
                                stringsAsFactors = FALSE)
    names(geodata_df) <- "search query"
    rownames(geodata_df) <- NULL
    geodata_df[, 2:5] <- get_geodata_from_json_google(geodata_json)
    
    # return dataframe with the geodata
    
    if (all(c("coordinates", "address") %in% fields) || "all" %in% fields) {
      geodata_df
    } else if ("coordinates" %in% fields) {
      geodata_df <- geodata_df[, 1:3]
    } else {
      geodata_df <- geodata_df[, c(1, 4:5)]
    }
    
  }
  
  # EXTRACT CONTACTS
  
  if ("contacts" %in% fields || "all" %in% fields) {
    
    # /// get place_id from Place Search API ///
    
    # construct url for place search
    url_place_search <- url_google_place_search(search_query, key)
    
    # get data from google
    place_json <- getURL(url_place_search)
    
    # get place_id from json output
    place_id <- get_place_id_from_json_google(place_json)
    
    # /// get contacts from Place Details API ///
    
    # construct url for place details
    url_place_details <- url_google_place_details(place_id, key)
    
    # get data from google
    place_details_json <- getURL(url_place_details)
    
    # get place_id from json output
    contacts <- get_contacts_from_json_google(place_details_json)
    
    # /// add contacts to our output data frame ///
    
    if (!exists("geodata_df")) {
      geodata_df <- as.data.frame(sapply(search_query, as.character),
                                  stringsAsFactors = FALSE)
      names(geodata_df) <- "search query"
      rownames(geodata_df) <- NULL
    }
    
    geodata_df[, c("phone", "web page")] <- contacts
    
  }
  
  return(geodata_df)
  
}

# ///////////////////////////////////////////////

# ///////////////////////////////////////////////
# SUPLEMENTARY FUNCTIONS
# ///////////////////////////////////////////////

# ///////////////////////////////////////////////
# 1. GENERATE API CALLS
# ///////////////////////////////////////////////

url_google_rev_geocoding <- function(coordinates_url, key_url) {
  
  # load libraries
  library(RCurl)
  
  # convert everything into data frame
  
  if (is.matrix(coordinates_url) || is.data.frame(coordinates_url)) {
    coordinates <- data.frame(matrix(NA, nrow(coordinates_url), ncol(coordinates_url)))
    names(coordinates) <- c("lat", "lng")
    coordinates[, 1] <- coordinates_url[, 1]
    coordinates[, 2] <- coordinates_url[, 2]
  } else if (is.list(coordinates_url)) {
    coordinates <- data.frame(matrix(NA, nrow = length(coordinates_url), ncol = 2))
    names(coordinates) <- c("lat", "lng")
    for (i in 1:length(coordinates_url)) {
      coordinates[i, 1] <- coordinates_url[[i]][1]
      coordinates[i, 2] <- coordinates_url[[i]][2]
    }
  } else if (is.vector(coordinates_url)) {
    coordinates <- data.frame(lat = NA, lng = NA)
    coordinates[1,1] <- coordinates_url[1]
    coordinates[1,2] <- coordinates_url[2]
  }
  
  coordinates$lat_lng <- paste0(coordinates$lat, ",", coordinates$lng)
  
  # google gecoding api url
  url_geocoding_api <- "https://maps.googleapis.com/maps/api/geocode/"
  
  # construct search request for reverse geocoding
  url_rev_geocoding_call <- paste0(url_geocoding_api, "json",
                                   "?latlng=", coordinates$lat_lng, "&key=", key_url)
  
  # return data frame with coordinates and API call
  
  coordinates$api_call <- url_rev_geocoding_call
  
  return(coordinates)
  
}

# ///////////////////////////////////////////////
# 2. EXTRACT DATA FROM JSON
# ///////////////////////////////////////////////

get_rev_geodata_from_json_google <- function(geodata_json) {
  
  # load library
  library(jsonlite)
  
  # convert json output into r object
  geodata <- lapply(geodata_json, fromJSON,simplify = FALSE)
  
  # extract address, city and country from the json output
  
  address_df <- data.frame(address = NA, city = NA, country = NA)
  
  for (i in 1:length(geodata)) {
    
    if (geodata[[i]]$status=="OK") {
      
      # extract address
      address <- geodata[[i]]$results[[1]]$formatted_address
      
      # find out how many elements there are in "address_components"
      n <- length(geodata[[i]]$results[[1]]$address_components)
      
      # extract city and country
      
      for (j in 1:n) {
        
        # extract type of "address_components"
        type <- geodata[[i]]$results[[1]]$address_components[[j]]$types[[1]]
        
        # extract city and country
        
        if (type %in% c("postal_town","locality")){
          city <- geodata[[i]]$results[[1]]$address_components[[j]]$long_name 
        } else if (type == "country") {
          country <- geodata[[i]]$results[[1]]$address_components[[j]]$long_name
        }
        
      }
      
      # prepare output
      address_df[i, ] <- c(address, city, country)
      
    } else {
      address_df[i, ] <- NA
    }
    
  }
  
  return(address_df)
  
}

# ///////////////////////////////////////////////
# MAIN FUNCTION
# ///////////////////////////////////////////////

rev_geocode_google <- function(coordinates, key) {
  
  # load libraries
  library(RCurl)
  
  # construct url for reverse geocoding
  rev_geocoding_info <- url_google_rev_geocoding(coordinates, key)
  
  # get data from google
  geodata_json <- getURL(rev_geocoding_info$api_call)
  
  # get data from json output
  geodata_df <- rev_geocoding_info[, c("lat", "lng")]
  geodata_df[, 3:5] <- get_rev_geodata_from_json_google(geodata_json)
  
  # return dataframe with the geodata
  return(geodata_df)
  
}

# ///////////////////////////////////////////////

# ///////////////////////////////////////////////
# SUPLEMENTARY FUNCTIONS
# ///////////////////////////////////////////////

# ///////////////////////////////////////////////
# 1. GENERATE API CALLS
# ///////////////////////////////////////////////

url_nominatim_search <- function(search_query_url, country_url,
                                 language_url, email_url) {
  
  # load libraries
  library(RCurl)
  
  # nominatim search api url
  url_nominatim_search_api <- "https://nominatim.openstreetmap.org/search/"
  
  # convert input into a list
  search_query_url <- sapply(search_query_url, as.list)
  
  # percent-encode search request
  search_query_url <- sapply(search_query_url, URLencode)
  
  # parameters
  
  if (!is.null(country_url)) {
    country_url <- paste0("&countrycodes=", country_url)
  }
  
  parameters_url <- paste0("?format=json",
                           "&addressdetails=1&extratags=1&limit=1",
                           country_url, "&accept-language=", language_url,
                           "&email=", email_url)
  
  # construct search request for geocode
  url_nominatim_search_call <- paste0(url_nominatim_search_api,
                                      search_query_url, parameters_url)
  
  return(url_nominatim_search_call)
  
}

# ///////////////////////////////////////////////
# 2. EXTRACT DATA FROM JSON
# ///////////////////////////////////////////////

get_geodata_from_json_nominatim <- function(geodata_json) {
  
  # load library
  library(jsonlite)
  
  # convert json output into r object
  geodata <- lapply(geodata_json, fromJSON,simplify = FALSE)
  
  # extract coordinates, address and contacts
  
  lat_lng_a_c <- data.frame(lat = NA, lng = NA, address = NA, pub_name = NA,
                            street_name = NA, house_number = NA, suburb = NA,
                            postcode = NA, state_district = NA, website_1 = NA,
                            website_2 = NA, website_3 = NA, phone_1 = NA,
                            phone_2 = NA, email_1 = NA, email_2 = NA)
  
  for(i in 1:length(geodata)) {
    
    if(length(geodata[[i]]) != 0) {
      
      # get data
      
      lat <- geodata[[i]][[1]]$lat
      lng <- geodata[[i]][[1]]$lon
      address <- geodata[[i]][[1]]$display_name
      pub_name <- geodata[[i]][[1]]$address$pub
      street_name <- geodata[[i]][[1]]$address$road
      house_number <- geodata[[i]][[1]]$address$house_number
      suburb <- geodata[[i]][[1]]$address$suburb
      postcode <- geodata[[i]][[1]]$address$postcode
      state_district <- geodata[[i]][[1]]$address$state_district
      website_1 <- geodata[[i]][[1]]$extratags$website
      website_2 <- geodata[[i]][[1]]$extratags$url
      website_3 <- geodata[[i]][[1]]$extratags$`contact:website`
      phone_1 <- geodata[[i]][[1]]$extratags$phone
      phone_2 <- geodata[[i]][[1]]$extratags$`contact:phone`
      email_1 <- geodata[[i]][[1]]$extratags$email
      email_2 <- geodata[[i]][[1]]$extratags$`contact:website`
      
      # get rid of NULLs
      
      info <- list(lat, lng, address, pub_name, street_name,
                   house_number, suburb, postcode, state_district,
                   website_1, website_2, website_3,
                   phone_1, phone_2, email_1, email_2)
      
      for (j in 1:length(info)) {
        if (is.null(info[[j]])) info[[j]] <- NA
      }
      
      # create output data frame
      
      lat_lng_a_c[i, ] <- info
      
    } else {
      lat_lng_a_c[i, ] <- NA
    }
  }
  
  return(lat_lng_a_c)
  
}

# ///////////////////////////////////////////////
# MAIN FUNCTION
# ///////////////////////////////////////////////

geocode_nominatim <- function(search_query, country = NULL, language = "en",
                              fields = "coordinates", email) {
  
  # LOAD LIBRARIES
  
  library(RCurl)
  
  # EXTRACT DATA
  
  # construct url for geocoding
  url_geocode <- url_nominatim_search(search_query, country, language, email)
  
  # get data from nominatim
  # wait 3 seconds between each call
  
  geodata_json <- list()
  
  for (i in 1:length(url_geocode)) {
    geodata_json[i] <- getURL(url_geocode[i])
    Sys.sleep(3)
  }
  
  # get data from json output
  
  geodata_df <- as.data.frame(sapply(search_query, as.character),
                              stringsAsFactors = FALSE)
  names(geodata_df) <- "search query"
  rownames(geodata_df) <- NULL
  
  geodata_df[, 2:17] <- get_geodata_from_json_nominatim(geodata_json)
  geodata_df_query <- data.frame(search_query = geodata_df[, 1],
                                 stringsAsFactors = FALSE)
  geodata_df_coordinates <- geodata_df[, 2:3]
  geodata_df_address <- geodata_df[, 4:10]
  geodata_df_contacts <- geodata_df[, 11:17]
  
  # return dataframe with the geodata
  
  geodata_result <- geodata_df_query
  
  if("all" %in% fields) {
    geodata_result <- cbind(geodata_result, geodata_df[, 2:17])
  }
  
  if("coordinates" %in% fields) {
    geodata_result <- cbind(geodata_result, geodata_df_coordinates)
  }
  
  if("address" %in% fields) {
    geodata_result <- cbind(geodata_result, geodata_df_address)
  }
  
  if("contacts" %in% fields) {
    geodata_result <- cbind(geodata_result, geodata_df_contacts)
  }
  
  return(geodata_result)
  
}

# ///////////////////////////////////////////////

# ///////////////////////////////////////////////
# SUPLEMENTARY FUNCTIONS
# ///////////////////////////////////////////////

# ///////////////////////////////////////////////
# 1. GENERATE API CALLS
# ///////////////////////////////////////////////

url_nominatim_rev_geocoding <- function(coordinates_url, language_url, email_url) {
  
  # load libraries
  library(RCurl)
  
  # convert everything into a data frame
  
  if (is.matrix(coordinates_url) || is.data.frame(coordinates_url)) {
    coordinates <- data.frame(matrix(NA, nrow(coordinates_url), ncol(coordinates_url)))
    names(coordinates) <- c("lat", "lng")
    coordinates[, 1] <- coordinates_url[, 1]
    coordinates[, 2] <- coordinates_url[, 2]
  } else if (is.list(coordinates_url)) {
    coordinates <- data.frame(matrix(NA, nrow = length(coordinates_url), ncol = 2))
    names(coordinates) <- c("lat", "lng")
    for (i in 1:length(coordinates_url)) {
      coordinates[i, 1] <- coordinates_url[[i]][1]
      coordinates[i, 2] <- coordinates_url[[i]][2]
    }
  } else if (is.vector(coordinates_url)) {
    coordinates <- data.frame(lat = NA, lng = NA)
    coordinates[1,1] <- coordinates_url[1]
    coordinates[1,2] <- coordinates_url[2]
  }
  
  # nominatim reverse api url
  url_nominatim_reverse_api <- "https://nominatim.openstreetmap.org/reverse"
  
  # parameters
  
  lat <- coordinates$lat
  lon <- coordinates$lng
  
  parameters_url <- paste0("?format=json", "&lat=", lat, "&lon=", lon,
                           "&addressdetails=1&extratags=1","&accept-language=",
                           language_url, "&zoom=18", "&email=", email_url)
  
  # construct search request for geocode
  url_nominatim_reverse_call <- paste0(url_nominatim_reverse_api, parameters_url)
  
  # return data frame with coordinates and API call
  coordinates$api_call <- url_nominatim_reverse_call
  
  return(coordinates)
  
}

# ///////////////////////////////////////////////
# 2. EXTRACT DATA FROM JSON
# ///////////////////////////////////////////////

get_rev_geodata_from_json_nominatim <- function(geodata_json) {
  
  # load library
  library(jsonlite)
  
  # convert json output into r object
  geodata <- lapply(geodata_json, fromJSON,simplify = FALSE)
  
  # extract address, city and country
  
  address_df <- data.frame(address = NA, pub_name = NA, street_name = NA,
                           house_number = NA, suburb = NA, postcode = NA,
                           state_district = NA, country = NA)
  
  for(i in 1:length(geodata)) {
    
    if(length(geodata[[i]]) != 0) {
      
      # get data
      
      address <- geodata[[i]]$display_name
      pub_name <- geodata[[i]]$address$pub
      street_name <- geodata[[i]]$address$road
      house_number <- geodata[[i]]$address$house_number
      suburb <- geodata[[i]]$address$suburb
      postcode <- geodata[[i]]$address$postcode
      state_district <- geodata[[i]]$address$state_district
      country <- geodata[[i]]$address$country
      
      # get rid of NULLs
      
      info <- list(address, pub_name, street_name, house_number,
                   suburb, postcode, state_district, country)
      
      for (j in 1:length(info)) {
        if (is.null(info[[j]])) info[[j]] <- NA
      }
      
      # create output data frame
      
      address_df[i, ] <- info
      
    } else {
      address_df[i, ] <- NA
    }
  }
  
  return(address_df)
  
}

# ///////////////////////////////////////////////
# MAIN FUNCTION
# ///////////////////////////////////////////////

rev_geocode_nominatim <- function(coordinates, language = "en", email) {
  
  # load libraries
  library(RCurl)
  
  # construct url for reverse geocoding
  rev_geocoding_info <- url_nominatim_rev_geocoding(coordinates, language, email)
  
  # get data from nominatim
  # wait 3 seconds between each call
  
  geodata_json <- list()
  
  for (i in 1:dim(rev_geocoding_info)[1]) {
    geodata_json[i] <- getURL(rev_geocoding_info$api_call[i])
    Sys.sleep(3)
  }
  
  # get data from json output
  geodata_df <- rev_geocoding_info[, c("lat", "lng")]
  geodata_df[, 3:10] <- get_rev_geodata_from_json_nominatim(geodata_json)
  
  # return dataframe with the geodata
  return(geodata_df)
  
}

# ///////////////////////////////////////////////

