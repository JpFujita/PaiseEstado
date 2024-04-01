#loading libraries and setting work directory
library(tidyverse) 
library(data.table)
library(CoordinateCleaner) 
library(rgdal) 
library(raster)
library(plyr)


setwd("C:/Users/Joao Pedro Fujita/OneDrive/Documentos/GitHub/PaiseEstado")

#------------------------------------------------------------------------------#

#============#
#Reading data#
#============#

#reading the base dataset 
sampling_data <- read.csv("datasets/sampling_new_herbaria.csv")
#reading dataset containing the location and collector data
location_data <- read.csv("datasets/mimosa-pre_grid.csv")

#selecting the data that will be used in the loacion dataset
location_data <- location_data %>% dplyr::select(gen_sp,
                                                 country,
                                                 stateprovince,
                                                 county,
                                                 locality)

#removing registers without country information
location_data <- location_data %>% filter(!is.na(location_data$country))

#------------------------------------------------------------------------------#

#-----------------------------------#
#standardizing the country registers#
#-----------------------------------#

#creating a dataset containing country name and codes based on the dataset from 
#CoordinateCleaner
countrycode <- countryref %>% filter(type %in% c("country"))
#removing cases without a countrycode
countrycode <- countrycode %>% filter(!is.na(countrycode$iso2))
location_data <- location_data %>% filter(!is.na(location_data$country))

#changing the registerns in code to the country name
for(i in 1:nrow(location_data)){
  for(j in 1:nrow(countrycode)){
    if(location_data$country[i] == countrycode$iso2[j]){
      location_data$country[i] <- countrycode$name[j] 
    }
  }
}

#changing country names to english
location_data$country <- gsub("Brasil", "Brazil", location_data$country)
location_data$country <- gsub("Bolívia", "Bolivia", location_data$country)
location_data$country <- gsub("Colômbia", "Colombia", location_data$country)
location_data$country <- gsub("Ecuador", "Equador", location_data$country)
location_data$country <- gsub("Uruguai", "Uruguay", location_data$country)
location_data$country <- gsub("Paraguai", "Paraguay", location_data$country)
location_data$country <- gsub("Guiana", "Guyana", location_data$country)
#------------------------------------------------------------------------------#

#===============================================#
#removing repeateed locatons for the same specie#
#===============================================#

#creating two datasets: one containing the country info, other with the stateprovince info
country_data <- location_data %>% dplyr::select(gen_sp,
                                                country)

state_data <- location_data %>% dplyr::select(gen_sp,
                                              stateprovince)

country_data <- unique(country_data, by = c("gen_sp",
                                             "country"))

state_data <- unique(state_data, by = c("gen_sp",
                                           "stateprovince"))

#removing species that are not part of the sampling data and tha have no country info
country_data <- country_data [which(country_data$gen_sp %in% sampling_data$taxon), ]
country_data <- country_data %>% dplyr::filter(!is.na(country_data$country))

#removing species that are not part of the sampling data and that have no stateprovince info
state_data <- state_data [which(state_data$gen_sp %in% sampling_data$taxon), ]
state_data <- state_data %>% dplyr:: filter(!is.na(state_data$stateprovince))

#------------------------------------------------------------------------------#

#============================================#
#Adding location info to the sampling dataset#
#============================================#

#creatingo coluns for country and state info
sampling_data$country <- NA
sampling_data$state <- NA

#adding the data
for (i in 1:nrow(sampling_data)) {
  for (j in 1:nrow(country_data)) {
    if (sampling_data$taxon[i] == country_data$gen_sp[j]) {
      sampling_data$country[i] <- paste (sampling_data$country[i], country_data$country[j], sep = ".")
    }
  }
}
for (i in 1:nrow(sampling_data)) {
  for (j in 1:nrow(state_data)) {
    if (sampling_data$taxon[i] == state_data$gen_sp[j]) {
      sampling_data$state[i] <- paste (sampling_data$state[i], state_data$stateprovince[j], sep = ".")
    }
  }
}

#removing NA from the rows
sampling_data$country <- gsub("NA.", "", sampling_data$country)
sampling_data$state <- gsub("NA.", "", sampling_data$state)
sampling_data$state <- gsub("NA", NA, sampling_data$state)

#------------------------------------------------------------------------------#

#=========================================#
#Creating a dataframe with collection info#
#=========================================#

collection_info <- read.csv("datasets/mimosa-pre_grid.csv")
collection_info <- collection_info %>% dplyr::select(gen_sp,
                                                   collectioncode,
                                                   catalognumber,
                                                   collector,
                                                   collectornumber,
                                                   country,
                                                   stateprovince)

#creating a dataset containing only registers present in the sampling data
collection_info <- collection_info[which(collection_info$gen_sp %in% sampling_data$taxon), ]

#changing the registerns in code to the country name
for(i in 1:nrow(collection_info)){
  for(j in 1:nrow(countrycode)){
    if(collection_info$country[i] == countrycode$iso2[j]){
      collection_info$country[i] <- countrycode$name[j] 
    }
  }
}

#changing Brazil name to english
collection_info$country <- gsub("Brasil", "Brazil", location_data$country)

#writing csv
write.csv(sampling_data, file = "sampling_new_herbaria_location.csv")
wirte.csv(collection_info, file = "sample_collection_info.csv")