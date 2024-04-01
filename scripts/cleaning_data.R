#libraries and setting work directory
library(tidyverse) 
library(data.table) 
library(CoordinateCleaner) 
library(flora) 
library(rgdal) 
library(raster)
library(rWCVP)
library(rWCVPdata)
library(plyr)
 
setwd("C:/Users/Joao Pedro Fujita/OneDrive/Documentos/GitHub/PaiseEstado")

#==============================================================================#

#----------------#
#Reading datasets#
#----------------#

#---#
#GBIF
#---#

#Reading data
gbif <- fread("datasets/dados_mimosa/gbif.txt", 
              na.strings = c("", NA), stringsAsFactors = F, encoding = "UTF-8")
#Warnig message due to double quotes. Not a problem in this case

#Reducing data by selecting only the necessary columns 
gbif <- gbif %>% dplyr::select(institutionCode,
                               collectionCode,
                               catalogNumber,
                               genus,
                               specificEpithet,
                               infraspecificEpithet,
                               basisOfRecord,
                               identifiedBy,
                               recordNumber,
                               recordedBy,
                               year,
                               stateProvince,
                               county,
                               municipality,
                               locality,
                               decimalLongitude,
                               decimalLatitude,
                               countryCode)

#Renaming the atributes to match the specieslink's columns
gbif <- gbif %>% dplyr::rename("species" = "specificEpithet",
                        "institutioncode" = "institutionCode",
                        "collectioncode" = "collectionCode",
                        "catalognumber" = "catalogNumber",
                        "basisofrecord" = "basisOfRecord",
                        "identifiedby" = "identifiedBy",
                        "collector" = "recordedBy",
                        "yearcollected" = "year",  
                        "collectornumber" = "recordNumber",
                        "stateprovince" = "stateProvince",
                        "longitude" = "decimalLongitude",
                        "latitude" = "decimalLatitude",
                        "subspecies" = "infraspecificEpithet",
                        "country" = "countryCode")

#converting the yearcollected field into strings
gbif$yearcollected <- as.character(gbif$yearcollected)

#giving an specific ID for each record
gbif <- cbind(id = 1:nrow(gbif), gbif)

#----------#
#SpeciesLink
#----------#

#Reading data
splink <- fread("datasets/dados_mimosa/splink.txt", 
                na.strings = c("", NA), stringsAsFactors = F, encoding = "UTF-8")
#Warning message due to double quotes. Not a problem for the procedure in this case

#Reducing the matrix to only the necessary colunms
splink <- splink %>% dplyr::select(institutioncode,
                                   collectioncode,
                                   catalognumber,
                                   genus,
                                   species,
                                   subspecies, 
                                   basisofrecord,
                                   identifiedby,
                                   collector,
                                   collectornumber,
                                   yearcollected,
                                   stateprovince,
                                   county,
                                   locality,
                                   longitude,
                                   latitude,
                                   country)

#converting the yearcollected to string
splink$yearcollected <- as.character(splink$yearcollected)

#giving a unique ID code to each record
splink <- cbind(id = (nrow(gbif) + 1):(nrow(gbif) + nrow(splink)), splink)


#==============================================================================#


#-----------------------------#
#Merging the datasets into one#
#-----------------------------#

#Creating a function that merge the datasets with the source
merge.w.source <- function(x , y , name.x = "X" , name.y = "Y") {
  x.data.frame <- cbind(x, datasource.x = name.x) #add the column datasource.x to the x dataset, name.X is the content in the column
  y.data.frame <- cbind(y, datasource.y = name.y) #add the column datasource.y to the y dateset, name.Y is the content in the column
  #merge the datasets in one
  merged.dataframe <- merge(x = x.data.frame,    
                            y = y.data.frame,
                            all = TRUE)           
  
  #change the NAs in the source columns to blank spaces
  merged.dataframe[is.na(merged.dataframe$datasource.x), "datasource.x"] <- ""  
  merged.dataframe[is.na(merged.dataframe$datasource.y), "datasource.y"] <- ""  
  
  #merge the datasource.x anda datasource.y in only one column(datasource)
  merged.dataframe$datasource <- paste(merged.dataframe$datasource.x, merged.dataframe$datasource.y)
  
  #remove the columns datasource.x and datasource.y
  merged.dataframe$datasource.x <- rm() 
  merged.dataframe$datasource.y <- rm()
  
  #return the merged dataframe
  return(merged.dataframe)
}


#Merging the datasets creating a new colunm with the source of the data
mimosa <- merge.w.source (x = gbif,
                          y = splink,
                          name.x = "gbif",
                          name.y = "splink")

#removing the function and the individual datasets, they will no longer be used
rm(gbif, splink, merge.w.source)

# Indicating the source of the municipality attribute
mimosa <- mimosa %>% dplyr::rename("municipality_gbif" = municipality)


#==============================================================================#


#-------------#
#Pre-refinment#
#-------------#

#Observations with NA for any of these will be disconsider:
# institution code, collection code and catalog number

a <- mimosa
a.1 <- a[!is.na(a$institutioncode) &
           !is.na(a$collectioncode)&
           !is.na(a$catalognumber)]

a.2 <- a[!is.na(a$institutioncode)|
           !is.na(a$collectioncode)|
           !is.na(a$catalognumber)]

a <- unique(a.1, by = c("institutioncode",
                        "collectioncode",
                        "catalognumber"))

a <- rbind(a, a.2)
mimosa <- a 
rm (a, a.1, a.2)

#Replacing coordinates equal to 0 to NA
# Even though the Equator line crosses the area of study, 
#plain zero coordinates are unreliable (see Zizka et al., 2019)
mimosa$latitude[mimosa$latitude == 0] <- NA
mimosa$longitude[mimosa$longitude == 0] <- NA

#removing records without valid coordinates
mimosa <- mimosa %>% filter(!is.na(mimosa$longitude))
mimosa <- mimosa %>% filter(!is.na(mimosa$latitude))

#removing records without species level identification
mimosa <- mimosa %>% filter(!is.na(mimosa$species))

#removing records without the indentifier's name
#mimosa <- mimosa %>% filter(!is.na(mimosa$identifiedby))

#removing records that are not based on preserved speciments
plyr::count(mimosa$basisofrecord) #used to check the strings that represents the
#preserved speciments indicators
mimosa <- mimosa %>% filter(basisofrecord %in% c("PRESERVED_SPECIMEN",
                                                 "PreservedSpecimen"))
#removing the basisofrecord column (will no longer be used for further analysis)
mimosa <- mimosa %>% dplyr::select(-basisofrecord)

#==============================================================================#

#------------------------#
#Cleaning taxonomic names#
#------------------------#
#creating a prefix column for species with infraspecific classification
mimosa$var <- paste("var.")
# Generating a column for scientific names (without authors and including the infraspecific epithet)
mimosa$gen_sp <- paste(mimosa$genus,
                       mimosa$species,
                       mimosa$var,
                       mimosa$subspecies,
                       sep = " ")
#removing prefix column, we don't need it anymore
mimosa$var <- NULL

# Extracting scientific names into a vector to work with
taxa <- plyr::count(mimosa$gen_sp)
taxa <- as.character(taxa$x)

# Standardizing the formatting of all names
for(i in 1:length(taxa)){
  #removing var. to species without infraspecific classification or double prefixes
  taxa[i] <- gsub("var. NA", "", taxa[i]) 
  taxa[i] <- gsub("var. var.", "var.", taxa[i] )
  # removing the character NA
  taxa[i] <- gsub("NA", "", taxa[i])
  # removing white space
  taxa[i] <- trimws(taxa[i]) 
  # replacing double spaces by single spaces
  taxa[i] <- gsub(pattern = "  ", x = taxa[i], replacement = " ") 
}
# Removing names with annotations that indicate uncertainty
taxa <- taxa[-which(grepl(c("aff."), taxa, fixed = TRUE))]
taxa <- taxa[-which(grepl(c("cf."), taxa, fixed = TRUE))]
taxa <- taxa[-which(grepl(c("sp."), taxa, fixed = TRUE))]

#==============================================================================#

#--------------#
# Running rWCVP#
#--------------#
taxa.df <- data.frame(row.number = 1:length(taxa), taxa = taxa)
wcvp_names <- rWCVPdata::wcvp_names
taxa.res <- wcvp_match_names(taxa.df, wcvp_names, name_col = "taxa")

exact_match <- taxa.res %>% filter(match_similarity == 1)
exact_match <- subset(exact_match, select = -c(match_similarity,
                                               match_edit_distance, 
                                               wcvp_id, wcvp_authors, 
                                               wcvp_rank, wcvp_homotypic, 
                                               wcvp_ipni_id, 
                                               wcvp_accepted_id,
                                               match_type,
                                               wcvp_status))
#saving exact mtaches in an csv
#write.csv(exact_match, file = "datasets/exactmatch.csv", fileEncoding = "UTF-8")
#reloading the exact matches results
exact_match <- read.csv("datasets/exactmatch.csv")

#resolving fuzzy matches
fuzzy_match <- taxa.res %>% filter(str_detect(match_type, "Fuzzy")) %>%
  mutate(
    keep = case_when( #set up a keep column
      match_similarity < 0.9 ~ NA_real_, # fill with blank for dissimilar names
      match_edit_distance == 1 ~ 1, # fill with 1 if only one letter different
    )
  )
#checking how many fuzzy matches were resolved
table(fuzzy_match$keep, useNA = "always")
#removing unused columns in the fuzzymatch matrix
fuzzy_match <- subset(fuzzy_match, select = -c(match_similarity,
                                               match_edit_distance, 
                                               wcvp_id, wcvp_authors, 
                                               wcvp_rank, wcvp_homotypic, 
                                               wcvp_ipni_id, 
                                               wcvp_accepted_id))
#saving the fuzzymatches in a csv file to manual check
#write.csv(fuzzy_match, file = "datasets/fuzzymatch.csv", fileEncoding = "UTF-8")
#manual checking the fuzzymatches
#removing the fuzzymatches, we don't need it anymore
rm(fuzzy_match)  

#Restablishing dataset after manual check
#reloading the csv with the fuzzy matches manualy resolved
fuzzy_checked <- read_csv("datasets/fuzzymatch.csv",
                          show_col_types=FALSE) %>%
  dplyr::select(-keep) %>%
  mutate(resolved_match_type=ifelse(! is.na(resolved_match_type),
                                    resolved_match_type,
                                    match_type))


checked_matches <- fuzzy_checked %>%  filter(resolved_match_type %in% c("Fuzzy (phonetic)",
                                                                  "Fuzzy (edit distance)"))

checked_matches <- subset(checked_matches, select = -c(resolved_match_type,
                                                     match_type))

taxa_corrected <- bind_rows(checked_matches, exact_match)

#==============================================================================#

# Establishing a dataset with information on genus, species and varieties (or subspecies)
# The order of observations is the same as for the taxa_corrected dataset
taxa_gensp <- tibble(gen = NA, sp = NA, infra = NA, .rows = nrow(taxa_corrected))
for(i in 1:nrow(taxa_corrected)){
  str <- strsplit(taxa_corrected$wcvp_name[i], split = " ")[[1]]
  if(length(str) == 2){
    taxa_gensp$gen[i] <- str[1]
    taxa_gensp$sp[i] <- str[2]
  } else if(length(str) == 4){
    taxa_gensp$gen[i] <- str[1]
    taxa_gensp$sp[i] <- str[2]
    taxa_gensp$infra[i] <- paste(str[3], str[4], sep = " ")
  }
}

# Adding a field that contains the original name for each corrected name (i.e., the names that should be replaced)
taxa_gensp$replace <- taxa_corrected$taxa

# Removing invalid taxa
invalid_taxa <- taxa_gensp$replace[is.na(taxa_gensp$gen) | taxa_gensp$gen != "Mimosa"] # generating a list of invalid taxa
mimosa <- mimosa[!mimosa$gen_sp %in% invalid_taxa, ] # removing invalid taxa

#Correcting the dataset according to the taxa_gen_sp list
taxa_gensp <- taxa_gensp[!is.na(taxa_gensp$gen), ] # removing records that are NA for the gen field
for(i in 1:nrow(mimosa)){
  for(j in 1:nrow(taxa_gensp)){
    if(mimosa$gen_sp[i] == taxa_gensp$replace[j]){
      mimosa$gen_sp[i] <- paste(taxa_gensp$gen[j], taxa_gensp$sp[j], taxa_gensp$infra[j], sep = " ") # pasting all the taxa_gensp fields into one
    }
  }
}

# Removing NA strings from records of taxa without information regarding the infraspecific epithet
mimosa$gen_sp <- gsub("var. NA", "", mimosa$gen_sp)
mimosa$gen_sp <- gsub("NA", "", mimosa$gen_sp)
mimosa$gen_sp <- trimws(mimosa$gen_sp) # removing white spaces

mimosa$gensp <- gsub(" ", "", mimosa$gen_sp)
mimosa$gensp <- gsub("-", "", mimosa$gensp)
mimosa$gensp <- gsub("var.", "", mimosa$gensp)
mimosa$gensp <- gsub("Var", "", mimosa$gensp)
mimosa$gensp <- gsub("f.", "", mimosa$gensp)
mimosa$gensp <- gsub("subsp.", "", mimosa$gensp)
mimosa$gensp <- gsub(" ", "", mimosa$gensp)

# Replacing space and hyphen by underscore and removing 'var.' and 'subsp.'
mimosa$gen_sp <- gsub("Var", "var.", mimosa$gen_sp)
mimosa$gensp <- gsub("Subsp.", "subsp.", mimosa$gen_sp)
mimosa$gen_sp <- gsub("__", "", mimosa$gen_sp)
mimosa$gen_sp <- gsub("  ", " ", mimosa$gen_sp)

#==============================================================================#

#--------------------#
#Cleaning coordinates#
#--------------------#

#removing a invalid line that represent a ivalid coordinate value (indicated by the clean_coordinates function below when running previously)
mimosa <- mimosa[-c(157241)]

# Flagging problematic record according to the 'CoordinateCleaner' package 
mimosa_coordFlagged <- mimosa %>% clean_coordinates(lon = "longitude",
                                                    lat = "latitude",
                                                    species = "gen_sp",
                                                    value = "flagged",
                                                    tests = c("equal", "gbif", 
                                                              "institutions", 
                                                              "outliers", "seas",
                                                              "zeros"))

invalid_coords <- mimosa[mimosa_coordFlagged == FALSE, ] # subsetting flagged records
mimosa_coordClean <- mimosa[mimosa_coordFlagged  == TRUE, ] # subsetting valid records

# Writing *.csv for subsequent analyses 
write.csv(mimosa_coordClean, file = "datasets/mimosa-pre_grid.csv", row.names = F, fileEncoding = "UTF-8")

