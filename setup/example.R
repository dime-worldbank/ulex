# Crashmap Algorithm

# Package Dependencies ---------------------------------------------------------
library(magrittr)
library(lubridate)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(tidytext)
library(stringr)
library(stringi)
library(ngram)
library(hunspell)
library(stringdist)
library(tm)
library(raster)
library(rgeos)
library(parallel)
library(jsonlite)
library(maptools)
library(sf)

# Algorithm Inputs -------------------------------------------------------------
tweet <- "crash near airtel on mombasa rd words words words yaya center kenyatta ave westlands"

# Load Data ------------------------------------------------------------------
AUG_GAZ <- F

if(AUG_GAZ){
  landmark_gazetteer_orig <- readRDS(file.path(algorithm_inputs, "gazetteers_augmented", "gazetteer_aug.Rds"))
} else{
  landmark_gazetteer_orig <- readRDS(file.path(algorithm_inputs, "gazetteers_raw","merged", "gazetter_allsources_raw.Rds"))
}

landmark_gazetteer_orig <- landmark_gazetteer_orig[!is.na(landmark_gazetteer_orig$lat),]
coordinates(landmark_gazetteer_orig) <- ~lon+lat
crs(landmark_gazetteer_orig) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

roads_nairobi <- readRDS(file.path(algorithm_inputs, "roads_augmented", "osm_roads_aug.Rds"))

areas_nairobi <- readRDS(file.path(algorithm_inputs, "nairobi_estates", "nairobi_estates.Rds"))
areas_nairobi@data <- areas_nairobi@data %>%
  dplyr::rename(name = estate)

landmarks <- landmark_gazetteer_orig

# Parameters -----------------------------------------------------------------
prepositions <- list(c("at", "next to","around", "just after", "opposite","opp", "apa", "hapa","happened at","just before","at the","outside","right before"),
                          c("near", "after", "toward","along", "towards", "approach"),
                          c("past","from","on"))
event <- c("accidents", "accident", "crash", "overturn", "collision", "wreck")
junction <- c("intersection", "junction")
false_positive <- c("githurai bus", "githurai matatu", 
                            "githurai 45 bus", "githurai 45 matatu",
                            "city hoppa bus", "hoppa bus",
                            "rongai bus", "rongai matatu", "rongai matatus",
                            "machakos bus", "machakos minibus", "machakos matatu")
type <- c("")

# Algorithm --------------------------------------------------------------------
source("~/Documents/Github/Unique-Location-Extractor/R/locate_event.R")
source("~/Documents/Github/Unique-Location-Extractor/R/helper_functions.R")

text_all <- c("",
              "accident at garden city",
              "")

alg_out_sf <- locate_event(text = tweet,
                           landmark_gazetteer = landmark_gazetteer_orig, 
                           roads = roads_nairobi, 
                           areas = areas_nairobi, 
                           prepositions_list = prepositions, 
                           event_words = event, 
                           junction_words = junction, 
                           false_positive_phrases = false_positive, 
                           type_list = type, 
                           clost_dist_thresh = 500,
                           fuzzy_match = TRUE,
                           fuzzy_match.min_word_length = c(5,11),
                           fuzzy_match.dist = c(1,2),
                           fuzzy_match.ngram_max = 3,
                           fuzzy_match.first_letters_same = TRUE,
                           fuzzy_match.last_letters_same = TRUE,
                           crs_distance = "+init=epsg:21037", 
                           crs_out = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                           quiet = F)






