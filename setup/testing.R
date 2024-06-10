# Test Package

# Setup ------------------------------------------------------------------------

#### Install package
## Install/Load Package Dependencies
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, tidyr, readr, purrr, tidytext,
               stringr, stringi, ngram, hunspell, stringdist, tm,
               parallel, sf, quanteda, geodist, raster)

library(spacyr)

## Load ULEx Functions
source("~/Documents/Github/ulex/R/helper_functions.R")
source("~/Documents/Github/ulex/R/augment_gazetteer.R")
source("~/Documents/Github/ulex/R/locate_event.R")

# Load data --------------------------------------------------------------------
library(geodata)
library(osmdata)
library(leaflet)
library(stringr)

#### Areas
# ken_sf <- gadm(country = "KEN", level = 1, path = tempdir()) %>% st_as_sf()
# nbo_sf <- ken_sf %>%
#   filter(NAME_1 %in% "Nairobi") %>%
#   rename(name = NAME_3) %>%
#   dplyr::select(name)

ken_sf <- gadm(country = "KEN", level = 3, path = tempdir()) %>% st_as_sf()
nbo_sf <- ken_sf %>%
  filter(NAME_1 %in% "Nairobi") %>%
  rename(name = NAME_3) %>%
  dplyr::select(name)

#### Roads
roads_sf <- opq(st_bbox(nbo_sf),
                timeout = 999) %>%
  add_osm_feature(key = "highway", value = c("motorway",
                                             "trunk",
                                             "primary",
                                             "secondary",
                                             "tertiary",
                                             "unclassified")) %>%
  osmdata_sf()
roads_sf <- roads_sf$osm_lines

roads_sf <- roads_sf %>%
  filter(!is.na(name)) %>%
  dplyr::select(name) %>%
  mutate(name = name %>% tolower())

#### Landmarks
## Amenities
amenities_sf <- opq(st_bbox(nbo_sf),
                    timeout = 999) %>%
  add_osm_feature(key = "amenity") %>%
  osmdata_sf()

amenities_pnt_sf <- amenities_sf$osm_points
amenities_ply_sf <- amenities_sf$osm_polygons %>%
  st_centroid()

amenities_sf <- bind_rows(amenities_pnt_sf,
                          amenities_ply_sf) %>%
  dplyr::mutate(type = amenity)

## Bus Stops
busstops_sf <- opq(st_bbox(nbo_sf),
                   timeout = 999) %>%
  add_osm_feature(key = "highway",
                  value = "bus_stop") %>%
  osmdata_sf()
busstops_sf <- busstops_sf$osm_points
busstops_sf <- busstops_sf %>%
  mutate(type = "bus_stop")

## Append
landmarks_sf <- bind_rows(amenities_sf,
                          busstops_sf) %>%
  filter(!is.na(name)) %>%
  dplyr::select(name, type) %>%
  mutate(name = name %>% tolower())

# Geocode ----------------------------------------------------------------------
#landmark_sf     <- st_read("~/Documents/Github/ulex/data/example_landmarks.geojson")
#neighborhood_sf <- st_read("~/Documents/Github/ulex/data/example_areas.geojson")
#roads_sf        <- st_read("~/Documents/Github/ulex/data/example_roads.geojson")

landmarks_aug_sf <- augment_gazetteer(landmarks_sf,
                                      quiet = F)

landmarks_aug_sf <- landmarks_aug_sf %>%
  dplyr::filter(!(name %in% c("hospital")))

landmarks_sf <- landmarks_sf %>%
  dplyr::filter(!(name %in% c("hospital")))

#### Locate Crashes in example tweets
tweets <- c("crash occurred near garden city on thika road on your way towards roysambu",
            "crash at garden city",
            "crash at intersection of juja road and outer ring rd",
            "crash occured near roysambu on thika rd",
            "crash near mathare centre along juja road")

crashes_sf <- locate_event(text = tweets,
                           landmark_gazetteer = landmarks_aug_sf,
                           areas = nbo_sf,
                           roads = roads_sf,
                           event_words = c("accident", "crash", "collision", "wreck", "overturn"),
                           quiet = F)

tweet_i <- "agtgaffraeggfefaer"
crashes_sf <- locate_event(text = tweet_i,
                           landmark_gazetteer = landmarks_aug_sf,
                           areas = nbo_sf,
                           roads = roads_sf,
                           event_words = c("accident", "crash", "collision", "wreck", "overturn"),
                           quiet = F)

crashes_sf
aa

 aa %>%
  st_as_sf() %>%
  add_latlon_vars_to_sf()

 data_sf <- aa %>% st_as_sf()
 if( !("lon" %in% names(data_sf)) ){

   coord_df <- data_sf %>%
     st_centroid() %>%
     st_coordinates() %>%
     as.data.frame()

   names(coord_df) <- c(lon_name, lat_name)

   data_sf <- data_sf %>%
     bind_cols(coord_df)

 }

crashes_sf


text = tweets[1]
landmark_gazetteer = landmark_aug_sf
areas = neighborhood_sf
roads = roads_sf
event_words = c("accident", "crash", "collision", "wreck", "overturn")
quiet = F

text_i <- text

text
landmark_gazetteer
landmark_gazetteer.name_var = "name"
landmark_gazetteer.type_var = "type"
landmark_gazetteer.gs_var = "general_specific"
roads
roads.name_var = "name"
areas
areas.name_var = "name"
prepositions_list = list(c("at", "next to","around",
                           "just after", "opposite","opp",
                           "apa", "hapa","happened at",
                           "just before","at the","outside",
                           "right before"),
                         c("near", "after", "toward",
                           "along", "towards", "approach"),
                         c("past","from","on"))
prep_check_order = "prep_then_pattern"
event_words = c("accidents", "accident", "crash",
                "overturn", "collision", "wreck")
junction_words = c("intersection", "junction")
false_positive_phrases = ""
type_list = NULL
clost_dist_thresh = 500
fuzzy_match = TRUE
fuzzy_match.min_word_length = c(5,11)
fuzzy_match.dist = c(1,2)
fuzzy_match.ngram_max = 3
fuzzy_match.first_letters_same = TRUE
fuzzy_match.last_letters_same = TRUE
crs_distance = 4326
crs_out = 4326
quiet = T
mc_cores = 1


