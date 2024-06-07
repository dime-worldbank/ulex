

# Landmarks
landmark_gazetteer_orig <- readRDS(file.path(algorithm_inputs, "gazetteers_raw","merged", "gazetter_allsources_raw.Rds"))
landmark_gazetteer_orig <- landmark_gazetteer_orig[!is.na(landmark_gazetteer_orig$lat),]
landmark_gazetteer_orig <- landmark_gazetteer_orig[grepl("garden city|yaya center|roasters|pangani|roysambu", landmark_gazetteer_orig$name),]
coordinates(landmark_gazetteer_orig) <- ~lon+lat
crs(landmark_gazetteer_orig) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
landmark_gazetteer_orig@data <- landmark_gazetteer_orig@data %>%
  dplyr::select(name, type)

##
roads_nairobi <- readRDS(file.path(algorithm_inputs, "roads_augmented", "osm_roads_aug.Rds"))
roads_nairobi <- roads_nairobi[grepl("mombasa|thika|juja|outer ring", roads_nairobi$name),]

##
areas_nairobi <- readRDS(file.path(algorithm_inputs, "nairobi_estates", "nairobi_estates.Rds"))
areas_nairobi@data <- areas_nairobi@data %>%
  dplyr::rename(name = estate)
areas_nairobi <- areas_nairobi[areas_nairobi$name %in% c("kilimani", "parklands", "upper hill"),]

## TYPE
landmark_sf <- landmark_gazetteer_orig %>% st_as_sf()
roads_sf <- roads_nairobi %>% st_as_sf()
areas_sf <- areas_nairobi %>% st_as_sf()

st_write(landmark_sf, file.path("~/Documents/Github/Unique-Location-Extractor/data/example_landmarks.geojson"), delete_dsn=T)
st_write(roads_sf, file.path("~/Documents/Github/Unique-Location-Extractor/data/example_roads.geojson"), delete_dsn=T)
st_write(areas_sf, file.path("~/Documents/Github/Unique-Location-Extractor/data/example_areas.geojson"), delete_dsn=T)

# TEST EXAMPLE
landmarks <- st_read("https://raw.githubusercontent.com/ramarty/Unique-Location-Extractor/master/data/example_landmarks.geojson")
neighborhoods <- st_read("https://raw.githubusercontent.com/ramarty/Unique-Location-Extractor/master/data/example_areas.geojson")
roads <- st_read("https://raw.githubusercontent.com/ramarty/Unique-Location-Extractor/master/data/example_roads.geojson")

landmarks_aug <- augment_gazetteer(landmarks,
                                   crs_distance = "+init=epsg:21037")

tweets <- c("crash occurred near garden city on thika road on your way towards roysambu",
            "crash at garden city",
            "crash at intersection of juja road and outer ring rd",
            "crash occured near roysambu on thika rd",
            "crash at pangani")
crash_locs <- locate_event(text = tweets, 
                           landmark_gazetteer = landmarks_aug,
                           areas = neighborhoods,
                           roads = roads,
                           crs_distance = "+init=epsg:21037",
                           quiet = F)

leaflet() %>%
  addTiles() %>%
  addCircles(data=crash_locs, 
             label = ~text,
             opacity = 1,
             weight=10,
             color = "red")

leaflet() %>%
  addTiles() %>%
  addCircles(data=landmarks[grepl("garden city", landmarks$name),],
             color="red",
             label = ~name) 

leaflet() %>%
  addTiles() %>%
  addPolylines(data=roads,
               label=~name)


roads_w <- roads  %>% spTransform("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

leaflet() %>%
  addTiles() %>%
  #addPolylines(data=roads_w) %>%
  addCircles(data=landmarks_aug[landmarks_aug$name %in% "garden city",],
             color="red") %>%
  addPolylines(data=roads_nairobi, label = name)

l <- landmarks[grepl("garden city", landmarks$name),] %>% as("Spatial") %>% spTransform("+init=epsg:21037")

a <- extract_dominant_cluster(l)


leaflet() %>%
  addTiles() %>%
  addCircles(data=crash_locs, 
             label = ~text,
             opacity = 1,
             weight=10,
             color = "red")


leaflet() %>%
  addTiles() %>%
  addCircles(data=landmarks_aug[landmarks_aug$name %in% "garden city",])



text = "crash at intersection of juja road and outer ring rd"
text_i = text
landmark_gazetteer = landmarks_aug
landmark_gazetteer.name_var = "name"
landmark_gazetteer.type_var = "type"
landmark_gazetteer.gs_var = "general_specific"
roads = roads
roads.name_var = "name"
areas = neighborhoods
areas.name_var = "name"
prepositions_list = list(c("at", "next to","around", 
                           "just after", "opposite","opp", 
                           "apa", "hapa","happened at",
                           "just before","at the","outside",
                           "right before"),
                         c("near", "after", "toward",
                           "along", "towards", "approach"),
                         c("past","from","on"))
event_words = c("accidents", "accident", "crash", 
                "overturn", "collision", "wreck")
junction_words = c("intersection", "junction")
false_positive_phrases = "" 
type_list = "" # NOT IMPLEMENTED YET
clost_dist_thresh = 500
fuzzy_match = TRUE
fuzzy_match.min_word_length = c(5,11)
fuzzy_match.dist = c(1,2)
fuzzy_match.ngram_max = 3
fuzzy_match.first_letters_same = TRUE
fuzzy_match.last_letters_same = TRUE
crs_distance = "+init=epsg:21037"
crs_out = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
quiet = T









