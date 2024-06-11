

#### Setup
library(ulex)
library(dplyr)
library(sf)
library(geodata)
library(osmdata)
library(stringr)

#### Areas dictionary
ken_sf <- gadm(country = "KEN", level = 3, path = tempdir()) |> st_as_sf()
nbo_sf <- ken_sf |>
  filter(NAME_1 %in% "Nairobi") |>
  rename(name = NAME_3) |>
  dplyr::select(name)

#### Roads dictionary
roads_sf <- opq(st_bbox(nbo_sf),
                timeout = 999) |>
  add_osm_feature(key = "highway", value = c("motorway",
                                             "trunk",
                                             "primary",
                                             "secondary",
                                             "tertiary",
                                             "unclassified")) |>
  osmdata_sf()
roads_sf <- roads_sf$osm_lines

roads_sf <- roads_sf |>
  filter(!is.na(name)) |>
  dplyr::select(name) |>
  mutate(name = name |> tolower())

#### Landmark dictionary
## Amenities
amenities_sf <- opq(st_bbox(nbo_sf),
                    timeout = 999) |>
  add_osm_feature(key = "amenity") |>
  osmdata_sf()

amenities_pnt_sf <- amenities_sf$osm_points
amenities_ply_sf <- amenities_sf$osm_polygons |>
  st_centroid()

amenities_sf <- bind_rows(amenities_pnt_sf,
                          amenities_ply_sf) |>
  dplyr::mutate(type = amenity)

## Bus Stops
busstops_sf <- opq(st_bbox(nbo_sf),
                   timeout = 999) |>
  add_osm_feature(key = "highway",
                  value = "bus_stop") |>
  osmdata_sf()
busstops_sf <- busstops_sf$osm_points
busstops_sf <- busstops_sf |>
  mutate(type = "bus_stop")

## Append
landmarks_sf <- bind_rows(amenities_sf,
                          busstops_sf) |>
  filter(!is.na(name)) |>
  dplyr::select(name, type) |>
  mutate(name = name |> tolower())

#### Augment Gazetteer
landmarks_aug_sf <- augment_gazetteer(landmarks_sf)

#### Locate Event
tweets <- c("crash occurred near garden city on thika road towards roysambu",
            "crash at garden city",
            "crash at intersection of juja road and outer ring rd",
            "crash occured near roysambu on thika rd",
            "crash near mathare centre along juja road")

crashes_sf <- locate_event(text = tweets,
                           landmark_gazetteer = landmarks_aug_sf,
                           areas = nbo_sf,
                           roads = roads_sf,
                           event_words = c("accident", "crash", "collision"),
                           quiet = F)
