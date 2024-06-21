library(ulex)
library(sf)

## Landmarks
landmarks_sf <- data.frame(lat = runif(3),
                           lon = runif(3),
                           name = c("restaurant", "bank", "hotel"),
                           type = c("poi", "poi", "poi")) |>
  st_as_sf(coords = c("lon", "lat"),
           crs = 4326)

## Road
coords <- matrix(runif(4), ncol = 2)
road_sf <- coords |>
  st_linestring() |>
  st_sfc(crs = 4326)
road_sf <- st_sf(geometry = road_sf)
road_sf$name <- "main st"

## Area
n <- 5
coords <- matrix(runif(2 * n, min = 0, max = 10), ncol = 2)
coords <- rbind(coords, coords[1,])
polygon <- st_polygon(list(coords))
area_sf <- st_sfc(polygon, crs = 4326)
area_sf <- st_sf(geometry = area_sf)
area_sf$name <- "place"

## Locate Event
event_sf <- locate_event(text = "accident near hotel",
                         landmark_gazetteer = landmarks_sf,
                         roads = road_sf,
                         areas = area_sf,
                         event_words = c("accident", "crash"))

