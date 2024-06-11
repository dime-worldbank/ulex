library(ulex)

lm_sf <- data.frame(name = c("white house",
                             "the world bank group",
                             "the george washington university"),
                    lat = c(38.897778,
                            38.89935,
                            38.9007),
                    lon = c(-77.036389,
                            -77.04275,
                            -77.0508),
                    type = c("building", "building", "building")) |>
  sf::st_as_sf(coords = c("lon", "lat"),
               crs = 4326)

lm_aug_sf <- augment_gazetteer(lm_sf)


library(ulex)
library(geodata)





