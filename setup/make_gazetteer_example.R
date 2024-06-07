

library(osmdata)
library(raster)
library()

#### Define Area of Interest
kenya <- getData('GADM', country='KEN', level=3)
nairobi <- kenya[kenya$NAME_1 %in% "Nairobi",]

#### OSM Prep
q <- opq(bbox = nairobi %>% extent() %>% as.vector(),
         timeout = 9999)
roads <- q %>%
  add_osm_feature(key = 'highway', value = 'motorway') %>%
  osmdata_sf()


