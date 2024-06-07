library(hexSticker)
library(ggplot2)
library(rgeos)
library(rgdal)
library(raster)

set.seed(42)

landmark_gazetteer_orig <- readRDS(file.path(algorithm_inputs, "gazetteers_augmented", "gazetteer_aug.Rds"))

landmark_gazetteer_orig <- landmark_gazetteer_orig[!is.na(landmark_gazetteer_orig$lat),]
coordinates(landmark_gazetteer_orig) <- ~lon+lat
crs(landmark_gazetteer_orig) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

roads_nairobi <- readRDS(file.path(algorithm_inputs, "roads_augmented", "osm_roads_aug.Rds"))

areas_nairobi <- readRDS(file.path(algorithm_inputs, "nairobi_estates", "nairobi_estates.Rds"))
areas_nairobi@data <- areas_nairobi@data %>%
  dplyr::rename(name = estate)

#### Subset
area_sub <- areas_nairobi[1:40,] # 1:40
area_sub_buff <- gBuffer(area_sub, width=1/111.12, byid=F)
area_sub_buff$id <- 1
roads_sub <- roads_nairobi %>% crop(area_sub)
landmarks_sub <- landmark_gazetteer_orig %>% crop(area_sub)
landmarks_sub <- landmarks_sub[order(runif(nrow(landmarks_sub))),]
landmarks_sub <- landmarks_sub[1:50,] %>% as.data.frame() # 1:50

center_df <- gCentroid(area_sub) %>% as.data.frame()

extend <- .05
xmin = extent(area_sub)@xmin - extend
xmax = extent(area_sub)@xmax + extend

p <- ggplot() +
  geom_text(data=center_df, aes(x=x,y=y), 
  label="occured at intersection just before
the road next to landmark near the
occured at intersection just before
the road next to landmark near the
intersection just before",size=3,alpha=0.4) + 
  geom_polygon(data=area_sub_buff, aes(x=long, y=lat, group=group),
               fill="floralwhite", color="floralwhite", size=0.2) + 
  geom_polygon(data=area_sub, aes(x=long, y=lat, group=group),
               fill="floralwhite", color="black", size=0.25) + 
  geom_path(data=roads_sub, aes(x=long, y=lat, group=group),
            color="gray50", size=0.025) + 
  geom_point(data=landmarks_sub, aes(x=lon, y=lat),
             color="red", size=.015) +
  geom_point(data=landmarks_sub[10,], aes(x=lon, y=lat),
            color="black",  fill="green", size=2, pch=21) +
  xlim(xmin, xmax) +
  theme_void() +
  theme_transparent() 

sticker(p, package="ULEx", p_color = "black", s_width=2, s_height=1.4,
        s_x = 1,
        p_y=1.5,
        h_fill="floralwhite", h_color="black", filename="~/Documents/Github/Unique-Location-Extractor/man/figures/logo.png")
