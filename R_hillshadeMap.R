library(raster)
library(tidyverse)
library(ggnewscale)
library(ggsn)
library(scico)
library(sf)
library(tictoc)

setwd("C:/Users/evanmuis.stu/Sync/Masters/Data")

inPark = "Gar"

disturbanceJoin <- read_csv("joinTables/disturbance.csv")

distMap <- function(inPark){ 
  tic()
  #generate hillshade
  parkDEM = raster(paste0("outputs/", inPark, "/rasters/", inPark, "_PACE-elevation.tif"))
  
  parkSLOPE = terrain(parkDEM, opt = "slope")
  parkASPECT = terrain(parkDEM, opt = "aspect")
  parkHILL = hillShade(parkSLOPE,parkASPECT)
  
  #convert to spatial pixel data frame
  parkPixels = as(parkHILL, "SpatialPixelsDataFrame")
  parkPixels = as.data.frame(parkPixels)
  colnames(parkPixels) <- c("Elev", "x", "y")
  
  #dataset to go on top of hillshade
  dataset = raster(paste0("outputs/", inPark, "/rasters/", inPark, "_PACE-disturbance_all.tif"))
  dataPixels = as(dataset, "SpatialPixelsDataFrame")
  dataPixels = as.data.frame(dataPixels)
  colnames(dataPixels) <- c("Value", "x", "y")
  
  #only for disturbance, need an if statement chain for vlce, etc.
  dataPixels <- left_join(dataPixels, disturbanceJoin, by = c("Value" = "class_val"))
  dataPixels$class <- fct_reorder(dataPixels$class, dataPixels$relevel, .fun = min)
  dataPixels <- dataPixels %>% filter(Value != 10, Value != 0 )
  
  #park polygon
  boundaries <- read_sf(paste0("outputs/", inPark, "/", inPark, "_PACE.shp"))
  
  lims <- st_bbox(boundaries)
  
  dist = unname((((lims$xmax - lims$xmin) / 1000 / 3) - (((lims$xmax - lims$xmin) / 1000 / 3) %% 5)) / 2)
  
  #plot Using geom_tile
  figure <- ggplot() +
    geom_raster(data = parkPixels, aes(x = x, y = y, fill = Elev), show.legend = FALSE) +
    scale_fill_gradient(low = "gray30", high = "gray99") +
    new_scale_fill() +
    #geom_raster(data = dataPixels, aes(x = x, y = y, fill = class), alpha = 0.6) +
    #scale_fill_manual(values=c("#828282", "#ff0000", "#009900", "#00a9e6", "#000000", "#c8c8c8")) +
    geom_tile(data = dataPixels, aes(x = x, y = y, fill = class)) +
    scale_fill_scico_d(palette = "batlow", alpha = 0.80) +
    geom_sf(data = boundaries, aes(colour = ppa_gpe), fill = NA, alpha = 0.4) +
    scale_colour_manual(values = c("grey60", "black")) +
    theme_void() +
    theme(axis.title = element_blank()) +
    coord_sf() +
    labs(fill = "Disturbance",
         colour = "") +
    guides(colour = guide_legend(reverse = T)) +
    north(boundaries, location = "topleft") + 
    scalebar(boundaries, dist = dist, dist_unit = "km",
             transform = F, location = "bottomleft",
             st.size = 3)
  
  ggsave(paste0("outputs/", inPark, "/plots/disturbance_map.png"), figure,  device = "png")
  toc()
}

parks <- c("Gar", "Spats", "Strath", "JoffreLakesPark", "MountSeymourPark", "StuartRiverParkLowerSite")

map(parks, distMap)
