library(raster)
library(tidyverse)
library(ggnewscale)
library(ggsn)
library(scico)
library(sf)

disturbanceJoin <- read_csv("joinTables/disturbance.csv")

#generate hillshade
parkDEM = raster("outputs/Gar/rasters/Gar_PACE-elevation.tif")

parkSLOPE = terrain(parkDEM, opt = "slope")
parkASPECT = terrain(parkDEM, opt = "aspect")
parkHILL = hillShade(parkSLOPE,parkASPECT)

#convert to spatial pixel data frame
parkPixels = as(parkHILL, "SpatialPixelsDataFrame")
parkPixels = as.data.frame(parkPixels)
colnames(parkPixels) <- c("Elev", "x", "y")

#dataset to go on top of hillshade
dataset = raster("outputs/Gar/rasters/Gar_PACE-disturbance_all.tif")
dataPixels = as(dataset, "SpatialPixelsDataFrame")
dataPixels = as.data.frame(dataPixels)
colnames(dataPixels) <- c("Value", "x", "y")

#only for disturbance, need an if statement chain for vlce, etc.
dataPixels <- left_join(dataPixels, disturbanceJoin, by = c("Value" = "class_val"))
dataPixels$class <- fct_reorder(dataPixels$class, dataPixels$relevel, .fun = min)
dataPixels <- dataPixels %>% filter(Value != 10)

#park polygon
boundaries <- read_sf("outputs/Gar/Gar_PACE.shp")

#plot Using geom_tile
ggplot() +
  geom_raster(data = parkPixels, aes(x = x, y = y, fill = Elev), show.legend = FALSE) +
  scale_fill_gradient(low = "gray30", high = "gray99") +
  new_scale_fill() +
  geom_raster(data = dataPixels, aes(x = x, y = y, fill = class), alpha = 0.4) +
  scale_fill_manual(values=c("#828282", "#ff0000", "#009900", "#00a9e6", "#000000", "#c8c8c8")) +
  #geom_raster(data = dataPixels, aes(x = x, y = y, fill = class) +
  #scale_fill_scico_d(palette = "batlow", alpha = 0.60) +
  geom_sf(data = boundaries, aes(colour = fct_rev(ppa_gpe)), fill = NA, alpha = 0.4) +
  scale_colour_manual(values = c("black", "grey60")) +
  theme_void() +
  theme(axis.title = element_blank()) +
  coord_sf() +
  labs(fill = "Disturbance",
       colour = "")

ggsave("outputs/Gar/plots/egg.png", device = "png")

ggplot() +
  geom_sf(data = boundaries, aes(colour = ppa_gpe), fill = NA) +
  scale_colour_manual(values = c("grey60", "black"))
