library(ggnewscale)
library(ggspatial)
inPark = "Gar"

boundaries <- read_sf(paste0("outputs/", inPark, "/", inPark, "_PACE.shp"))
lccJoin <- read_csv("joinTables/lcc.csv")

#generate hillshade
parkDEM = raster(paste0("outputs/", inPark, "/rasters/", inPark, "_PACE-elevation.tif"))
parkSLOPE = terrain(parkDEM, opt = "slope")
parkASPECT = terrain(parkDEM, opt = "aspect")

parkHILL = hillShade(parkSLOPE, parkASPECT)

#convert to spatial pixel data frame
parkPixels = as(parkHILL, "SpatialPixelsDataFrame")
parkPixels = as.data.frame(parkPixels)
colnames(parkPixels) <- c("Elev", "x", "y")

hillFig <- ggplot() +
  geom_raster(data = parkPixels, aes(x = x, y = y, fill = Elev), show.legend = FALSE) +
  scale_fill_gradient(low = "#000000", high = "#FFFFFF") +
  theme_void() +
  theme(axis.title = element_blank()) +
  coord_sf() +
  new_scale_fill() +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "br",
                         height = unit(1, "cm"),
                         width = unit(1, "cm"),
                         style = north_arrow_fancy_orienteering)

hillFig

dataset = raster(paste0("outputs/", inPark, "/rasters/", inPark, "_PACE-2015-HMM.tif"))
dataPixels = as(dataset, "SpatialPixelsDataFrame")
dataPixels = as.data.frame(dataPixels)
colnames(dataPixels) <- c("Value", "x", "y")

#only for disturbance, need an if statement chain for vlce, etc.
dataPixels <- left_join(dataPixels, lccJoin, by = c("Value" = "class_val"))
dataPixels$class <- fct_rev(fct_reorder(dataPixels$class, dataPixels$relevel, .fun = min))
dataPixels <- dataPixels %>% filter(Value != 0)

#plot Using geom_tile
figure <- hillFig +
  geom_tile(data = dataPixels, aes(x = x, y = y, fill = class)) +
  scale_fill_scico_d(palette = "batlow", alpha = 0.50) +
  geom_sf(data = boundaries, aes(colour = ppa_gpe), fill = NA, alpha = 0.4) +
  scale_colour_manual(values = c("grey60", "black")) +
  labs(fill = "Land Cover",
       colour = "") +
  guides(colour = guide_legend(reverse = F)) +
  guides(fill = guide_legend(order = 1),
         colour = guide_legend(order = 2, reverse = T))

figure
ggsave(paste0("outputs/", inPark, "/plots/lcc_map.png"), 
       figure,  device = "png", height = 7, width = 7)
