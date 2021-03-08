library(raster)
library(tidyverse)
library(ggnewscale)
library(ggspatial)
library(scico)
library(sf)
library(viridis)
library(scales)
library(tictoc)

inPark = "AKK"

#add more joinTables when needed
disturbance_join <- read_csv("joinTables/disturbance.csv")
lcc_join <- read_csv("joinTables/lcc.csv")
nightlights_join <- read_csv("joinTables/nightlights.csv")
parks_join <- read_csv("joinTables/parkNames.csv")
structure_join <- read_csv("joinTables/structure.csv")

meanVars <- unique(structure_join$var)
structVars <- meanVars[4:6]
elevVars <- meanVars[7:9]

#functions used in main function

makeMaps <- function(inPark){
  tic(paste("all", inPark,  "maps"))
  
  distMap <- function(){ 
    
    #dataset to go on top of hillshade
    dataset = raster(paste0("outputs/", inPark, "/rasters/", inPark, "_PACE-disturbance_all.tif"))
    dataPixels = as(dataset, "SpatialPixelsDataFrame")
    dataPixels = as.data.frame(dataPixels)
    colnames(dataPixels) <- c("Value", "x", "y")
    
    #only for disturbance, need an if statement chain for vlce, etc.
    dataPixels <- left_join(dataPixels, disturbance_join, by = c("Value" = "class_val"))
    
    disturbance_classes <- c(1, 2)
    dataPixels <- dataPixels %>% filter(Value %in% disturbance_classes)
    
    colours <- dataPixels %>% pull(colour) %>% unique()
    
    #plot Using geom_tile
    figure <- hillFig +
      #geom_raster(data = dataPixels, aes(x = x, y = y, fill = class), alpha = 0.6) +
      geom_tile(data = dataPixels, aes(x = x, y = y, fill = class)) +
      scale_fill_manual(values=colours) +
      #scale_fill_scico_d(palette = "batlow", alpha = 0.80) +
      geom_sf(data = boundaries, aes(colour = ppa_gpe), fill = NA, alpha = 0.4) +
      scale_colour_manual(values = c("grey60", "black")) +
      labs(fill = "Disturbance",
           colour = "") +
      guides(fill = guide_legend(order = 1), 
             colour = guide_legend(order = 2, reverse = T)) +
      theme(legend.position = "none")
    
    ggsave(paste0("outputs/", inPark, "/plots/disturbance_map.png"),
           figure,  device = "png", height = 7, width = 7,
           bg = "transparent")
    
    figure
  }
  
  lccMap <- function(){ 
    
    #dataset to go on top of hillshade
    dataset = raster(paste0("outputs/", inPark, "/rasters/", inPark, "_PACE-2015-HMM.tif"))
    dataPixels = as(dataset, "SpatialPixelsDataFrame")
    dataPixels = as.data.frame(dataPixels)
    colnames(dataPixels) <- c("Value", "x", "y")
    
    #only for disturbance, need an if statement chain for vlce, etc.
    dataPixels <- left_join(dataPixels, lcc_join, by = c("Value" = "class_val"))
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
    
    ggsave(paste0("outputs/", inPark, "/plots/lcc_map.png"), 
           figure,  device = "png", height = 7, width = 7)
  }
  
  fNoFMap <- function(){ 
    
    #dataset to go on top of hillshade
    dataset = raster(paste0("outputs/", inPark, "/rasters/", inPark, "_PACE-2015-HMM.tif"))
    dataPixels = as(dataset, "SpatialPixelsDataFrame")
    dataPixels = as.data.frame(dataPixels)
    colnames(dataPixels) <- c("Value", "x", "y")
    
    #only for disturbance, need an if statement chain for vlce, etc.
    dataPixels <- left_join(dataPixels, lcc_join, by = c("Value" = "class_val"))
    dataPixels$class <- fct_reorder(dataPixels$class, dataPixels$relevel, .fun = min)
    dataPixels <- dataPixels %>% filter(Value != 0)
    
    #plot Using geom_tile
    figure <- hillFig +
      geom_tile(data = dataPixels, aes(x = x, y = y, fill = fNoF)) +
      scale_fill_manual(values = scico(11, palette = "batlow", alpha = .50)[c(5, 8, 11)]) +
      geom_sf(data = boundaries, aes(colour = ppa_gpe), fill = NA, alpha = 0.4) +
      scale_colour_manual(values = c("grey60", "black")) +
      labs(fill = "Land Cover",
           colour = "") +
      guides(colour = guide_legend(reverse = T)) +
      guides(fill = guide_legend(order = 1), 
             colour = guide_legend(order = 2, reverse = T))
    
    ggsave(paste0("outputs/", inPark, "/plots/lcc_fNoF_map.png"),
           figure,  device = "png", height = 7, width = 7)
  }
  
  lightMap <- function(){ 
    
    #dataset to go on top of hillshade
    dataset = raster(paste0("outputs/", inPark, "/rasters/", inPark, "_PACE-2015-nightlights.tif"))
    dataPixels = as(dataset, "SpatialPixelsDataFrame")
    dataPixels = as.data.frame(dataPixels)
    colnames(dataPixels) <- c("Value", "x", "y")
    
    #only for disturbance, need an if statement chain for vlce, etc.
    dataPixels <- left_join(dataPixels, nightlights_join, by = c("Value" = "class_val"))
    dataPixels$class <- fct_reorder(dataPixels$class, dataPixels$relevel, .fun = min)
    dataPixels <- dataPixels %>% filter(Value != 1)
    
    #plot Using geom_tile
    figure <- hillFig +
      geom_tile(data = dataPixels, aes(x = x, y = y, fill = class)) +
      scale_fill_scico_d(palette = "batlow", alpha = 0.50) +
      geom_sf(data = boundaries, aes(colour = ppa_gpe), fill = NA, alpha = 0.4) +
      scale_colour_manual(values = c("grey60", "black")) +
      labs(fill = "Human Development",
           colour = "") +
      guides(colour = guide_legend(reverse = T)) +
      guides(fill = guide_legend(order = 1),
             colour = guide_legend(order = 2, reverse = T))
    
    ggsave(paste0("outputs/", inPark, "/plots/nightlights_map.png"),
           figure,  device = "png", height = 7, width = 7)
  } 
  
  structMap <- function(variable){
    tic(paste(variable, "map"))
    
    varInfo <- structure_join %>% filter(var == variable)
    divideBy <- varInfo[["divideBy"]]
    name = varInfo[["var2s"]]
    
    #dataset to go on top of hillshade
    if (variable %in% structVars){
      dataset = raster(paste0("outputs/", inPark, "/rasters/", inPark, "_PACE-2015-", variable, ".tif"))
    }
    
    else if (variable %in% elevVars){
      dataset = raster(paste0("outputs/", inPark, "/rasters/", inPark, "_PACE-", variable, ".tif"))
    }
    
    dataPixels = as(dataset, "SpatialPixelsDataFrame")
    dataPixels = as.data.frame(dataPixels)
    colnames(dataPixels) <- c("Value", "x", "y")
    dataPixels = dataPixels %>% mutate(Value = Value / divideBy)
    
    if (variable == "elev_cv"){
      dataPixels <- dataPixels %>% filter(Value < 1)
    } 
    
    
    myLims <- c(quantile(dataPixels$Value, 0.25), 
                quantile(dataPixels$Value, 0.75))
    #breaks[1] = 1
    #breaks[length(breaks)] = floor(max(dataPixels$Value))
    dataPixels$Value <- squish(dataPixels$Value, range = myLims)
    breaks <- c(min(dataPixels$Value) + max(dataPixels$Value) / 100, 
                max(dataPixels$Value) - max(dataPixels$Value) / 100)

    #plot Using geom_tile
    figure <- hillFig +
      geom_raster(data = dataPixels, aes(x = x, y = y, fill = Value)) +
      scale_fill_scico(palette = "bamako", alpha = 0.8, direction = -1,
                       breaks = breaks,
                       #limits = myLims,
                       labels = c("Low", "High"),
                       #oob = scales::squish(dataPixels$Value, range = myLims)
                       ) +
      geom_sf(data = boundaries, aes(colour = ppa_gpe), fill = NA, alpha = 0.4) +
      scale_colour_manual(values = c("grey60", "black")) +
      labs(fill = name,
           colour = "") +
      guides(fill = guide_colourbar(order = 1, ticks = F),
             colour = guide_legend(order = 2, reverse = T))
    
    ggsave(paste0("outputs/", inPark, "/plots/", variable, "_map.png"),
           figure,  device = "png", height = 7, width = 7)
    toc()
  } 
  
  terrainMap <- function(variable){
    tic(paste(variable, "map"))
    
    varInfo <- structure_join %>% filter(var == variable)
    divideBy <- varInfo[["divideBy"]]
    name = varInfo[["var2s"]]
    
    #dataset to go on top of hillshade

    dataset = raster(paste0("outputs/", inPark, "/rasters/", inPark, "_PACE-", variable, ".tif"))
    
    dataPixels = as(dataset, "SpatialPixelsDataFrame")
    dataPixels = as.data.frame(dataPixels)
    colnames(dataPixels) <- c("Value", "x", "y")
    dataPixels = dataPixels %>% mutate(Value = Value / divideBy)
    
    #if (variable == "aspect"){
    #  dataPixels <- dataPixels %>% mutate(Value = cos(Value))
    #} 
    
    breaks <- pretty(dataPixels$Value)
    breaks[1] = 1
    breaks[length(breaks)] = floor(max(dataPixels$Value))
    
    #plot Using geom_tile
    figure <- hillFig +
      geom_raster(data = dataPixels, aes(x = x, y = y, fill = Value)) +
      scale_fill_viridis(option = "magma", alpha = 0.6) +#, +
                         #breaks = breaks,
                         #limits = c(0, max(dataPixels$Value))) +
      geom_sf(data = boundaries, aes(colour = ppa_gpe), fill = NA, alpha = 0.4) +
      scale_colour_manual(values = c("grey60", "black")) +
      labs(fill = name,
           colour = "") +
      guides(fill = guide_colourbar(order = 1),
             colour = guide_legend(order = 2, reverse = T))
    
    ggsave(paste0("outputs/", inPark, "/plots/", variable, "_map.png"),
           figure,  device = "png", height = 7, width = 7)
    toc()
  } 
  
  boundaries <- read_sf(paste0("outputs/", inPark, "/", inPark, "_PACE.shp"))
  
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
    scale_fill_gradient(low = "grey30", high = "grey75") +
    theme_void() +
    theme(axis.title = element_blank()) +
    coord_sf() +
    new_scale_fill() +
    annotation_scale(location = "bl", width_hint = 0.4) +
    annotation_north_arrow(location = "br",
                           height = unit(1, "cm"),
                           width = unit(1, "cm"),
                           style = north_arrow_fancy_orienteering)
  
  dir.create(file.path("outputs", inPark, "plots"), showWarnings = F)
  
  tic("disturbance map")
  distMap()
  toc()
  
  tic("lcc map")
  lccMap()
  toc()
  
  tic("fNoF map")
  fNoFMap()
  toc()
  
  tic("nightlights map")
  lightMap()
  toc()
  
  tic("structure maps")
  map(structVars, structMap)
  toc()
  
  tic("terrain maps")
  map(elevVars, terrainMap)
  toc()
  
  toc()
}

makeMaps("Gar")
