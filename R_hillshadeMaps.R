library(raster)
library(tidyverse)
library(ggnewscale)
#library(ggsn)
library(scico)
source("C:/Users/evanmuis.stu/Sync/Non-Masters/ggsn/R/north.R")
source("C:/Users/evanmuis.stu/Sync/Non-Masters/ggsn/R/scalebar.R")
library(sf)
library(tictoc)
library(viridis)
library(scales)
library(ggthemes)

inPark = "Gar"

#add more joinTables when needed
disturbanceJoin <- read_csv("joinTables/disturbance.csv")
lccJoin <- read_csv("joinTables/lcc.csv")
lightsJoin <- read_csv("joinTables/nightlights.csv")
parkJoin <- read_csv("joinTables/parkNames.csv")
structJoin <- read_csv("joinTables/structure.csv")

meanVars <- unique(structJoin$var)
structVars <- meanVars[1:6]
elevVars <- meanVars[7:9]

#functions used in main function


makeMaps <- function(inPark){
  tic(paste("all", inPark,  "maps"))
  getScale <- function(boundaries){
    lims <- st_bbox(boundaries)
    totalM <- unname(lims$xmax - lims$xmin)
    
    if (totalM < 10000) {
      dist_unit = "km"
      dist = (((totalM / 3) - ((totalM / 3) %% 1000)) / 2) / 1000
    }
    else {
      dist_unit = "km"
      totalKm = totalM / 1000
      dist = (((totalKm / 4) - ((totalKm / 4) %% 10)) / 2)
    }
    
    return(c(dist, dist_unit))
  }
  
  distMap <- function(){ 
    
    #dataset to go on top of hillshade
    dataset = raster(paste0("outputs/", inPark, "/rasters/", inPark, "_PACE-disturbance_all.tif"))
    dataPixels = as(dataset, "SpatialPixelsDataFrame")
    dataPixels = as.data.frame(dataPixels)
    colnames(dataPixels) <- c("Value", "x", "y")
    
    #only for disturbance, need an if statement chain for vlce, etc.
    dataPixels <- left_join(dataPixels, disturbanceJoin, by = c("Value" = "class_val"))
    dataPixels$class <- fct_reorder(dataPixels$class, dataPixels$relevel, .fun = min)
    dataPixels <- dataPixels %>% filter(Value != 10, Value != 0)
    
    #plot Using geom_tile
    figure <- hillFig +
      #geom_raster(data = dataPixels, aes(x = x, y = y, fill = class), alpha = 0.6) +
      #scale_fill_manual(values=c("#828282", "#ff0000", "#009900", "#00a9e6", "#000000", "#c8c8c8")) +
      geom_tile(data = dataPixels, aes(x = x, y = y, fill = class)) +
      scale_fill_scico_d(palette = "batlow", alpha = 0.80) +
      geom_sf(data = boundaries, aes(colour = ppa_gpe), fill = NA, alpha = 0.4) +
      scale_colour_manual(values = c("grey60", "black")) +
      labs(fill = "Disturbance",
           colour = "") +
      north(boundaries, location = "bottomright") + 
      scalebar(boundaries, dist = dist, dist_unit = dist_unit,
               transform = F, location = "bottomleft",
               st.size = 3)  +
      guides(fill = guide_legend(order = 1), 
             colour = guide_legend(order = 2, reverse = T))
    
    ggsave(paste0("outputs/", inPark, "/plots/disturbance_map.png"),
           figure,  device = "png", height = 7, width = 7)
  }
  
  lccMap <- function(){ 
    
    #dataset to go on top of hillshade
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
      north(boundaries, location = "bottomright") + 
      scalebar(boundaries, dist = dist, dist_unit = dist_unit,
               transform = F, location = "bottomleft",
               st.size = 3)  +
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
    dataPixels <- left_join(dataPixels, lccJoin, by = c("Value" = "class_val"))
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
      north(boundaries, location = "bottomright") + 
      scalebar(boundaries, dist = dist, dist_unit = dist_unit,
               transform = F, location = "bottomleft",
               st.size = 3)  +
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
    dataPixels <- left_join(dataPixels, lightsJoin, by = c("Value" = "class_val"))
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
      north(boundaries, location = "bottomright") + 
      scalebar(boundaries, dist = dist, dist_unit = dist_unit,
               transform = F, location = "bottomleft",
               st.size = 3)  +
      guides(fill = guide_legend(order = 1),
             colour = guide_legend(order = 2, reverse = T))
    
    ggsave(paste0("outputs/", inPark, "/plots/nightlights_map.png"),
           figure,  device = "png", height = 7, width = 7)
  } 
  
  structMap <- function(variable){
    tic(paste(variable, "map"))
    
    varInfo <- structJoin %>% filter(var == variable)
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
      north(boundaries, location = "bottomright") + 
      scalebar(boundaries, dist = dist, dist_unit = dist_unit,
               transform = F, location = "bottomleft",
               st.size = 3) +
      guides(fill = guide_colourbar(order = 1, ticks = F),
             colour = guide_legend(order = 2, reverse = T))
    
    ggsave(paste0("outputs/", inPark, "/plots/", variable, "_map.png"),
           figure,  device = "png", height = 7, width = 7)
    toc()
  } 
  
  terrainMap <- function(variable){
    tic(paste(variable, "map"))
    
    varInfo <- structJoin %>% filter(var == variable)
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
      north(boundaries, location = "bottomright") + 
      scalebar(boundaries, dist = dist, dist_unit = dist_unit,
               transform = F, location = "bottomleft",
               st.size = 3) +
      guides(fill = guide_colourbar(order = 1),
             colour = guide_legend(order = 2, reverse = T))
    
    ggsave(paste0("outputs/", inPark, "/plots/", variable, "_map.png"),
           figure,  device = "png", height = 7, width = 7)
    toc()
  } 
  
  boundaries <- read_sf(paste0("outputs/", inPark, "/", inPark, "_PACE.shp"))
  
  scaleParams <- getScale(boundaries)
  dist <- as.numeric(scaleParams[1])
  dist_unit = scaleParams[2]
  
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
    new_scale_fill()
  
  tic("disturbance map")
  #distMap()
  toc()
  
  tic("lcc map")
  #lccMap()
  toc()
  
  tic("fNoF map")
  #fNoFMap()
  toc()
  
  tic("nightlights map")
  lightMap()
  toc()
  
  tic("structure maps")
  #map(structVars, structMap)
  #structMap(structVars[5])
  toc()
  
  tic("terrain maps")
  #map(elevVars, terrainMap)
  toc()
  
  toc()
}

#makeMaps("Gar")


parks <- c("Gar", "Spats", "Strath", "JoffreLakesPark", "MountSeymourPark", "StuartRiverParkLowerSite")

map(parks, makeMaps)


#ggplot(boundaries) +
#geom_sf() +
#north(boundaries) +
#scalebar(boundaries, dist = dist, dist_unit = dist_unit,
#         transform = F, location = "bottomleft",
#         st.size = 3) +
#theme_void()
#variable = "loreys_height"

