library(tmap)
library(raster)
library(tidyverse)
library(sf)
library(scico)
library(tictoc)

lcc_join <- read_csv("joinTables/lcc.csv")
fNoF_join <- read_csv("joinTables/fNoF.csv")
disturbance_join <- read_csv("joinTables/disturbance.csv")
nightlights_join <- read_csv("joinTables/nightlights.csv")
structure_join <- read_csv("joinTables/structure.csv")

reportcard_tmaps <- function(inPark, legend.show = F){
  tic("all")
  
  dir.create(file.path("outputs", inPark, "plots"), showWarnings = F)
  
  ppa <- read_sf(paste0("outputs/", inPark, "/", inPark, ".shp"))
  
  #generate hillshade
  rasterLoc = file.path("outputs", inPark, "rasters/")
  parkDEM = raster(file.path(rasterLoc, paste0(inPark, "_PACE-elevation.tif")))
  parkSLOPE = terrain(parkDEM, opt = "slope")
  parkASPECT = terrain(parkDEM, opt = "aspect")
  
  parkHILL = hillShade(parkSLOPE, parkASPECT)
  rm(parkDEM, parkSLOPE, parkASPECT)
  
  #make the start of the map, including scale bar and n arrow with a hillshade
  hillshade_map <- tm_shape(parkHILL, 
                            raster.downsample = F
  ) +
    tm_raster(palette = gray(0:100 / 100), 
              n = 100, 
              legend.show = F) +
    tm_layout(frame = F, bg.color = "#00000000") +
    tm_scale_bar(position = c("left", "bottom"),
                 bg.color = "white",
                 bg.alpha = 0) +
    tm_compass(position = c("right", "bottom"),
               bg.color = "white",
               bg.alpha = 0)
  
  # elevation maps, elevation and slope ----
  #lajolla palette
  
  elevation_function <- function(variable){
    if (variable == "elevation") {
      elevation_colours <- terrain.colors(10)
    } else {
      elevation_colours <- scico(10, palette = "lajolla")
    }
    tic(variable)
    inRaster <- raster(file.path(rasterLoc, paste0(inPark, "_Pace-", variable, ".tif")))
    
    created_map <- hillshade_map +
      tm_shape(inRaster, raster.downsample = F) +
      tm_raster(alpha = .65, palette = elevation_colours, style = "cont", legend.show = legend.show) +
      tm_shape(ppa) +
      tm_borders(col = "black", lwd = 2)
    
    save_name = file.path("outputs", inPark, "plots", paste0(variable, "_tmap.png"))
    tmap_save(created_map, save_name, height = 5, width = 5, bg = "transparent")
    toc()
    
    created_map
  }
  
  elevation_variables <- c("elevation", "slope")
  tic("elevation all")
  
  elev_maps <- map(elevation_variables, elevation_function)
  
  toc()
  
  # structure maps, biomass, canopy cover, canopy height ----
  #bamako palette
  structure_colours <- scico(10, palette = "bamako")
  structure_function <- function(variable){
    tic(variable)
    inRaster <- raster(file.path(rasterLoc, paste0(inPark, "_Pace-2015-", variable, ".tif")))
    
    created_map <- hillshade_map +
      tm_shape(inRaster, raster.downsample = F) +
      tm_raster(alpha = .65, palette = structure_colours, style = "cont", legend.show = legend.show) +
      tm_shape(ppa) +
      tm_borders(col = "black", lwd = 2)
    
    save_name = file.path("outputs", inPark, "plots", paste0(variable, "_tmap.png"))
    tmap_save(created_map, save_name, height = 5, width = 5, bg = "transparent")
    toc()
    
    created_map
  }
  
  structure_variables <- c("percentage_first_returns_above_2m", 
                           "total_biomass", "loreys_height")
  tic("structure all")
  
  struct_maps <- map(structure_variables, structure_function)
  
  toc()
  
  # Land Cover----
  
  tic("lcc")
  
  lcc <- raster(file.path(rasterLoc, paste0(inPark, "_Pace-2015-HMM.tif")))
  
  lcc_unique <- unique(lcc)
  
  lcc_colours <- lcc_join %>% filter(class_val %in% lcc_unique) %>% pull(colour)
  
  lcc_map <- hillshade_map +
    tm_shape(lcc, raster.downsample = F) +
    tm_raster(alpha = .50, palette = lcc_colours, n = 13, style = "cat", legend.show = legend.show) +
    tmap_options(max.categories = 13) +
    tm_shape(ppa) +
    tm_borders(col = "black", lwd = 2)
  
  
  
  save_name = file.path("outputs", inPark, "plots", "lcc_tmap.png")
  
  tmap_save(lcc_map, save_name, height = 5, width = 5, bg = "transparent")
  
  rm(lcc, lcc_unique, lcc_colours)
  
  toc()
  
  
  # forest non-forest ----
  tic("frags")
  
  fNoF <- raster(file.path(rasterLoc, paste0(inPark, "_Pace-2015-fragstats.tif")))
  
  fNoF_unique <- unique(fNoF)
  
  fNoF_colours <- fNoF_join %>% filter(class_val %in% fNoF_unique) %>% pull(colour)
  
  fNoF_map <- hillshade_map +
    tm_shape(fNoF, raster.downsample = F) +
    tm_raster(alpha = .50, palette = fNoF_colours, style = "cat", legend.show = legend.show) +
    tm_shape(ppa) +
    tm_borders(col = "black", lwd = 2)
  
  save_name = file.path("outputs", inPark, "plots", "lcc_fNoF_tmap.png")
  
  tmap_save(fNoF_map, save_name, height = 5, width = 5, bg = "transparent")
  rm(fNoF, fNoF_unique, fNoF_colours)
  toc()
  
  # disturbance ----
  tic("disturbance")
  
  disturbance <- raster(file.path(rasterLoc, paste0(inPark, "_Pace-disturbance_all.tif")))
  
  disturbance_unique <- unique(disturbance)
  
  disturbance_colours <- disturbance_join %>% filter(class_val %in% disturbance_unique) %>% pull(colour)
  
  
  disturbance_map <- hillshade_map +
    tm_shape(disturbance, raster.downsample = F) +
    tm_raster(palette = disturbance_colours, style = "cat", legend.show = legend.show) +
    tm_shape(ppa) +
    tm_borders(col = "black", lwd = 2)
  
  save_name = file.path("outputs", inPark, "plots", "disturbance_tmap.png")
  tmap_save(disturbance_map, save_name, height = 5, width = 5, bg = "transparent")
  rm(disturbance, disturbance_colours)
  toc()
  
  # light maps ----
  tic("nightlights")
  nightlights <- raster(file.path(rasterLoc, paste0(inPark, "_Pace-2015-nightlights.tif")))
  
  nightlights_unique <- unique(nightlights)
  
  nightlights_colours <- nightlights_join %>% filter(class_val %in% nightlights_unique) %>% pull(colour)
  
  #if (length(nightlights_unique) == 1) {
  #  nightlights_colours = c(nightlights_colours, nightlights_colours)
  #}
  
  nightlights_map <- hillshade_map +
    tm_shape(nightlights, raster.downsample = F) +
    tm_raster(palette = nightlights_colours, style = "cat", legend.show = legend.show) +
    tm_shape(ppa) +
    tm_borders(col = "black", lwd = 2)
  
  save_name = file.path("outputs", inPark, "plots", "nightlights_tmap.png")
  tmap_save(nightlights_map, save_name, height = 5, width = 5, bg = "transparent")
  rm(nightlights, nightlights_colours)
  
  toc()
  toc()
  
  outputs <- c(elev_maps, 
               struct_maps, 
               list(lcc_map), 
               list(fNoF_map), 
               list(disturbance_map), 
               list(nightlights_map))
  
  names(outputs) <- c("elevation", "slope", 
                      "percentage_first_returns_above_2m", "total_biomass", "loreys_height",
                      "lcc",
                      "fNoF",
                      "disturbance",
                      "nightlights")
  
  return(outputs)
}


#outputs <- reportcard_tmaps("AKK")
