library(tidyverse)
library(here)
library(magick)
library(grid)
library(gridExtra)

lcc_join <- read_csv("joinTables/lcc.csv")
fNoF_join <- read_csv("joinTables/fNoF.csv")
disturbance_join <- read_csv("joinTables/disturbance.csv")
nightlights_join <- read_csv("joinTables/nightlights.csv")
structure_join <- read_csv("joinTables/structure.csv")
park_join <- read_csv("jointables/parkNames.csv")

source("R_hillshadeMaps_tmap.R")
source("R_lccPlot.R")
source("R_fNoFplot.R")
source("R_nightlights_plots.R")
source("R_filtered_disturbance_plots.R")
source("R_structure_graphs.R")

#place this background in the params r file
background <- image_read_pdf("report_cards/background_template.pdf")

reportCard <- function(inPark, generate_maps = F) {
  
  if (generate_maps) {
    reportcard_tmaps(inPark)
  }
  
  lccPlot(inPark)
  
  lcc_donut <- image_read(here::here("outputs", inPark, "plots", "lcc_plot.png"))
  
  lcc_map <- image_read(here::here("outputs", inPark, "plots", "lcc_tmap.png"))
  
  fNoF_donut <- fNoFplot(inPark)
  
  fNoF_donut <- image_read(here::here("outputs", inPark, "plots", "lcc_fNoF_plot.png"))
  
  fNoF_map <- image_read(here::here("outputs", inPark, "plots", "lcc_fNoF_tmap.png"))
  
  nightlights_donut <- nightlights_donut_plot(inPark)
  
  nightlights_map <- image_read(here::here("outputs", inPark, "plots", "nightlights_tmap.png"))
  
  disturbance_donut_plot(inPark)
  
  disturbance_yearly_plot(inPark)
  
  disturbance_donut <- image_read(here::here("outputs", inPark, "plots", "disturbance_donut_plot.png"))
  
  disturbance_yearly <- image_read(here::here("outputs", inPark, "plots", "disturbance_yearly_plot.png"))
  
  disturbance_map <- image_read(here::here("outputs", inPark, "plots", "disturbance_tmap.png"))
  
  structure_plots <- structurePlot(inPark)
  
  biomass_plot <- structure_plots[3]
  canopy_plot <- structure_plots[2]
  height_plot <- structure_plots[1]
  
  canopy_map <- image_read(here::here("outputs", inPark, "plots",
                                      "percentage_first_returns_above_2m_tmap.png"))
  biomass_map <- image_read(here::here("outputs", inPark, "plots", "total_biomass_tmap.png"))
  height_map <- image_read(here::here("outputs", inPark, "plots", "loreys_height_tmap.png"))
  
  canopy_plot <- image_read(here::here("outputs", inPark, "plots",
                                       "percentage_first_returns_above_2m_plot.png"))
  biomass_plot <- image_read(here::here("outputs", inPark, "plots", "total_biomass_plot.png"))
  height_plot <- image_read(here::here("outputs", inPark, "plots", "loreys_height_plot.png"))
  
  park_join <- read_csv("jointables/parkNames.csv")
  
  inPark_info <- park_join %>% 
    filter(processing_name == inPark)
  
  full_name <- inPark_info %>% pull(full_name)
  centroid <- inPark_info %>% pull(centroid)
  established <- inPark_info %>% pull(established)
  hectares <- inPark_info %>% pull(hectares)
  
  vplayout <- function(x, y) viewport(layout.pos.col = x, layout.pos.row = y)
  
  #save location needs to be generated based on file name
  save_location <- here::here("report_cards", "outputs", paste0(inPark, ".png"))
  
  #width and height units in inches
  width = 8.5
  height = 11
  
  png(save_location, width = width, height = height, units = "in", res = 500)
  grid.newpage() 
  pushViewport(viewport(layout = grid.layout(height * 10, width * 10)))
  
  grid.raster(background)
  
  grid.text(full_name, y = unit(0.96, "npc"), 
            gp = gpar(col = "black", cex = 2.2))
  
  grid.text(established, y = unit(0.928, "npc"), 
            gp = gpar(col = "black", cex = 1))
  
  grid.text(centroid, 
            x = unit(.17, "npc"),
            y = unit(0.928, "npc"), 
            gp = gpar(col = "black", cex = 1))
  
  grid.text(hectares, 
            x = unit(.83, "npc"),
            y = unit(0.928, "npc"), 
            gp = gpar(col = "black", cex = 1))
  
  #VLCE
  grid.raster(image_trim(lcc_map), vp = vplayout(5:40, 15:45))
  
  grid.raster(image_trim(lcc_donut), vp = vplayout(45:80, 13:28))
  
  #fNoF
  grid.raster(image_trim(fNoF_map), vp = vplayout(45:60, 32:56))
  
  grid.raster(image_trim(fNoF_donut), vp = vplayout(65:80, 32:56))
  
  #disturbance
  grid.raster(disturbance_yearly, vp = vplayout(57:82, 58:85))
  
  grid.raster(image_trim(disturbance_map), vp = vplayout(32:55, 60:83))
  
  grid.raster(image_trim(disturbance_donut), vp = vplayout(5:30, 63:83))
  
  #structure
  ##canopy
  grid.raster(image_trim(canopy_map), vp = vplayout(10:24, 89:102.5))
  
  grid.raster(canopy_plot, vp = vplayout(26:32, 89:102.5))
  
  grid.text("Canopy Cover", 
            x = unit(2, "inches"),
            y = unit(2.3, "inches"), 
            gp = gpar(col = "black", cex = 1))
  
  ##biomass
  grid.raster(image_trim(biomass_map), vp = vplayout(34:48, 89:102.5))
  
  grid.raster(biomass_plot, vp = vplayout(50:56, 89:102.5))
  
  grid.text("Aboveground Biomass", 
            x = unit(4.3, "inches"),
            y = unit(2.3, "inches"), 
            gp = gpar(col = "black", cex = 1))
  
  #height
  grid.raster(image_trim(height_map), vp = vplayout(58:72, 90:102.5))
  
  grid.raster(height_plot, vp = vplayout(74:80, 89:102.5))
  
  grid.text("Canopy Height", 
            x = unit(6.9, "inches"),
            y = unit(2.3, "inches"), 
            gp = gpar(col = "black", cex = 1))
  
  dev.off()
}

valid_parks <- read_csv("joinTables/parknames.csv") %>% pull(processing_name)

valid_parks <- valid_parks[file.exists(file.path("outputs", valid_parks, "rasters"))]

map(valid_parks, reportCard, generate_maps = T)

#reportCard("SouthChilcotinMountainsPark", T)
