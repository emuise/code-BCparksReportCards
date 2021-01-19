library(tidyverse)
library(scico)

inPark = "Gar"

parkJoiner <-  read_csv("joinTables/parkNames.csv")

#mean variables by subzone
meanVars <- read_csv("outputCsvs/meanVars.csv")
meanVarNames <- read_csv("joinTables/structure.csv")

meanVars <- left_join(meanVars, meanVarNames)
meanVars <- left_join(meanVars, parkJoiner)

terrainPlot <- function(inPark){
  #elevation
  elevation <- meanVars %>% 
    filter(var == "elevation" | var == "slope") %>% 
    filter(park == inPark) #%>%
    #mutate(MEAN = if_else(var == "aspect", cos(MEAN), MEAN)) #convert aspect to cos because ncc said i should
  
  
  elevWmean = elevation %>% 
    group_by(full_name, var2s, ppa_gpe) %>% 
    summarize(wmean = round(weighted.mean(MEAN, Shape_Area , na.rm = T)))
  
  
  figure <- ggplot() +
    geom_boxplot(data = elevation, aes(x = fct_rev(ppa_gpe), y = MEAN), alpha = .30) +
    geom_point(data = elevWmean, aes(x = fct_rev(ppa_gpe), y = wmean, size = 5), shape = 1) +
    scale_size_identity(guide = "legend", labels = "Overall Weighted Mean by Area") +
    facet_wrap(~var2s,
               #cols = vars(full_name),
               scales = "free") +
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "bottom") +
    labs(x = "",
         y = "Subzone Mean",
         size = "")
  
  newDir = file.path("outputs", inPark, "plots")
  dir.create(newDir, showWarnings = F)
  
  saveLoc = file.path(newDir, "terrain_plot.png")
  
  ggsave(saveLoc, figure, device = "png")
}

parks <- unique(meanVars$park)

map(parks, terrainPlot)