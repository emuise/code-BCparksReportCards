library(tidyverse)
library(scico)

parkJoiner <-  read_csv("joinTables/parkNames.csv")

#lights dataset
lights <- read_csv("outputCsvs/nightlights.csv")
lightJoiner <-  read_csv("joinTables/nightlights.csv")
lights <- left_join(lights, lightJoiner)
lights <- left_join(lights, parkJoiner)

lightPlot <- function(inPark){
  filtered <- lights %>%
    filter(park == inPark) %>%
    group_by(ppa_gpe) %>%
    mutate(percent_lcc = total_area / sum(total_area) * 100)
  
  filtered$class_name <- fct_reorder(filtered$class_name, filtered$relevel, .fun = mean)
  
  figure <- ggplot(filtered, aes(x = fct_rev(ppa_gpe), y = percent_lcc, fill = class_name)) +
    geom_bar(stat = "identity", position = "stack") +
    guides(fill = guide_legend(reverse = TRUE)) +
    scale_fill_scico_d(palette = "batlow") +
    theme_void() +
    theme() +
    theme(text = element_text(size = 13)) +
    coord_polar(theta = "y") +
    labs(fill = "Nightlights Class")
  
  newDir = file.path("outputs", inPark, "plots")
  dir.create(newDir, showWarnings = F)
  
  saveLoc = file.path(newDir, "nightlights_plot.png")
  
  ggsave(saveLoc, figure, device = "png")
}

parks <- unique(lights$park)

map(parks, lightPlot)
