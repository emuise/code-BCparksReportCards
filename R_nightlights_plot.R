library(tidyverse)
library(scico)

parkJoiner <-  read_csv("joinTables/parkNames.csv")

#lights dataset
lights <- read_csv("outputCsvs/nightlights.csv")

park <- unique(lights$park)
class_val <- c(1, 2, 3, 4)
ppa_gpe <- c("PPA", "GPE")
year = 2015

missingVals <- expand_grid(park, class_val, ppa_gpe, year) %>% 
  mutate(proportion_of_landscape = ifelse(class_val == 1, 100, 0), 
         var = "nightlights",
         total_area = NA)

none <- rbind(lights, missingVals) %>% 
  filter(class_val == 1) %>% 
  group_by(class_val, ppa_gpe, park, year) %>% 
  filter(proportion_of_landscape == min(proportion_of_landscape)) %>%
  distinct(class_val, ppa_gpe, park, var, year, .keep_all = T)
  
some <- rbind(lights, missingVals) %>% 
  filter(class_val != 1) %>% 
  group_by(class_val, ppa_gpe, park, year) %>% 
  filter(proportion_of_landscape == max(proportion_of_landscape)) %>%
  distinct(class_val, ppa_gpe, park, year, .keep_all = T)

lights <- rbind(none, some)

lightJoiner <-  read_csv("joinTables/nightlights.csv")
lights <- left_join(lights, lightJoiner)
lights <- left_join(lights, parkJoiner)

rm(some, none, class_val, park, ppa_gpe, year, missingVals)


lightPlot <- function(inPark){
  filtered <- lights %>%
    filter(park == inPark) %>%
    group_by(ppa_gpe)
  
  filtered$class_name <- fct_reorder(str_wrap(filtered$class_name, 20), filtered$relevel, .fun = mean)
  
  figure <- ggplot(filtered, aes(x = fct_rev(ppa_gpe),
                                 y = proportion_of_landscape,
                                 fill = class_name)) +
    geom_bar(stat = "identity", position = "stack") +
    #guides(fill = guide_legend(reverse = TRUE)) +
    scale_fill_manual(values = c("#999999", scico(3, palette = "batlow"))) +
    theme_void() +
    theme() +
    theme(text = element_text(size = 13)) +
    coord_polar(theta = "y") +
    labs(fill = "Human Development")
  
  newDir = file.path("outputs", inPark, "plots")
  dir.create(newDir, showWarnings = F)
  
  saveLoc = file.path(newDir, "nightlights_plot.png")
  
  ggsave(saveLoc, figure, device = "png")
}

parks <- unique(lights$park)

map(parks, lightPlot)
