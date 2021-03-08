library(tidyverse)
library(scico)
options(scipen = 999)
theme_set(theme_bw())

disturbance <- bind_rows(read_csv("outputCsvs/disturbance.csv"), 
                         read_csv("outputCsvs/large_disturbance.csv")) %>%
  distinct() #occasionally some year/park combos were processed twice, this removes them

disturbance_join <- read_csv("joinTables/disturbance.csv")

valid_parks <- read_csv("joinTables/parknames.csv") %>% pull(processing_name)

valid_parks <- valid_parks[file.exists(file.path("outputs", valid_parks, "rasters"))]

disturbance <- disturbance %>% 
  filter(park %in% valid_parks)

#gets total park area for all valid parks, has to be done before removing invalid classes
total_park_area <- disturbance %>% 
  group_by(park, year, ppa_gpe) %>%
  summarize(total_area = sum(total_area)) %>%
  group_by(park, ppa_gpe) %>%
  summarize(full_total_area = mean(total_area))


valid_classes <- c(0, 1, 2, 1001, 1002)

disturbance <- disturbance %>% 
  filter(class_val %in% valid_classes) 

rm(valid_classes)

#disturbance <- disturbance %>% filter(park == "RM")
  
others <- disturbance %>% filter(class_val != 0) %>%
  group_by(park, year, ppa_gpe) %>%
  summarize(total_percent_others = sum(proportion_of_landscape))

#makes disturbance percentages actually add up to 100
disturbance <- disturbance %>% 
  left_join(others) %>% 
  mutate(proportion_of_landscape = case_when(
                                   class_val == 0 & is.na(total_percent_others) ~ 100,
                                   class_val == 0 ~ 100 - total_percent_others,
                                   TRUE ~ proportion_of_landscape)) %>%
  complete(park, ppa_gpe, year, class_val, var) %>%
  mutate(proportion_of_landscape = case_when(
                                   is.na(proportion_of_landscape) & class_val == 0 ~ 100,
                                   is.na(proportion_of_landscape) & class_val != 0 ~ 0,
                                   TRUE ~ proportion_of_landscape)) %>%
  left_join(total_park_area) %>%
  mutate(total_area = proportion_of_landscape * full_total_area / 100) %>%
  dplyr::select(park, year, ppa_gpe, class_val, proportion_of_landscape, total_area)

disturbance <- left_join(disturbance, disturbance_join)

disturbance_yearly <- disturbance

rm(others)

#determines amount of area disturbed by class over time series
disturbed_classes <- disturbance %>% 
  filter(class_val != 0) %>%
  group_by(park, ppa_gpe, class_val) %>%
  summarize(class_percent = sum(proportion_of_landscape),
            class_area = sum(total_area))

#determines amount of area not disturbed over time series
non_disturbed_classes <- disturbed_classes %>%
  group_by(park, ppa_gpe) %>%
  summarize(class_val = 0,
            class_percent = 100 - sum(class_percent))

#merges time series disturbed and not disturbed and calculates area disturbed
#note area disturbed is kind of wonky IF there are multiple disturbances on the same pixel
#so we dont use it much
disturbance <- bind_rows(disturbed_classes, non_disturbed_classes) %>%
  left_join(total_park_area) %>%
  mutate(class_area = full_total_area * class_percent / 100) %>%
  dplyr::select(!full_total_area) %>% 
  left_join(disturbance_join)

rm(disturbed_classes, non_disturbed_classes, total_park_area)

# donut plotting ----
disturbance_donut_plot <- function(inPark) {

  filtered <- disturbance %>% filter(park == inPark)
  
  unique_classes <- filtered %>% 
    pull(class_val) %>% 
    unique()
  
  colours <- disturbance_join %>% 
    filter(class_val %in% unique_classes,
           class_val != 0) %>%
    pull(colour)
  
  #needs spare colour at the beginning because class 0 needs to be transparent for the maps
  colours <- c("#999999", colours)
  
  figure <- ggplot(filtered, aes(x = fct_rev(ppa_gpe), y = class_percent, 
                                 fill = fct_reorder(class, relevel, .fun = max))) +
    geom_bar(stat = "identity", position = "stack") +
    theme_void() +
    theme(text = element_text(size = 13),
          legend.position = c(.5, 0.1),
          legend.direction = "horizontal",
          legend.text = element_text(size = 15)) +
    coord_polar(theta = "y") +
    scale_fill_manual(values = colours) +
    labs(fill = "")  +
    annotate("text", x = 0, y = 0, label = 'bold("PPA")', size = 5, parse = T) +
    annotate("text", x = 2.8, y = 25, label = 'bold("GPE")', size = 5, parse = T)
  
  newDir = file.path("outputs", inPark, "plots")
  dir.create(newDir, showWarnings = F)
  
  saveLoc = file.path(newDir, "disturbance_donut_plot.png")
  
  ggsave(saveLoc, figure, device = "png", height = 5, width = 5, bg = "transparent")
  
  figure
}


# yearly disturbance ----

disturbance_yearly_plot <- function(inPark) {
  #cumulative line can be added from R_disturbance_figures.R file if desired (hint: its not)

  filtered <- disturbance_yearly %>% filter(park == inPark, class_val != 0)
  
  unique_classes <- filtered %>% 
    pull(class_val) %>% 
    unique()
  
  colours <- disturbance_join %>% 
    filter(class_val %in% unique_classes) %>%
    pull(colour)
    
  figure <- filtered %>% ggplot(aes(x = year, y = proportion_of_landscape, fill = class)) +
    geom_col() +
    facet_grid(rows = vars(ppa_gpe)) +
    scale_fill_manual(values = colours) +
    labs(fill = "Disturbance", x = "Year", y = "% of Landscape Disturbed") +
    theme(panel.grid = element_blank(),
          legend.position = "none")
  
  newDir = file.path("outputs", inPark, "plots")
  dir.create(newDir, showWarnings = F)
  
  saveLoc = file.path(newDir, "disturbance_yearly_plot.png")
  
  ggsave(saveLoc, figure, device = "png", bg = "transparent", height = 3.5, width = 3.8)
  
  figure
}

