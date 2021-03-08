library(tidyverse)
library(scico)

#get total park area from disturbance ----
disturbance <- bind_rows(read_csv("outputCsvs/disturbance.csv"), 
                         read_csv("outputCsvs/large_disturbance.csv")) %>%
  distinct() #occasionally some year/park combos were processed twice, this removes them

total_park_area <- disturbance %>% 
  group_by(park, year, ppa_gpe) %>%
  summarize(total_area = sum(total_area)) %>%
  group_by(park, ppa_gpe) %>%
  summarize(full_total_area = mean(total_area))

rm(disturbance)

#actual nightlights stuff ----

nightlights <- bind_rows(read_csv("outputCsvs/nightlights.csv"), 
                          read_csv("outputCsvs/large_nightlights.csv")) %>%
  distinct() #occasionally some year/park combos were processed twice, this removes them

nightlights_join <- read_csv("joinTables/nightlights.csv")

valid_parks_df <- read_csv("joinTables/parknames.csv")

valid_parks <- valid_parks_df %>% pull(processing_name)

valid_parks <- valid_parks[file.exists(file.path("outputs", valid_parks, "rasters"))]

valid_parks_df <- valid_parks_df %>%
  filter(processing_name %in% valid_parks)

nightlights_joined <- left_join(valid_parks_df, nightlights, by = c("processing_name" = "park")) %>%
  dplyr::select(!c(full_name, centroid, established, shape_area, full_date, x, y)) %>%
  complete(processing_name, ppa_gpe, class_val, year, var)

others <- nightlights_joined %>% filter(class_val != 1) %>%
  group_by(processing_name, year, ppa_gpe) %>%
  summarize(total_percent_others = sum(proportion_of_landscape))

#makes proportion of landscape add up to 100 and correct total area
nightlights_joined <- nightlights_joined %>% 
  left_join(others) %>% 
  mutate(proportion_of_landscape = case_when(
    class_val == 1 & is.na(total_percent_others) ~ 100,
    class_val == 1 ~ 100 - total_percent_others,
    TRUE ~ proportion_of_landscape)) %>%
  complete(processing_name, ppa_gpe, year, class_val, var) %>%
  mutate(proportion_of_landscape = case_when(
    is.na(proportion_of_landscape) & class_val == 1 ~ 100,
    is.na(proportion_of_landscape) & class_val != 1 ~ 0,
    TRUE ~ proportion_of_landscape)) %>%
  left_join(total_park_area) %>%
  mutate(total_area = proportion_of_landscape * full_total_area / 100) %>%
  dplyr::select(processing_name, year, ppa_gpe, class_val, proportion_of_landscape, total_area) 

nightlights_joined <- nightlights_joined %>% 
  left_join(nightlights_join)

rm(valid_parks_df, nightlights, others, total_park_area)

nightlights_donut_plot <- function(inPark) {

  filtered <- nightlights_joined %>% filter(processing_name == inPark)
  
  filtered$class_name <- fct_reorder(str_wrap(filtered$class_name, 20), filtered$relevel, .fun = mean)
  
  nightlights_unique <- filtered %>% 
    pull(class_val) %>% 
    unique()
  
  nightlights_colours <- nightlights_join %>% 
    filter(class_val %in% nightlights_unique,
           class_val != 1) %>% 
    pull(colour)
  
  nightlights_colours <- c("#999999", nightlights_colours)
  
  figure <- ggplot(filtered, aes(x = fct_rev(ppa_gpe),
                       y = proportion_of_landscape,
                       fill = class_name)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(values = nightlights_colours) +
    theme_void() +
    theme(text = element_text(size = 13)) +
    coord_polar(theta = "y") +
    labs(fill = "Human Development")
  
  newDir = file.path("outputs", inPark, "plots")
  dir.create(newDir, showWarnings = F)
  
  saveLoc = file.path(newDir, "nightlights_plot.png")
  
  ggsave(saveLoc, figure, device = "png", height = 7, width = 7, bg = "transparent")
  
  figure
}
