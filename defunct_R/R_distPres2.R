library(tidyverse)
library(scico)
options(scipen=999)

parkJoiner <-  read_csv("joinTables/parkNames.csv")
#the filter here prevents agriculture from being included the whole way through
distDf <- read_csv("outputCsvs/disturbance.csv") %>% filter(class_val != 1005)

#grabs total area for each park before adding NA to the mix (for some reason that breaks things)
totArea <- distDf %>% 
  group_by(park, year, ppa_gpe) %>% 
  summarize(total_area = sum(total_area)) %>%
  group_by(park, ppa_gpe) %>% 
  summarize(total_area = mean(total_area))

#splits disturbances into no disturbance and disturbance for some data manip
#data manip is adding missing values where the entire landscape was either 100% or 0% one class
noDist <- distDf %>% filter(class_val == 0)
dists <- distDf %>% filter(class_val != 0)

#creates missing no disturbance values, where they should be 100
park <- unique(noDist$park)
class_val <- c(0)
ppa_gpe <- c("PPA", "GPE")
year <- c(1985:2018)
oneDists <- expand_grid(class_val, ppa_gpe, park, year) %>% 
  mutate(total_area = NA, proportion_of_landscape = 100, var = "disturbance")
#there may be a solve for total_area = NA, but it isn't fully necessary

noDist <- rbind(noDist, oneDists)
noDist <- noDist %>% group_by(class_val, ppa_gpe, park, year) %>% filter(proportion_of_landscape == min(proportion_of_landscape))

#creates missing disturbance values, where they would normally be 0
park <- unique(dists$park)
class_val <- c(1000, 1001, 1002, 1003, 1004)
ppa_gpe <- c("PPA", "GPE")
year <- c(1985:2018)
zeroDists <- expand_grid(class_val, ppa_gpe, park, year) %>% 
  mutate(total_area = 0, proportion_of_landscape = 0, var = "disturbance")

dists <- rbind(dists, zeroDists)
dists <- dists %>% group_by(class_val, ppa_gpe, park, year) %>% filter(proportion_of_landscape == max(proportion_of_landscape))

#merges added missing value dfs together, then removes duplicate values
distDf <- rbind(noDist, dists) %>%
  arrange(class_val, park, year) %>%
  distinct(class_val, ppa_gpe, park, year, .keep_all = T)

#removes variables that are no longer needed
rm(dists, noDist, oneDists, zeroDists, class_val, park, ppa_gpe, year)

#class names, relevel values
classVals = read_csv("joinTables/disturbance.csv")

#total values over each year, leads to erronous no-distrbance values
distTot <- distDf %>% group_by(class_val, park, ppa_gpe) %>% summarize(area = sum(total_area, na.rm = T))


#generates amount of undisturbed area for each park, by summing disturbed area for all years,
#subtracting that from total area, and only selecting relevant columns
undistArea <- distTot %>% 
  filter(class_val != 0) %>% 
  group_by(park, ppa_gpe) %>% 
  summarize(distArea = sum(area)) %>%
  left_join(totArea) %>%
  mutate(area = total_area - distArea, class_val = 0) %>%
  dplyr::select(class_val, park, ppa_gpe, area)

#removes incorrect values where it is undisturbed area
distTot <- distTot %>% filter(class_val != 0)

#joins tables together: disturbed area, undisturbed area, and total area
#calculates # area of disturbance for each class for each park and gpe
per_dist <- rbind(distTot, undistArea) %>% 
  left_join(., classVals)
per_dist <- per_dist %>%
  left_join(., totArea) %>%
  mutate(per_dist = area / total_area * 100)

#checks to see if per_total is 100 for each park gpe pair
per_dist %>% group_by(park, ppa_gpe) %>% summarize(per_total = sum(per_dist))
per_dist <- left_join(per_dist, parkJoiner)

#adds plotting things to distDf
distDf <- left_join(distDf, classVals)
distDf <- left_join(distDf, parkJoiner)

#function to make a disturbance double donut plot for a given park, using filter on per_dist
#has inbuilt manual fill in comments, or "batlow" palette

  
filtered <- per_dist %>% filter(park == "Gar" | park == "Spats" | park == "Strath")

ggplot(filtered, aes(x = fct_rev(ppa_gpe), y = per_dist, 
                               fill = fct_reorder(class, relevel, .fun = max))) +
  geom_bar(stat = "identity", position = "stack") +
  theme_void() +
  theme(text = element_text(size = 13),
        legend.position = "bottom") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#999999", scico(6, palette = "hawaii")[2:6])) +
  #scale_fill_scico_d(palette = "batlow") +
  #scale_fill_manual(values=c("#828282", "#ff0000", "#009900", "#00a9e6", "#000000", "#c8c8c8")) +
  #guides(fill = guide_legend(reverse = T)) +
  labs(fill = "")  +
  facet_wrap(~ full_name, nrow = 1)


ggsave("pres2Figs/disturbance.png", device = "png", width = 8)



#function to create a disturbance by year plot
filtered <- distDf %>% filter(park == "Gar" | park == "Spats" | park == "Strath", class_val != 0)

cumulative <- filtered %>% 
  group_by(park, ppa_gpe, year) %>% 
  summarize(yearly_dist = sum(proportion_of_landscape)) %>%
  mutate(cumulative = cumsum(yearly_dist))

filtCumulative <- left_join(filtered, cumulative)


filtCumulative %>% ggplot(aes(x = year, y = proportion_of_landscape, fill = fct_reorder(class, relevel, .fun = max))) +
  geom_col() +
  #geom_line(aes(x = year, y = cumulative)) +
  facet_grid(rows = vars(fct_rev(ppa_gpe)),
             cols = vars(full_name)) +
  #scale_fill_scico_d(palette = "batlow") +
  #scale_fill_manual(values = c("#ff0000", "#009900", "#00a9e6", "#000000", "#c8c8c8")) +
  scale_fill_manual(values = scico(6, palette = "hawaii")[2:6]) +
  labs(fill = "", x = "Year", y = "% of Landscape Disturbed") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 13),
        legend.position = "bottom")



ggsave("pres2Figs/distYearly.png", device = "png", width= 8, height = 5)
