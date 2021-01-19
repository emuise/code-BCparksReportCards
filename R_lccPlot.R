library(tidyverse)
library(scico)

vlce <- read_csv("C:/Users/evanmuis.stu/Sync/Masters/Data/outputCsvs/vlce.csv")
vlceJoin <- read_csv("C:/Users/evanmuis.stu/Sync/Masters/Data/joinTables/lcc.csv")
vlce <- left_join(vlce, vlceJoin)


##Calculating percent land cover in PPA vs. GPE in 2015
land_cover_2015 <- vlce %>%
  filter(year == "2015", class_val != 0, class_val != 20)

lccPlot <- function(inPark){
  filtered <- land_cover_2015 %>%
    filter(park == inPark) %>%
    group_by(ppa_gpe) %>%
    mutate(percent_lcc = total_area / sum(total_area) * 100)
  
  filtered$class_name <- fct_rev(fct_reorder(filtered$class_name, filtered$relevel, .fun = mean))
  
  figure <- ggplot(filtered, aes(x = fct_rev(ppa_gpe), y = percent_lcc, fill = class_name)) +
    geom_bar(stat = "identity", position = "stack") +
    guides(fill = guide_legend(reverse = F)) +
    scale_fill_scico_d(palette = "batlow") +
    theme_void() +
    theme() +
    theme(text = element_text(size = 13)) +
    coord_polar(theta = "y") +
    labs(fill = "Land Cover Class") +
    annotate("text", x = 0, y = 0, label = "PPA") +
    annotate("text", x = 2.8, y = 25, label = "GPE")
  
  newDir = file.path("outputs", inPark, "plots")
  dir.create(newDir, showWarnings = F)
  
  saveLoc = file.path(newDir, "lcc_plot.png")
  
  ggsave(saveLoc, figure, device = "png")
}

parks <- unique(vlce$park)

map(parks, lccPlot)