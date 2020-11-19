library(tidyverse)
library(scico)

vlce <- read_csv("C:/Users/evanmuis.stu/Sync/Masters/Data/outputCsvs/vlce.csv")
vlceJoin <- read_csv("C:/Users/evanmuis.stu/Sync/Masters/Data/joinTables/lcc.csv")
vlce <- left_join(vlce, vlceJoin)

inPark = "Gar"

fNoFplot <- function(inPark){
  filtered <- vlce %>% 
    filter(year == "2015", class_val != 0, class_val != 20) %>%
    filter(park == inPark) %>%
    group_by(park, ppa_gpe) %>%
    mutate(percent_lcc = total_area / sum(total_area) * 100)
  
  fNoF <- filtered %>% 
    group_by(ppa_gpe, park, fNoF) %>%
    summarize(percent_lcc = sum(percent_lcc))
  
  figure <- ggplot(fNoF, aes(x = fct_rev(ppa_gpe), y = percent_lcc, fill = fNoF)) +
    geom_bar(stat = "identity", position = "stack") +
    #guides(fill = guide_legend(reverse = TRUE)) +
    scale_fill_manual(values = scico(11, palette = "batlow")[c(5, 8, 11)]) +
    #scale_fill_scico_d(palette = "batlow") +
    theme_void() +
    theme(text = element_text(size = 13)) +
    coord_polar(theta = "y") +
    labs(fill = "Aggregate Land Cover Class")
  
  newDir = file.path("outputs", inPark, "plots")
  dir.create(newDir, showWarnings = F)
  
  saveLoc = file.path(newDir, "lcc_FnoF_plot.png")
  
  ggsave(saveLoc, figure, device = "png")
}

parks <- unique(vlce$park)

map(parks, fNoFplot)
