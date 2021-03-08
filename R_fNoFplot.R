library(tidyverse)
library(scico)

vlce <- bind_rows(read_csv("outputCsvs/vlce.csv"),
                  read_csv("outputCsvs/large_vlce.csv")) %>%
  distinct()
lcc_join <- read_csv("joinTables/lcc.csv")
vlce <- left_join(vlce, lcc_join)

fNoF_join <- read_csv("joinTables/fNoF.csv")

fNoFplot <- function(inPark){
  filtered <- vlce %>% 
    filter(year == "2015", class_val != 0, class_val != 20) %>%
    filter(park == inPark) %>%
    group_by(park, ppa_gpe) %>%
    mutate(percent_lcc = total_area / sum(total_area) * 100)
  
  fNoF <- filtered %>% 
    group_by(ppa_gpe, park, fNoF) %>%
    summarize(percent_lcc = sum(percent_lcc))
  
  unique_fNoF <- fNoF %>% 
    pull(fNoF) %>% 
    unique()
  
  unique_colours <- fNoF_join %>% 
    filter(fNoF %in% unique_fNoF) %>% 
    pull(colour)
  
  figure <- ggplot(fNoF, aes(x = fct_rev(ppa_gpe), y = percent_lcc, fill = fNoF)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(values = unique_colours) +
    theme_void() +
    theme(text = element_text(size = 13),
          legend.position = c(0.5, 0.1),
          legend.direction = "horizontal",
          legend.text = element_text(size = 15)) +
    coord_polar(theta = "y") +
    labs(fill = "") +
    annotate("text", x = 0, y = 0, label = 'bold("PPA")', size = 5, parse = T) +
    annotate("text", x = 2.8, y = 25, label = 'bold("GPE")', size = 5, parse = T)
  
  newDir = file.path("outputs", inPark, "plots")
  dir.create(newDir, showWarnings = F)
  
  saveLoc = file.path(newDir, "lcc_FnoF_plot.png")
  
  ggsave(saveLoc, figure, device = "png", height = 5, width = 5, bg = "transparent")
  
  figure
}
