library(tidyverse)
library(scico)

vlce <- bind_rows(read_csv("outputCsvs/vlce.csv"), 
                   read_csv("outputCsvs/large_vlce.csv")) %>%
  distinct() %>% #occasionally some year/park combos were processed twice, this removes them
  filter(class_val != 0) %>%
  complete(class_val, ppa_gpe, park, var, year, fill = list(proportion_of_landscape = 0, total_area = 0))

lcc_join <- read_csv("joinTables/lcc.csv")
vlce <- left_join(vlce, lcc_join)


##Calculating percent land cover in PPA vs. GPE in 2015
vlce <- vlce %>%
  filter(year == "2015")

lccPlot <- function(inPark){
  filtered <- vlce %>%
    filter(park == inPark) %>%
    group_by(ppa_gpe) %>%
    mutate(percent_lcc = total_area / sum(total_area) * 100)
  
  colours <- filtered %>% pull(colour) %>% unique() %>% rev()
  
  filtered$class_name <- fct_rev(fct_reorder(filtered$class_name, filtered$relevel, .fun = mean))
  
  figure <- ggplot(filtered, aes(x = fct_rev(ppa_gpe), y = percent_lcc, fill = class_name)) +
    geom_bar(stat = "identity", position = "stack") +
    guides(fill = guide_legend(reverse = F)) +
    scale_fill_manual(values = colours) +
    theme_void() +
    theme(text = element_text(size = 15)) +
    coord_polar(theta = "y") +
    labs(fill = "Land Cover Class") +
    annotate("text", x = 0, y = 0, label = 'bold("PPA")', size = 5, parse = T) +
    annotate("text", x = 2.8, y = 25, label = 'bold("GPE")', size = 5, parse = T)
  
  newDir = file.path("outputs", inPark, "plots")
  dir.create(newDir, showWarnings = F)
  
  saveLoc = file.path(newDir, "lcc_plot.png")
  
  ggsave(saveLoc, figure, device = "png", height = 7, width = 7, bg = "transparent")
  
  figure
}
