#Manning Complex Report Card Example 

##Loading in libraries 
library(tidyverse)
library(jpeg)
library(RColorBrewer)
library(multipanelfigure)
library(magrittr)

##Read in land cover data 
Df <- read_csv("hmmDf.csv")


##Calculating percent land cover in PPA vs. GPE in 2015 
land_cover_2015 <- Df %>% 
  filter(var == "HMM", year == "2015", class_name != "unclassifed")


##Manning complex 
mann <- land_cover_2015 %>% 
  filter(park == "Gar") %>% 
  group_by(ppa_gpe) %>%
  mutate(percent_lcc = value / sum(value) * 100)
mann

mann_graph <- ggplot(mann, aes(x = reorder(class_name, percent_lcc), y = percent_lcc, fill = ppa_gpe)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Land Cover Class", y = "Percent of Total Area") +
  theme_bw() +
  theme(legend.title = element_blank())+  
  theme(text = element_text(size = 13))+
  #guides(fill = guide_legend(reverse=TRUE))+
  coord_flip() +
  guides(fill = guide_legend(reverse = TRUE))

mann_graph

ggsave("gar_lcc.png", device = "png", width = 13.3333, height = 7.5, units = "in")



#------------------------------------------------------------------------------
##StructDf Manning Complex 

mann_structDf$ppa_gpe <- as.factor(structDf$ppa_gpe)%>% fct_rev()
mann_structDf <- structDf %>% filter(!(var == "elev_cv" & value > 1))


#plotting manning complex structure data 
mann_struct_graph <- mann_structDf %>% ggplot(aes(x = value, colour = ppa_gpe)) +
  geom_freqpoly() + 
  facet_wrap(var2 ~ ., scale = "free",
             labeller = labeller(var2 = label_wrap_gen(25))) + 
  theme_bw() + 
  theme(legend.title = element_blank(), 
        text = element_text(size = 10), 
        plot.title = element_text(size = 13), 
        legend.position = c(0.8, 1.35),
        legend.direction = "horizontal")+
  labs(x = "Pixel Value", y = "Number of Pixels", 
       title = "Structure information (Manning Complex, 2015)", 
       colour = "Land Strata")

mann_struct_graph

ggsave("2015_Mann_Struct1.png", plot = mann_struct_graph, device = "png", width = 10)


#------------------------------------------------------------------------------
#Manning Complex disturbance analysis 
mannDist2015 <- read_csv("outputs/disturbance.csv") %>%
  filter(class != "Ag", class != "No Disturbance", parkName == "Gar")
mannDist2015$ppa_gpe <- fct_rev(mannDist2015$ppa_gpe) 


#plot
mann_Dist15_graph <- mannDist2015 %>% ggplot(aes(x = reorder(class, per_dist), 
                                                 y= per_dist, 
                                                 fill = class, 
                                                 colour = fct_rev(ppa_gpe))) +
  geom_bar(position = "dodge", stat = "identity") + theme_bw() +
  theme(legend.title = element_blank()) +
  labs(y = "% of Area Disturbed", x = "Disturbance") +
  theme(text = element_text(size = 13))+
  coord_flip()

mann_Dist15_graph <- mann_Dist15_graph + scale_fill_manual(values=c("#38A800", "#005CE6", "#000000", "#FFA900"), guide = F) + 
  scale_colour_manual(values = c("black", "blue")) +
  guides(colour = guide_legend(reverse = TRUE))

mann_Dist15_graph

ggsave("2015_gar_dist_all2.png", plot = mann_Dist15_graph, device = "png", width = 10)



#------------------------------------------------------------------------------
##Mann Report Card 

#joining figures together for report card

report <- multi_panel_figure(width = 250, height = 208, 
                             columns = 8, rows = 8, 
                             row_spacing  = 1, 
                             column_spacing = 3)

report 


report %<>% fill_panel("Manning_Unzoomed.jpg", column = 1:3, row = 1:3)
report %<>% fill_panel(mann_graph, column = 4:8, row = 1:3)
report %<>% fill_panel(mann_struct_graph, column = 4:8, row = 4:6)
report %<>% fill_panel(mann_Dist15_graph, column = 4:8, row = 7:8)

report



ggsave("2015_mann_report_card.png", report, device = "png", width = 10)