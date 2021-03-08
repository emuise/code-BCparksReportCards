library(tidyverse)
library(scico)
library(ggpubr)
library(rstatix)

#parkJoiner
parkJoiner <-  read_csv("joinTables/parkNames.csv")

#lights ----

lights <- read_csv("outputCsvs/nightlights.csv")
lightJoiner <-  read_csv("joinTables/nightlights.csv")


lights <- lights %>% filter(park == "Gar" | park == "Spats" | park == "Strath") %>% left_join(lightJoiner)
lights <- lights %>% left_join(parkJoiner)


lights$class_name <- fct_reorder(lights$class_name, lights$class_val)

ggplot(lights, aes(x = fct_rev(ppa_gpe), y = proportion_of_landscape, fill = class_name)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Land Cover Class", y = "Percent of Total Area") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(values = c("#999999", scico(4, palette = "bilbao")[2:4])) +
  theme_void() +
  theme(text = element_text(size = 13),
        legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 2, byrow = T)) +
  coord_polar(theta = "y") +
  labs(fill = "") +
  facet_wrap(~ full_name, nrow = 1)
#  guide = guide_legend(reverse = T)
ggsave("pres2Figs/nightlights.png", device = "png", width = 8)

#vlce ----

vlce <- read_csv("C:/Users/evanmuis.stu/Sync/Masters/Data/outputCsvs/vlce.csv")
vlceJoin <- read_csv("C:/Users/evanmuis.stu/Sync/Masters/Data/joinTables/lcc.csv")
vlce <- left_join(vlce, vlceJoin)
vlce <- left_join(vlce, parkJoiner)


##Calculating percent land cover in PPA vs. GPE in 2015
land_cover_2015 <- vlce %>%
  filter(year == "2015", class_val != 0, class_val != 20)


filtered <- land_cover_2015 %>%
  filter(park == "Gar" | park == "Spats" | park == "Strath") %>%
  group_by(park, ppa_gpe) %>%
  mutate(percent_lcc = total_area / sum(total_area) * 100)

filtered$class_name <- fct_rev(fct_reorder(filtered$class_name, filtered$relevel, .fun = mean))

ggplot(filtered, aes(x = fct_rev(ppa_gpe), y = percent_lcc, fill = class_name)) +
  geom_bar(stat = "identity", position = "stack") +
  #guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_scico_d(palette = "batlow") +
  theme_void() +
  theme(text = element_text(size = 13),
        legend.position = "bottom") +
  coord_polar(theta = "y") +
  labs(fill = "") +
  facet_wrap(~ full_name, nrow = 1) +
  guides(fill = guide_legend(nrow = 2, byrow = T))

ggsave("pres2Figs/lccPlot.png", device = "png", width = 8)

#frags ----
fNoF <- filtered %>% 
  group_by(ppa_gpe, full_name, fNoF) %>%
  summarize(percent_lcc = sum(percent_lcc))

ggplot(fNoF, aes(x = fct_rev(ppa_gpe), y = percent_lcc, fill = fNoF)) +
  geom_bar(stat = "identity", position = "stack") +
  #guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(values = scico(11, palette = "batlow")[c(5, 8, 11)]) +
  #scale_fill_scico_d(palette = "batlow") +
  theme_void() +
  theme(text = element_text(size = 13),
        legend.position = "bottom") +
  coord_polar(theta = "y") +
  labs(fill = "") +
  facet_wrap(~ full_name, nrow = 1)

ggsave("pres2Figs/fragCircle.png", device = "png", width = 8)

#mean variables by subzone
meanVars <- read_csv("outputCsvs/meanVars.csv")
meanVarNames <- read_csv("joinTables/structure.csv")

meanVars <- left_join(meanVars, meanVarNames)
meanVars <- left_join(meanVars, parkJoiner)

#elevation ----
elevation <- meanVars %>% 
  filter(var == "elevation" | var == "slope") %>% 
  filter(park == "Gar" | park == "Spats" | park == "Strath") %>%
  mutate(MEAN = if_else(var == "aspect", cos(MEAN), MEAN)) %>% #convert aspect to cos because ncc said i should
  group_by(full_name, var2s, ppa_gpe) %>%
  mutate(outlier = is_outlier(MEAN))

elevOutlier <- elevation %>%
  filter(outlier == T)

elevNonOutlier <- elevation %>%
  filter(outlier == F)


elevWmean = elevation %>% 
  group_by(full_name, var2s, ppa_gpe) %>% 
  summarize(wmean = round(weighted.mean(MEAN, Shape_Area , na.rm = T)))


ggplot() +
  geom_line(data = elevNonOutlier, aes(x = fct_rev(ppa_gpe), y = MEAN)) +
  geom_point(data = elevWmean, aes(x = fct_rev(ppa_gpe), y = wmean), shape = 1, size = 5) +
  geom_point(data = elevOutlier, aes(x = fct_rev(ppa_gpe), y = MEAN), alpha = 0.5) +
  scale_size_identity(guide = "legend", labels = "Overall Weighted Mean by Area") +
  facet_grid(rows = vars(var2s),
             cols = vars(full_name),
             scales = "free_y",
             labeller = labeller(var2s = label_wrap_gen(10))) +
  theme_bw() +
  theme(#axis.line = element_line(colour = "black"),
        #panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "bottom",
        strip.text.y.right = element_text(angle = 0)) +
  labs(x = "",
       y = "Subzone Mean",
       size = "")

ggsave("pres2Figs/elevation.png", device = "png", width = 5, height = 5)

#structure ----
structure = meanVars %>% 
  filter(!(var == "elevation" | var == "aspect" | var == "slope")) %>% 
  filter(park == "Gar" | park == "Spats" | park == "Strath") %>% 
  mutate(adjustMean = MEAN / divideBy)

splitVars = unique(structure$var)[1:3]

structureSecond = structure %>%
  filter(!var %in% splitVars)

#weighted means
secondWmean = structureSecond %>%
  group_by(full_name, ppa_gpe, var2s) %>% 
  summarize(wmean = weighted.mean(adjustMean, AREA))

#grouped outliers
struct2Outliers <- structureSecond %>% 
  group_by(full_name, ppa_gpe, var2s) %>%
  identify_outliers(adjustMean) %>%
  select(full_name, ppa_gpe, var2s, adjustMean)

#remove group outliers
structureSecond <- structureSecond %>%
  group_by(full_name, ppa_gpe, var2s) %>%
  mutate(outlier = is_outlier(adjustMean)) %>%
  filter(!outlier)

second <- ggplot(data = structureSecond) +
  geom_line(aes(x = fct_rev(ppa_gpe), y = adjustMean)) +
  geom_point(data = secondWmean, aes(x = fct_rev(ppa_gpe), y = wmean), shape = 1, size = 5) +
  geom_point(data = struct2Outliers, aes(x = fct_rev(ppa_gpe), y = adjustMean), alpha = 0.5) +
  scale_size_identity(guide = "legend", labels = "Overall Weighted Mean by Area") +
  facet_grid(rows = vars(var2s),
             cols = vars(full_name),
             scales = "free_y",
             labeller = labeller(var2s = label_wrap_gen(10))) +
  theme_bw() +
  theme(#axis.line = element_line(colour = "black"),
        #panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "bottom",
        strip.text.y.right = element_text(angle = 0)) +
  labs(x = "",
       y = "Subzone Mean",
       size = "")

ggsave("pres2Figs/structure.png", second,  device = "png", width = 5, height = 5)




#inside outside circle plot ----
ppa_gpe <- c("PPA", "GPE")
percent <- c(100, 100)

forPlot <- tibble(ppa_gpe, percent)

(legendPlot <- ggplot(forPlot) +
  geom_bar(aes(x = ppa_gpe, y = percent), stat = "identity", fill = "grey70") +
  coord_polar(theta = "y") +
  theme_void() +
  annotate("text", x = .6, y = 0, label = "PPA", fontface = "bold", size = 15) +
  annotate("text", x = 1.9, y = 0, label = "GPE", fontface = "bold", size = 15))

ggsave("pres2Figs/legendPlot.png", legendPlot,  device = "png", width = 5, height = 5, bg = "transparent")
