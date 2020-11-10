library(tidyverse)
library(scico)

filter = "Gar"

struct <- read_csv("outputCsvs/meanVars.csv") %>% filter(park == filter)

metricFactors <- read_csv("joinTables/structure.csv")

struct <- full_join(struct, metricFactors)

struct <- struct %>% mutate(adjustMean = MEAN / divideBy)



struct %>% ggplot(aes(x = fct_rev(ppa_gpe), y = adjustMean, col = struct$ZONE)) +
  geom_boxplot(outlier.alpha = 0, colour = "black") +
  geom_jitter(position = position_jitter(0.2), alpha = 0.25) +
  facet_wrap(var2s ~ ., scale = "free",
             labeller = labeller(var2s = label_wrap_gen(25))) + 
  labs(x = "", y = "Mean") +
  scale_colour_scico_d(palette = "batlow") +
  theme_bw() +
  theme(legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))


ggsave("struct_2015.png", device = "png", width = 13.3333, height = 7.5, units = "in")

aspect <- filter(struct, var == "aspect")

aspect %>% ggplot(aes(x = adjustMean, colour = ZONE)) +
  geom_freqpoly() +
  facet_wrap(~ppa_gpe) +
  coord_polar() +
  scale_colour_scico_d(palette = "batlow") +
  theme_bw()

