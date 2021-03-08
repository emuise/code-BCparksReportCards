library(tidyverse)
library(scico)
library(grid)
library(gridExtra)

myData <- diamonds %>% filter(cut == "Ideal", color == "E")

breaks <- c(min(myData$z) + max(myData$z) / 100, 
            max(myData$z) - max(myData$z) / 100)

myFig <- ggplot(myData, aes(x, y, colour = z)) +
  geom_point() +
  scale_colour_scico(direction = -1, 
                     palette = "bamako",
                     breaks = breaks, 
                     labels = c("Low", "High")) +
  labs(colour = "") +
  guides(colour = guide_colourbar(ticks = F)) +
  theme(legend.background = element_blank())

myLegend <- cowplot::get_legend(myFig)

grid.newpage()
egg <- grid.draw(myLegend)

egg <- as_ggplot(myLegend) +
  theme(plot.margin = unit(c(-2, 0, -2, -.1), "cm"))
ggsave("test.png", plot = egg, device = "png")

plot_grid(myLegend)
