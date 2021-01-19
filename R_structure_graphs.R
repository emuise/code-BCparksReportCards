library(tidyverse)
library(scico)
library(rstatix)

inPark = "Gar"

parkJoiner <-  read_csv("joinTables/parkNames.csv")

#mean variables by subzone
meanVars <- read_csv("outputCsvs/meanVars.csv")
meanVarNames <- read_csv("joinTables/structure.csv")

meanVars <- left_join(meanVars, meanVarNames)
meanVars <- left_join(meanVars, parkJoiner)

structVars <- meanVarNames$var[4:6]

structurePlot <- function(inPark){
  indivStructPlot <- function(variable){
    
    structure <- structure %>% 
      filter(var == variable)
    
    structWmean <- structure %>% 
      group_by(full_name, ppa_gpe, var2s) %>%
      summarize(wmean = weighted.mean(MEAN, Shape_Area))
    
    structure <- structure %>% group_by(ppa_gpe) %>%
      mutate(is.outlier = is_outlier(MEAN))
    
    non_outliers <- structure %>%
      filter(is.outlier == F)
    
    outliers <- structure %>%
      filter(is.outlier == T)
    
    #figure <- structure %>% group_by(full_name, ppa_gpe, var2s) %>%
     # summarize(x = 1,
      #          y0 = quantile(MEAN, 0.25) - 1.5 * IQR(MEAN),
       #         y25 = quantile(MEAN, 0.25),
        #        y50 = weighted.mean(MEAN, Shape_Area),   # <=== replace by mean
         #       y75 = quantile(MEAN, 0.75),
          #      y100 = quantile(MEAN, 0.75) + 1.5 * IQR(MEAN)) %>%
    figure <- ggplot(data = non_outliers, aes(x = fct_rev(ppa_gpe))) +
      geom_line(aes(x = fct_rev(ppa_gpe), y = MEAN)) +
      geom_point(data = structWmean, aes(x = fct_rev(ppa_gpe), y = wmean), shape = 1, size = 5) +
      geom_point(data = outliers, aes(x = fct_rev(ppa_gpe), y = MEAN), alpha = 0.5) +
      #scale_size_identity(guide = "legend", labels = "Weighted Mean by Area") +
      facet_wrap(~ var2s, scale = "free_y") +
      labs(x = "", 
           y = "Subzone Mean", 
           #subtitle = "Note the traditional median in this plot is the mean weighted by area",
           size = "") +
      theme_bw() +
      theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.position = "bottom") 
    
    newDir = file.path("outputs", inPark, "plots")
    dir.create(newDir, showWarnings = F)
    
    saveLoc = file.path(newDir, paste0(variable, "_plot.png"))
    
    ggsave(saveLoc, figure, device = "png", width = 2.5)
  }
  
  #structure
  structure <- meanVars %>% 
    filter(var %in% structVars) %>% 
    filter(park == inPark) %>%
    mutate(MEAN = MEAN / divideBy) %>%
    filter(!(MEAN > 1 & var == "elev_cv"))#%>%
  #mutate(MEAN = if_else(var == "aspect", cos(MEAN), MEAN)) #convert aspect to cos because ncc said i should
  
  
  structWmean <- structure %>% 
    group_by(full_name, ppa_gpe, var2s) %>%
    summarize(wmean = weighted.mean(MEAN, Shape_Area))
  
  
  outliers <- structure %>% group_by(full_name, ppa_gpe, var2s) %>%
    identify_outliers(MEAN)
  
  figure <- structure %>% group_by(full_name, ppa_gpe, var2s) %>%
    summarize(x = 1,
              y0 = quantile(MEAN, 0.25) - 1.5 * IQR(MEAN),
              y25 = quantile(MEAN, 0.25),
              y50 = weighted.mean(MEAN, Shape_Area),   # <=== replace by mean
              y75 = quantile(MEAN, 0.75),
              y100 = quantile(MEAN, 0.75) + 1.5 * IQR(MEAN)) %>%
    ggplot(aes(x = fct_rev(ppa_gpe))) +
    geom_boxplot(aes(ymin = y0, lower = y25, middle = y25, upper = y75, ymax = y100),
                 stat = "identity", fatten = .5, outlier.shape = NA) +
    geom_point(data = structWmean, aes(x = fct_rev(ppa_gpe), y = wmean, size = 5), shape = 1) +
    geom_point(data = outliers, aes(x = fct_rev(ppa_gpe), y = MEAN), alpha = 0.5) +
    scale_size_identity(guide = "legend", labels = "Overall Weighted Mean by Area") +
    facet_wrap(~ var2s, scale = "free_y") +
    labs(x = "", 
         y = "Subzone Mean", 
         #subtitle = "Note the traditional median in this plot is the mean weighted by area",
         size = "") +
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "bottom") 
  
  newDir = file.path("outputs", inPark, "plots")
  dir.create(newDir, showWarnings = F)
  
  saveLoc = file.path(newDir, "structure_plot.png")
  
  ggsave(saveLoc, figure, device = "png")
  
  map(structVars, indivStructPlot)
}

parks <- unique(meanVars$park)

map(parks, structurePlot)

