library(tidyverse)
library(stringr)
library(lubridate)

options(pillar.sigfigs = 6)

parks <- read_csv("parksExploration/parks.csv") %>%
  janitor::clean_names()

complexes <- read_csv("parksExploration/complexes.csv") %>%
  janitor::clean_names()

complex_join <- read_csv("parksExploration/complex_join.csv") %>%
  janitor::clean_names() %>%
  filter(!is.na(complex)) %>%
  select(name, complex, established_date)


complexes_sub <- complexes %>% 
  left_join(complex_join) %>%
  group_by(complex) %>%
  summarize(established = as_date(min(established_date))) %>%
  left_join(complexes) %>%
  select(!c(objectid, shape_length), processing_name = complex) %>%
  mutate(interim_name = case_when(processing_name == "AK-K" ~ "Akamina-Kishinena",
                                  processing_name == "Dune" ~ "Dune Za Keyih",
                                  processing_name == "Duu G" ~ "Duu Guusd",
                                  processing_name == "Foch" ~ "Foch-Gilttoyees",
                                  processing_name == "Gar" ~ "Garabaldi",
                                  processing_name == "Itcha" ~ "Itcha Ilgachuz",
                                  processing_name == "Khutz" ~ "Khutzeymateen",
                                  processing_name == "Liard" ~ "Liard River",
                                  processing_name == "Mann" ~ "E.C. Manning",
                                  processing_name == "Naik" ~ "Naikoon",
                                  processing_name == "No RM" ~ "Northern Rocky Mountains",
                                  processing_name == "Om" ~ "Omineca",
                                  processing_name == "Purc" ~ "Purcell",
                                  processing_name == "RM" ~ "Mount Robson",
                                  processing_name == "Spats" ~ "Spatsizi",
                                  processing_name == "Stein" ~ "Stein Valley",
                                  processing_name == "Strath" ~ "Strathcona",
                                  processing_name == "Tat-Alsek" ~ "Tatshenshini-Alsek",
                                  processing_name == "Tsyl" ~ "Ts'yl-Os",
                                  processing_name == "Tweeds" ~ "Tweedsmuir",
                                  processing_name == "WG" ~ "Wells Gray",
                                  TRUE ~ processing_name),
         full_name = paste(interim_name, "Complex"),
         shape_area = shape_area / 10000,
         processing_name = str_replace_all(processing_name, regex("\\W+"), "")) %>%
  select(!interim_name)

valid_categories = c("Ia", "Ib", "II", "IV")

parks_sub <- parks %>%
  select(processing_name = protected, 
         full_name = name, 
         iucn_categ, 
         established = establishe,
         shape_area,
         x, y) %>% 
  mutate(shape_area = shape_area / 10000,
         established = as_date(established)) %>%
  filter(shape_area > 100,
         iucn_categ %in% valid_categories) %>%
  select(!iucn_categ)

merged <- bind_rows(complexes_sub, parks_sub)

merged <- merged %>% 
  mutate(x_char = as.character(round(abs(x), digits = 4)),
         y_char = as.character(round(abs(y), digits = 4)),
         centroid = paste0(y_char, "°N, ", x_char, "°W")) %>%
  mutate(month = month(established, label = T, abbr = F),
         year = year(established),
         full_date = established, 
         established = paste("Est.", month, year)) %>%
  mutate(hectares = paste0(format(round(as.numeric(shape_area), 0), nsmall = 0, big.mark = ",") %>%
                             str_trim(), "ha")) %>%
  select(processing_name, full_name, centroid, established, hectares, shape_area, full_date, x, y)

write_csv(merged, "joinTables/parkNames.csv")
