library(tidyverse)
library(lubridate)

#Hailey file path
setwd("C:/Users/hmcLD/OneDrive/Desktop/Diez_Lab_Parks_Project/2025/data")

br_neighbors <- read.csv("25_neighbors_br.csv") %>%
  rename(c(PLOT_TYPE = PLOT, 
           PLOT = PLOT.1)) %>%
  select(-NOTES)
rf_neighbors <- read.csv("25_neighbors_rf.csv") %>%
  rename(c(PLOT_TYPE = PLOT, 
           PLOT = PLOT.1)) %>%
  select(-NOTES)
sem_neighbors <- read.csv("25_neighbors_sem.csv") %>%
  rename(c(PLOT_TYPE = PLOT, 
           PLOT = PLOT.1)) %>%
  select(-NOTES)
wir_neighbors <- read.csv("25_neighbors_wir.csv") %>%
  rename(c(PLOT_TYPE = PLOT, 
           PLOT = PLOT.1)) %>%
  select(-NOTES)

colnames(br_neighbors)
colnames(rf_neighbors)
colnames(sem_neighbors)
colnames(wir_neighbors)

neighbors_25 <- bind_rows(br_neighbors, rf_neighbors, sem_neighbors, wir_neighbors) %>%
  rename(AGRCAP = agrostis.spp)

fitness_25 <- read.csv("25_fitness.csv") %>% 
  select(1:16) %>%
  rename(c(PLOT_TYPE = PLOT, 
           PLOT = PLOT.1)) %>%
  filter(!grepl("gone", NOTES, ignore.case = TRUE)) %>%
  drop_na(HEIGHT)

# alones_fitness <- fitness_25 %>%
#   filter(PLOT_TYPE == "ALONE")
# 
# diverse_fitness <- fitness_25 %>%
#   filter(PLOT_TYPE == "DIVERSE") %>%
#   drop_na(HEIGHT, FINALFR)

#join fitness and neighborhood data
neighbor_fitness <- left_join(fitness_25, 
                              neighbors_25, 
                              by = c("PARK", "PLOT_TYPE", "PLOT", "SUBPLOT"), 
                              relationship = "many-to-many") %>%
  drop_na(FINALSEED) %>%
  select(-NOTES, -contains("INFL"), -contains("FR")) %>%
  mutate(PLOT = if_else(PLOT_TYPE == "ALONE", 0L, PLOT))

#replace NAs from species columns with 0 for computation
neighbor_fitness[, 10:ncol(neighbor_fitness)][is.na(neighbor_fitness[, 10:ncol(neighbor_fitness)])] <- 0

#create a column of the total number of competitors in each neighborhood
neighbor_fitness$comp_density <- rowSums(neighbor_fitness[, 10:ncol(neighbor_fitness)])

neighbor_fitness <- neighbor_fitness %>% 
  filter(!(PLOT_TYPE == "DIVERSE" & comp_density == 0)) %>% #filter out neighborhoods in diverse mix that were skipped
  mutate(comp_density = case_when(PLOT_TYPE == "DIVERSE" ~ comp_density - 1, #subtract one to take out the focal from the competitor count
                                  TRUE ~ comp_density))

saveRDS(neighbor_fitness, file = "25_neighbor_fitness.rds")




