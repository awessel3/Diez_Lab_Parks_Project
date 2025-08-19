library(tidyverse)
library(lubridate)

setwd("C:/Users/hmcLD/OneDrive/Desktop/Diez_Lab_Parks_Project")

br_neighbors <- read.csv("Data/2025/25_neighbors_br.csv") %>%
  rename(c(PLOT_TYPE = PLOT, 
           PLOT = PLOT.1)) %>%
  select(-NOTES)
rf_neighbors <- read.csv("Data/2025/25_neighbors_rf.csv") %>%
  rename(c(PLOT_TYPE = PLOT, 
           PLOT = PLOT.1)) %>%
  select(-NOTES)
sem_neighbors <- read.csv("Data/2025/25_neighbors_sem.csv") %>%
  rename(c(PLOT_TYPE = PLOT, 
           PLOT = PLOT.1)) %>%
  select(-NOTES)
wir_neighbors <- read.csv("Data/2025/25_neighbors_wir.csv") %>%
  rename(c(PLOT_TYPE = PLOT, 
           PLOT = PLOT.1)) %>%
  select(-NOTES)

colnames(br_neighbors)
colnames(rf_neighbors)
colnames(sem_neighbors)
colnames(wir_neighbors)

neighbors_25 <- bind_rows(br_neighbors, rf_neighbors, sem_neighbors, wir_neighbors) %>%
  rename(AGRCAP = agrostis.spp)

fitness_25 <- read.csv("Data/2025/25_fitness.csv") %>% 
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

neighbor_fitness <- left_join(fitness_25, 
                              neighbors_25, 
                              by = c("PARK", "PLOT_TYPE", "PLOT", "SUBPLOT"), 
                              relationship = "many-to-many") %>%
  drop_na(FINALSEED) %>%
  select(-NOTES, -contains("INFL"), -contains("FR")) %>%
  mutate(PLOT = if_else(PLOT_TYPE == "ALONE", 0L, PLOT))

neighbor_fitness[, 10:ncol(neighbor_fitness)][is.na(neighbor_fitness[, 10:ncol(neighbor_fitness)])] <- 0

neighbor_fitness$comp_density <- rowSums(neighbor_fitness[, 10:ncol(neighbor_fitness)])

neighbor_fitness <- neighbor_fitness %>% 
  filter(!(PLOT_TYPE == "DIVERSE" & comp_density == 0)) %>%
  mutate(comp_density = case_when(PLOT_TYPE == "DIVERSE" ~ comp_density - 1,
                                  TRUE ~ comp_density))

saveRDS(neighbor_fitness, file = "Data/2025/25_neighbor_fitness.rds")




