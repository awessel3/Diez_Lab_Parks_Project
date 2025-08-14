library(lubridate)
library(tidyverse)
library(neonUtilities)
library(patchwork)
library(sp)
library(raster)


setwd("~/Desktop/Diez_Lab_Parks_Project/Data/2024")

neighbors_br_24 <- read.csv("24_neighbors_br.csv")
neighbors_rf_24 <- read.csv("24_neighbors_rf.csv")
neighbors_sem_24 <- read.csv("24_neighbors_sem.csv")
neighbors_wir_24 <- read.csv("24_neighbors_wir.csv")

fitness_24 <- read.csv("24_fitness.csv")
fitness_24 <- fitness_24 %>% dplyr::select(PARK, PLOT, SPECIES, REP, DATE, PLOT.1, SUBPLOT, 
                                           HEIGHT, BIOMASS, FINALFR, SEEDSPERFR, FINALSEED,NOTES)

# fitness_24_diverse <- fitness_24 %>% 
#   filter(PLOT == 'DIVERSE')

neighbors_br_24 <- rename(neighbors_br_24, AGRCAP = ABRCAP)
str(neighbors_wir_24 )

neighbors_24 <- bind_rows(
  neighbors_br_24,
  neighbors_rf_24,
  neighbors_sem_24,
  neighbors_wir_24)


neighbor_fitness_combine <- left_join(fitness_24, neighbors_24, by = c("PARK", "PLOT.1", "PLOT", "SUBPLOT"))
neighbor_alone <-  neighbor_fitness_combine %>% 
  filter(PLOT == 'ALONE') %>% 
  dplyr::select(-X)
neighbor_diverse <- neighbor_fitness_combine %>% 
  filter(PLOT == 'DIVERSE') %>% 
  filter(!is.na(HEIGHT), !(is.na(PLOT.1))) %>% 
  dplyr::select(-X)
neighbor_diverse[is.na(neighbor_diverse)] <- 0
str(neighbor_diverse)

neighbor_fitness <- bind_rows(neighbor_alone, neighbor_diverse)
neighbor_fitness <- neighbor_fitness %>% arrange(PARK)

neighbor_fitness$comp_density <- base::rowSums(neighbor_fitness[, 14:60]) 
neighbor_fitness <- neighbor_fitness %>% 
  filter(!(comp_density == 0 & PLOT == 'DIVERSE')) %>% 
  mutate( comp_density = case_when(PLOT == "DIVERSE" ~ comp_density - 1, TRUE ~ comp_density))
neighbor_fitness[is.na(neighbor_fitness)] <- 0

# outliers 
neighbor_fitness <- neighbor_fitness %>% 
  filter(!grepl("GONE", NOTES, ignore.case = TRUE)) %>% 
  filter(FINALSEED <= 40000)

saveRDS(neighbor_fitness, "24_neighbor_fitness.rds")


