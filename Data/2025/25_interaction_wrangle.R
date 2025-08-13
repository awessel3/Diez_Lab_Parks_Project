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
  filter(!grepl("gone", NOTES, ignore.case = TRUE))

join <- left_join(fitness_25, neighbors_25, relationship = "many-to-many") %>%
  drop_na(FINALSEED)

dim(fitness_25)
dim(join)
join


