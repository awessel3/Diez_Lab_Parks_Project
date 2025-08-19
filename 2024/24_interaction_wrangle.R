library(lubridate)
library(tidyverse)
library(neonUtilities)
library(patchwork)
library(sp)
library(raster)

setwd("C:/Users/hmcLD/OneDrive/Desktop/Diez_Lab_Parks_Project/Data/2024")
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

mia_coef_br <- read.csv("mean_interaction_matrix_br.csv")
mia_coef_rf <- read.csv("mean_interaction_matrix_rf.csv")
mia_coef_sem <- read.csv("mean_interaction_matrix_sem.csv")
mia_coef_wir <- read.csv("mean_interaction_matrix_wir.csv")

pheno_sum_24 <- read_rds("diverse_phenology_summary_24.rds")

mia_coef_br$PARK <- "BR"
mia_coef_rf$PARK <- "RF"
mia_coef_sem$PARK <- "SEM"
mia_coef_wir$PARK <- "WIR"

competitors_natives <- colnames(mia_coef_br[, 2:9])

mia_coef <- bind_rows(mia_coef_br, mia_coef_rf, mia_coef_sem, mia_coef_wir)
mia_coef <- mia_coef %>% dplyr::select(all_of(colnames(mia_coef_br[, 1:9])), PARK) %>% 
  rename(SPECIES = RowNames)

pheno_sum_24 <- unique(pheno_sum_24)

coef_pheno <- left_join(pheno_sum_24, mia_coef, by = c("PARK", "SPECIES"), relationship = "many-to-many")

coef_pheno <- coef_pheno %>% pivot_longer(cols = all_of(competitors_natives), 
                                          names_to = "competitors",
                                          values_to = "coef")


#wir_coef_pheno <- coef_pheno %>% filter(PARK == "WIR")
ggplot(coef_pheno, aes(x = onset_fl, y = coef, color = competitors)) + geom_point() +
  geom_hline(yintercept = 0) + facet_wrap(~PARK) + geom_smooth(method = 'lm', se = FALSE)
