library(lubridate)
library(tidyverse)
library(neonUtilities)
library(patchwork)
library(sp)
library(raster)


setwd("~/Desktop/Diez_Lab_Parks_Project/Data/2024")

pheno_24 <- read.csv("24_phenology.csv")
dim(pheno_24)
colnames(pheno_24)

pheno_24$Value[is.na(pheno_24$Value)] <- 0
str(pheno_24)

pheno_24 <- pheno_24 %>%
  mutate(Date =ymd(Date)
         ,doy = yday(Date)
  ) 

pheno_24_alone <- pheno_24 %>% filter(PLOT == 'ALONE')
pheno_24_diverse <- pheno_24 %>% filter(PLOT == 'DIVERSE')

dim(pheno_24_alone)
dim(pheno_24_diverse)

# GILCAP, EPIDEN, NAVSQU

# diverse 
new_rows_diverse <- expand_grid(
  PARK = c("BR", "RF", "SEM", "WIR"),
  PLOT = "ALONE",                
  SPECIES = c("GILCAP", "EPIDEN", "NAVSQU"),
  Date = as.Date("2025-07-29"),
  Phenophase = c("B", "FL", "FR", "V"),
  doy = yday(Date)) %>%
  mutate(Value = if_else(Phenophase == "FL", 1, 0))

pheno_24_diverse <- bind_rows(pheno_24_diverse, new_rows_diverse)
dim(pheno_24_diverse)

diverse_onset_24 <- pheno_24_diverse %>% 
  filter(Phenophase == 'FL', Value != 0) %>% 
  group_by(SPECIES, PARK) %>% 
  summarise(onset = min(doy))

# alone 
new_rows_alone <- expand_grid(
  PARK = c("BR", "RF", "SEM", "WIR"),
  PLOT = "ALONE",                
  SPECIES = c("GILCAP", "EPIDEN", "NAVSQU"),
  Date = as.Date("2025-07-29"),
  Phenophase = c("B", "FL", "FR", "V"),
  doy = yday(Date)) %>%
  mutate(Value = if_else(Phenophase == "FL", 1, 0))

pheno_24_alone <- bind_rows(pheno_24_alone, new_rows_alone)
dim(pheno_24_alone)


alone_onset_24 <- pheno_24_alone %>% 
  filter(Phenophase == 'FL', Value != 0) %>% 
  group_by(SPECIES, PARK) %>% 
  summarise(onset = min(doy)) %>% arrange(onset)

park_colors <- c(
  "BR"  = "#955F8E",  # red
  "RF"  = "#0F9554",  # green
  "WIR" = "#17BEBB",  # blue
  "SEM" = "#F5BB00"   # yellow
)

ggplot(alone_onset_24, aes(x = onset, y = SPECIES, fill = PARK)) + geom_col(position = 'dodge') +
  theme_minimal() + scale_fill_manual(values = park_colors)


