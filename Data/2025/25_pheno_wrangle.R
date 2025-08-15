library(tidyverse)
library(lubridate)

setwd("C:/Users/hmcLD/OneDrive/Desktop/Diez_Lab_Parks_Project")

pheno_25 <- read.csv("Data/2025/25_phenology.csv")


park_colors <- c(
  "BR"  = "#955F8E",  # red
  "RF"  = "#0F9554",  # green
  "WIR" = "#17BEBB",  # blue
  "SEM" = "#F5BB00"   # yellow
)

#data cleaning/wrangling

pheno_25 <- pheno_25 %>%
  mutate(Date = mdy(Date),
         doy = yday(Date))

pheno_25$Value[is.na(pheno_25$Value)] <- 0

#alones
alones_25 <- pheno_25 %>%
  filter(PLOT == "ALONE")

new_rows_alone <- expand_grid(
  PARK = c("BR", "RF", "SEM", "WIR"),
  PLOT = "ALONE",                
  SPECIES = c("GILCAP", "EPIDEN", "NAVSQU"),
  Date = as.Date("2025-07-29"),
  Phenophase = c("B", "FL", "FR", "V"),
  doy = yday(Date)) %>%
  mutate(Value = if_else(Phenophase == "FL", 1, 0))

alones_25 <- bind_rows(alones_25, new_rows_alone)

#diverse
diverse_25 <- pheno_25 %>%
  filter(PLOT == "DIVERSE")

new_rows_diverse <- expand_grid(
  PARK = c("BR", "RF", "SEM", "WIR"),
  PLOT = "DIVERSE",                
  SPECIES = c("GILCAP", "EPIDEN", "NAVSQU"),
  Date = as.Date("2025-07-29"),
  Phenophase = c("B", "FL", "FR", "V"),
  doy = yday(Date)) %>%
  mutate(Value = if_else(Phenophase == "FL", 1, 0))

diverse_25 <- bind_rows(diverse_25, new_rows_diverse)

onset_alone <- alones_25 %>% 
  filter(Value != 0 & Phenophase == "FL") %>%
  group_by(PARK, SPECIES) %>%
  summarise(onset = min(doy))
  
ggplot(onset_alone, aes(x = onset, y = SPECIES, fill = PARK)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = park_colors) +
  labs(
    x = "Onset (Day of Year)",
    y = "Species",
    title = "Onset Timing 2025 Alones"
  ) +
  theme_minimal() 

onset_diverse <- diverse_25 %>% 
  filter(Value != 0 & Phenophase == "FL") %>%
  group_by(PARK, SPECIES) %>%
  summarise(onset = min(doy))

ggplot(onset_diverse, aes(x=onset, y=SPECIES, fill = PARK)) +
  geom_col(position = "dodge") + 
  scale_fill_manual(values = park_colors) +
  labs(
    x = "Onset (Day of Year)",
    y = "Species",
    title = "Onset Timing 2025 Diverse"
  ) +
  theme_minimal() 

#Can't use NAVSQU for onset in diverse

#July29 - last day of BR data collection (end day for EPIDEN, NAVSQU, GILCAP)
