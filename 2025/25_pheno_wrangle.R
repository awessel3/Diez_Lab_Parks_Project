library(tidyverse)
library(lubridate)

#Hailey file path
setwd("C:/Users/hmcLD/OneDrive/Desktop/Diez_Lab_Parks_Project/2025/data")

pheno_25 <- read.csv("25_phenology.csv")


park_colors <- c(
  "BR"  = "#D81E5B",  # red
  "RF"  = "#7DCE82",  # green
  "WIR" = "#0496FF",  # blue
  "SEM" = "#F18F01"   # yellow
)

#data cleaning/wrangling

pheno_25 <- pheno_25 %>%
  mutate(Date = mdy(Date),
         doy = yday(Date))

pheno_25$Value[is.na(pheno_25$Value)] <- 0 #turn NA values to 0 for computation

#alones
alones_25 <- pheno_25 %>%
  filter(PLOT == "ALONE")

#add a phenophase block for each of the listed species to make the 
#final flower date the final day of harvest
new_rows_alone <- expand_grid(
  PARK = c("BR", "RF", "SEM", "WIR"),
  PLOT = "ALONE",                
  SPECIES = c("GILCAP", "EPIDEN", "NAVSQU"),
  Date = as.Date("2025-07-29"),
  Phenophase = c("B", "FL", "FR", "V"),
  doy = yday(Date)) %>%
  mutate(Value = if_else(Phenophase == "FL", 1, 0)) #0 for all other phenophases, only interested in flowering

alones_25 <- bind_rows(alones_25, new_rows_alone)

#saveRDS(alones_25, "pheno_25_alone.rds")

#diverse
diverse_25 <- pheno_25 %>%
  filter(PLOT == "DIVERSE")

#same as alones adding phenophase blocks for a final flower date
new_rows_diverse <- expand_grid(
  PARK = c("BR", "RF", "SEM", "WIR"),
  PLOT = "DIVERSE",                
  SPECIES = c("GILCAP", "EPIDEN", "NAVSQU"),
  Date = as.Date("2025-07-29"),
  Phenophase = c("B", "FL", "FR", "V"),
  doy = yday(Date)) %>%
  mutate(Value = if_else(Phenophase == "FL", 1, 0))

diverse_25 <- bind_rows(diverse_25, new_rows_diverse)

#saveRDS(diverse_25, "pheno_25_diverse.rds")

#FIND ONSET DATE ----
#find flowering onset day of year
onset_alone <- alones_25 %>% 
  filter(Value != 0 & Phenophase == "FL") %>%
  group_by(PARK, SPECIES) %>%
  summarise(onset = min(doy)) #take the min to get the first flowering instance
  
#visualize onset alones
ggplot(onset_alone, aes(x = onset, y = SPECIES, fill = PARK)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = park_colors) +
  labs(
    x = "Onset (Day of Year)",
    y = "Species",
    title = "Onset Timing 2025 Alones"
  ) +
  theme_minimal() 

#same as alones
onset_diverse <- diverse_25 %>% 
  filter(Value != 0 & Phenophase == "FL") %>%
  group_by(PARK, SPECIES) %>%
  summarise(onset = min(doy))

#visualize onset diverse
ggplot(onset_diverse, aes(x=onset, y=SPECIES, fill = PARK)) +
  geom_col(position = "dodge") + 
  scale_fill_manual(values = park_colors) +
  labs(
    x = "Onset (Day of Year)",
    y = "Species",
    title = "Onset Timing 2025 Diverse"
  ) +
  theme_minimal() 

#Can't use NAVSQU for onset in diverse, never flowered
  #What appears on the graph is from the added phenophase blocks

#July29 - last day of BR data collection (end day for EPIDEN, NAVSQU, GILCAP)
