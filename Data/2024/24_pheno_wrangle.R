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

### onset

# diverse flowering
new_rows_diverse <- expand_grid(
  PARK = c("BR", "RF", "SEM", "WIR"),
  PLOT = "ALONE",                
  SPECIES = c("GILCAP", "EPIDEN", "NAVSQU"),
  Date = as.Date("2024-07-25"),
  Phenophase = c("B", "FL", "FR", "V"),
  doy = yday(Date)) %>%
  mutate(Value = if_else(Phenophase %in% c("FL", "FR"), 1, 0))


pheno_24_diverse <- bind_rows(pheno_24_diverse, new_rows_diverse)
dim(pheno_24_diverse)

collom_onset <- pheno_24_diverse %>%
  pivot_wider(names_from = Phenophase, values_from = Value) %>%
  filter(
    SPECIES == "COLLOM",
    FR == 1,
    B == 1,
    FL %in% c(0, 1),
  ) %>% 
  pivot_longer(
    cols = c(FR, B, FL, V),
    names_to = "Phenophase",
    values_to = "Value"
  ) %>% 
  mutate(onset =yday(Date)) %>% 
  distinct(PARK, Date, .keep_all = TRUE) %>% 
  group_by(PARK) %>% 
  summarise(SPECIES = 'COLLOM', 
            onset = min(doy)) %>% 
  filter(!PARK == 'RF')


diverse_onset_24 <- pheno_24_diverse %>% 
  filter(Phenophase == 'FL', Value != 0) %>% 
  group_by(SPECIES, PARK) %>% 
  summarise(onset = min(doy)) 

diverse_onset_24 <- bind_rows(diverse_onset_24, collom_onset)

# diverse fruiting onset

diverse_onset_24_fr <- pheno_24_diverse %>% 
  filter(Phenophase == 'FR', Value != 0) %>% 
  group_by(SPECIES, PARK) %>% 
  summarise(onset = min(doy)) 


# alone 
new_rows_alone <- expand_grid(
  PARK = c("BR", "RF", "SEM", "WIR"),
  PLOT = "ALONE",                
  SPECIES = c("GILCAP", "EPIDEN", "NAVSQU"),
  Date = as.Date("2024-07-29"),
  Phenophase = c("B", "FL", "FR", "V"),
  doy = yday(Date)) %>%
  mutate(Value = if_else(Phenophase %in% c("FL", "FR"), 1, 0))

pheno_24_alone <- bind_rows(pheno_24_alone, new_rows_alone)
dim(pheno_24_alone)

collom_onset_alone <- pheno_24_alone %>%
  group_by(PARK, PLOT, SPECIES, Date, doy, Phenophase) %>%
  summarise(Value = max(Value), .groups = "drop") %>%  # pick max value for duplicates
  pivot_wider(names_from = Phenophase, values_from = Value) %>%
  filter(
    SPECIES == "COLLOM",
    FR != 0,
    B != 0
  ) %>% 
  pivot_longer(
    cols = c(FR, B, FL, V),
    names_to = "Phenophase",
    values_to = "Value"
  ) %>% 
  mutate(onset = yday(Date)) %>% 
  distinct(PARK, Date, .keep_all = TRUE) %>% 
  filter(!(PARK %in% c('RF', 'WIR'))) %>% 
  group_by(PARK) %>% 
  summarise(
    SPECIES = 'COLLOM',
    onset = min(onset),
    .groups = 'drop'
  )

alone_onset_24 <- pheno_24_alone %>% 
  filter(Phenophase == 'FL', Value != 0) %>% 
  group_by(SPECIES, PARK) %>% 
  summarise(onset = min(doy)) %>% arrange(onset)


alone_onset_24 <- bind_rows(alone_onset_24, collom_onset_alone)


# saveRDS(pheno_24_alone, "pheno_24_alone.rds")
# saveRDS(pheno_24_diverse, "pheno_24_diverse.rds")
# 
# saveRDS(alone_onset_24, "alone_onset_24.rds")
# saveRDS(diverse_onset_24, "diverse_onset_24.rds")

### offset

#diverse flowering 
diverse_offset_24 <- pheno_24_diverse %>% 
  filter(Phenophase == 'FL', Value != 0) %>% 
  group_by(SPECIES, PARK) %>% 
  summarise(offset = max(doy)) %>% arrange(offset)


collom_offset_diverse <- pheno_24_diverse %>%
  pivot_wider(names_from = Phenophase, values_from = Value) %>%
  filter(
    SPECIES == "COLLOM",
    FR == 1,
    B == 1,
    FL %in% c(0, 1),
  ) %>% 
  pivot_longer(
    cols = c(FR, B, FL, V),
    names_to = "Phenophase",
    values_to = "Value"
  ) %>% 
  mutate(offset =yday(Date)) %>% 
  distinct(PARK, Date, .keep_all = TRUE) %>% 
  group_by(PARK) %>% 
  summarise(SPECIES = 'COLLOM', 
            offset = max(doy)) %>% 
  filter(!PARK == 'RF')

diverse_offset_24 <-bind_rows(diverse_offset_24, collom_offset_diverse)

diverse_offset_24 <- pheno_24_diverse %>% 
  filter(Phenophase == 'FL', Value != 0) %>% 
  group_by(SPECIES, PARK) %>% 
  summarise(offset = max(doy)) %>% arrange(offset)


collom_offset_diverse <- pheno_24_diverse %>%
  pivot_wider(names_from = Phenophase, values_from = Value) %>%
  filter(
    SPECIES == "COLLOM",
    FR == 1,
    B == 1,
    FL %in% c(0, 1),
  ) %>% 
  pivot_longer(
    cols = c(FR, B, FL, V),
    names_to = "Phenophase",
    values_to = "Value"
  ) %>% 
  mutate(offset =yday(Date)) %>% 
  distinct(PARK, Date, .keep_all = TRUE) %>% 
  group_by(PARK) %>% 
  summarise(SPECIES = 'COLLOM', 
            offset = max(doy)) %>% 
  filter(!PARK == 'RF')

diverse_offset_24 <-bind_rows(diverse_offset_24, collom_offset_diverse)

# diverse fruiting offset

diverse_offset_24_fr <- pheno_24_diverse %>% 
  filter(Phenophase == 'FR', Value != 0) %>% 
  group_by(SPECIES, PARK) %>% 
  summarise(offset = max(doy)) %>% arrange(offset)

diverse_offset_24 <-bind_rows(diverse_offset_24, collom_offset_diverse)

# combine with onset - duration

diverse_fruiting_24 <- left_join(diverse_onset_24_fr, diverse_offset_24_fr)
diverse_fruiting_24 <- rename(diverse_fruiting_24, onset_fr = onset)
diverse_fruiting_24 <- rename(diverse_fruiting_24, offset_fr = offset)

diverse_flowering_24 <- left_join(diverse_onset_24, diverse_offset_24)
diverse_flowering_24 <- rename(diverse_flowering_24, onset_fl = onset)
diverse_flowering_24 <- rename(diverse_flowering_24, offset_fl = offset)

diverse_pheno_sum_24 <- merge(diverse_flowering_24, diverse_fruiting_24, by = c('SPECIES', 'PARK'))
diverse_pheno_sum_24 <- diverse_pheno_sum_24 %>% arrange(PARK)

diverse_pheno_sum_24 <- diverse_pheno_sum_24 %>%
  group_by(PARK, SPECIES) %>% 
  mutate(duration_fl = offset_fl - onset_fl,
         duration_fr = offset_fr - onset_fr,
         overlap = abs(duration_fr - duration_fl))

ggplot(diverse_pheno_sum_24) +
  geom_segment(aes(
    x = onset_fl, xend = offset_fl,
    y = SPECIES, yend = SPECIES,
    color = "FL"
  ), size = 3) +
  geom_segment(aes(
    x = onset_fr, xend = offset_fr,
    y = SPECIES, yend = SPECIES,
    color = "FR"
  ), size = 3, alpha = 0.6) +
  scale_color_manual(values = c("FL" = "#17BEBB", "FR" = "#F5BB00")) +
  facet_wrap(~PARK) +
  labs(
    x = "Day of Year",
    y = "Species",
    color = "Phenophase",
    title = "Flowering and Fruiting Periods"
  ) +
  theme_minimal()

ggplot(diverse_pheno_sum_24) +
  geom_segment(aes(
    x = onset_fl, xend = offset_fl,
    y = PARK, yend = PARK,
    color = "FL"
  ), size = 3) +
  geom_segment(aes(
    x = onset_fr, xend = offset_fr,
    y = PARK, yend = PARK,
    color = "FR"
  ), size = 3, alpha = 0.6) +
  scale_color_manual(values = c("FL" = "#17BEBB", "FR" = "#F5BB00")) +
  facet_wrap(~SPECIES) +
  labs(
    x = "Day of Year",
    y = "Species",
    color = "Phenophase",
    title = "Flowering & Fruiting Periods"
  ) +
  theme_minimal()

park_colors <- c(
  "BR"  = "#955F8E",  
  "RF"  = "#0F9554", 
  "WIR" = "#17BEBB",  
  "SEM" = "#F5BB00"   
)

# alone
ggplot(alone_onset_24, aes(x = onset, y = SPECIES, fill = PARK)) + geom_col(position = 'dodge') +
  theme_minimal() + scale_fill_manual(values = park_colors)

# diverse 
ggplot(diverse_onset_24, aes(x = onset, y = SPECIES, fill = PARK)) + geom_col(position = 'dodge') +
  theme_minimal() + scale_fill_manual(values = park_colors) + labs(title = 'diverse')
diverse_offset_24 <- diverse_offset_24 %>%
  mutate(Date = as.Date(offset - 1, origin = as.Date("2024-01-01")))





