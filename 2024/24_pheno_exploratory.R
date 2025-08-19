library(lubridate)
library(tidyverse)
library(neonUtilities)
library(patchwork)
library(sp)
library(raster)
library(ggplot2)
library(ggeffects)

#Hailey file path
setwd("C:/Users/hmcLD/OneDrive/Desktop/Diez_Lab_Parks_Project/Data/2024")

pheno_24_alone <- read_rds("pheno_24_alone.rds")
pheno_24_diverse <- read_rds("pheno_24_diverse.rds")

alone_onset_24 <- readRDS("alone_onset_24.rds")
diverse_onset_24 <- readRDS("diverse_onset_24.rds")

colnames(pheno_24_alone)


park_colors <- c(
  "BR"  = "#D81E5B",  # red
  "RF"  = "#7DCE82",  # green
  "WIR" = "#0496FF",  # blue
  "SEM" = "#F18F01"   # yellow
)

#PHENOPHASE SUMMARIES ----
# alone
alone_phase <- pheno_24_alone %>% pivot_wider(names_from = Phenophase, values_from = Value)

phenophase_cols <- c("B","V","FL","FR")
alone_phase[phenophase_cols] <- lapply(alone_phase[phenophase_cols], function(x) sapply(x, `[`, 1))
str(alone_phase)

value_prop <- alone_phase %>%
  group_by(PARK, Date, SPECIES) %>%
  summarise(
    B_total = sum(B, na.rm = TRUE),
    V_total = sum(V, na.rm = TRUE),
    FL_total = sum(FL, na.rm = TRUE),
    FR_total = sum(FR, na.rm = TRUE)
  ) %>%
  mutate(
    total_all = B_total + V_total + FL_total + FR_total,
    B_prop = B_total / total_all,
    V_prop = V_total / total_all,
    FL_prop = FL_total / total_all,
    FR_prop = FR_total / total_all
  )

value_prop <- value_prop %>% 
  pivot_longer(cols = c(FR_prop, B_prop, FL_prop, V_prop),
               names_to = "Phenophase",
               values_to = "Value")

ggplot(alone_phase, aes(x = Date, y = FL)) + geom_point() + geom_smooth() +
  facet_wrap(~SPECIES)

phase_colors <- c(
  "B_prop"  = "#0AA5FF",  
  "V_prop"  = "#0FBD77",  
  "FR_prop" = "#E0CA3C",  
  "FL_prop" = "#D683D8"  
)

#CLAPUR 
clapur_prop <- value_prop %>% filter(SPECIES == 'CLAPUR')

ggplot(clapur_prop, aes(x = Date, y = Value, color = Phenophase)) + geom_point() + geom_smooth(alpha = 0.2) +
  facet_wrap(~PARK) + labs(title = 'CLAPUR Phenophases over Time',
                           y = 'Proportion in Phase') + theme_minimal() + 
  scale_color_manual(values = phase_colors)

#COLLOM
collom_prop <- value_prop %>% filter(SPECIES == 'COLLOM')

ggplot(collom_prop, aes(x = Date, y = Value, color = Phenophase)) + geom_point() + geom_smooth(alpha = 0.2) +
  facet_wrap(~PARK) + labs(title = 'COLLOM Phenophases over Time',
                           y = 'Proportion in Phase') + theme_minimal() + 
  scale_color_manual(values = phase_colors)

#COLLIN
collin_prop <- value_prop %>% filter(SPECIES == 'COLLIN') %>% 
  na.omit()

ggplot(collin_prop, aes(x = Date, y = Value, color = Phenophase)) + geom_point() + geom_smooth(alpha = 0.2) +
  facet_wrap(~PARK) + labs(title = 'COLLIN Phenophases over Time',
                           y = 'Proportion in Phase') + theme_minimal()+ 
  scale_color_manual(values = phase_colors)

#GILCAP
gilcap_prop <- value_prop %>% filter(SPECIES == 'GILCAP')

ggplot(gilcap_prop, aes(x = Date, y = Value, color = Phenophase)) + geom_point() + geom_smooth(alpha = 0.2) +
  facet_wrap(~PARK) + labs(title = 'GILCAP Phenophases over Time',
                           y = 'Proportion in Phase') + theme_minimal()+ 
  scale_color_manual(values = phase_colors)

#PLECON
plecon_prop <- value_prop %>% filter(SPECIES == 'PLECON') %>% 
  na.omit()

ggplot(plecon_prop, aes(x = Date, y = Value, color = Phenophase)) + geom_point() + geom_smooth(alpha = 0.2) +
  facet_wrap(~PARK) + labs(title = 'PLECON Phenophases over Time',
                           y = 'Proportion in Phase') + theme_minimal()+ 
  scale_color_manual(values = phase_colors)

#NAVSQU
navsqu_prop <- value_prop %>% filter(SPECIES == 'NAVSQU')

ggplot(navsqu_prop, aes(x = Date, y = Value, color = Phenophase)) + geom_point() + geom_smooth(alpha = 0.2) +
  facet_wrap(~PARK) + labs(title = 'NAVSQU Phenophases over Time',
                           y = 'Proportion in Phase') + theme_minimal() + 
  scale_color_manual(values = phase_colors)

#PLAFIG
plafig_prop <- value_prop %>% filter(SPECIES == 'PLAFIG') %>% na.omit()

ggplot(plafig_prop, aes(x = Date, y = Value, color = Phenophase)) + geom_point() + geom_smooth(alpha = 0.2) +
  facet_wrap(~PARK) + labs(title = 'PLAFIG Phenophases over Time',
                           y = 'Proportion in Phase') + theme_minimal() + 
  scale_color_manual(values = phase_colors)

# diverse
  
diverse_phase <- pheno_24_diverse 

phase_colors <- c(
  "B"  = "#0AA5FF",  
  "V"  = "#0FBD77",  
  "FR" = "#E0CA3C",  
  "FL" = "#D683D8"  
)

#CLAPUR 
clapur_phase <- diverse_phase %>% filter(SPECIES == 'CLAPUR')

ggplot(clapur_phase, aes(x = Date, y = Value, color = Phenophase)) + geom_point() + geom_smooth(alpha = 0.2) +
  facet_wrap(~PARK) + labs(title = 'CLAPUR Diverse Phenophases over Time',
                           y = 'Proportion in Phase') + theme_minimal() + 
  scale_color_manual(values = phase_colors)

#COLLOM
collom_phase <- diverse_phase %>% filter(SPECIES == 'COLLOM')

ggplot(collom_phase, aes(x = Date, y = Value, color = Phenophase)) + geom_point() + geom_smooth(alpha = 0.2) +
  facet_wrap(~PARK) + labs(title = 'COLLOM Diverse Phenophases over Time',
                           y = 'Proportion in Phase') + theme_minimal() + 
  scale_color_manual(values = phase_colors)

#COLLIN
collin_phase <- diverse_phase %>% filter(SPECIES == 'COLLIN') %>% 
  na.omit()

ggplot(collin_phase, aes(x = Date, y = Value, color = Phenophase)) + geom_point() + geom_smooth(alpha = 0.2) +
  facet_wrap(~PARK) + labs(title = 'COLLIN Diverse Phenophases over Time',
                           y = 'Proportion in Phase') + theme_minimal()+ 
  scale_color_manual(values = phase_colors)

#GILCAP
gilcap_phase <- diverse_phase %>% filter(SPECIES == 'GILCAP')

ggplot(gilcap_phase, aes(x = Date, y = Value, color = Phenophase)) + geom_point() + geom_smooth(alpha = 0.2) +
  facet_wrap(~PARK) + labs(title = 'GILCAP Diverse Phenophases over Time',
                           y = 'Proportion in Phase') + theme_minimal()+ 
  scale_color_manual(values = phase_colors)

#PLECON
plecon_phase <- diverse_phase %>% filter(SPECIES == 'PLECON') %>% 
  na.omit()

ggplot(plecon_phase, aes(x = Date, y = Value, color = Phenophase)) + geom_point() + geom_smooth(alpha = 0.2) +
  facet_wrap(~PARK) + labs(title = 'PLECON Diverse Phenophases over Time',
                           y = 'Proportion in Phase') + theme_minimal()+ 
  scale_color_manual(values = phase_colors)

#NAVSQU
navsqu_phase <- diverse_phase %>% filter(SPECIES == 'NAVSQU')

ggplot(navsqu_phase, aes(x = Date, y = Value, color = Phenophase)) + geom_point() + geom_smooth(alpha = 0.2) +
  facet_wrap(~PARK) + labs(title = 'NAVSQU Diverse Phenophases over Time',
                           y = 'Proportion in Phase') + theme_minimal() + 
  scale_color_manual(values = phase_colors)

#PLAFIG
plafig_phase <- diverse_phase %>% filter(SPECIES == 'PLAFIG') %>% na.omit()

ggplot(plafig_phase, aes(x = Date, y = Value, color = Phenophase)) + geom_point() + geom_smooth(alpha = 0.2) +
  facet_wrap(~PARK) + labs(title = 'PLAFIG Diverse Phenophases over Time',
                           y = 'Proportion in Phase') + theme_minimal() + 
  scale_color_manual(values = phase_colors)

# Overlap ----

diverse_pheno_sum_24 <- read_rds("diverse_phenology_summary_24.rds")

#facet by park
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

#facet by species
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

# alone
ggplot(alone_onset_24, aes(x = onset, y = SPECIES, fill = PARK)) + geom_col(position = 'dodge') +
  theme_minimal() + scale_fill_manual(values = park_colors)

# diverse 
ggplot(diverse_onset_24, aes(x = onset, y = SPECIES, fill = PARK)) + geom_col(position = 'dodge') +
  theme_minimal() + scale_fill_manual(values = park_colors) + labs(title = 'diverse')
diverse_offset_24 <- diverse_offset_24 %>%
  mutate(Date = as.Date(offset - 1, origin = as.Date("2024-01-01")))


#ONSET FECUNDITY ----
fitness_24 <- read.csv("24_fitness.csv") %>%
    select(1:18) %>%
    rename(c(PLOT_TYPE = PLOT, 
             PLOT = PLOT.1)) %>%
    filter(!grepl("gone", NOTES, ignore.case = TRUE))

ff_heights_24 <- read.csv("24_ff_heights.csv") %>%
   rename(c(PLOT_TYPE = PLOT, 
            FFHEIGHT = HEIGHT,
            FFDATE = DATEFF)) 

colnames(fitness_24)
colnames(ff_heights_24)

onset_fec <- left_join(
  fitness_24,
  ff_heights_24,
  by = c("PARK", "PLOT_TYPE", "SPECIES", "REP")) %>% 
  select(PARK, PLOT_TYPE, SPECIES, REP, HEIGHT, FINALSEED, FFDATE, FFHEIGHT) %>%
  drop_na(FINALSEED, FFDATE, FFHEIGHT) %>%
  mutate(FFDATE = mdy(FFDATE),
         doy = yday(FFDATE)) 

CLAPUR <- onset_fec %>% filter(SPECIES == "CLAPUR") %>%
  ggplot(aes(x = doy, y = FINALSEED, linetype = PLOT_TYPE, color = PARK)) +
  geom_point(aes(shape = PLOT_TYPE)) +
  geom_smooth(method = "loess", span = 1) +
  facet_wrap(~PARK, scales = "free") +
  scale_color_manual(values = park_colors) +
  labs(
    x = "FF Day of Year",
    y = "Fecundity",
    title = "CLAPUR") + 
  theme_minimal()

ggsave("figures/ff_fitness_clapur_25.jpeg", plot = CLAPUR, width = 8, height = 6, dpi = 300)

COLLIN <- onset_fec %>% filter(SPECIES == "COLLIN") %>%
  ggplot(aes(x = doy, y = FINALSEED, linetype = PLOT_TYPE, color = PARK)) +
  geom_point(aes(shape = PLOT_TYPE)) +
  geom_smooth(method = "loess", span = 1) +
  facet_wrap(~PARK, scales = "free") +
  scale_color_manual(values = park_colors) +
  labs(
    x = "FF Day of Year",
    y = "Fecundity",
    title = "COLLIN") + 
  theme_minimal()

ggsave("figures/ff_fitness_collin_25.jpeg", plot = COLLIN, width = 8, height = 6, dpi = 300)

GILCAP <- onset_fec %>% filter(SPECIES == "GILCAP") %>%
  ggplot(aes(x = doy, y = FINALSEED, linetype = PLOT_TYPE, color = PARK)) +
  geom_point(aes(shape = PLOT_TYPE)) +
  geom_smooth(method = "loess", span = 1) +
  facet_wrap(~PARK, scales = "free") +
  scale_color_manual(values = park_colors) +
  labs(
    x = "FF Day of Year",
    y = "Fecundity",
    title = "GILCAP") + 
  theme_minimal()

ggsave("figures/ff_fitness_gilcap_25.jpeg", plot = GILCAP, width = 8, height = 6, dpi = 300)

COLLOM <- onset_fec %>% filter(SPECIES == "COLLOM") %>%
  ggplot(aes(x = doy, y = FINALSEED, linetype = PLOT_TYPE, color = PARK)) +
  geom_point(aes(shape = PLOT_TYPE)) +
  geom_smooth(method = "loess", span = 1) +
  facet_wrap(~PARK, scales = "free") +
  scale_color_manual(values = park_colors) +
  labs(
    x = "FF Day of Year",
    y = "Fecundity",
    title = "COLLOM") + 
  theme_minimal()

ggsave("figures/ff_fitness_collom_25.jpeg", plot = COLLOM, width = 8, height = 6, dpi = 300)

PLECON <- onset_fec %>% filter(SPECIES == "PLECON") %>%
  ggplot(aes(x = doy, y = FINALSEED, linetype = PLOT_TYPE, color = PARK)) +
  geom_point(aes(shape = PLOT_TYPE)) +
  geom_smooth(method = "loess", span = 1) +
  facet_wrap(~PARK, scales = "free") +
  scale_color_manual(values = park_colors) +
  labs(
    x = "FF Day of Year",
    y = "Fecundity",
    title = "PLECON") + 
  theme_minimal()

ggsave("figures/ff_fitness_plecon_25.jpeg", plot = PLECON, width = 8, height = 6, dpi = 300)

NAVSQU <- onset_fec %>% filter(SPECIES == "NAVSQU") %>%
  ggplot(aes(x = doy, y = FINALSEED, linetype = PLOT_TYPE, color = PARK)) +
  geom_point(aes(shape = PLOT_TYPE)) +
  geom_smooth(method = "loess", span = 1) +
  facet_wrap(~PARK, scales = "free") +
  scale_color_manual(values = park_colors) +
  labs(
    x = "FF Day of Year",
    y = "Fecundity",
    title = "NAVSQU") + 
  theme_minimal()

ggsave("figures/ff_fitness_navsqu_25.jpeg", plot = NAVSQU, width = 8, height = 6, dpi = 300)

EPIDEN <- onset_fec %>% filter(SPECIES == "EPIDEN") %>%
  ggplot(aes(x = doy, y = FINALSEED, linetype = PLOT_TYPE, color = PARK)) +
  geom_point(aes(shape = PLOT_TYPE)) +
  geom_smooth(method = "loess", span = 1) +
  facet_wrap(~PARK, scales = "free") +
  scale_color_manual(values = park_colors) +
  labs(
    x = "FF Day of Year",
    y = "Fecundity",
    title = "EPIDEN") + 
  theme_minimal()

ggsave("figures/ff_fitness_epiden_25.jpeg", plot = EPIDEN, width = 8, height = 6, dpi = 300)

PLAFIG <- onset_fec %>% filter(SPECIES == "PLAFIG") %>%
  ggplot(aes(x = doy, y = FINALSEED, linetype = PLOT_TYPE, color = PARK)) +
  geom_point(aes(shape = PLOT_TYPE)) +
  geom_smooth(method = "loess", span = 1) +
  facet_wrap(~PARK, scales = "free") +
  scale_color_manual(values = park_colors) +
  labs(
    x = "FF Day of Year",
    y = "Fecundity",
    title = "PLAFIG") + 
  theme_minimal()

ggsave("figures/ff_fitness_plafig_25.jpeg", plot = PLAFIG, width = 8, height = 6, dpi = 300)

#view plots
CLAPUR
COLLIN
GILCAP
COLLOM
PLECON
NAVSQU
EPIDEN
PLAFIG

#sample sizes
sample_sizes <- onset_fec %>% group_by(PARK, PLOT_TYPE, SPECIES) %>%
  summarise(n = n())
sample_sizes
