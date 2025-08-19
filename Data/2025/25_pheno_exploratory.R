library(tidyverse)
library(lubridate)

setwd("C:/Users/hmcLD/OneDrive/Desktop/Diez_Lab_Parks_Project")

fitness_25 <- read.csv("Data/2025/25_fitness.csv") %>% 
  select(1:16) %>%
  rename(c(PLOT_TYPE = PLOT, 
           PLOT = PLOT.1)) %>%
  filter(!grepl("gone", NOTES, ignore.case = TRUE))

ff_heights_25 <- read.csv("Data/2025/25_ff_heights.csv") %>%
   select(1:10) %>%
   rename(c(PLOT_TYPE = PLOT, 
          FFHEIGHT = HEIGHT)) 

pheno_25_alone <- read_rds("Data/2025/pheno_25_alone.rds")
pheno_25_diverse <- read_rds("Data/2025/pheno_25_diverse.rds")

park_colors <- c(
  "BR"  = "#D81E5B",  # red
  "RF"  = "#7DCE82",  # green
  "WIR" = "#0496FF",  # blue
  "SEM" = "#F18F01"   # yellow
)

prop_colors <- c(
  "B_prop"  = "#0AA5FF",  
  "V_prop"  = "#0FBD77",  
  "FR_prop" = "#E0CA3C",  
  "FL_prop" = "#D683D8"  
)

phase_colors <- c(
  "B"  = "#0AA5FF",  
  "V"  = "#0FBD77",  
  "FR" = "#E0CA3C",  
  "FL" = "#D683D8"  
)

#PHENOPHASE SUMMARIES ----

# alone
alone_phase <- pheno_25_alone %>% pivot_wider(names_from = Phenophase, values_from = Value)

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

#flowering curves
ggplot(alone_phase, aes(x = Date, y = FL)) + geom_point() + geom_smooth() +
  facet_wrap(~SPECIES)

#CLAPUR 
clapur_prop <- value_prop %>% filter(SPECIES == 'CLAPUR')

ggplot(clapur_prop, aes(x = Date, y = Value, color = Phenophase)) + geom_point() + geom_smooth(alpha = 0.2) +
  facet_wrap(~PARK) + labs(title = 'CLAPUR Phenophases over Time',
                           y = 'Proportion in Phase') + theme_minimal() + 
  scale_color_manual(values = prop_colors)

#COLLOM
collom_prop <- value_prop %>% filter(SPECIES == 'COLLOM')

ggplot(collom_prop, aes(x = Date, y = Value, color = Phenophase)) + geom_point() + geom_smooth(alpha = 0.2) +
  facet_wrap(~PARK) + labs(title = 'COLLOM Phenophases over Time',
                           y = 'Proportion in Phase') + theme_minimal() + 
  scale_color_manual(values = prop_colors)

#COLLIN
collin_prop <- value_prop %>% filter(SPECIES == 'COLLIN') %>% 
  na.omit()

ggplot(collin_prop, aes(x = Date, y = Value, color = Phenophase)) + geom_point() + geom_smooth(alpha = 0.2) +
  facet_wrap(~PARK) + labs(title = 'COLLIN Phenophases over Time',
                           y = 'Proportion in Phase') + theme_minimal()+ 
  scale_color_manual(values = prop_colors)

#GILCAP
gilcap_prop <- value_prop %>% filter(SPECIES == 'GILCAP')

ggplot(gilcap_prop, aes(x = Date, y = Value, color = Phenophase)) + geom_point() + geom_smooth(alpha = 0.2) +
  facet_wrap(~PARK) + labs(title = 'GILCAP Phenophases over Time',
                           y = 'Proportion in Phase') + theme_minimal()+ 
  scale_color_manual(values = prop_colors)

#PLECON
plecon_prop <- value_prop %>% filter(SPECIES == 'PLECON') %>% 
  na.omit()

ggplot(plecon_prop, aes(x = Date, y = Value, color = Phenophase)) + geom_point() + geom_smooth(alpha = 0.2) +
  facet_wrap(~PARK) + labs(title = 'PLECON Phenophases over Time',
                           y = 'Proportion in Phase') + theme_minimal()+ 
  scale_color_manual(values = prop_colors)

#NAVSQU
navsqu_prop <- value_prop %>% filter(SPECIES == 'NAVSQU')

ggplot(navsqu_prop, aes(x = Date, y = Value, color = Phenophase)) + geom_point() + geom_smooth(alpha = 0.2) +
  facet_wrap(~PARK) + labs(title = 'NAVSQU Phenophases over Time',
                           y = 'Proportion in Phase') + theme_minimal() + 
  scale_color_manual(values = prop_colors)

#PLAFIG
plafig_prop <- value_prop %>% filter(SPECIES == 'PLAFIG') %>% na.omit()

ggplot(plafig_prop, aes(x = Date, y = Value, color = Phenophase)) + geom_point() + geom_smooth(alpha = 0.2) +
  facet_wrap(~PARK) + labs(title = 'PLAFIG Phenophases over Time',
                           y = 'Proportion in Phase') + theme_minimal() + 
  scale_color_manual(values = prop_colors)

#EPIDEN
epiden_prop <- value_prop %>% filter(SPECIES == 'EPIDEN')

ggplot(epiden_prop, aes(x = Date, y = Value, color = Phenophase)) + geom_point() + geom_smooth(alpha = 0.2) +
  facet_wrap(~PARK) + labs(title = 'EPIDEN Phenophases over Time',
                           y = 'Proportion in Phase') + theme_minimal() + 
  scale_color_manual(values = prop_colors)



#diverse
diverse_phase <- pheno_25_diverse 

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

#EPIDEN
epiden_phase <- diverse_phase %>% filter(SPECIES == 'EPIDEN') %>% na.omit()

ggplot(plafig_phase, aes(x = Date, y = Value, color = Phenophase)) + geom_point() + geom_smooth(alpha = 0.2) +
  facet_wrap(~PARK) + labs(title = 'PLAFIG Diverse Phenophases over Time',
                           y = 'Proportion in Phase') + theme_minimal() + 
  scale_color_manual(values = phase_colors)


#ONSET FECUNDITY ----

colnames(fitness_25)
colnames(ff_heights_25)

onset_fec <- left_join(
  fitness_25,
  ff_heights_25,
  by = c("PARK", "PLOT_TYPE", "SPECIES", "REP")) %>% 
  select(PARK, PLOT_TYPE, SPECIES, REP, HEIGHT, FINALSEED, FFDATE, FFHEIGHT) %>%
  drop_na(FINALSEED, FFDATE, FFHEIGHT) %>%
  mutate(FFDATE = mdy(FFDATE),
         doy = yday(FFDATE)) 

# onset_fec_zoom <- onset_fec %>%
#   mutate(FINALSEED = if_else(
#     (SPECIES %in% c("EPIDEN") & FINALSEED > 7500) | (SPECIES %in% c("PLECON") & FINALSEED > 1000),
#     NA, FINALSEED
#   )) %>% 
#   drop_na(FINALSEED)
# 
# ggplot(onset_fec_zoom, aes(x = doy, y = FINALSEED, color = PARK)) + 
#   scale_color_manual(values = park_colors) +
#   geom_point() + 
#   geom_smooth(se = FALSE) +
#   facet_wrap(~SPECIES, scales = "free") +
#   labs(
#     x = "FF Day of Year",
#     y = "Fecundity"
#   ) + theme_minimal()
  
CLAPUR <- onset_fec %>% filter(SPECIES == "CLAPUR") %>%
  ggplot(aes(x = doy, y = FINALSEED, linetype = PLOT_TYPE, color = PARK)) +
  geom_point(aes(shape = PLOT_TYPE)) +
  stat_smooth(formula = y ~ x + I(x^2)) +
  facet_wrap(~PARK, scales = "free") +
  scale_color_manual(values = park_colors) +
  labs(
         x = "FF Day of Year",
         y = "Fecundity",
         title = "CLAPUR") + 
  theme_minimal()

ggsave("Data/2025/figures/ff_fitness_clapur_25.jpeg", plot = CLAPUR, width = 8, height = 6, dpi = 300)

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

ggsave("Data/2025/figures/ff_fitness_collin_25.jpeg", plot = COLLIN, width = 8, height = 6, dpi = 300)

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

ggsave("Data/2025/figures/ff_fitness_gilcap_25.jpeg", plot = GILCAP, width = 8, height = 6, dpi = 300)

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

ggsave("Data/2025/figures/ff_fitness_collom_25.jpeg", plot = COLLOM, width = 8, height = 6, dpi = 300)

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

ggsave("Data/2025/figures/ff_fitness_plecon_25.jpeg", plot = PLECON, width = 8, height = 6, dpi = 300)

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

ggsave("Data/2025/figures/ff_fitness_navsqu_25.jpeg", plot = NAVSQU, width = 8, height = 6, dpi = 300)

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

ggsave("Data/2025/figures/ff_fitness_epiden_25.jpeg", plot = EPIDEN, width = 8, height = 6, dpi = 300)

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

ggsave("Data/2025/figures/ff_fitness_plafig_25.jpeg", plot = PLAFIG, width = 8, height = 6, dpi = 300)

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
