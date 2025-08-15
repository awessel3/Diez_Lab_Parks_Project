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

park_colors <- c(
  "BR"  = "#D81E5B",  # red
  "RF"  = "#7DCE82",  # green
  "WIR" = "#0496FF",  # blue
  "SEM" = "#F18F01"   # yellow
)

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

CLAPUR
COLLIN
GILCAP
COLLOM
PLECON
NAVSQU
EPIDEN
PLAFIG
