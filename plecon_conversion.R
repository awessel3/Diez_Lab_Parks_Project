library(tidyverse)
library(lme4)
library(janitor)
library(writexl)

#load data ----
drought_plec <- read.csv("LAB/data/PLEC_drought.csv")

alones <- read.csv("LAB/data/alone focals 2024.csv") %>% select(1:16)
diverse <- read.csv("LAB/data/diverse focals 2024.csv") %>% select(1:16)

fitness_2025 <- read.csv("LAB/data/parks fitness 2025.csv") %>% select(1:16) %>%
  rename(c(PLOT_TYPE = PLOT, 
           PLOT = PLOT.1))
  

#PLEC FRUIT MODEL ----
##drought data wrangle ----
drought_plec <- drought_plec %>% filter(SPECIES == "PLECON")

drought_plec$TOTAL_NUM_SEED[drought_plec$TOTAL_NUM_SEED == "#DIV/0!"] <- NA

drought_plec <- drought_plec %>% 
  filter(!is.na(TOTAL_NUM_SEED)) %>% 
  select(8:18, TOTAL_NUM_SEED) %>% drop_na

drought_plec <- drought_plec %>% mutate(TOTAL_INFL = TOTAL_LARGE_INFL+TOTAL_MID_INFL+TOTAL_SMALL_INFL,
                                        s_prop = TOTAL_SMALL_INFL/TOTAL_INFL,
                                        m_prop = TOTAL_MID_INFL/TOTAL_INFL,
                                        l_prop = TOTAL_LARGE_INFL/TOTAL_INFL) %>%
  rename(HEIGHT = FINAL_FOCAL_HT)

##models ----
small_mod <- lm(s_prop ~ HEIGHT, drought_plec)
summary(small_mod)

med_mod <- lm(m_prop ~ HEIGHT, drought_plec)
summary(med_mod)

large_mod <- lm(l_prop ~ HEIGHT, drought_plec)
summary(large_mod)

all_infl_mod <- lm(TOTAL_INFL ~ HEIGHT, drought_plec)
summary(all_infl_mod)

# ggplot(drought_plec, aes(x = HEIGHT, y = s_prop)) + geom_point() + geom_smooth(method=lm)
# ggplot(drought_plec, aes(x = HEIGHT, y = m_prop)) + geom_point() + geom_smooth(method=lm)
# ggplot(drought_plec, aes(x = HEIGHT, y = l_prop)) + geom_point() + geom_smooth(method=lm)
# ggplot(drought_plec, aes(x = HEIGHT, y = TOTAL_INFL)) + geom_point() + geom_smooth(method=lm)

#PARKS 2024 ----
##predict ----
heights_2024 <- rbind(alones, diverse) %>% 
  filter(SPECIES %in% c("PLECON","PLECON ")) %>%
  select(HEIGHT, TOTALINFL) %>%
  drop_na() %>%
  select(HEIGHT)

s_props <- predict(small_mod, newdata = heights_2024)
m_props <- predict(med_mod, newdata = heights_2024)
l_props <- predict(large_mod, newdata = heights_2024)

props <- data.frame(s_props, m_props, l_props)
normalized_props <- props / rowSums(props)

small_prop <- normalized_props$s_props
med_prop <- normalized_props$m_props
large_prop <- normalized_props$l_props


# rbind(alones, diverse) %>% 
#   filter(SPECIES == "PLECON") %>%
#   select(PLOT_TYPE, HEIGHT, TOTALINFL) %>%
#   drop_na() %>% 
#   mutate(small_infl = small_prop*TOTALINFL, 
#          med_infl = med_prop*TOTALINFL,
#          large_infl = large_prop*TOTALINFL)


##infl size categories for parks ----
PLEC_parks_2024 <- rbind(alones, diverse) %>% 
  filter(SPECIES %in% c("PLECON", "PLECON ")) %>%
  select(PARK, PLOT_TYPE, SPECIES, REP, PLOT, HEIGHT, TOTALINFL) %>%
  drop_na(PLOT_TYPE, HEIGHT, TOTALINFL) %>% 
  mutate(
    small_prop = small_prop,   
    med_prop  = med_prop,
    large_prop = large_prop) %>%
  rowwise() %>%
  mutate(
    #unrounded counts
    small_raw = small_prop*TOTALINFL,
    med_raw = med_prop*TOTALINFL,
    large_raw = large_prop*TOTALINFL,
    
    #round the values
    small_rnd = floor(small_raw),
    med_rnd   = floor(med_raw),
    large_rnd = floor(large_raw),
    
    #unaccounted inflourescence
    remainder = TOTALINFL - (small_rnd + med_rnd + large_rnd),
    
    #find largest remainder
    add_to = which.max(c(small_raw - small_rnd,
              med_raw - med_rnd,
              large_raw - large_rnd)),
    
    add_to2 = which.max(c(small_raw, med_raw, large_raw)),
    
    TOTAL_SMALL_INFL = case_when(
      TOTALINFL == 1 & HEIGHT < 15 ~ 1L,
      TOTALINFL == 1 ~ 0L,
      TOTALINFL == 3 ~ small_rnd + ifelse(add_to2 == 1, remainder, 0),
      TRUE ~ small_rnd + ifelse(add_to == 1, remainder, 0)),
    TOTAL_MID_INFL = case_when(
      TOTALINFL == 1 & HEIGHT >= 15 & HEIGHT < 20 ~ 1L,
      TOTALINFL == 1 ~ 0L,
      TOTALINFL == 3 ~ med_rnd + ifelse(add_to2 == 2, remainder, 0),
      TRUE ~ med_rnd + ifelse(add_to == 2, remainder, 0)),
    TOTAL_LARGE_INFL = case_when(
      TOTALINFL == 1 & HEIGHT >= 20 ~ 1L,
      TOTALINFL == 1 ~ 0L,
      TOTALINFL == 3 ~ large_rnd + ifelse(add_to2 == 3, remainder, 0),
      TRUE ~ large_rnd + ifelse(add_to == 3, remainder, 0))) %>%
  ungroup() %>%
  select(-ends_with("raw"), -ends_with("rnd"), -ends_with("prop"), -starts_with("add"), -remainder)

PLEC_parks_2024

#PARKS 2025 ----
##predict ----
heights_2025 <- fitness_2025 %>% 
  filter(SPECIES == "PLECON") %>%
  select(HEIGHT, TOTALINFL) %>%
  drop_na()

s_props <- predict(small_mod, newdata = heights_2025)
m_props <- predict(med_mod, newdata = heights_2025)
l_props <- predict(large_mod, newdata = heights_2025)

props <- data.frame(s_props, m_props, l_props)
normalized_props <- props / rowSums(props)

small_prop <- normalized_props$s_props
med_prop <- normalized_props$m_props
large_prop <- normalized_props$l_props

##infl size categories for parks ----
PLEC_parks_2025 <- fitness_2025 %>% 
  filter(SPECIES == "PLECON") %>%
  select(PARK, PLOT_TYPE, SPECIES, REP, PLOT, HEIGHT, TOTALINFL) %>%
  drop_na(PLOT_TYPE, HEIGHT, TOTALINFL) %>% 
  mutate(
    small_prop = small_prop,   
    med_prop  = med_prop,
    large_prop = large_prop) %>%
  rowwise() %>%
  mutate(
    #unrounded counts
    small_raw = small_prop*TOTALINFL,
    med_raw = med_prop*TOTALINFL,
    large_raw = large_prop*TOTALINFL,
    
    #round the values
    small_rnd = floor(small_raw),
    med_rnd   = floor(med_raw),
    large_rnd = floor(large_raw),
    
    #unaccounted inflourescence
    remainder = TOTALINFL - (small_rnd + med_rnd + large_rnd),
    
    #find largest remainder
    add_to = which.max(c(small_raw - small_rnd,
                         med_raw - med_rnd,
                         large_raw - large_rnd)),
    
    add_to2 = which.max(c(small_raw, med_raw, large_raw)),
    
    TOTAL_SMALL_INFL = case_when(
      TOTALINFL == 1 & HEIGHT < 15 ~ 1L,
      TOTALINFL == 1 ~ 0L,
      TOTALINFL == 3 ~ small_rnd + ifelse(add_to2 == 1, remainder, 0),
      TRUE ~ small_rnd + ifelse(add_to == 1, remainder, 0)),
    TOTAL_MID_INFL = case_when(
      TOTALINFL == 1 & HEIGHT >= 15 & HEIGHT < 20 ~ 1L,
      TOTALINFL == 1 ~ 0L,
      TOTALINFL == 3 ~ med_rnd + ifelse(add_to2 == 2, remainder, 0),
      TRUE ~ med_rnd + ifelse(add_to == 2, remainder, 0)),
    TOTAL_LARGE_INFL = case_when(
      TOTALINFL == 1 & HEIGHT >= 20 ~ 1L,
      TOTALINFL == 1 ~ 0L,
      TOTALINFL == 3 ~ large_rnd + ifelse(add_to2 == 3, remainder, 0),
      TRUE ~ large_rnd + ifelse(add_to == 3, remainder, 0))) %>%
  ungroup() %>%
  select(-ends_with("raw"), -ends_with("rnd"), -ends_with("prop"), -starts_with("add"), -remainder)

PLEC_parks_2025

#ADD SEED COUNTS ----
drought_plec <- drought_plec %>%
  mutate(
    s_seed_per_fruit = round(SMALLSEED/S_NUM_COLLECT,0),
    m_seed_per_fruit = round(MIDSEED/M_NUM_COLLECT,0),
    l_seed_per_fruit = round(LARGESEED/L_NUM_COLLECT,0)
  )

s_avg_seed <- round(mean(drought_plec$s_seed_per_fruit), 0)
m_avg_seed <- round(mean(drought_plec$m_seed_per_fruit), 0)
l_avg_seed <- round(mean(drought_plec$l_seed_per_fruit), 0)

## 2024 ----
PLEC_parks_2024 <- PLEC_parks_2024 %>% 
  mutate(TOTAL_NUM_SEED = 
           s_avg_seed*TOTAL_SMALL_INFL +
           m_avg_seed*TOTAL_MID_INFL +
           l_avg_seed*TOTAL_LARGE_INFL)

write_xlsx(PLEC_parks_2024, "LAB/data/parks_plec_2024.xlsx")

## 2025 ----
PLEC_parks_2025 <- PLEC_parks_2025 %>% 
  mutate(TOTAL_NUM_SEED = 
           s_avg_seed*TOTAL_SMALL_INFL +
           m_avg_seed*TOTAL_MID_INFL +
           l_avg_seed*TOTAL_LARGE_INFL)

write_xlsx(PLEC_parks_2025, "LAB/data/parks_plec_2025.xlsx")

