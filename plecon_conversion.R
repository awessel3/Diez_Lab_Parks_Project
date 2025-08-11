library(tidyverse)
library(lme4)
library(janitor)
library(writexl)

#read in data

# PLECON <- read.csv("LAB/PLECON_Explore.csv") %>%
#   select(PARK, PLOT, HEIGHT, TOTALINFL)
# PLECON
# 
# #str(PLECON)
# 
# ggplot(PLECON, aes(x = HEIGHT, y = TOTALINFL)) + geom_point() + geom_smooth()
# 
# PLECON$PARK <- as.factor(PLECON$PARK)
# 
# str(PLECON)
# 
# model1 <- lm(TOTALINFL ~ HEIGHT*PARK, PLECON)
# summary(model1)

#PLEC FRUIT MODEL ----

##load data ----
drought_plec <- read.csv("LAB/PLEC_drought.csv")
alones <- read.csv("LAB/alone focals.csv") %>% select(1:16)
diverse <- read.csv("LAB/diverse focals.csv") %>% select(1:16)

##clean data ----
parks_heights <- rbind(alones, diverse) %>% 
  filter(SPECIES == "PLECON") %>%
  select(HEIGHT, TOTALINFL) %>%
  drop_na() %>%
  select(HEIGHT)

drought_plec <- drought_plec %>% filter(SPECIES == "PLECON")

drought_plec$TOTAL_NUM_SEED[drought_plec$TOTAL_NUM_SEED == "#DIV/0!"] <- NA


drought_plec <- drought_plec %>% 
  filter(!is.na(TOTAL_NUM_SEED)) %>% 
  select(8:18, TOTAL_NUM_SEED) %>% drop_na
#drought_plec %>% select(8:18, TOTAL_NUM_SEED) %>% drop_na
drought_plec <- drought_plec %>% mutate(TOTAL_INFL = TOTAL_LARGE_INFL+TOTAL_MID_INFL+TOTAL_SMALL_INFL,
                                        s_prop = TOTAL_SMALL_INFL/TOTAL_INFL,
                                        m_prop = TOTAL_MID_INFL/TOTAL_INFL,
                                        l_prop = TOTAL_LARGE_INFL/TOTAL_INFL) %>%
  rename(HEIGHT = FINAL_FOCAL_HT)

##run models ----
small_mod <- lm(s_prop ~ HEIGHT, drought_plec)
summary(small_mod)

med_mod <- lm(m_prop ~ HEIGHT, drought_plec)
summary(med_mod)

large_mod <- lm(l_prop ~ HEIGHT, drought_plec)
summary(large_mod)

all_infl_mod <- lm(TOTAL_INFL ~ HEIGHT, drought_plec)
summary(all_infl_mod)

ggplot(drought_plec, aes(x = HEIGHT, y = s_prop)) + geom_point() + geom_smooth(method=lm)
ggplot(drought_plec, aes(x = HEIGHT, y = m_prop)) + geom_point() + geom_smooth(method=lm)
ggplot(drought_plec, aes(x = HEIGHT, y = l_prop)) + geom_point() + geom_smooth(method=lm)
ggplot(drought_plec, aes(x = HEIGHT, y = TOTAL_INFL)) + geom_point() + geom_smooth(method=lm)

##predict from models ----
s_props <- predict(small_mod, newdata = parks_heights)
m_props <- predict(med_mod, newdata = parks_heights)
l_props <- predict(large_mod, newdata = parks_heights)

props <- data.frame(s_props, m_props, l_props)
normalized_props <- props / rowSums(props)

small_prop <- normalized_props$s_props
med_prop <- normalized_props$m_props
large_prop <- normalized_props$l_props

##get inflourescence size categories for parks ----
PLEC_parks <- rbind(alones, diverse) %>% 
  filter(SPECIES == "PLECON") %>%
  select(PLOT_TYPE, HEIGHT, TOTALINFL) %>%
  drop_na() %>% 
  mutate(
    small_prop = small_prop,   
    med_prop   = med_prop,
    large_prop = large_prop) %>%
  rowwise() %>%
  mutate(
    #unrounded counts
    small_raw = small_prop * TOTALINFL,
    med_raw = med_prop * TOTALINFL,
    large_raw = large_prop * TOTALINFL,
    
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
    
    TOTAL_SMALL_INFL = case_when(
      TOTALINFL == 1 & HEIGHT < 15 ~ 1L,
      TOTALINFL == 1 ~ 0L,
      TRUE ~ small_rnd + ifelse(add_to == 1, remainder, 0)),
    TOTAL_MID_INFL = case_when(
      TOTALINFL == 1 & HEIGHT >= 15 & HEIGHT < 20 ~ 1L,
      TOTALINFL == 1 ~ 0L,
      TRUE ~ med_rnd + ifelse(add_to == 2, remainder, 0)),
    TOTAL_LARGE_INFL = case_when(
      TOTALINFL == 1 & HEIGHT >= 20 ~ 1L,
      TOTALINFL == 1 ~ 0L,
      TRUE ~ large_rnd + ifelse(add_to == 3, remainder, 0))) %>%
  ungroup() %>%
  select(-ends_with("raw"), -ends_with("rnd"), -ends_with("prop"), -remainder, -add_to)
      

PLEC_parks

#SEED COUNTS ----
drought_plec <- drought_plec %>%
  mutate(
    s_seed_per_fruit = round(SMALLSEED/S_NUM_COLLECT,0),
    m_seed_per_fruit = round(MIDSEED/M_NUM_COLLECT,0),
    l_seed_per_fruit = round(LARGESEED/L_NUM_COLLECT,0)
  )

s_avg_seed <- round(mean(drought_plec$s_seed_per_fruit), 0)
m_avg_seed <- round(mean(drought_plec$m_seed_per_fruit), 0)
l_avg_seed <- round(mean(drought_plec$l_seed_per_fruit), 0)

PLEC_parks <- PLEC_parks %>% 
  mutate(TOTAL_NUM_SEED = 
           s_avg_seed*TOTAL_SMALL_INFL +
           m_avg_seed*TOTAL_MID_INFL +
           l_avg_seed*TOTAL_LARGE_INFL)

write_xlsx(PLEC_parks, "LAB/parks_plec.xlsx")