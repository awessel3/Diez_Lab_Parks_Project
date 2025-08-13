library(tidyverse)
library(lubridate)

fitness_25 <- read.csv("Data/2025/25_fitness.csv") %>% 
  select(1:16) %>%
  rename(c(PLOT_TYPE = PLOT, 
           PLOT = PLOT.1)) %>%
  filter(!grepl("^gone$", NOTES, ignore.case = TRUE))

ff_heights_25 <- read.csv("Data/2025/25_ff_heights.csv") %>% 
  select(1:10) %>%
  rename(PLOT_TYPE = PLOT)