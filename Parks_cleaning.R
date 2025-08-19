library(tidyverse)
library(ggplot2)

setwd("Desktop/Diez Lab")

SEM <-read.csv("Data/2025/25_neighbors_sem.csv")
RF <- read.csv("Data/2025/25_neighbors_rf.csv")
WIR <- read.csv("Data/2025/25_neighbors_wir.csv")
dim(SEM)
dim(RF)
dim(WIR)

str(rf)
colnames(SEM)

RF$BROSIT <- 0

rated_species <- c("SHEARV", 'GALAPA', 'BROHOR', 'CYNECH', 'AIRCAR','ARRELA',
                   'BRODIA','BROSTE', 'HOLLAN', 'VULBRO', 'BROSIT', 'agrostis.spp', 
                   'TAECAP', 'CARTUM', 'DACGLO')

test.SEM <- SEM
test.RF <- RF

removeChr <- function(df) {
  df <- df %>% 
    replace(is.na(.), 0)  %>%
    rowwise() %>%
    mutate(across(all_of(rated_species), ~ case_when(
      . == "<1" ~ 1,
      . == "1" ~ 1,
      . == "" ~ 0,
      . == "2" ~ 2
    ))) %>% 
  ungroup()
}

ratingToStemcount <- function(df) {
  
  df <- df %>% 
    replace(is.na(.), 0)  %>%
    rowwise() %>%
    mutate(across(all_of(rated_species), ~ case_when(
      . == 0 ~ 0,
      . == 1 ~ 5,
      . == 2 ~ 12,
      . == 3 ~ 18,
      . == 4 ~ 25,
      . == 5 ~ 32,
      . == 6 ~ 37,
      . == 7 ~ 42,
      . == 8 ~ 47,
      . == 9 ~ 52,
      . == 10 ~ 55,
      . > 55 ~ NA_real_
    ))) %>%
    ungroup()
}

test.RF <- removeChr(test.RF)
test.RF$GALAPA

RF <- removeChr(RF)
SEM <- removeChr(SEM)
WIR <- removeChr(WIR)

test.SEM <- ratingToStemcount(test.SEM)
test.RF <- ratingToStemcount(test.RF)

SEM <- ratingToStemcount(SEM)
RF <- ratingToStemcount(RF)
WIR <- ratingToStemcount(WIR)

RF$GALAPA
test.RF$GALAPA

SEM$SHEARV
test.SEM$SHEARV

SEM$GALAPA
test.SEM$GALAPA

write_csv(SEM, "Data/2025/25_neighbors_sem.csv")
write_csv(RF, "Data/2025/25_neighbors_rf.csv")
write_csv(WIR, "Data/2025/25_neighbors_wir.csv")
