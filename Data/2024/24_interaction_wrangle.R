library(lubridate)
library(tidyverse)
library(neonUtilities)
library(patchwork)
library(sp)
library(raster)


setwd("~/Desktop/Diez_Lab_Parks_Project/Data/2024")

neighbors_br_24 <- read.csv("24_neighbors_br.csv")
neighbors_rf_24 <- read.csv("24_neighbors_rf.csv")
neighbors_sem_24 <- read.csv("24_neighbors_sem.csv")
neighbors_wir_24 <- read.csv("24_neighbors_wir.csv")


neighbors_br_24 <- rename(neighbors_br_24, AGRCAP = ABRCAP)
str(neighbors_wir_24 )


neighbors_24 <- bind_rows(
  neighbors_br_24,
  neighbors_rf_24,
  neighbors_sem_24,
  neighbors_wir_24)
