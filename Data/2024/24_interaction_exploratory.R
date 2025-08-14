library(lubridate)
library(tidyverse)
library(neonUtilities)
library(patchwork)
library(sp)
library(raster)


setwd("~/Desktop/Diez_Lab_Parks_Project/Data/2024")

neighbor_fitness <- read_rds("24_neighbor_fitness.rds")

park_colors <- c(
  "BR"  = "#D81E5B",  # red
  "RF"  = "#7DCE82",  # green
  "WIR" = "#0496FF",  # blue
  "SEM" = "#F18F01"   # yellow
)

competitors <- colnames(neighbor_fitness[, 14:21])
non_native <- colnames(neighbor_fitness[, 22:60])


ggplot(neighbor_fitness, aes(x = comp_density, y = FINALSEED)) + geom_point() + geom_smooth() +
  facet_wrap(~SPECIES, scale = 'free')

clapur_comp <- neighbor_fitness %>% filter(SPECIES == 'CLAPUR')
ggplot(clapur_comp, aes(x = GILCAP, y = FINALSEED)) + geom_point() + geom_smooth() +
  facet_wrap(~PARK, scale = 'free')

plot_total <- 0

for (nativex in all_of(competitors)) {
  all_comp <- list()
  for (nativey in all_of(competitors)) {
    native_comp <- neighbor_fitness %>% filter(SPECIES == nativex)
    comp_plot <- ggplot(native_comp, aes(x = .data[[nativey]], y = FINALSEED, color = PARK)) + 
      geom_point() + geom_smooth() +
      facet_wrap(~PARK, scale = 'free') + 
      scale_color_manual(values = park_colors)
      labs(title = sprintf("Fecundity of %s Against %s Density", nativex, nativey)) + theme_minimal()
    
    #save plot 
    plot_total <- plot_total + 1
    all_comp[[length(all_comp) + 1]] <- comp_plot
    
  }
  wrapped <- wrap_plots(all_comp, nrow = 2)
  ggsave(sprintf("figures/comp_figs/comp_plot_%s.jpeg", nativex), 
         wrapped, width = 25, height = 12, dpi = 300)
} 


