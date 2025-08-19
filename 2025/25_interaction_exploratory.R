library(tidyverse)
library(lubridate)
library(patchwork)

setwd("C:/Users/hmcLD/OneDrive/Desktop/Diez_Lab_Parks_Project/2025/data")
setwd("~/Desktop/Diez_Lab_Parks_Project/2025/data")

neighbor_fitness <- read_rds("25_neighbor_fitness.rds")

park_colors <- c(
  "BR"  = "#D81E5B",  # red
  "RF"  = "#7DCE82",  # green
  "WIR" = "#0496FF",  # blue
  "SEM" = "#F18F01"   # yellow
)

CLAPUR <- neighbor_fitness %>%
  filter(SPECIES == "CLAPUR") %>%
  ggplot(aes(x = comp_density, y = FINALSEED, color = PARK)) +
  geom_point() + 
  geom_smooth() +
  scale_color_manual(values = park_colors) +
  facet_wrap(~PARK, scale = "free") + 
  labs(
    x = "# competitors",
    y = "fecundity",
    title = "CLAPUR"
  )
  
PLECON <- neighbor_fitness %>%
  filter(SPECIES == "PLECON") %>%
  ggplot(aes(x = comp_density, y = FINALSEED, color = PARK)) + 
  geom_point() + 
  geom_smooth() +
  scale_color_manual(values = park_colors) +
  facet_wrap(~PARK, scale = "free") +
  labs(
    x = "# competitors",
    y = "fecundity",
    title = "PLECON"
  )

COLLIN <- neighbor_fitness %>%
  filter(SPECIES == "COLLIN") %>%
  ggplot(aes(x = comp_density, y = FINALSEED, color = PARK)) + 
  geom_point() + 
  geom_smooth() +
  scale_color_manual(values = park_colors) +
  facet_wrap(~PARK, scale = "free") +
  labs(
    x = "# competitors",
    y = "fecundity",
    title = "COLLIN"
  )

COLLOM <- neighbor_fitness %>%
  filter(SPECIES == "COLLOM") %>%
  ggplot(aes(x = comp_density, y = FINALSEED, color = PARK)) + 
  geom_point() + 
  geom_smooth() +
  scale_color_manual(values = park_colors) +
  facet_wrap(~PARK, scale = "free") +
  labs(
    x = "# competitors",
    y = "fecundity",
    title = "COLLOM"
  )

GILCAP <- neighbor_fitness %>%
  filter(SPECIES == "GILCAP") %>%
  ggplot(aes(x = comp_density, y = FINALSEED, color = PARK)) + 
  geom_point() + 
  geom_smooth(method = "loess", span = 1) +
  scale_color_manual(values = park_colors) +
  facet_wrap(~PARK, scale = "free") +
  labs(
    x = "# competitors",
    y = "fecundity",
    title = "GILCAP"
  )

NAVSQU <- neighbor_fitness %>%
  filter(SPECIES == "NAVSQU") %>%
  ggplot(aes(x = comp_density, y = FINALSEED, color = PARK)) + 
  geom_point() + 
  geom_smooth(method = "loess", span = 1) +
  scale_color_manual(values = park_colors) +
  facet_wrap(~PARK, scale = "free") +
  labs(
    x = "# competitors",
    y = "fecundity",
    title = "NAVSQU"
  )

EPIDEN <- neighbor_fitness %>%
  filter(SPECIES == "EPIDEN") %>%
  ggplot(aes(x = comp_density, y = FINALSEED, color = PARK)) + 
  geom_point() + 
  geom_smooth() +
  scale_color_manual(values = park_colors) +
  facet_wrap(~PARK, scale = "free") +
  labs(
    x = "# competitors",
    y = "fecundity",
    title = "EPIDEN"
  )

PLAFIG <- neighbor_fitness %>%
  filter(SPECIES == "PLAFIG") %>%
  ggplot(aes(x = comp_density, y = FINALSEED, color = PARK)) + 
  geom_point() + 
  geom_smooth(method = "loess", span = 1) +
  scale_color_manual(values = park_colors) +
  facet_wrap(~PARK, scale = "free") +
  labs(
    x = "# competitors",
    y = "fecundity",
    title = "PLAFIG"
  )

(CLAPUR | PLECON | COLLIN | COLLOM) /
  (GILCAP | NAVSQU | EPIDEN | PLAFIG) +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

natives <- colnames(neighbor_fitness[ ,10:17])
non_natives <- colnames(neighbor_fitness[ ,18:ncol(neighbor_fitness)])

for (native1 in natives){
  plots <- list()
  for (native2 in natives){
    native_comp <- neighbor_fitness %>% filter(SPECIES == native1)
    
    plot <- ggplot(native_comp, aes(x = .data[[native2]], y = FINALSEED, color = PARK)) + 
      geom_point() + 
      geom_smooth(method = "loess", span = 1) +
      #facet_wrap(~PARK, scale = "free") +
      labs(
        x = sprintf("# %s", native2),
        y = "fecundity"
      ) +
      scale_color_manual(values = park_colors) + 
      theme_minimal()
    
    plots[[native2]] <- plot
  }
  all_plots <- (plots[[1]] | plots[[2]] | plots[[3]] | plots[[4]]) /
    (plots[[5]] | plots[[6]] | plots[[7]] | plots[[8]]) +
    plot_annotation(
      title = "Fecundity vs Competitor Density of Natives",
      subtitle = sprintf("%s", native1)
    ) +
    plot_layout(guides = "collect") & theme(legend.position = "bottom", strip.text = element_text(size = 10))
    
  
  ggsave(sprintf("../figures/25_comp_%s.jpeg", native1), all_plots, width = 15, height = 10, dpi = 300)
}



