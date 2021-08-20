#lines below give a simple plot of how the habitat fills over time

habitat.occupancy <- read.csv("~/IBM-Coding-Project/Pink_Salmon/model_v.1/output/habitat_occupancy.csv")

library(ggplot2); library(patchwork)
p.filled <- ggplot(habitat.occupancy, aes(x = year, y = p.filled, group = run, col = run)) +
  geom_line() +
  theme_classic() 

num_rivs <- ggplot(habitat.occupancy, aes(x = year, y = num.river.with.fish, group = run, col = run)) +
  geom_line() +
  theme_classic()

p.filled + num_rivs
