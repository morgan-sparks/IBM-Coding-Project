#lines below give a simple plot of how the habitat fills over time
library(ggplot2); library(patchwork)

habitat.occupancy <- read.csv("~/IBM-Coding-Project/Pink_Salmon/past_runs/092921_bigtosmall_2and3_habitat_occupancy.csv")

p.filled <- ggplot(habitat.occupancy, aes(x = year, y = p.filled/2, group = run, col = run)) +
  geom_line() +
  ylab(label = "Proportion Filled") +
  theme_classic(base_size = 16) 

num_rivs <- ggplot(habitat.occupancy, aes(x = year, y = num.river.with.fish, group = run, col = run)) +
  geom_line() +
  ylab(label = "Number of Rivers with Spawners") +
  theme_classic(base_size = 16)

p.filled
p.filled / num_rivs
