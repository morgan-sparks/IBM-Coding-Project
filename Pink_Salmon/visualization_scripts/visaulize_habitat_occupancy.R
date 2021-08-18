#lines below give a simple plot of how the habitat fills over time

habitat.occupancy <- read.csv("~/IBM-Coding-Project/Pink_Salmon/model_v.1/output/habitat_occupancy.csv")

library(ggplot2)
ggplot(habitat.occupancy, aes(x = year, y = p.filled, group = run, col = run)) +
  geom_line() +
  theme_classic() +
  ggsave()

ggplot(habitat.occupancy, aes(x = year, y = num.river.with.fish, group = run, col = run)) +
  geom_line() +
  theme_classic()