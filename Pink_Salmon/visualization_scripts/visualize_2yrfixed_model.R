iteration_summary <- read.csv("~/IBM-Coding-Project/Pink_Salmon/past_runs/091621_randhab_2_iteration_summary.csv")

library(ggplot2)

ggplot(iteration_summary) +
  stat_smooth(aes(x = year, y = two_yrs_RRS, group = run, color = "2yrs"),  geom = "line", fun.y="mean",  size =1, alpha = 1) +
  geom_line(aes(x = year, y = two_yrs_home_RRS, group = run ,  color = "2yrs_home"), size =.5, alpha = 0.25) +
  stat_smooth(aes(x = year, y = two_yrs_home_RRS, color = "2yrs_home"),  geom = "line", fun.y="mean", size =1, alpha = 1) +
  geom_line(aes(x = year, y = two_yrs_stray_RRS, group = run, color = "2yrs_stray"), size =.5, alpha = 0.25) +
  stat_smooth(aes(x = year, y = two_yrs_stray_RRS, color = "2yrs_stray"),  geom = "line", fun.y="mean", size =1, alpha = 1) +
  labs(x = "Year", y = "RRS", color = "Dispersal Phenotype") +
  scale_color_manual(values =c("2yrs" = "darkgreen", "2yrs_home" = "darkgoldenrod3", "2yrs_stray" = "darkolivegreen3" ),
                     labels = c("All 2-yr Olds", "Home Returning 2-yr olds", "Straying 2-yr olds")) +
  lims(y = c(0,5)) +
  theme_classic(base_size = 20) 
  #theme(legend.position = c(0.75, 0.7))
