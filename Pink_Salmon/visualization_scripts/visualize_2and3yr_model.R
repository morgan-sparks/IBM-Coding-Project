library(ggplot2)
iteration_summary <- read.csv("~/IBM-Coding-Project/Pink_Salmon/past_runs/092921_bigtosmall_2and3_iteration_summary.csv")


ggplot(iteration_summary) +
  #--- runs
  geom_line(aes(x = year, y = two_yrs_home_RRS, group = run ,  color = "2yrs_home"), size =.5, alpha = 0.05) +
  geom_line(aes(x = year, y = three_yrs_RRS, group = run ,  color = "3yrs_home"), size =.5, alpha = 0.05) +
  geom_line(aes(x = year, y = two_yrs_stray_RRS, group = run, color = "2yrs_stray"), size =.5, alpha = 0.05) +
  geom_line(aes(x = year, y = three_yrs_home_RRS, group = run ,  color = "3yrs_home"), size =.5, alpha = 0.05) +
  geom_line(aes(x = year, y = three_yrs_stray_RRS, group = run, color = "3yrs_stray"), size =.5, alpha = 0.05) +
  #---average
  stat_smooth(aes(x = year, y = two_yrs_RRS, group = run, color = "2yrs",),  geom = "line", fun.y="mean",  size =1, alpha = 1) +
  stat_smooth(aes(x = year, y = two_yrs_home_RRS, color = "2yrs_home"),  geom = "line", fun.y="mean", size =1, alpha = 1) +
  stat_smooth(aes(x = year, y = two_yrs_stray_RRS, color = "2yrs_stray"),  geom = "line", fun.y="mean", size =1, alpha = 1) +
  stat_smooth(aes(x = year, y = three_yrs_RRS, color = "3yrs",),  geom = "line", fun.y="mean",  size =1, alpha = 1) +
  stat_smooth(aes(x = year, y = three_yrs_home_RRS, color = "3yrs_home"),  geom = "line", fun.y="mean", size =1, alpha = 1) +
  stat_smooth(aes(x = year, y = three_yrs_stray_RRS, color = "3yrs_stray"),  geom = "line", fun.y="mean", size =1, alpha = 1) +
  labs(x = "Year", y = "RRS", color = "Dispersal Phenotype") +
  scale_color_manual(values =c("2yrs" = "darkgreen", "2yrs_home" = "darkgoldenrod3", "2yrs_stray" = "darkolivegreen3",
                               "3yrs" = "midnightblue", "3yrs_home" = "dodgerblue3", "3yrs_stray" = "darkturquoise"),
                     labels = c("All 2-yr Olds", "Home Returning 2-yr olds", "Straying 2-yr olds",
                                "All 3-yr Olds", "Home Returning 3-yr olds", "Straying 3-yr olds")) +
  #lims(y = c(.6,1.2)) +
  theme_classic(base_size = 20) 
  #theme(legend.position = c(0.75, 0.3))
