######
# Git test for group made my Morgan 2-1-2021

######

library(tidyverse); library(patchwork)

#####################################################################################


##### unnbiased model 2 from CH1


### run multiple simulations and plot their results
# Scaling up

### a new function to run multipe sims at once
unbiased_transmission_2 <- function(N, t_max, r_max) {
  output <- tibble(generation = rep(1:t_max, r_max), 
                   p = as.numeric(rep(NA, t_max * r_max)), 
                   run = as.factor(rep(1:r_max, each = t_max))) 
  # For each run
  for (r in 1:r_max) { 
    # Create first generation
    population <- tibble(trait = sample(c("A", "B"), N, replace = TRUE))
    
    # Add first generation's p for run r
    output[output$generation == 1 & output$run == r, ]$p <-
      sum(population$trait == "A") / N 
    
    # For each generation
    for (t in 2:t_max) {
      # Copy individuals to previous_population tibble
      previous_population <- population 
      
      # Randomly copy from previous generation
      population <- tibble(trait = sample(previous_population$trait, N, replace = TRUE))
      
      # Get p and put it into output slot for this generation t and run r
      output[output$generation == t & output$run == r, ]$p <- 
        sum(population$trait == "A") / N 
    }
  }
  # Export data from function
  output 
}
### quick plotting function for multiple runs

plot_multiple_runs <- function(data_model) {
  ggplot(data = data_model, aes(y = p, x = generation)) +
    geom_line(aes(colour = run)) +
    stat_summary(fun = mean, geom = "line", size = 1) +
    ylim(c(0, 1)) +
    theme_bw() +
    labs(y = "p (proportion of individuals with trait A)")
}

#####################################################################################

### everyone add a different version with different N, t_max with 10 runs and plot it

morgan_run <- unbiased_transmission_2(N = 100, t_max = 200, r_max = 10)
morgan_plot <- plot_multiple_runs(morgan_run) + ggtitle("N = 100, t_max = 200, r_max = 10")
morgan_plot

### add your own below
benny_run <- unbiased_transmission_2(N = 500, t_max = 100, r_max = 25)
benny_plot <- plot_multiple_runs(benny_run) + ggtitle("N = 500, t_max = 100, r_max = 25")
benny_plot 

#look ma I did it!
claire_run <- unbiased_transmission_2(N = 1000, t_max = 500, r_max = 30)
claire_plot <- plot_multiple_runs(claire_run) + ggtitle("N = 1000, t_max = 500, r_max = 30")
claire_plot 

claire_run2 <- unbiased_transmission_2(N = 1000, t_max = 500, r_max = 5)
claire_plot2 <- plot_multiple_runs(claire_run2) + ggtitle("N = 1000, t_max = 500, r_max = 5")
claire_plot2 

### spot for ryan
rl_run <- unbiased_transmission_2(N = 1000, t_max = 200, r_max = 30)
rl_plot <- plot_multiple_runs(rl_run) + ggtitle("N = 1000, t_max = 500, r_max = 30")
rl_plot


### group plot
group_plot <- morgan_plot + benny_plot + claire_plot + claire_plot2
group_plot
