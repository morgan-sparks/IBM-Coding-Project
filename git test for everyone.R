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



unbiased_mutation <- function(N, mu, p_0, t_max, r_max) {
  # Create the output tibble
  output <- tibble(generation = rep(1:t_max, r_max), 
                   p = as.numeric(rep(NA, t_max * r_max)), 
                   run = as.factor(rep(1:r_max, each = t_max))) 
  
  for (r in 1:r_max) {
    population <- tibble(trait = sample(c("A", "B"), N, replace = TRUE, 
                                        prob = c(p_0, 1 - p_0)))
    # Add first generation's p for run r
    output[output$generation == 1 & output$run == r, ]$p <- 
      sum(population$trait == "A") / N 
    for (t in 2:t_max) {
      # Copy individuals to previous_population tibble
      previous_population <- population 
      
      # Determine 'mutant' individuals
      mutate <- sample(c(TRUE, FALSE), N, prob = c(mu, 1 - mu), replace = TRUE) 
      
      # If there are 'mutants' from A to B
      if (nrow(population[mutate & previous_population$trait == "A", ]) > 0) { 
        # Then flip them to B
        population[mutate & previous_population$trait == "A", ]$trait <- "B" 
      }
      
      # If there are 'mutants' from B to A
      if (nrow(population[mutate & previous_population$trait == "B", ]) > 0) { 
        # Then flip them to A
        population[mutate & previous_population$trait == "B", ]$trait <- "A" 
      }
      
      # Get p and put it into output slot for this generation t and run r
      output[output$generation == t & output$run == r, ]$p <- 
        sum(population$trait == "A") / N 
    }
  }
  # Export data from function
  output 
}

biased_mutation <- function(N, mu_b, p_0, t_max, r_max) {
  # Create the output tibble
  output <- tibble(generation = rep(1:t_max, r_max), 
                   p = as.numeric(rep(NA, t_max * r_max)), 
                   run = as.factor(rep(1:r_max, each = t_max)))
  
  for (r in 1:r_max) {
    population <- tibble(trait = sample(c("A", "B"), N, replace = TRUE, 
                                        prob = c(p_0, 1 - p_0)))
    # Add first generation's p for run r
    output[output$generation == 1 & output$run == r, ]$p <- 
      sum(population$trait == "A") / N 
    for (t in 2:t_max) {
      # Copy individuals to previous_population tibble
      previous_population <- population 
      
      # Determine 'mutant' individuals
      mutate <- sample(c(TRUE, FALSE), N, prob = c(mu_b, 1 - mu_b), replace = TRUE) 
      
      # If there are 'mutants' from B to A
      if (nrow(population[mutate & previous_population$trait == "B", ]) > 0) {
        # Then flip them to A
        population[mutate & previous_population$trait == "B", ]$trait <- "A"
      }
      # Get p and put it into output slot for this generation t and run r
      output[output$generation == t & output$run == r, ]$p <- 
        sum(population$trait == "A") / N 
    }
  }
  # Export data from function
  output 
}

biased_transmission_direct <- function (N, s_a, s_b, p_0, t_max, r_max) {
  
  output <- tibble(generation = rep(1:t_max, r_max), 
                   p = as.numeric(rep(NA, t_max * r_max)), 
                   run = as.factor(rep(1:r_max, each = t_max)))
  
  for (r in 1:r_max) {
    # Create first generation
    population <- tibble(trait = sample(c("A", "B"), N, 
                                        replace = TRUE, prob = c(p_0, 1 - p_0))) 
    
    # Add first generation's p for run r
    output[output$generation == 1 & output$run == r, ]$p <- 
      sum(population$trait == "A") / N 
    
    for (t in 2:t_max) {
      # Copy individuals to previous_population tibble
      previous_population <- population 
      
      # For each individual, pick a random individual from the previous generation
      demonstrator_trait <- 
        tibble(trait = sample(previous_population$trait, N, replace = TRUE)) 
      
      # Biased probabilities to copy:
      s_a_fluct <- rnorm(1,mean = s_a, sd = s_a/10)
      s_b_fluct <- rnorm(1,mean = s_b, sd = s_b/10)
      copy_a <- sample(c(TRUE, FALSE), N, prob = c(s_a_fluct, 1 - s_a_fluct), replace = TRUE) 
      copy_b <- sample(c(TRUE, FALSE), N, prob = c(s_b_fluct, 1 - s_b_fluct), replace = TRUE) 
      
      # If the demonstrator has trait A and the individual wants to copy A, then copy A
      if (nrow(population[copy_a & demonstrator_trait$trait == "A", ]) > 0) {
        population[copy_a & demonstrator_trait$trait == "A", ]$trait <- "A" 
      }  
      
      # If the demonstrator has trait B and the individual wants to copy B, then copy B
      if (nrow(population[copy_b & demonstrator_trait$trait == "B", ]) > 0) {
        population[copy_b & demonstrator_trait$trait == "B", ]$trait <- "B" 
      }  
      # Get p and put it into output slot for this generation t and run r
      output[output$generation == t & output$run == r, ]$p <- 
        sum(population$trait == "A") / N 
    }
  }
  # Export data from function
  output 
}

benny_run2 <- unbiased_mutation(N = 500, mu = 0.1, p_0 = 0.1, t_max = 100, r_max = 25)
benny_plot <- plot_multiple_runs(benny_run2) + ggtitle("N = 500, mu = 0.1, t_max = 100, r_max = 25")
benny_plot 

benny_run3 <- biased_mutation(N = 500, mu = 0.1, p_0 = 0.1, t_max = 100, r_max = 25)
benny_plot <- plot_multiple_runs(benny_run3) + ggtitle("N = 500, mu = 0.1, t_max = 100, r_max = 25")
benny_plot 

benny_run4 <- biased_transmission_direct(N = 50, s_a = 0.25, s_b = 0.23, p_0 = 0.1, t_max = 100, r_max = 25)
benny_plot <- plot_multiple_runs(benny_run4) + ggtitle("N = 50, s_a = 0.25, s_b = 0.23\nt_max = 100, r_max = 25")
benny_plot

library(tidyverse)

N <- 100
p_0 <- 0.5
D <- 1

# Create first generation
population <- tibble(trait = sample(c("A", "B"), N, 
                                    replace = TRUE, prob = c(p_0, 1 - p_0))) 

# Create a tibble with a set of 3 randomly-picked demonstrators for each agent
demonstrators <- tibble(dem1 = sample(population$trait, N, replace = TRUE), 
                        dem2 = sample(population$trait, N, replace = TRUE), 
                        dem3 = sample(population$trait, N, replace = TRUE))

# Visualise the tibble
demonstrators

# Get the number of As in each 3-demonstrator combinations
num_As <- rowSums(demonstrators == "A")

# For 3-demonstrator combinations with all As, set to A
population$trait[num_As == 3] <- "A"  
# For 3-demonstrator combinations with all Bs, set to B
population$trait[num_As == 0] <- "B"  

prob_majority <- sample(c(TRUE, FALSE), 
                        prob = c((2/3 + D/3), 1 - (2/3 + D/3)), N, replace = TRUE)

demonstrators <- add_column(demonstrators, new_trait = population$trait)

# Visualise the tibble
demonstrators
prob_minority <- sample(c(TRUE, FALSE), 
                        prob = c((1/3 - D/3), 1 - (1/3 - D/3)), N, replace = TRUE)

# 3-demonstrator combinations with two As and one B
if (nrow(population[prob_majority & num_As == 2, ]) > 0) {
  population[prob_majority & num_As == 2, ] <- "A"
}
if (nrow(population[prob_majority == FALSE & num_As == 2, ]) > 0) {
  population[prob_majority == FALSE & num_As == 2, ] <- "B"
}  

# 3-demonstrator combinations with one A and two Bs
if (nrow(population[prob_minority & num_As == 1, ]) > 0) {
  population[prob_minority & num_As == 1, ] <- "A"
}
if (nrow(population[prob_minority == FALSE & num_As == 1, ]) > 0) {
  population[prob_minority == FALSE & num_As == 1, ] <- "B"
}  

demonstrators <- add_column(demonstrators, new_trait = population$trait)

# Visualise the tibble
demonstrators

conformist_transmission <- function (N, p_0, D, t_max, r_max) {
  
  output <- tibble(generation = rep(1:t_max, r_max), 
                   p = as.numeric(rep(NA, t_max * r_max)), 
                   run = as.factor(rep(1:r_max, each = t_max)))
  
  for (r in 1:r_max) {
    # Create first generation
    population <- tibble(trait = sample(c("A", "B"), N, 
                                        replace = TRUE, prob = c(p_0, 1 - p_0)))
    
    # Add first generation's p for run r
    output[output$generation == 1 & output$run == r, ]$p <- 
      sum(population$trait == "A") / N 
    
    for (t in 2:t_max) {
      
      # Create a tibble with a set of 3 randomly-picked demonstrators for each agent
      #demonstrators <- tibble(dem1 = sample(population$trait, N, replace = TRUE), 
       #                       dem2 = sample(population$trait, N, replace = TRUE), 
        #                      dem3 = sample(population$trait, N, replace = TRUE))
      #alternative version
      demonstrators <- matrix(sample(population$trait, 3*N, replace = TRUE), nrow(population), 3)
      
      # Get the number of As in each 3-demonstrator combinations
      #num_As <- rowSums(demonstrators == "A")
      num_As <- apply(demonstrators, 1, function(d) sum(d == "A"))
      
      # For 3-demonstrator combinations with all As, set to A
      population$trait[num_As == 3] <- "A"  
      # For 3-demonstrator combinations with all Bs, set to B
      population$trait[num_As == 0] <- "B"  
      
      prob_majority <- sample(c(TRUE, FALSE), 
                              prob = c((2/3 + D/3), 1 - (2/3 + D/3)), N, replace = TRUE)
      prob_minority <- sample(c(TRUE, FALSE), 
                              prob = c((1/3 - D/3), 1 - (1/3 - D/3)), N, replace = TRUE)
      
      # 3-demonstrator combinations with two As and one B
      if (nrow(population[prob_majority & num_As == 2, ]) > 0) {
        population[prob_majority & num_As == 2, ] <- "A"
      }
      if (nrow(population[prob_majority == FALSE & num_As == 2, ]) > 0) {
        population[prob_majority == FALSE & num_As == 2, ] <- "B"
      }  
      # 3-demonstrator combinations with one A and two Bs
      if (nrow(population[prob_minority & num_As == 1, ]) > 0) {
        population[prob_minority & num_As == 1, ] <- "A"
      }
      if (nrow(population[prob_minority == FALSE & num_As == 1, ]) > 0) {
        population[prob_minority == FALSE & num_As == 1, ] <- "B"
      }  
      
      # Get p and put it into output slot for this generation t and run r
      output[output$generation == t & output$run == r, ]$p <- 
        sum(population$trait == "A") / N 
    }
  }
  # Export data from function
  output  
}

data_model <- conformist_transmission(N = 1000, p_0 = 0.5, D = 1, t_max = 50, r_max = 10)
plot_multiple_runs(data_model)

data_model <- conformist_transmission(N = 1000, p_0 = 0.5, D = 0, t_max = 50, r_max = 10)
plot_multiple_runs(data_model)

data_model <- conformist_transmission(N = 1000, p_0 = 0.55, D = 1, t_max = 50, r_max = 10)
plot_multiple_runs(data_model)

data_model <- conformist_transmission(N = 1000, p_0 = 0.45, D = 1, t_max = 50, r_max = 10)
plot_multiple_runs(data_model)
