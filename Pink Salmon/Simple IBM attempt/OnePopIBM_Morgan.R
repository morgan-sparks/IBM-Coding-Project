#####################################################################################
# Morgan Sparks
# 03/03/2021 msparks1309@gmail.com

# Script to make a population of fish that starts with 100 individuals and grows 
# until carrying capacity 5000, with births, deaths, and emigration (maybe)

#####################################################################################
library(ggplot2)
#------- initialize population

current_river <- data.frame(fish = 1:100, year = 1, emigrate = 0, birth = 0, death = 0)

# 10% chance of emigration
emigrate_draw <-  c(rep(0,9), 1)

#65% chance of replacement, 15% of doubling, 10% of tripling, 10% of not at all
birth_draw <- c(rep(1,65), rep(2,15), rep(3, 10), rep(0,10))


#------- emigrate function

emigrate <-  function(current_river){
  for(i in 1:nrow(current_river)) {
    current_river[i,3] <- sample(emigrate_draw, size = 1, replace = TRUE)
  }
  return(current_river)
}

#------- density dependence

density_dependence <- function(current_river){
  pop_size <- nrow(current_river) # make pop size = to length of pop
  k <-round(rnorm(1, mean = 5000, sd = 250),0) # K is a random draw from normal dist w/ mean 5000 and sd 250
  
  if(pop_size > k){
    number_to_kill <-pop_size - k # get number of individuals to kill
    individuals_to_kill <- sample(1:pop_size, size = number_to_kill, replace = FALSE)
    
    # kill individuals in current river that match those in individuals to kill
    current_river[individuals_to_kill,5]  <- 1
  }
  return(current_river)
}

#------- birth function

make_baby <- function(current_river){
  for(i in 1:nrow(current_river)){
    #only do individuals that haven't died
    if(current_river[i,5] == 0){
    current_river[i,4] <- sample(birth_draw, size = 1, replace = TRUE)
    }
  }
  return(current_river)
}



#####################################################################################
# execute the ibm


#------- run for 100 gens
current_river <- data.frame(fish = 1:100, year = 1, emigrate = 0, birth = 0, death = 0)
gens <- seq(from = 3, to = 201, by = 2)
emigrants <- NULL
census <- current_river

for(j in gens){
  #------- emigration
 
  current_river <- emigrate(current_river)
  emigrants <- rbind(emigrants, current_river[current_river[,3]==1,]) # add emigrants to emigrant dataframe
  current_river <- current_river[current_river[,3]==0,] #drop individuals tha emigrated
  
  #------- density dependence
  current_river <- density_dependence(current_river)
  
  #------ births
  current_river <- make_baby(current_river)
  
  #------ add individuals to census, add babies to next gen, add gen
  census <- rbind(census, current_river)
  #remake current river based on sum of births 
  current_river <- data.frame(fish = 1:sum(current_river[,4]), year = j, emigrate = 0, birth = 0, death = 0)
  
}

#------- make data in format ggplot likes
census_summary <- NULL
for (i in gens){
  pop <- census[census[,2] == i, ]
  ems <- emigrants[emigrants[,2]==i, ]
  pop.size <- c(year = i, n = nrow(pop), ems = nrow(ems))
  census_summary <- rbind(census_summary, pop.size)
}
census_summary <-data.frame(census_summary)

#plot
p1 <- ggplot(data = census_summary[1:99,]) +
  geom_line(aes(x = year, y = n, color = "Current River"), size =1) +
  geom_line(aes(x =year, y = ems, color = "Strays"), size =1 ) +
  geom_hline(yintercept = 5000, linetype = "dashed" ) +
  scale_color_manual(name = "Population", 
                     values = c("Current River" = "dodgerblue", "Strays" = "orange")) +
  labs( x ="Generations", y = "Population size") +
  theme_classic(base_size = 14) +
  theme(legend.position = c(0.75, 0.5)) 

ggsave("~/IBM-Coding-Project/Pink Salmon/Simple IBM attempt/MorganIBM.pdf", plot = p1)
