#####################################################################################
# Morgan Sparks
# 03/03/2021 msparks1309@gmail.com

# Script to make a population of fish that starts with 100 individuals and grows 
# until carrying capacity 5000, with births, deaths, and emigration (maybe)

#####################################################################################
library(ggplot2)
#------- initialize population

current_river <- data.frame(fish = 1:100, year = 1, age = 1, age_maturity = 2, emigrate = 0, birth = 0, death = 0)

# 10% chance of emigration
emigrate_draw <-  c(rep(0,9), 1)

#50% chance of replacement, 30% of doubling, 10% of tripling, 10% of not at all
birth_draw <- c(rep(1,50), rep(2,30), rep(3, 10), rep(0,10))

lake <- NULL

#------- emigrate function

emigrate <-  function(current_river){
  if(is.null(current_river)==TRUE){
    print(paste("No strays, fish are in lake"))
  } else{
    for(i in 1:nrow(current_river)) {
    current_river[i,5] <- sample(emigrate_draw, size = 1, replace = TRUE)
    }
  }
  return(current_river)
}

#------- migrate to lake

migrate <- function(current_river, lake){
  
  #append current river fish into lake
  lake <- rbind(lake, current_river)
  
  #clear out current river
  current_river <- NULL
  #make temp lake
  lake_temp <-NULL
  
  for(y in 1:nrow(lake)){
    
    # if age == age at maturity, put those fish back into current river
    if(lake[y,3] == lake[y,4]){
      
      #put fish whose age == age at maturity into current river
      current_river <- rbind(current_river, lake[y,])
      
    } else{
      # put fish whose age != age at maturity into temp lake
      lake_temp <-rbind(lake_temp, lake[y,])
    }
  }
  
  # make "lake" fish that are less that age at maturity
  lake <- lake_temp
  # add a year of age 
  lake[,3] <- lake[,3] + 1
  
  #create list and return it
  waterbodies <- list(current_river = current_river, lake = lake)
  return(waterbodies)
}

#------- density dependence

density_dependence <- function(current_river){
  if(is.null(current_river)==TRUE){
    print(paste("No limitation on spawning, fish are in lake"))
  }else{
    pop_size <- nrow(current_river) # make pop size = to length of pop
    k <-round(rnorm(1, mean = 5000, sd = 250),0) # K is a random draw from normal dist w/ mean 5000 and sd 250
    if(pop_size > k){
      number_to_kill <- pop_size - k # get number of individuals to kill
      individuals_to_kill <- sample(1:pop_size, size = number_to_kill, replace = FALSE)
      # kill individuals in current river that match those in individuals to kill
      current_river[individuals_to_kill,7]  <- 1
    }
  }
  return(current_river)
}

#------- birth function

make_baby <- function(current_river){
  if(is.null(current_river)==TRUE){
    print(paste("No spawning, fish are in lake"))
    } else{ 
      for(i in 1:nrow(current_river)){
      #only do individuals that haven't died
        if(current_river[i,7] == 0){
          current_river[i,6] <- sample(birth_draw, size = 1, replace = TRUE)
      }
    }
  }
  return(current_river)
}



#####################################################################################
### execute the ibm

# 10% chance of emigration
emigrate_draw <-  c(rep(0,9), 1)

#50% chance of replacement, 30% of doubling, 10% of tripling, 10% of not at all
birth_draw <- c(rep(1,50), rep(2,30), rep(3, 10), rep(0,10))


#------- run for 100 gens
waterbodies <- list(current_river = data.frame(fish = 1:100, year = 1, age = 1, 
                                               age_maturity = 2, emigrate = 0, birth = 0, death = 0),
                    lake = NULL)

# current_river <- data.frame(fish = 1:100, year = 1, age = 1, age_maturity = 2, emigrate = 0, birth = 0, death = 0)
# lake <- NULL
gens <- c(2:200)
emigrants <- NULL
census <- waterbodies$current_river
for(j in gens){
  #------- emigration
  
  waterbodies$current_river <- emigrate(waterbodies$current_river)
  emigrants <- rbind(emigrants, waterbodies$current_river[waterbodies$current_river[,5]==1,]) # add emigrants to emigrant dataframe
  waterbodies$current_river <- waterbodies$current_river[waterbodies$current_river[,5]==0,] #drop individuals tha emigrated
  
  #------- migrate
  waterbodies <- migrate(current_river = waterbodies$current_river, lake = waterbodies$lake)
  
  #------- density dependence
  waterbodies$current_river <- density_dependence(waterbodies$current_river)
  
  #------ births
  waterbodies$current_river <- make_baby(waterbodies$current_river)
  
  #------ add individuals to census, add babies to next gen, add gen
  census <- rbind(census, waterbodies$current_river, waterbodies$lake)
  #remake current river based on sum of births 
  if(is.null(waterbodies$current_river)==TRUE){
    print(paste("No births, fish are in lake. The year is", j))
  } else{
  waterbodies$current_river <- data.frame(fish = 1:sum(waterbodies$current_river[,6]), year = j, age = 1, age_maturity = 2, emigrate = 0, birth = 0, death = 0)
  }
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
