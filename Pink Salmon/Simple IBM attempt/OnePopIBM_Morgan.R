#####################################################################################
# Morgan Sparks
# 03/03/2021 msparks1309@gmail.com

# Script to make a population of fish that starts with 100 individuals and grows 
# until carrying capacity 5000, with births, deaths, and emigration (maybe)

#####################################################################################

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
    individuals_to_kill <- order(sample(1:pop_size, size = number_to_kill, replace = FALSE))
    
    # kill individuals in current river that match those in individuals to kill
    current_river[individuals_to_kill,5]  <- 1
  }
  return(current_river)
}

#------- birth function

make_baby <- function(current_river){
  if(current_river[,5] == 0){
  for(i in 1:nrow(current_river)){
    #only do individuals that dies
    if(current_river[,5] == 0)
    current_river[i,4] <- sample(birth_draw, size = 1, replace = TRUE)
  }
    }
  return(current_river)
  }


#####################################################################################
# execute the ibm


#------- run for 50 gens
current_river <- data.frame(fish = 1:100, year = 1, emigrate = 0, birth = 0, death = 0)
gens <- seq(from = 3, to = 101, by = 2)
emigrants <- NULL
census <- current_river

for(j in gens){
  #------- emigration
 
  emigrate(current_river)
  emigrants <- rbind(emigrants, current_river[current_river[,5]==1,]) # add emigrants to emigrant dataframe
  current_river <- current_river[current_river[,5]==0,] #drop individuals tha emigrated
  
  #------- density dependence
  density_dependence(current_river)
  
  #------ births
  make_baby(current_river)
  
  #------ add individuals to census, add babies to next gen, add gen
  census <- rbind(census)
  #remake dataframe of current river baed on sum of births in birth column
  current_river <- data.frame(fish = 1:sum(current_river[,4]), year = j, emigrate = 0, birth = 0, death = 0)
  
  j <- j +2
  
}


emigrate(current_river)
emigrants <- rbind(emigrants, current_river[current_river[,3]==1,]) # add emigrants to emigrant dataframe
current_river <- current_river[current_river[,3]==0,] #drop individuals tha emigrated

#------- density dependence
density_dependence(current_river)

#------ births
make_baby(current_river)

#------ add individuals to census, add babies to next gen, add gen
census <- rbind(census)
current_river <- data.frame(fish = 1:sum(current_river[,4]), year = j, emigrate = 0, birth = 0, death = 0)






