
#------- initialize population

### make waterbodies 

# make list of rivers, only current river will have fish to start, everything else will be empty
waterbodies <- list(rivers = list(current_river = data.frame(fish = 1:100, mig_river = "current_river", year = 1, age = 1, age_maturity = 2, emigrate = 0, birth = 0, death = 0, stringsAsFactors = FALSE),
               wolf_river = NULL), lake = NULL)

### draw parameters for births, emigrants, etc

# 10% chance of emigration
curr_riv_emdraw <-  c(rep("current_river",90), rep("wolf_river", 10))
wolf_riv_emdraw <-  c(rep("wolf_river",90), rep("current_river", 10))

#50% chance of replacement, 30% of doubling, 10% of tripling, 10% of not at all
birth_draw <- c(rep(1,50), rep(2,30), rep(3, 10), rep(0,10))


#####################################################################

# functions

#####################################################################

#------- emigrate function
emigrate <-  function(rivers){
  #test to see if anything in the river
  if(is.null(rivers)==TRUE){
    print(paste("No fish in", rivers))
  } 
  # if there are fish in river then sample from their respective emigrant draw
  else{
    if(rivers == "current_river"){
      for(i in 1:nrow(rivers)) {
        rivers[i,"mig_river"] <- sample(curr_riv_emdraw, size = 1, replace = TRUE)
      }
    } else{
      for(i in 1:nrow(rivers)) {
        rivers[i,"mig_river"] <- sample(wolf_riv_emdraw, size = 1, replace = TRUE)
      }
    }
  }
  return(rivers)
}

#------- outmigrate function

outmigrate <-  function(current_river, wolf_river, lake){
  
  #send fish in rivers into lake
  lake <- rbind(lake, current_river, wolf_river)
  
  #clear rivers
  current_river <- NULL
  wolf_river <- NULL
  
  #make and return waterbodies
  
  waterbodies <- list(rivers = list(current_river, wolf_river), lake  = lake)
  
  return(waterbodies)
  
}

#------- migrate to lake and age

migrate <- function(lake){
  
  # index fish migrating to current river and appropriate age
  current_river_mig <- which(lake[,"mig_river"]=="current_river" & 
                               lake[,"age"]==lake[,"age_maturity"])
  
  current_river <-lake[current_river_mig,]
  
  # index fish migrating to wolf river and appropriate age
  wolf_river_mig <- which(lake[,"mig_river"]=="wolf_river" & 
                               lake[,"age"]==lake[,"age_maturity"])
  
  wolf_river <- lake[wolf_river_mig,]
  
  # index lake for fish that are less than age at maturity, drop fish that have aged out
  # then add a year
  
  lake_nonmigs <- which(lake[,"age"] < lake[,"age_maturity"]) #lake non-migrants
  lake <- lake[lake_nonmigs,]
  
  lake[,"age"] <- lake[,"age"] + 1
  
  #remake and return waterbodies
  waterbodies <- list(rivers = list(current_river, wolf_river), lake = lake)
  return(waterbodies)
  
}


#------- old ### migrate to lake and age (must take those fish and move)
# migrate <- function(rivers, lake){
#   
#   #append current river fish into lake
#   lake <- rbind(lake, rivers)
#   
#   #clear out rivers
#   rivers <- NULL
#   #make temp lake
#   lake_temp <-NULL
#   
#   
#   for(y in 1:nrow(lake)){
#     
#     # if age == age at maturity, put those fish back into current river
#     if(lake[y,3] == lake[y,4]){
#       
#       #put fish whose age == age at maturity into current river
#       rivers <- rbind(rivers, lake[y,])
#       
#     } else{
#       # put fish whose age != age at maturity into temp lake
#       lake_temp <-rbind(lake_temp, lake[y,])
#     }
#   }
#   
#   # make "lake" fish that are less that age at maturity
#   lake <- lake_temp
#   # add a year of age 
#   lake[,"age"] <- lake[,"age"] + 1
#   
#   #create list and return it
#   return(waterbodies)
# }
#------- density dependence

density_dependence <- function(rivers){
  if(is.null(rivers)==TRUE){
    print(paste("No limitation on spawning, fish are in lake"))
  }else{
    pop_size <- nrow(rivers) # make pop size = to length of pop
    k <-round(rnorm(1, mean = 5000, sd = 250),0) # K is a random draw from normal dist w/ mean 5000 and sd 250
    if(pop_size > k){
      number_to_kill <- pop_size - k # get number of individuals to kill
      individuals_to_kill <- sample(1:pop_size, size = number_to_kill, replace = FALSE)
      # kill individuals in current river that match those in individuals to kill
      rivers[individuals_to_kill,"death"]  <- 1
    }
  }
  return(rivers)
}

#------- births
make_baby <- function(rivers){
  if(nrow(rivers)==0){
    print(paste("No spawning, fish are in lake"))
  } else{ 
    for(i in 1:nrow(rivers)){
      #only do individuals that haven't died
      if(rivers[i,"death"] == 0){
        rivers[i,"birth"] <- sample(birth_draw, size = 1, replace = TRUE)
      }
    }
  }
  return(rivers)
}

#####################################################################

# run IBM

#####################################################################

#------- intialize starting parameters
### make waterbodies 

# make list of rivers, only current river will have fish to start, everything else will be empty
waterbodies <- list(rivers = list(current_river = data.frame(fish = 1:100, mig_river = "current_river", year = 1, age = 1, age_maturity = 2, emigrate = 0, birth = 0, death = 0, stringsAsFactors = FALSE),
                                  wolf_river = NULL), lake = NULL)

### draw parameters for births, emigrants, etc

# 10% chance of emigration
curr_riv_emdraw <-  c(rep("current_river",90), rep("wolf_river", 10))
wolf_riv_emdraw <-  c(rep("wolf_river",90), rep("current_river", 10))

#50% chance of replacement, 30% of doubling, 10% of tripling, 10% of not at all
birth_draw <- c(rep(1,50), rep(2,30), rep(3, 10), rep(0,10))

# num of gens
gens <- c(2:200)

# empty data frame for sum stats

sum_stats <- NULL

#### run the IBM
for (j in gens){
  
  #------- emigrate 
  
  waterbodies$rivers <- lapply(waterbodies$rivers, emigrate)
  
  #------- outmigrate 
  
  waterbodies <- outmigrate(current_river = waterbodies$rivers$current_river,
                            wolf_river = waterbodies$rivers$wolf_river,
                            lake = waterbodies$lake)
  
  #------- migrate
  
  waterbodies <- migrate(lake = waterbodies$lake)
  
  #------- density dependence
  
  waterbodies$rivers <- lapply(waterbodies$rivers, density_dependence)
  
  #------- births
  
  waterbodies$rivers <- lapply(waterbodies$rivers, make_baby)
  
  #------- compile summary statistics
  
  ### for current river
  if(is.null(waterbodies$rivers[1]) == TRUE){
    cr_N <- 0
    cr_births <- 0
  }else{
  cr_N <- nrow(waterbodies$rivers$current_river) # number of fish that spawned
  cr_births <- sum(waterbodies$rivers$current_river[,"birth"]) # number of babies born
  }
  
  ### for wolf river
  if(is.null(waterbodies$rivers[2]) == TRUE){
    wr_N <- 0
    wr_births <- 0
  } else{
  wr_N <- nrow(waterbodies$rivers$wolf_river) # number of fish that spawned
  wr_births <- sum(waterbodies$rivers$wolf_river[,"birth"]) # number of babies born
  }
  
  ### for lake
  if(is.null(waterbodies$lake) == TRUE){
    lake_N <- 0
  } else{
  lake_N <- nrow(waterbodies$lake)
  }
  
  sum_stats <- rbind(sum_stats, cbind(cr_N, cr_births, wr_N, wr_births, lake_N))
  
  #------- remake waterbodies with new births
  
  ### current river
  if(is.null(waterbodies$rivers$current_river)== TRUE){
    current_river <- NULL
  }else{
  current_river <- data.frame(fish = sum(waterbodies$rivers$current_river[,"birth"]), 
                              mig_river = "current_river", year = j, age = 1, age_maturity = 2, 
                              emigrate = 0, birth = 0, death = 0, stringsAsFactors = FALSE)
  }
  
  ### wolf river
  if(is.null(waterbodies$rivers$wolf_river)== TRUE){
    wolf_river <- NULL
  }else{
    wolf_river <- data.frame(fish = sum(waterbodies$rivers$wolf_river[,"birth"]), 
                                mig_river = "wolf_river", year = j, age = 1, age_maturity = 2, 
                                emigrate = 0, birth = 0, death = 0, stringsAsFactors = FALSE)
  }
  
  waterbodies <- list(rivers = list(current_river = current_river, wolf_river = wolf_river), 
                      lake = waterbodies$lake)
  

}

