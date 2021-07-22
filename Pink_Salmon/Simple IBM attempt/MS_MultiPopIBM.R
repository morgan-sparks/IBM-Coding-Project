
### libraries
library(ggplot2); library(patchwork)

#####################################################################

# functions

#####################################################################

#------- emigrate function
emigrate <-  function(rivers){
  #test to see if anything in the river
  if(nrow(rivers)==0){
    print(paste("No fish in rivers"))
  } 
  # if there are fish in river then sample from their respective emigrant draw
  else{
    if(rivers[1,"mig_river"]=="current_river"){
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
  current_river <- current_river[FALSE,]
  wolf_river <- wolf_river[FALSE,]
  
  #make and return waterbodies
  
  waterbodies <- list(rivers = list(current_river = current_river, wolf_river = wolf_river), lake  = lake)
  
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
  waterbodies <- list(rivers = list(current_river = current_river, wolf_river = wolf_river), lake = lake)
  return(waterbodies)
  
}

#------- density dependence

density_dependence <- function(rivers){
  if(is.null(rivers)==TRUE){
    print(paste("No limitation on spawning, fish are in lake"))
  }else{
    if(nrow(rivers)<20){ #if ther are less 20 fish in the river thet can't spawn and set pop = 0
      rivers <- rivers[FALSE,]
    }else{
    pop_size <- nrow(rivers) # make pop size = to length of pop
    k <-round(rnorm(1, mean = 5000, sd = 250),0) # K is a random draw from normal dist w/ mean 5000 and sd 250
    if(pop_size > k){
      number_to_kill <- 1.1*(pop_size - k) # get number of individuals to kill (1.1* # over K)
      individuals_to_kill <- sample(1:pop_size, size = number_to_kill, replace = FALSE)
      # kill individuals in current river that match those in individuals to kill
      rivers[individuals_to_kill,"death"]  <- 1
    }
    }
  }
  rivers <- rivers[which(rivers[,"death"] ==0),]
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

current_river <- data.frame(fish = 1:100, mig_river = "current_river", year = 1, age = 1, age_maturity = 2, emigrate = 0, birth = 0, death = 0, stringsAsFactors = FALSE)
wolf_river <- current_river[FALSE,] # make empty df but inherit names from current river
lake <- current_river[FALSE,] # make empty df but inherit names from current river

waterbodies <- list(rivers = list(current_river = current_river, wolf_river = wolf_river),
                    lake = lake)
### draw parameters for births, emigrants, etc

# 10% chance of emigration
curr_riv_emdraw <-  c(rep("current_river",90), rep("wolf_river", 10))
wolf_riv_emdraw <-  c(rep("wolf_river",90), rep("current_river", 10))

#50% chance of replacement, 30% of doubling, 10% of tripling, 10% of not at all
birth_draw <- c(rep(1,50), rep(2,30), rep(3, 10), rep(0,10))

# age at maturyit draw
#mat_age_draw <- c(rep(2,90), rep(3,10))
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
  if(nrow(waterbodies$rivers$current_river) == 0){
    cr_N <- 0
    cr_births <- 0
  }else{
  cr_N <- nrow(waterbodies$rivers$current_river) # number of fish that spawned
  cr_births <- sum(waterbodies$rivers$current_river[,"birth"]) # number of babies born
  }
  
  ### for wolf river
  if(nrow(waterbodies$rivers$wolf_river) == 0){
    wr_N <- 0
    wr_births <- 0
  } else{
  wr_N <- nrow(waterbodies$rivers$wolf_river) # number of fish that spawned
  wr_births <- sum(waterbodies$rivers$wolf_river[,"birth"]) # number of babies born
  }
  
  ### for lake
  if(nrow(waterbodies$lake) == 0){
    lake_N <- 0
  } else{
  lake_N <- nrow(waterbodies$lake)
  }
  
  sum_stats <- rbind(sum_stats, cbind(year = j,cr_N, cr_births, wr_N, wr_births, lake_N))
  
  #------- remake waterbodies with new births
  
  ### current river
  if(nrow(waterbodies$rivers$current_river)== 0){
    current_river <- waterbodies$rivers$current_river
  }else{
  current_river <- data.frame(fish = 1:sum(waterbodies$rivers$current_river[,"birth"]), 
                              mig_river = "current_river", year = j, age = 1, age_maturity = 2, 
                              emigrate = 0, birth = 0, death = 0, stringsAsFactors = FALSE)
  }
  
  ### wolf river
  if(nrow(waterbodies$rivers$wolf_river) == 0){
    wolf_river <- waterbodies$rivers$wolf_river
  }else{
    wolf_river <- data.frame(fish = 1:sum(waterbodies$rivers$wolf_river[,"birth"]), 
                                mig_river = "wolf_river", year = j, age = 1, age_maturity = 2, 
                                emigrate = 0, birth = 0, death = 0, stringsAsFactors = FALSE)
  }
  
  waterbodies <- list(rivers = list(current_river = current_river, wolf_river = wolf_river), 
                      lake = waterbodies$lake)
  

}


#####################################################################

# plot IBM

#####################################################################

sum_stats <- data.frame(sum_stats)

y200 <- ggplot(data = sum_stats)+
 geom_line(aes(x = year, y = cr_N, color = "Current River"), size =0.5) +
  geom_line(aes(x =year, y = wr_N, color = "Wolf River"), size =0.5 ) +
  geom_hline(yintercept = 5000, linetype = "dashed" ) +
  scale_color_manual(name = "Population", 
                     values = c("Current River" = "dodgerblue", "Wolf River" = "orange")) +
  labs( x ="Year", y = "Population size", title= "Years 1-200") +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") 

y10 <- ggplot(data = sum_stats[1:10,])+
  geom_line(aes(x = year, y = cr_N, color = "Current River"), size =1) +
  geom_line(aes(x =year, y = wr_N, color = "Wolf River"), size =1 ) +
  #geom_hline(yintercept = 5000, linetype = "dashed" ) +
  scale_color_manual(name = "Population", 
                     values = c("Current River" = "dodgerblue", "Wolf River" = "orange")) +
  labs( x ="Year", y = "Population size", title= "Years 1-10") +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") 

y50 <- ggplot(data = sum_stats[1:50,])+
  geom_line(aes(x = year, y = cr_N, color = "Current River"), size =0.5) +
  geom_line(aes(x =year, y = wr_N, color = "Wolf River"), size =0.5 ) +
  geom_hline(yintercept = 5000, linetype = "dashed" ) +
  scale_color_manual(name = "Population", 
                     values = c("Current River" = "dodgerblue", "Wolf River" = "orange")) +
  labs( x ="Year", y = "Population size", title= "Years 1-50") +
  theme_classic(base_size = 14) +
  theme(legend.position = "none") 

(y10 + y50)/y200 + theme(legend.position = "bottom")





