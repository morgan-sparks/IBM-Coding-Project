##################################################
#Last edited by B. Goller on 31 March 2021
#Set up simple salmon IBM
##################################################
#Get the required packages
library(ggplot2)
##################################################
#Clear the workspace
rm(list = ls())
##################################################
##################################################
#DEFINE FUNCTIONS
##################################################
##################################################
#function that determines which fish reproduce, and how many offspring
#no generational overlap, so all adults from previous population die
reproduction <- function(return.fish, percent.repro, g.rate){
  return.fish$num.f1 <- rep(0,nrow(return.fish))
  repro.active <- sample(c(0,1), nrow(return.fish), prob = c(1-percent.repro, percent.repro), replace = TRUE)
  mean.offspring <- (nrow(return.fish)*g.rate)/sum(repro.active)
  return.fish$num.f1[repro.active==1] <- round(rnorm(sum(repro.active), mean = mean.offspring, sd = mean.offspring/10))
  return(return.fish$num.f1)
}
##################################################
#assign mass to fishies
nextgen.fish <- function(spawn.pop){
  nextgen <- data.frame(matrix(NA, sum(spawn.pop$num.f1), ncol(spawn.pop)))
  colnames(nextgen) <- colnames(spawn.pop)
  nextgen$year <- rep(spawn.pop$year[1], nrow(nextgen))
  nextgen$fish.num <- c(1:nrow(nextgen))+max(spawn.pop$fish.num)
  nextgen$migrant <- rep(0, nrow(nextgen))
  nextgen$age <- rep(0, nrow(nextgen))
  
  #SELECT ONE OPTION TO DETERMINE WHETHER AGE AT MATURATION IS FIXED
  #if prob = c(100,0) then there is no chance of developing a different maturation time
  #nextgen$age.mat <- sample(c(2,3), nrow(nextgen), prob = c(100,0), replace = TRUE)
  #allow some fish to evolve a 3-yr maturation if probability of selecting a 3 is >0
  nextgen$age.mat <- sample(c(2,3), nrow(nextgen), prob = c(97,3), replace = TRUE)
  
  #give offspring their parent's mass
  nextgen$mass <- unlist(apply(X = spawn.pop[,c("mass","num.f1")], MARGIN = 1, function(X) rep(X[1], X[2])))
  nextgen$river <- unlist(apply(X = spawn.pop[,c("river","num.f1")], MARGIN = 1, function(X) rep(X[1], X[2])))
  #keep track of parent fish.num
  nextgen$parent.num <- unlist(apply(X = spawn.pop[,c("fish.num","num.f1")], MARGIN = 1, function(X) rep(X[1], X[2])))
  return(nextgen)
}
##################################################
#assign mass to fishies
fatten.fish <- function(pop, mean, sd){
  masses <- rnorm(nrow(pop), mean = mean, sd = sd)
  return(masses)
}
##################################################
#enforce mortality by carrying capacity
k.mortality <- function(pop, k.lim){
  #ifelse(nrow(pop) > k.lim, survivors <- sample(c(1:nrow(pop)), k.lim, replace = FALSE), survivors <- c(1:nrow(pop)))
  
  #can add some random fluctuation to the carrying capacity
  var.lim <- k.lim+round(rnorm(1,mean = 0, sd = k.lim/5))
  
  ifelse(nrow(pop) > var.lim, survivors <- sample(c(1:nrow(pop)), var.lim, replace = FALSE), survivors <- c(1:nrow(pop)))
  
  #or could have a survivor sampling method that uses mass to increase likelihood of survival
  #ifelse(nrow(pop) > k.lim, survivors <- sample(c(1:nrow(pop)), k.lim, prob = pop$mass, replace = FALSE), survivors <- c(1:nrow(pop)))
  
  return(survivors)
}
##################################################
#allow some fishes to change their "river" designation
migration <- function(pop, river.list, migr.rate){
  #given a population, allow the river desigations to change 
  #according to the river.list provided, and migr.rates provided (vectors must match in length)
  #returns the new river designations only
  origin <- pop$river
  river.home <- data.frame(river = sample(river.list, nrow(pop), prob = migr.rate, replace = TRUE), migrant = rep(0,nrow(pop)))
  #keep track of any fish that switched river homes
  river.home[river.home[,1] != origin,2] <- 1
  
  return(river.home)
}
##################################################
##################################################
#Model a population
#start with 100 fish
#carrying capacity (K) = 5000
#population growth rate 1.2 (120%)
pop.initial <- 100
gen.growth <- 1.2
#will say that only half the fish reproduce
p.repro <- 0.5
yr.start <- 1970
habitat <- data.frame(river = c("Current", "Wolf", "Steel"), 
                      pop.k = c(500, 500, 2000), 
                      migr.curr = c(95,5,0), 
                      migr.wolf = c(5,90,5), 
                      migr.steel = c(0,5,95))

salmon.df.init <- data.frame(matrix(NA, pop.initial, 9))
colnames(salmon.df.init) <- c("year", "river", "fish.num", "mass", "age", "age.mat", "migrant", "num.f1","parent.num")
salmon.df.init$year <- rep(yr.start,nrow(salmon.df.init))
salmon.df.init$river <- factor(rep("Current",nrow(salmon.df.init)), levels = c("Current", "Steel", "Wolf"))
salmon.df.init$fish.num <- c(1:pop.initial)
salmon.df.init$age <- rep(0,nrow(salmon.df.init))
salmon.df.init$age.mat <- rep(2,nrow(salmon.df.init))
salmon.df.init$migrant <- rep(0,nrow(salmon.df.init))
salmon.df.init$mass <- fatten.fish(salmon.df.init, 8, 1)
#salmon.df.init$num.f1 <- reproduction(salmon.df.init, p.repro, gen.growth)



#set up a lake population
#fish from initial population swim out to the lake
lake.fishes <- salmon.df.init
lake.survey <- NULL
spawner.survey <- NULL
lake.survey[[1]] <- lake.fishes

for(i in c(1:200)){
  #clear the babies variable to make sure we don't accidentally carry over fish from last year
  babies <- NULL
  
  #fish in lake spend 1 year growing
  lake.fishes$year <- lake.fishes$year + 1
  lake.fishes$age <- lake.fishes$age + 1
  print(lake.fishes$year[1])
  
  #allow some fishes to change their river affiliation because of migration
  for(m in 1:nrow(habitat)){
    curr.residents <- which(lake.fishes$river == habitat$river[m] & lake.fishes$migrant == 0)
    if(length(curr.residents) > 0){
      lake.fishes[curr.residents, c("river", "migrant")] <- migration(lake.fishes[curr.residents,], habitat$river, habitat[m,3:5])
    }
  }
  
  for(r in 1:nrow(habitat)){
    #after that year was added, do a reproductive age check for the river
    spawners <- which(lake.fishes$age == lake.fishes$age.mat & lake.fishes$river == habitat$river[r])
    
    #only allow spawning if there are at least 10 fishes returning to spawn for any given river
    #otherwise fishes that reach reproductive age but don't have mates just die
    ifelse(length(spawners) > 19,
           {lake.fishes$num.f1[spawners] <- reproduction(lake.fishes[spawners,], p.repro, gen.growth);
           spawner.survey <- rbind(spawner.survey, lake.fishes[spawners,]);
           #get a data frame with all the babies that are leaving the stream and entering the lake
           babies <- nextgen.fish(lake.fishes[spawners,]);
           babies <- babies[k.mortality(babies, k.lim = habitat$pop.k[r]),]
           #let the babies swim out into the lake and join the non-spawner lake fish
           lake.fishes <- rbind(lake.fishes[-spawners,], babies)},
           ifelse(length(spawners) > 0,
                  lake.fishes <- lake.fishes[-spawners,],
                  lake.fishes <- lake.fishes))
  }
  
  
  #store the snapshot of what the lake population looks like for the year
  #post reproduction if any
  lake.survey[[i+1]] <- lake.fishes
}

lake.fish.count <- matrix(NA, length(lake.survey), 7)
for(a in 1:length(lake.survey)){
  lake.fish.count[a,] <- c(a, lake.survey[[a]]$year[1], nrow(lake.survey[[a]]),
                           sum(lake.survey[[a]]$river == "Current"),
                           sum(lake.survey[[a]]$river == "Wolf"),
                           sum(lake.survey[[a]]$river == "Steel"),
                           sum(lake.survey[[a]]$age.mat == 3))
}

lake.fish.count <- data.frame(lake.fish.count)
colnames(lake.fish.count) <- c("entry.num", "year", "total.pop", "current.pop", "wolf.pop", "steel.pop", "yrmat3.pop")
library(ggplot2)

ggplot(data = lake.fish.count, aes(x = year, y = total.pop))+
  geom_point()+
  geom_point(data = lake.fish.count, aes(x = year, y = current.pop), col = 2)+
  geom_point(data = lake.fish.count, aes(x = year, y = wolf.pop), col = 3)+
  geom_point(data = lake.fish.count, aes(x = year, y = steel.pop), col = 4)+
  theme_classic()

ggplot(data = lake.fish.count, aes(x = year, y = total.pop))+
  geom_point()+
  geom_point(data = lake.fish.count, aes(x = year, y = yrmat3.pop), col = 2)+
  theme_classic()
