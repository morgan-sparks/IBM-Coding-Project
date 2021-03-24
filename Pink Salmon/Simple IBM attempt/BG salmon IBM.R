##################################################
#Last edited by B. Goller on 22 March 2021
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
  nextgen$age <- rep(0, nrow(nextgen))
  
  #SELECT ONE OPTION TO DETERMINE WHETHER AGE AT MATURATION IS FIXED
  #if prob = c(100,0) then there is no chance of developing a different maturation time
  nextgen$age.mat <- sample(c(2,3), nrow(nextgen), prob = c(100,0), replace = TRUE)
  #allow some fish to evolve a 3-yr maturation if probability of selecting a 3 is >0
  #nextgen$age.mat <- sample(c(2,3), nrow(nextgen), prob = c(95,5), replace = TRUE)
  
  #give offspring their parent's mass
  nextgen$mass <- unlist(apply(X = spawn.pop[,c(3,6)], MARGIN = 1, function(X) rep(X[1], X[2])))
  #keep track of parent fish.num
  nextgen$parent.num <- unlist(apply(X = spawn.pop[,c(2,6)], MARGIN = 1, function(X) rep(X[1], X[2])))
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
  #or could have a survivor sampling method that uses mass to increase likelihood of survival
  ifelse(nrow(pop) > k.lim, survivors <- sample(c(1:nrow(pop)), k.lim, prob = pop$mass, replace = FALSE), survivors <- c(1:nrow(pop)))
  
  return(survivors)
}
##################################################
##################################################
#Model a population
#start with 100 fish
#carrying capacity (K) = 5000
#population growth rate 1.2 (120%)
pop.initial <- 100
habitat.K <- 5000
gen.growth <- 1.2
#will say that only half the fish reproduce
p.repro <- 0.5
yr.start <- 1970


salmon.df.init <- data.frame(matrix(NA, pop.initial, 7))
colnames(salmon.df.init) <- c("year", "fish.num", "mass", "age", "age.mat", "num.f1","parent.num")
salmon.df.init$year <- rep(yr.start,nrow(salmon.df.init))
salmon.df.init$fish.num <- c(1:pop.initial)
salmon.df.init$age <- rep(0,nrow(salmon.df.init))
salmon.df.init$age.mat <- rep(2,nrow(salmon.df.init))
salmon.df.init$mass <- fatten.fish(salmon.df.init, 8, 1)
#salmon.df.init$num.f1 <- reproduction(salmon.df.init, p.repro, gen.growth)



#set up a lake population
#fish from initial population swim out to the lake
lake.fishes <- salmon.df.init
lake.survey <- NULL
lake.survey[[1]] <- lake.fishes

for(i in c(1:50)){
  #clear the babies variable to make sure we don't accidentally carry over fish from last year
  babies <- NULL
  
  #fish in lake spend 1 year growing
  lake.fishes$year <- lake.fishes$year + 1
  lake.fishes$age <- lake.fishes$age + 1
  print(lake.fishes$year[1])
  
  #after that year, do a reproductive age check
  spawners <- which(lake.fishes$age == lake.fishes$age.mat)
  
  #only allow spawning if there are at least 10 fishes returning to spawn
  #otherwise fishes that reach reproductive age but don't have mates just die
  ifelse(length(spawners) > 9,
         {lake.fishes$num.f1[spawners] <- reproduction(lake.fishes[spawners,], p.repro, gen.growth);
          #get a data frame with all the babies that are leaving the stream and entering the lake
          babies <- nextgen.fish(lake.fishes[spawners,]);
          #let the babies swim out into the lake and join the non-spawner lake fish
          lake.fishes <- rbind(lake.fishes[-spawners,], babies)},
          ifelse(length(spawners) > 0,
                 lake.fishes <- lake.fishes[-spawners,],
                 lake.fishes <- lake.fishes))
  
  #store the snapshot of what the lake population looks like for the year
  #post reproduction if any
  lake.survey[[i+1]] <- lake.fishes
}

lake.fish.count <- matrix(NA, length(lake.survey), 2)
for(a in 1:length(lake.survey)){
  lake.fish.count[a,] <- c(a, nrow(lake.survey[[a]]))
}
plot(lake.fish.count[,1], lake.fish.count[,2])




##################################################
##################################################
salmon.gens <- NULL
salmon.gens[[1]] <- salmon.df.init
salmon.curr.gen <- salmon.df.init
salmon.abund <- matrix(NA, 100, 3)
salmon.abund[1,] <- c(1, nrow(salmon.df.init), mean(salmon.df.init$mass))
for(g in 2:100){
  salmon.curr.gen <- nextgen.fish(salmon.curr.gen)
  salmon.curr.gen$num.f1 <- reproduction(salmon.curr.gen, p.repro, gen.growth)
  salmon.curr.gen <- salmon.curr.gen[k.mortality(salmon.curr.gen, habitat.K),]
  salmon.abund[g,] <- c(g, nrow(salmon.curr.gen), mean(salmon.curr.gen$mass))
  salmon.gens[[g]] <- salmon.curr.gen
}
plot(salmon.abund[,1], salmon.abund[,2])
plot(salmon.abund[,1], salmon.abund[,3])




salmon.gens2 <- NULL
salmon.gens2[[1]] <- salmon.df.init
salmon.curr.gen <- salmon.df.init
gen.count <- 1
while(nrow(salmon.curr.gen)<habitat.K){
  gen.count <- gen.count+1
  salmon.curr.gen <- nextgen.fish(salmon.curr.gen)
  salmon.curr.gen$num.f1 <- reproduction(salmon.curr.gen, p.repro, gen.growth)
  salmon.gens2[[gen.count]] <- salmon.curr.gen
}
sprintf("it took %i generations to get to carrying capacity!", gen.count)

salmon.abund2 <- matrix(NA, gen.count, 2)
for(a in 1:gen.count){
  salmon.abund2[a,] <- c(a, nrow(salmon.gens2[[a]]))
}
plot(salmon.abund2[,1], salmon.abund2[,2])