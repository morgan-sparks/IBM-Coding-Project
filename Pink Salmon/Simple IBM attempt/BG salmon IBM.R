##################################################
#Last edited by B. Goller on 10 March 2021
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
  nextgen$fish.num <- c(1:nrow(nextgen))+max(spawn.pop$fish.num)
  #give offspring their parent's mass
  nextgen$mass <- unlist(apply(X = spawn.pop[,c(2,3)], MARGIN = 1, function(X) rep(X[1], X[2])))
  #keep track of parent fish.num
  nextgen$parent.num <- unlist(apply(X = spawn.pop[,c(1,3)], MARGIN = 1, function(X) rep(X[1], X[2])))
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


salmon.df.init <- data.frame(matrix(NA, pop.initial, 4))
colnames(salmon.df.init) <- c("fish.num", "mass", "num.f1","parent.num")
salmon.df.init$fish.num <- c(1:pop.initial)
salmon.df.init$mass <- fatten.fish(salmon.df.init, 8, 1)
salmon.df.init$num.f1 <- reproduction(salmon.df.init, p.repro, gen.growth)

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