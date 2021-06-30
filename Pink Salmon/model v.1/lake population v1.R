#Data structure to hold all fish currently in the lake population
directory = paste(getwd(), "/Pink Salmon/model v.1/", sep = '')
source(paste(directory, "source/FunctionSourcer.R", sep = ''))   #source functions and set source directory

habitat <- SampleHabitat()

#Data structure to hold the habitat information
habitat <- data.frame(river = c("Current", "Wolf", "Steel"), 
                      pop.k = c(500, 500, 2000), 
                      migr.curr = c(95,10,0), 
                      migr.wolf = c(5,90,5), 
                      migr.steel = c(0,5,95))
#Alternatively, have the habitat variants be their own .csv file
#habitat <- read.csv("filename.csv", header = TRUE)

#set up the data frame for the lake population
#start with some initial colonizing population (those first released into lake)
pop.initial <- 100
lake.salmon <- data.frame(matrix(NA, pop.initial, 12))
colnames(lake.salmon) <- c("year", "fish.num", "sex", "mass", "age", "age.mat", "river", "mig.river","died", "num.f1", "mom.num", "dad.num")

#what year were the fish released?
lake.salmon$year <- rep(1950, nrow(lake.salmon))

#number founding population with 1:pop.initial
lake.salmon$fish.num <- c(1:pop.initial)

#sex based on a ratio
p.male <- 0.5
#randomly sampled sex
lake.salmon$sex <- factor(sample(x = c("M","F"), size = pop.initial, replace = TRUE, prob = c(p.male, 1-p.male)))
#non-random sex ratio
#lake.salmon$sex <- factor(c(rep("M", round(pop.initial*p.male)), rep("F", pop.initial-round(pop.initial*p.male))))

#number founding population with 1:pop.initial
#arbitrary...not sure if we want to keep track of masses
lake.salmon$mass <- rnorm(nrow(lake.salmon), mean = 3, sd = 0.5)

#age at release assumed to be 0
lake.salmon$age <- rep(0,nrow(lake.salmon))
#age at maturation of the released fish assumed to be 2
lake.salmon$age.mat <- rep(2,nrow(lake.salmon))

#river is spawning location for the released fish
lake.salmon$river <- rep("Current",nrow(lake.salmon))
#will need a line for handling factors to ensure that river is a factor 
#with all possible rivers based on habitat
#initial fish have not had the opportunity to migrate
lake.salmon$mig.river <- rep("Current",nrow(lake.salmon))

#died <---- column to record death 0 == alive, 1 == dead

lake.salmon$died <- 0

#initial fish have not had a breeding opportunity so leave num.f1 as 'NA'
#similarly they are the original fish so we won't have info on their parents

census <- NULL
years <- c(1:100)

for (i in years){
  print(i)
  
  #####-----emigrate
  lake.salmon <- emigrate(lake.salmon, habitat)
  
  #####-----age_fish
  lake.salmon <- age_fish(lake.salmon)
  
  #####-----density dependence
  lake.salmon <- density_dependence(lake.salmon, habitat)
  
  #####-----reproduction
  lake.salmon <- reproduce(lake.salmon, habitat)
  
  #keep track of every fish that were alive in this year
  census[[i]] <- lake.salmon
  
  #####-----mortality
  lake.salmon <- mortality(lake.salmon)
}
