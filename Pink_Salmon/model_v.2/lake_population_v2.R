#Data structure to hold all fish currently in the lake population
directory = paste(getwd(), "/Pink_Salmon/model_v.1/", sep = '')
source(paste(directory, "source/FunctionSourcer.R", sep = ''))   #source functions and set source directory

#### set number of iterations to run and object to store fitness_summary for each run

runs <- 100
iteration_summary <- NULL

years <- c(1:103)
habitat.occupancy <- data.frame(matrix(NA, runs*length(years), 8))
colnames(habitat.occupancy) <- c("run", "year", "fixed.maturity", "habitat.struct", "num.river.with.fish", "total.count", "habitat.K", "p.filled")


for (z in c(1:runs)){
run <- z

#To generate a random habitat
#pattern is a toggle for different habitat structure generation
#1 random sampling according to the sizes and distribution given
#2 repeating pattern of small, small, big (500, 500, 2000)
#3 bigs all at the start, with the rest all small
#4 small all at the start, with big at the far side of the ring
#note that 2-4 will only work with 2 sizes, must be entered c(small, big) and distrib to match
habitat.gen.pattern <- 3
habitat <- SampleHabitat(n = 51, disp.dist = 2, pattern = habitat.gen.pattern)
habitat <- within(habitat, c(river <- factor(river)))

#Data structure to hold the habitat information
#habitat <- data.frame(river = c("Current", "Wolf", "Steel"), 
#                      size = c(500, 500, 2000), 
#                      migr.curr = c(95,5,0), 
#                      migr.wolf = c(5,90,5), 
#                      migr.steel = c(0,5,95))
#Alternatively, have the habitat variants be their own .csv file
#habitat <- read.csv("filename.csv", header = TRUE)

### toggle switches for different modeling approaches
fixed.maturity <- FALSE #when true fish only mature at 2, when false mature at 2 and 3

#set up the data frame for the lake population
#start with some initial colonizing population (those first released into lake)
pop.initial <- 100
lake.salmon <- data.frame(matrix(NA, pop.initial, 13))
colnames(lake.salmon) <- c("year", "fish.num", "sex", "mass", "age", "age.mat", "river", "mig.river","died", "num.f1", "mom.num", "dad.num", "ind.fitness")

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
#lake.salmon$river <- rep("Current",nrow(lake.salmon))
#will need a line for handling factors to ensure that river is a factor 
#with all possible rivers based on habitat
#initial fish have not had the opportunity to migrate
#lake.salmon$mig.river <- rep("Current",nrow(lake.salmon))

#to use the randomly generated habitat
lake.salmon$river <- rep(levels(habitat$river)[habitat$river[1]],nrow(lake.salmon))
lake.salmon$mig.river <- rep(levels(habitat$river)[habitat$river[1]],nrow(lake.salmon))


#died <---- column to record death 0 == alive, 1 == dead

lake.salmon$died <- 0

#an empty fitness column to be filled after the simulation
lake.salmon$ind.fitness <- NA

#initial fish have not had a breeding opportunity so leave num.f1 as 'NA'
#similarly they are the original fish so we won't have info on their parents

census <- NULL
print(sprintf("Run %i of %i, %i years long", run, runs, length(years)))

for (i in years){
  print(i)
  
  #####-----emigrate
  lake.salmon <- emigrate(lake.salmon, habitat)
  
  #take a snapshot of how much of the total carrying capacity of the
  #environment is currently occupied
  #taking this snapshot at the start of the year means reproduction and
  #density dependent mortality have not yet affected the population
  habitat.occupancy[((run-1)*length(years)+i),] <- c(run, i, fixed.maturity, habitat.gen.pattern, length(unique(lake.salmon$mig.river[!is.na(lake.salmon$mig.river)])),
                                                     nrow(lake.salmon), sum(habitat$size), nrow(lake.salmon)/sum(habitat$size))
  
  #####-----age_fish
  lake.salmon <- age_fish(lake.salmon)
  
  #####-----density dependence
  lake.salmon <- density_dependence(lake.salmon, habitat)
  
  #####-----reproduction
  lake.salmon <- reproduce(lake.salmon, habitat, fixed.maturity)
  
  #keep track of every fish that was alive in this year
  census[[i]] <- lake.salmon
  
  #####-----mortality
  lake.salmon <- mortality(lake.salmon)
}

#update census with fitness for each individual salmon
census <- fitness(census, max(years))

# fitness summary

fitness_summary <- fit_summ(census, fixed.maturity, run)

#append fitness summary into iteration summary
iteration_summary <- rbind(iteration_summary, fitness_summary) 
}

### write out files 
write.csv(iteration_summary, paste(directory, "output/iteration_summary.csv", sep = ''))

write.csv(habitat.occupancy, paste(directory, "output/habitat_occupancy.csv", sep = ''))


