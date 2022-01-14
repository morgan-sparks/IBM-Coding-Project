reproduce <-  function(lake.salmon, habitat, maturity.toggle){
  #Maturity is fixed at 2 year age at maturity if maturity.toggle == TRUE
  #Maturity is variable between 2 and 3 year old age at maturity if toggle == FALSE
  
  spawning_fish <- lake.salmon[which(lake.salmon$age.mat-lake.salmon$age == 0 & lake.salmon$died == 0),] #select only fish that will be in rivers and haven't died
  if(is.null(spawning_fish)== FALSE){
    new_babies <- NULL # set up a blank object to append babies in to
    
    fecundity <- rgamma(1000, shape = 1.5, rate = 0.5) # = ~3 fish born or 1.5 fish per parent
    #
    #select a river from habitat and subset that river
    for (r in levels(habitat$river)){
      population <- spawning_fish[which(spawning_fish$mig.river == r),] # subset population of fish returning to river r
      if(is.null(population) == FALSE){
        pop.f <- population[which(population$sex == "F"),] # select females
        pop.m <- population[which(population$sex == "M"),] # select males
        
        # num.mates is the number of maiting pairs based on the whether there are fewer males or females in a pop
        # if length of pop.f is shorter or equal to pop.m, make object length pop.f, else make object length pop.m
        num.mates <- ifelse(nrow(pop.f) <= nrow(pop.m), # logical statement
                            nrow(pop.f), # if true
                            nrow(pop.m)) # if false
        
        #set minimum spawning population threshold at 10 pairs
        #could potentially create a toggle that adjusts based on percentage of carrying capacity, or something else
        if(num.mates >= 10){
          # sample random male and females as pairs for length num.mates to pair and creat offspring (sample_n() samples an entire row)
          f.fish <- dplyr::sample_n(pop.f, num.mates, replace = FALSE)
          m.fish <- dplyr::sample_n(pop.m, num.mates, replace = FALSE)
          baby.num <- round(sample(fecundity, num.mates, replace = TRUE)) # sample fecundity distribution for number of fish born
          
          for (n in 1:num.mates){
            if(baby.num[n] > 0){
              babies <- data.frame(matrix(NA, baby.num[n], 13)) #make empty matrix of length baby.num with 13 columns
              colnames(babies) <- c("year", "fish.num", "sex", "mass", "age", "age.mat", "river", "mig.river", "died", "num.f1", "mom.num", "dad.num", "ind.fitness")
              
              babies$year <- max(lake.salmon$year) +1 # add a year to most recent in lake.salmon
              babies$fish.num <-  #give fish unique numbers
                if(is.null(new_babies) == TRUE){ # if you're on the first set of fish
                  c(max(lake.salmon$fish.num)+c(1:baby.num[n]))}else{ # derive numbers from lake.salmon
                    c(max(new_babies$fish.num)+c(1:baby.num[n])) # otherwise derive numbers from new_babies
                  }
              babies$sex <- factor(sample(x = c("M","F"), size = baby.num[n], replace = TRUE, prob = c(0.5, 0.5))) # sample sex on a 50:50 ratio
              babies$mass <- rnorm(nrow(babies), mean = 3, sd = 0.5) # give mass
              babies$age <- 0 #age = 0
              ######----------------
              ifelse(maturity.toggle == TRUE, 
                     babies$age.mat <- 2,
                     babies$age.mat <- sample(x = c(2,3), size = baby.num[n], replace = TRUE, prob = c(0.9,0.1))
                     )
              ######----------------
              babies$river <- f.fish[n,]$mig.river # make home river the river where mom spawned
              babies$mig.river <- NA # to be populated later
              babies$died <- 0 # all fish alive, 0 = alive
              babies$num.f1 <- NA # no reproduction yet
              babies$mom.num <- f.fish[n,]$fish.num # add mother's fish num for pedigree
              babies$dad.num <- m.fish[n,]$fish.num # add father's fish num for pedigree
              babies$ind.fitness <- NA #will be calculated after the simulation is complete
              
              # append babies into new babies
              new_babies <- rbind(new_babies, babies)
              
              #record parent fecundity for future fitness calculations
              lake.salmon$num.f1[which(lake.salmon$fish.num == f.fish$fish.num[n])] <- baby.num[n] 
              lake.salmon$num.f1[which(lake.salmon$fish.num == m.fish$fish.num[n])] <- baby.num[n]
              
            } #end if statements ensuring there are babies (baby.num > 0)
          } #end num.mates loop
        } #end minimum number of mates
      } #end population check (make sure there are at least 20 fish returning to spawn)
    } #end for loop through the different habitats
    lake.salmon <- rbind(lake.salmon, new_babies) # append new babies to lake.salmon
  } #end if statement to ensure there are at least 20 fish spawning this year
  # kill fish that spawned
  lake.salmon$died[which(lake.salmon$fish.num %in% spawning_fish$fish.num)] <- 1
  return(lake.salmon)
} #end reproduce function