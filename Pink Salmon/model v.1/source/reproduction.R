reproduce <-  function(lake.salmon){
  spawning_fish <- lake.salmon[which(lake.salmon$age.mat-lake.salmon$age == 0),] #select only fish that will be in rivers
  
  new_babies <- NULL # set up a blank object to append babies in to
  
  fecundity <- rgamma(1000, shape = 1.5, rate = 0.5) # = ~3 fish born or 1.5 fish per parent
  #
  #select a river from habitat and subset that river
  for (r in levels(habitat$river)){
    population <- spawning_fish[which(spawning_fish$mig.river == r),] # subset population of fish returning to river r
    
    pop.f <- population[which(population$sex == "F"),] # select females
    pop.m <- population[which(population$sex == "M"),] # select males
    
    # num.mates is the number of maiting pairs based on the whether there are fewer males or females in a pop
    # if length of pop.f is shorter or equal to pop.m, make object length pop.f, else make object length pop.m
    num.mates <- ifelse(nrow(pop.f) <= nrow(pop.m), # logical statement
                        nrow(pop.f), # if true
                        nrow(pop.m)) # if false
      
    # sample random male and females as pairs for length num.mates to pair and creat offspring (sample_n() samples an entire row)
      f.fish <- dplyr::sample_n(pop.f, num.mates, replace = FALSE)
      m.fish <- dplyr::sample_n(pop.m, num.mates, replace = FALSE)
      baby.num <- round(sample(fecundity, num.mates, replace = TRUE)) # sample fecundity distribution for number of fish born
      
      for (n in 1:num.mates){
      babies <- data.frame(matrix(NA, baby.num, 11)) #make empty matrix of length baby.num with 11 columns
      colnames(babies) <- c("year", "fish.num", "sex", "mass", "age", "age.mat", "river", "mig.river", "num.f1", "mom.num", "dad.num")
      
      babies$year <- max(lake.salmon$year) +1 # add a year to most recent in lake.salmon
      babies$fish.num <-  #give fish unique numbers
        if(is.null(new_babies) == TRUE){ # if you're on the first set of fish
          c(max(lake.salmon$fish.num)+c(1:baby.num))} 
        else{ # derive numbers from lake.salmon
         c(max(new_babies$fish.num)+c(1:baby.num)) # otherwise derive numbers from new_babies
          }
      babies$sex <- factor(sample(x = c("M","F"), size = baby.num, replace = TRUE, prob = c(0.5, 0.5))) # sample sex on a 50:50 ratio
      babies$mass <- rnorm(nrow(babies), mean = 3, sd = 0.5) # give mass
      babies$age <- 0 #age = 0
      babies$age.mat <- 2 # age at maturity = 0 (make to toggle for later)
      ######----------------
      babies$river <- f.fish[n,]$mig.river # make home river the river where mom spawned
      babies$mig.river <- NA # to be populated later
      babies$num.f1 <- 0 # no reproduction yet
      babies$mom.num <- f.fish[n,]$fish.num # add mother's fish num for pedigree
      babies$dad.num <- m.fish[n,]$dad.num # add father's fish num for pedigree
      
      # append babies into new babies
      new_babies <- rbind(new_babies, babies)
      
      #record parent fecundity for future fitness calculations
      lake.salmon$num.f1[which(lake.salmon$fish.num == f.fish$fish.num[n])] <- baby.num[n] 
      lake.salmon$num.f1[which(lake.salmon$fish.num == m.fish$fish.num[n])] <- baby.num[n]
      }
  }
  lake.salmon <- rbind(lake.salmon, new_babies) # append new babies to lake.salmon
  
  return(lake.salmon)
}