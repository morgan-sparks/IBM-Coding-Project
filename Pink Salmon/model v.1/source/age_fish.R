age_fish <- function(lake.salmon){
  
  #select fish that whose age is less than age at maturity and then add a year only to those fish
  
  lake.salmon[which(lake.salmon$age < lake.salmon$age.mat), "age"] <- lake.salmon[which(lake.salmon$age < lake.salmon$age.mat), "age"] + 1
  return(lake.salmon)
  }