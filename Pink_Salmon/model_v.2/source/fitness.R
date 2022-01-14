fitness <- function(census, sim.years){
  years <- c(1:(sim.years-3))
  
  for(y in years){
    census_parents <- census[[y]][which(census[[y]][,"age.mat"] == census[[y]][,"age"]),]
    #if you want to filter out only the individuals that reproduced can use line below
    #does not include fish with num.f1 = NA in further analysis
    #census_parents <- census[[y]][which(census[[y]][,"age.mat"] == census[[y]][,"age"] & is.na(census[[y]][,"num.f1"]) == 'FALSE'),]
    
    if(nrow(census_parents) > 0){
      for(p in census_parents$fish.num){
        #pull out parents that have num.f1 > 0
        p.row <- which(census_parents$fish.num == p)
        if(is.na(census_parents$num.f1[p.row])){
          #if they did not reproduce set fitness to zero
          census[[y]][which(census[[y]][,"fish.num"]==p),"ind.fitness"] <- 0
        } else{
          #grab all the census information needed to figure out what happened with the juveniles
          census_juveniles <- data.frame(rbind(census[[y+1]][which(census[[y+1]][,"mom.num"]==p | census[[y+1]][,"dad.num"]==p),], 
                                               census[[y+2]][which(census[[y+2]][,"mom.num"]==p | census[[y+2]][,"dad.num"]==p),], 
                                               census[[y+3]][which(census[[y+3]][,"mom.num"]==p | census[[y+3]][,"dad.num"]==p),]))
          
          #the number of unique juveniles should match the num.f1 for the parents
          if(census_parents$num.f1[p.row] != length(unique(census_juveniles$fish.num))){
            #we have a problem
            print(sprintf("***WARNING*** Cannot locate all of the offspring for fish.num %i", p))
          } else{
            census[[y]][which(census[[y]][,"fish.num"]==p),"ind.fitness"] <- nrow(census_juveniles[which(is.na(census_juveniles$num.f1) == 'FALSE'),])
          }
        }
      } #end loop through parents
    } #end if statement checking whether there were parents found this year
    #print(y)
  } #end loop through years
  return(census)
} #end fitness function