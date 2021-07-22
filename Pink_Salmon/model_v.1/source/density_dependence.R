density_dependence <-  function(lake.salmon , habitat){
  spawning_fish <- lake.salmon[which(lake.salmon$age.mat-lake.salmon$age == 0),] #select only fish that will be in rivers
  
  if(is.null(spawning_fish) == FALSE){
    fish_to_kill <- NULL # set up a blank object to append fish to kill
    
    #select a river from habitat and subset that river
    for (r in levels(habitat$river)){
      population <- spawning_fish[which(spawning_fish$mig.river == r),] # subset population of fish returning to river r
      if(nrow(population) > 0){
        pop.k <- habitat[which(habitat$river == r), "size"] # grab carrying capacity value from habitat df
        pop.size <- nrow(population) # how many fish are in population
        
        # if there are more fish than carrying capacity (size), kill excess
        if(pop.size > pop.k){
          kill.num <- pop.size - pop.k # number to kill (pop size - k)
          
          inds_to_kill <- sample(population$fish.num, size = kill.num, replace = FALSE) # 
          
          fish_to_kill <- c(fish_to_kill,inds_to_kill) # put all inds to kill in fish to kill
          
        }
      }
    }
    
    # kill fish that were in excess of k
    lake.salmon$died[which(lake.salmon$fish.num %in% fish_to_kill)] <- 1
    return(lake.salmon)
  }
}
