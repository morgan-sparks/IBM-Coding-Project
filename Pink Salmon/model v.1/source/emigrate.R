emigrate <-  function(lake.salmon , habitat){
 fish.to.mat <- lake.salmon[which(lake.salmon$age.mat-lake.salmon$age == 1),] # select only fish that are going to mature and return this year
 
 # for loop to iterate over individuals and draw if they will migrate
 if(nrow(fish.to.mat) > 0){
   for (r in 1:nrow(fish.to.mat)){
    fish.to.mat[r,"mig.river"] <- sample(x = as.character(habitat$river), #sample vector of river names from habitat
                                     size = 1, #only sample once
                                     replace = TRUE, #replace
                                     prob = habitat[which(habitat$river == fish.to.mat[r,"river"]), 3:ncol(habitat)])
                                     # above, select the row where the home river in fish.to.mat matches the row in habitat 
                                     # and select columns 3:5, which are the probabilities of straying to other rivers.
    # change mig.river in lake by selecting fish # in lake.salmon that match those you changed
    lake.salmon[which(lake.salmon$fish.num == fish.to.mat$fish.num[r]), "mig.river"] <- fish.to.mat[r, "mig.river"]
    }
 }
 return(lake.salmon)
}


