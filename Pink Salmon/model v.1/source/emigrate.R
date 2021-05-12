emigrate <-  function(lake.salmon , habitat){
 fish.to.mat <- lake.salmon[which(lake.salmon$age.mat-lake.salmon$age == 1),] # select only fish that are going to mature and return this year
 
 # for loop to iterate over individuals and draw if they will migrate
 for (r in 1:nrow(fish.to.mat)){
  fish.to.mat[r,"migrant"] <- sample(x = as.factor(habitat$river), #sample vector of river names from habitat
                                   size = 1, #only sample once
                                   replace = TRUE, #replace
                                   prob = habitat[which(habitat$river == fish.to.mat[r,"river"]), 3:5])
                                   # above, select the row where the home river in fish.to.mat matches the row in habitat 
                                   # and select columns 3:5, which are the probabilities of straying to other rivers.
 }
 # change migrant in lak
 lake.salmon[which(lake.salmon$fish.num == fish.to.mat$fish.num), "migrant"] <- fish.to.mat$migrant
 return(lake.salmon)
}


