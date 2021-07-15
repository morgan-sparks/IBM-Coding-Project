fitness <- function(census, years){
  years <- c(1:years)
  
  for( y in years){
    census_parents <- data.frame(census[[i]])
    census_juveniles <- data.frame(rbind(census[[i+1]], census[[i+2]], census[[i+3]]))
    
    
    for(p in census_parents$fish.num){
      #pull out parents that have num.f1 > 0
      parent.row <- which(census_parents$fish.num == p)
      
      if(is.na(census_parents$num.f1[parent.row]) == FALSE){
        #grab the babies, make sure the number of babies found makes sense
        
        #count babies that survived to reproduce
        #basically babies that have num.f1 > 0
      }
      
      #create new column when setting up lake population data structure for babies that survived and reproduced
      #call it 'fish.fitness'
      
      #want every parent (male and female) to have a value
      
    }
  }
}