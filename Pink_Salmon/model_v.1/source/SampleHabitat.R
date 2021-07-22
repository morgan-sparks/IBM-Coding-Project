SampleHabitat = function(n = 11, disp.dist = 1, disp.perc = 10, sizes = c(500,2000), size.distrib = c(0.7, 0.3)){
  #Generate randomly sampled habitat structure
  #Assume a ring structure so animals disperse evenly in either direction
  
  habitats.df <- data.frame(matrix(NA, nrow = n, ncol = n+2))
  
  habitats.df[,1] <- sprintf("River%i", c(1:n))
  colnames(habitats.df)[1] <- "river"
  
  habitats.df[,2] <- sample(x = sizes, size = n, prob = size.distrib, replace = TRUE)
  colnames(habitats.df)[2] <- "size"
  
  #create a function that calculates the series if
  #1) chance of dispersing left or right is equal (1/2 total dispersal percentage)
  #2) relative decay of dispersal probability with distance from original river is 1/2
  #so, for dispersal distance of 1 you end up with 0.5 left, 0.5 right
  #for dispersal distance of 2 you end up with 1/3 l/r 1 river and 1/6 l/r 2 rivers
  prob.series <- function(n){
    d <- c(1:n)
    first.prob <- 2^(n-2)/(2^(n)-1)
    s <- first.prob / (2^(d-1))
    return(s)
  }
  
  #set up the dispersal probability series for the first habitat (introduction point)
  #create the general series for dispersal based on the parameters provided in the function call
  prob.of.dispersal <- c((1-disp.perc/100), disp.perc/100*prob.series(disp.dist), rep(0,(n-2*disp.dist-1)), rev(disp.perc/100*prob.series(disp.dist)))
  
  colnames(habitats.df)[3:ncol(habitats.df)] <- sprintf("p.toR%i",c(1:n))
  for(i in c(1:n)){
    ifelse(i == 1,
           habitats.df[i,c(3:ncol(habitats.df))] <- prob.of.dispersal,
           habitats.df[i,c(3:ncol(habitats.df))] <- c(habitats.df[i-1,ncol(habitats.df)], habitats.df[i-1,c(3:(ncol(habitats.df)-1))]))
  }
  
  return(habitats.df)
}
