SampleHabitat = function(n = 10, disp.dist = 1, disp.perc = 10, sizes = c(500,2000), size.distrib = c(0.7, 0.3)){
  #Generate randomly sampled habitat structure
  #Assume a ring structure so animals disperse evenly in either direction
  
  habitats.df <- data.frame(matrix(NA, nrow = n, ncol = n+2))
  
  habitats.df[,1] <- sprintf("River%i", c(1:n))
  colnames(habitats.df)[1] <- "name"
  
  habitats.df[,2] <- sample(x = sizes, size = n, prob = size.distrib, replace = TRUE)
  colnames(habitats.df)[2] <- "size"
  
  colnames(habitats.df)[3:ncol(habitats.df)] <- sprintf("p.toR%i",c(1:n))
  for(i in c(1:n)){
    ifelse(i == 1,
           habitats.df[i,c(3:ncol(habitats.df))] <- c(0.9, 0.05, rep(0, n-3), 0.05),
           habitats.df[i,c(3:ncol(habitats.df))] <- c(habitats.df[i-1,ncol(habitats.df)], habitats.df[i-1,c(3:(ncol(habitats.df)-1))]))
  }
  
  return(habitats.df)
}
