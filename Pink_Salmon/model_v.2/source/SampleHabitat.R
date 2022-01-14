SampleHabitat = function(n = 11, disp.dist = 1, disp.perc = 10, sizes = c(500,2000), pattern = 1, size.distrib = c(0.7, 0.3)){
  #Generate habitat structure
  #Assume a ring structure so animals disperse evenly in either direction
  #pattern is a toggle for different habitat structure generation
  #1 random sampling according to the sizes and distribution given
  #2 repeating pattern of small, small, big (500, 500, 2000)
  #3 bigs all at the start, with the rest all small
  #4 small all at the start, with big at the far side of the ring
  #note that 2-4 will only work with 2 sizes, must be entered c(small, big) and distrib to match
  
  habitats.df <- data.frame(matrix(NA, nrow = n, ncol = n+2))
  
  habitats.df[,1] <- sprintf("River%i", c(1:n))
  colnames(habitats.df)[1] <- "river"
  
  #fill the size column depending on the pattern selected
  colnames(habitats.df)[2] <- "size"
  num.big <- floor(n*size.distrib[2])
  patt.reps <- ceiling(n/3)
  ifelse(pattern == 1,
         habitats.df[,2] <- sample(x = sizes, size = n, prob = size.distrib, replace = TRUE),
         ifelse(pattern == 2,
                habitats.df[,2] <- c(rep(c(sizes[1],sizes[1],sizes[2]), ceiling(patt.reps/2))[c(1:ceiling(n/2))], rev(rep(c(sizes[1],sizes[1],sizes[2]), ceiling(patt.reps/2))[c(2:(n-ceiling(n/2)+1))])),
                ifelse(pattern == 3,
                       habitats.df[,2] <- c(rep(sizes[2],ceiling(num.big/2)), rep(sizes[1], n-num.big), rep(sizes[2],floor(num.big/2))),
                       habitats.df[,2] <- c(rep(sizes[1], ceiling((n-num.big)/2)), rep(sizes[2], num.big), rep(sizes[1], floor((n-num.big)/2)))
                )))
  
  
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
