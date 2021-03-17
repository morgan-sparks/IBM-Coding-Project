inds           <- array(data = 0, dim = c(100, 5));
colnames(inds) <- c("ID","repr", "death","x","y");
#rownames(inds) <- seq(1,100,by=1);
inds[,1]       <- seq(1,100,by=1)
inds
#inds[,1]       <- rnorm(n = dim(inds)[1], mean = 23, sd = 3);
inds[,4]       <- sample(x = 5:30, size = dim(inds)[1], replace = TRUE);
inds[,5]       <- sample(x = 5:30, size = dim(inds)[1], replace = TRUE);
inds


seq(start_ID,finish_ID,by=1)

total_inds       <- dim(inds)[1]; # Get the number of individuals in inds
total_inds
ind_cols         <- dim(inds)[2]; # Total inds columns
ind_cols
inds[, 2] <- rpois(n = total_inds, lambda = 1.2); 
inds
total_off        <- sum(inds[, 2]);
total_off
# ---- We now have the total number of new offspring; now add to inds
new_inds     <- array(data = 0, dim = c(total_off, ind_cols));
start_ID<-total_inds+1
finish_ID<-total_inds+total_off
new_inds[,1] <- seq(start_ID,finish_ID, by=1)
new_inds
#new_inds[,1] <- rnorm(n = dim(new_inds)[1], mean = 23, sd = 3);
new_inds[,4] <- sample(x = 5:30, size = dim(new_inds)[1], replace = TRUE);
new_inds[,5] <- sample(x = 5:30, size = dim(new_inds)[1], replace = TRUE);
# ---- Our new offspring can now be attached in the inds array

inds <- rbind(inds, new_inds);
inds
return(inds);

#Birth Function
birth <- function(inds, lambda = 1.2, repr = 2){
  total_inds       <- dim(inds)[1]; # Get the number of individuals in inds
  ind_cols         <- dim(inds)[2]; # Total inds columns
  inds[, 2] <- rpois(n = total_inds, lambda = 1.2); 
  total_off        <- sum(inds[, 2]);
  # ---- We now have the total number of new offspring; now add to inds
  new_inds     <- array(data = 0, dim = c(total_off, ind_cols));
  start_ID<-total_inds+1
  finish_ID<-total_inds+total_off
  new_inds[,1] <- as.numeric(seq(start_ID,finish_ID, by=1))
  #new_inds[,1] <- rnorm(n = dim(new_inds)[1], mean = 23, sd = 3);
  new_inds[,4] <- sample(x = 5:15, size = dim(new_inds)[1], replace = TRUE);
  new_inds[,5] <- sample(x = 5:15, size = dim(new_inds)[1], replace = TRUE);
  # ---- Our new offspring can now be attached in the inds array
  
  inds <- rbind(inds, new_inds);
  inds
  return(inds);
}


#Death Function
death<- function(inds, mortality.rate=0.15){
  n.remove<-mortality.rate * length(inds[, 1])
  inds <- inds[-sample(1:length(inds[, 1]), n.remove, replace = FALSE), ]
}

movement <- function(inds, xloc = 4, yloc = 5){
  total_inds   <- dim(inds)[1]; # Get the number of individuals in inds
  move_dists   <- c(-1, 0, 1);  # Define the possible distances to move
  x_move       <- sample(x = move_dists, size = total_inds, replace = TRUE);
  y_move       <- sample(x = move_dists, size = total_inds, replace = TRUE);
  inds[, xloc] <- inds[, xloc] + x_move;
  inds[, yloc] <- inds[, yloc] + y_move;
  # =========   The reflecting boundary is added below
  # =========  Now all individuals should stay on the landscape
  return(inds);
}



#Simulation 
inds
ts         <- 0;
time_steps <- 10;
inds_hist  <- NULL;

while(ts < time_steps){
  #Birth
  inds <- birth(inds)
  #Death
  inds <- death(inds = inds)
  #Movement
  inds<-movement(inds)
  
  k.remove <- length(inds[, 1]) - K
  if(k.remove > 0){
    inds <- inds[-sample(1:length(inds[, 1]), k.remove, replace = FALSE), ]
  }
  
  ts              <- ts + 1; 
  inds_hist[[ts]] <- inds;
}

inds_hist[[10]]

ind_abund <- array(data = NA, dim = c(10, 2));
for(i in 1:10){
  ind_abund[i, 1] <- i;                      # Save the time step
  ind_abund[i, 2] <- dim(inds_hist[[i]])[1]; # rows in inds_hist[[i]]
}
colnames(ind_abund) <- c("time_step", "abundance");
print(ind_abund);


remove_x_20<-dim(inds[which(inds[,4]>=30),])[1]

if(remove_x_20 >= 1){
  inds <- inds[-sample(inds[which(inds[,4]>=30),],round(remove_x_20*.1), replace = FALSE), ]
}

else{inds}

remove_x_0<-dim(inds[which(inds[,4]<=5),])[1]

if(remove_x_0 >= 1){
  inds <- inds[-sample(inds[which(inds[,4]<=5),],round(remove_x_0*.1), replace = FALSE), ]
}

else{inds}

remove_y_20<-dim(inds[which(inds[,5]>=30),])[1]

if(remove_y_20 >= 1){
  inds <- inds[-sample(inds[which(inds[,5]>=30),],round(remove_y_20*.1), replace = FALSE), ]
}

else{inds}

remove_y_0<-dim(inds[which(inds[,5]<=5),])[1]

if(remove_y_0 >= 1){
  inds <- inds[-sample(inds[which(inds[,5]<=5),],round(remove_y_0*.1), replace = FALSE), ]
}

else{inds}

#Carrying Capacity
k.remove <- length(inds[, 1]) - K
if(k.remove > 0){
  inds <- inds[-sample(1:length(inds[, 1]), k.remove, replace = FALSE), ]
}

else{inds}


#Emmigration (if you go past the boundry you get penalized ( you potentially don't come back))
#remove_x_0<-round(dim(inds[which(inds[,4]<=0),])[1]*.1)
#inds<-inds[-sample(inds[which(inds[,4]==0),], remove_x_0, replace = FALSE), ]

#if(remove_x_0 >= 1){
#  inds <- inds[-sample(inds[which(inds[,4]<=0),], remove_x_0, replace = FALSE), ]
#}


inds           <- array(data = 0, dim = c(100, 5));
colnames(inds) <- c("ID","repr", "death","x","y");
rownames(inds) <- seq(1,100,by=1);
inds[,1]       <- seq(1,100,by=1)
inds
#inds[,1]       <- rnorm(n = dim(inds)[1], mean = 23, sd = 3);
inds[,4]       <- sample(x = 5:15, size = dim(inds)[1], replace = TRUE);
inds[,5]       <- sample(x = 5:15, size = dim(inds)[1], replace = TRUE);
inds

length(inds[,1])



length(inds[,1])

#Emmigration (if you go past the boundry you get penalized ( you potentially don't come back))
#remove_y_0<-round(dim(inds[which(inds[,5]<=0),])[1]*.1)
#inds<-inds[-sample(inds[which(inds[,4]==0),], remove_x_0, replace = FALSE), ]

#if(remove_y_0 >= 1){
# inds <- inds[-sample(inds[which(inds[,5]<=0),], remove_x_0, replace = FALSE), ]
#}

remove_y_20<-round(dim(inds[which(inds[,5]>=20),])[1]*.1)
#inds<-inds[-sample(inds[which(inds[,4]==0),], remove_x_0, replace = FALSE), ]

if(remove_y_20 >= 1){
  inds <- inds[-sample(inds[which(inds[,5]>=20),], remove_x_20, replace = FALSE), ]
}


