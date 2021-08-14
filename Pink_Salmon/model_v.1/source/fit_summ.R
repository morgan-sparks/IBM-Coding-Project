###############
#### NOTE ####
#This function is only complete for 2 yr olds

fit_summ <- function(census, fixed.maturity, run){
  
if(fixed.maturity == TRUE){
  
  fitness_summary <- data.frame(matrix(NA, 0, 8))
  colnames(fitness_summary) <- c("run", "year", 
                                 "two_yrs_fit", "two_yrs_home_fit", "two_yrs_stray_fit", 
                                 "two_yrs_RRS", "two_yrs_home_RRS", "two_yrs_stray_RRS")
  
  for(y in seq(from =2, to = length(census)-3, by =2)){ #leave off final 3 yrs because can't compute fitness
    census_year <- census[[y]]
    year <- y
    year_sum <- NULL
    ### select individuals on conditional statements and then sample and take the mean their ind.fitness column
    
    two_yrs_fit <- mean(census_year[which(census_year$age==2 & #select 2 yr olds
                                          census_year$age == census_year$age.mat), "ind.fitness" ])
   
    two_yrs_RRS <- 1 ### all RRS will be compared to this subset thus 1
    
    two_yrs_home_fit <- mean(census_year[which(census_year$age==2 & #select 2 yr olds
                                                census_year$age == census_year$age.mat & #only those that matured that year
                                                census_year$river == census_year$mig.river), "ind.fitness" ])
    
    two_yrs_home_RRS <-  two_yrs_home_fit/two_yrs_fit # make fitness relative to 2 yr olds as a group
    
    
    two_yrs_stray_fit <- mean(census_year[which(census_year$age==2 & #select 2 yr olds
                                                census_year$age == census_year$age.mat & #only those that matured that year
                                                census_year$river != census_year$mig.river), "ind.fitness" ])
    
    two_yrs_stray_RRS <-  two_yrs_stray_fit/two_yrs_fit # make fitness relative to 2 yr olds as a group
    
    
    year_sum <-cbind(run, year,two_yrs_fit, two_yrs_home_fit, two_yrs_stray_fit, two_yrs_RRS, two_yrs_home_RRS, two_yrs_stray_RRS )
    
    fitness_summary <- rbind(fitness_summary, year_sum)
    }
  }else
    
  fitness_summary <- data.frame(matrix(NA, 0, 14))
  colnames(fitness_summary) <- c("run", "year", 
                                 "two_yrs_fit", "two_yrs_home_fit", "two_yrs_stray_fit", 
                                 "two_yrs_RRS", "two_yrs_home_RRS", "two_yrs_stray_RRS",
                                 "three_yrs_fit", "three_yrs_home_fit", "three_yrs_stray_fit", 
                                 "three_yrs_RRS", "three_yrs_home_RRS", "three_yrs_stray_RRS")
  
  for(y in seq(from =2, to = length(census)-3, by =1)){ #leave off final 3 yrs because can't compute fitness
    census_year <- census[[y]]
    year <- y
    year_sum <- NULL
    ### select individuals on conditional statements and then sample and take the mean their ind.fitness column
    
    #### 2-yr olds ####
    two_yrs_fit <- mean(census_year[which(census_year$age==2 & #select 2 yr olds
                                            census_year$age == census_year$age.mat), "ind.fitness" ])
    
    two_yrs_RRS <- 1 ### all RRS will be compared to this subset thus 1
    
    two_yrs_home_fit <- mean(census_year[which(census_year$age==2 & #select 2 yr olds
                                                 census_year$age == census_year$age.mat & #only those that matured that year
                                                 census_year$river == census_year$mig.river), "ind.fitness" ])
    
    two_yrs_home_RRS <-  two_yrs_home_fit/two_yrs_fit # make fitness relative to 2 yr olds as a group
    
    
    two_yrs_stray_fit <- mean(census_year[which(census_year$age==2 & #select 2 yr olds
                                                  census_year$age == census_year$age.mat & #only those that matured that year
                                                  census_year$river != census_year$mig.river), "ind.fitness" ])
    
    two_yrs_stray_RRS <-  two_yrs_stray_fit/two_yrs_fit # make fitness relative to 2 yr olds as a group
   
    #### 3-yr olds ####
    three_yrs_fit <- mean(census_year[which(census_year$age==3 & #select 2 yr olds
                                            census_year$age == census_year$age.mat), "ind.fitness" ])
    
    three_yrs_RRS <- three_yrs_fit/two_yrs_fit
    
    
    three_yrs_home_fit <- mean(census_year[which(census_year$age==3 & #select 2 yr olds
                                                 census_year$age == census_year$age.mat & #only those that matured that year
                                                 census_year$river == census_year$mig.river), "ind.fitness" ])
    
    three_yrs_home_RRS <-  three_yrs_home_fit/two_yrs_fit # make fitness relative to 2 yr olds as a group
    
    
    three_yrs_stray_fit <- mean(census_year[which(census_year$age==3 & #select 2 yr olds
                                                  census_year$age == census_year$age.mat & #only those that matured that year
                                                  census_year$river != census_year$mig.river), "ind.fitness" ])
    
    three_yrs_stray_RRS <-  three_yrs_stray_fit/two_yrs_fit # make fitness relative to 2 yr olds as a group
    
    
    year_sum <-cbind(run, year,
                     two_yrs_fit, two_yrs_home_fit, two_yrs_stray_fit, 
                     two_yrs_RRS, two_yrs_home_RRS, two_yrs_stray_RRS,
                     three_yrs_fit, three_yrs_home_fit, three_yrs_stray_fit, 
                     three_yrs_RRS, three_yrs_home_RRS, three_yrs_stray_RRS)
    
    fitness_summary <- rbind(fitness_summary, year_sum)
  }
return(fitness_summary)
}