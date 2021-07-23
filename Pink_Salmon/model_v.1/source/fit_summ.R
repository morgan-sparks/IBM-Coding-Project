###############
#### NOTE ####
#This function is only complete for 2 yr olds

fit_summ <- function(census, fixed.maturity){
  
if(fixed.maturity == TRUE){
  
  fitness_summary <- data.frame(matrix(NA, 0, 7))
  colnames(fitness_summary) <- c("year", "two_yrs_fit", "two_yrs_home_fit", "two_yrs_stray_fit", "two_yrs_RRS", "two_yrs_home_RRS", "two_yrs_stray_RRS")
  
  for(y in seq(from =2, to = length(census), by =2)){
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
    
    
    year_sum <-cbind(year,two_yrs_fit, two_yrs_home_fit, two_yrs_stray_fit, two_yrs_RRS, two_yrs_home_RRS, two_yrs_stray_RRS )
    
    fitness_summary <- rbind(fitness_summary, year_sum)
    }
  }
return(fitness_summary)
}