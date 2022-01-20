#Set working directory, import packages, source functions, 
setwd(paste(directory,"/source/", sep = ''))    # set temp working directory 

#import packages
#library()

#source functions
source(paste(getwd(), "/SampleHabitat.R", sep = ''))
source(paste(getwd(), "/SampleHabitat3.R", sep = ''))
source(paste(getwd(), "/reproduction.R", sep = ''))
source(paste(getwd(), "/density_dependence.R", sep = ''))
source(paste(getwd(), "/emigrate.R", sep = ''))
source(paste(getwd(), "/mortality.R", sep = ''))
source(paste(getwd(), "/age_fish.R", sep = ''))
source(paste(getwd(), "/fitness.R", sep = ''))
source(paste(getwd(), "/fit_summ.R", sep = ''))

