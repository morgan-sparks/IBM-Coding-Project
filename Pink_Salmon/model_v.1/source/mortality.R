#Mortality Function
mortality <-  function(lake.salmon){
  lake.salmon<-lake.salmon[lake.salmon$died !=1,] #Fish with a 1 in the "died" column of the lake.salmon data frame are removed
  return(lake.salmon)
}
#Test data
x=round(rnorm(10,5,2))
y=round(rnorm(10,.5,.5))
df<-data.frame(cbind(x,y))
colnames(df)<-c("Fish","died")
#Test
mortality(df)