#--------------Bootstraping---------------------
listofdfs <- list()


for (i in c(7:13)){
  alg.data %>% 
    filter(alg.data[,i]>=1)%>% 
    mutate(Topic = colnames(alg.data)[i])%>% 
    mutate(Neutral = VADERclass=="Neutral")%>% 
    mutate(Positive = VADERclass=="Positive")%>% 
    mutate(Negative = VADERclass=="Negative")%>% 
    mutate(Aversive = VADERclass=="Aversive")%>% 
    group_by( VADERclass, Topic) ->listofdfs[[i]]
}
bootstrap<- bind_rows(listofdfs)
bootstrap %>% select(Year,Topic,VADER,VADERclass,Neutral,Positive,Negative,Aversive) -> bootstrap
bootstrap 


library(boot)

ObservedHeights <- DataFrame$height

bootstrap  -> ObservedHeight
ObservedHeights <- ObservedHeight$VADER
  
mean(ObservedHeights)
ReturnMean <- function(datav, sampleindices) 
  
{
  d <- datav[sampleindices] # we will use this for bootstrapping
  return( mean(d) )
}

## Bootstrapping with 10000 replications
results <- boot(data=as.vector(ObservedHeights), statistic=ReturnMean, R=10000)


hist(results$t)






# Bootstrap 95% CI for R-Squared

# function to obtain R-Squared from the data 
rsq <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample 
  fit <- lm(formula, data=d)
  return(summary(fit)$r.square)
} 

results <- boot(data=bootstrap, statistic=rsq, 
                R=1000, formula='VADER')

# view results
results 
plot(results)

# get 95% confidence interval 
boot.ci(results, type="bca")



# Author DataFlair

# Creating Function to obtain R-Squared from the data
r_squared <- function(formula, data, indices) {
  val <- data[indices,4] # selecting sample with boot 
  fit <- lm(formula, data=val)
  return(summary(fit)$r.square)
} 
# Performing 1500 replications with boot 
output <- boot(data=bootstrap, statistic=r_squared, 
               R=1500)

# Plotting the output
output 
plot(output)

# Obtaining a confidence interval of 95%
boot.ci(output, type="bca")
