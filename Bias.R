data<-read.csv(file=url("http://www1.appstate.edu/~thomleyje/data-files/BookPrices.csv"))

HSP <- subset(BookPrices, Area=="Math & Science", select=Price, drop=T)
SSP <- subset(BookPrices, Area=="Social Sciences", select=Price, drop=T)
obs.stat <- mean(HSP)/mean(SSP)

N <- 10^5
my.boot <- numeric(N)
for (i in 1:N)
{
  xhs <- sample(HSP, 27, replace=TRUE) 
  xss <- sample(SSP, 17, replace=TRUE) 
  my.boot[i] <- mean(xhs)/mean(xss)
}

hist(my.boot)
abline(v=c(obs.stat,mean(my.boot)),col=c("red","blue"))
boxplot(my.boot, horizontal=T)
mean(my.boot)
sd(my.boot)

bias <- mean(my.boot) - obs.stat   # computes estimated bias of the estimator
bias                                
rel.bias <- (bias/sd(my.boot))*100  # computes est. bias as a percentage of SE
rel.bias
