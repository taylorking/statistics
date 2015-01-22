##Chapter 4: R Scripts for The Bootstrap
#
#Example 5.2

my.sample <- rgamma(16, 1, 1/2)

B <- 10^5
my.boot <- numeric(B)
for (i in 1:B)
 {
  x <- sample(my.sample, 16, replace=TRUE)  #draw resample 
  my.boot[i] <- mean(x)                     #compute mean, store in my.boot
  }

 hist(my.boot)  #bootstrap distribution of mean
 mean(my.boot)  #mean
 sd(my.boot)    #bootstrap SE

#Example 5.3
##Arsenic in wells in Bangladesh
Arsenic <- Bangladesh$Arsenic
#Alternatively,
#Arsenic <- subset(Bangladesh, select=Arsenic,drop=T)

hist(Arsenic)
dev.new()
qqnorm(Arsenic)
qqline(Arsenic)

n<-length(Arsenic)
B<-10^4
arsenic.mean<-numeric(B)

for (i in 1:B)
{
   x <- sample(Arsenic, n, replace = TRUE)
   arsenic.mean[i]<-mean(x)
}

hist(arsenic.mean, main ="Bootstrap distribution of means") 
abline(v=mean(arsenic.mean),col="red")

dev.new()
qqnorm(arsenic.mean)
qqline(arsenic.mean)

mean(arsenic.mean)               #bootstrap mean
mean(arsenic.mean)-mean(Arsenic) #bias
sd(arsenic.mean)                 #bootstrap SE

sum(arsenic.mean > 161.3224)/B 
sum(arsenic.mean < 89.75262)/B

##------------------------------------------------------------------
#Example 5.4 TV example

#Import the TV data
TV <- read.csv("TV.csv")

times.Basic <- subset(TV, select=times, cable=="Basic", drop=T)
times.Ext   <- subset(TV, select=times, cable=="Extended", drop=T)

n.Basic <- length(times.Basic)
n.Ext <- length(times.Ext)
B <- 10^4

times.diff.mean <- numeric(B)

for (i in 1:B)
{
  Basic.boot <- sample(times.Basic, n.Basic, replace=TRUE)
  Ext.boot <- sample(times.Ext, n.Ext, replace=TRUE)
  times.diff.mean[i] <- mean(Basic.boot)-mean(Ext.boot)
}

hist(times.diff.mean, main="Bootstrap distribution of difference in means",
     xlab="Means")
abline(v = mean(times.diff.mean), col = "red")
abline(v = mean(times.Basic) - mean(times.Ext), col = "blue")

dev.new()
qqnorm(times.diff.mean)
qqline(times.diff.mean)

mean(times.Basic)-mean(times.Ext)
mean(times.diff.mean)
mean(times.diff.mean)-(mean(times.Basic)-mean(times.Ext))  #bias

sd(times.diff.mean)

quantile(times.diff.mean,c(0.025,0.975))

#-------
#Permutation test for TV means
observed <- mean(times.Basic)-mean(times.Ext)
B <- 9999  #set number of times to repeat this process

#set.seed(99)
result <- numeric(B) # space to save the random differences
for(i in 1:B)
  {
  index <- sample(20, size=10, replace = FALSE) #sample of numbers from 1:20
  result[i] <- mean(times[index]) - mean(times[-index])
}
(sum(result >= observed)+1)/(B + 1)  #P-value


hist(result, xlab = "xbar1 - xbar2", 
main="Permutation Distribution for TV times")
#lines(density(result))
abline(v = observed, col = "red")
windows()
qqnorm(result)
qqline(result)

##-------------------------------------------------------------
##Example 5.5 Verizon 

#Verizon <- read.csv("Verizon.csv")
Time.ILEC <- subset(Verizon, select=Time,Group=="ILEC", drop=T)
Time.CLEC <- subset(Verizon, select=Time,Group=="CLEC", drop=T)

n.ILEC <- length(Time.ILEC)
n.CLEC <- length(Time.CLEC)
B<-10^4

time.diff.boot <- numeric(B)
Time.ILEC.boot <- numeric(B)
Time.CLEC.boot <- numeric(B)
#set.seed(100)
for (i in 1:B)
 {
  ILEC.sample <- sample(Time.ILEC, n.ILEC, replace = TRUE)
  CLEC.sample <- sample(Time.CLEC, n.CLEC, replace = TRUE)
  Time.ILEC.boot[i] <- mean(ILEC.sample)
  Time.CLEC.boot[i] <- mean(CLEC.sample)
  time.diff.boot[i] <- mean(ILEC.sample)-mean(CLEC.sample)
}

hist(Time.ILEC.boot, main = "Bootstrap distribution of ILEC means",
     xlab="means")
abline(v = mean(Time.ILEC.boot), col = "red")
abline(v = mean(Time.ILEC), col = "blue")

dev.new()
qqnorm(Time.ILEC.boot)
qqline(Time.ILEC.boot)

dev.new()
hist(Time.CLEC.boot, main = "Bootstrap distribution of CLEC means",
    xlab = "means")
abline(v = mean(Time.CLEC.boot), col = "red")
abline(v = mean(Time.CLEC), col = "blue")

dev.new()
qqnorm(Time.CLEC.boot)
qqline(Time.CLEC.boot)

dev.new()
hist(time.diff.boot, main = "Bootstrap distribution of difference in means")
abline(v = mean(time.diff.boot), col = "red")
abline(v = mean(Time.ILEC)-mean(Time.CLEC), col = "blue")

dev.new()
qqnorm(time.diff.boot)
qqline(time.diff.boot)

#statistics
mean(time.diff.boot)
quantile(time.diff.boot,c(0.025,0.975))

###----------------------------------------------------
##Page 97 Camera prices
#Cameras <- read.csv("Cameras.csv")

JR.prices<-Cameras$JR
BH.prices<-Cameras$BH
#Alternatively:
#JR.prices<-subset(Cameras, select=JR, drop=T)
#BH.prices<-subset(Cameras, select=BH, drop=T)

#Distribution of difference in prices
hist(JR.prices-BH.prices, xlab = "Price", main="Distribution of price differences",
    cex=.7)

priceDiff <- JR.prices-BH.prices
n <- length(priceDiff)
B <- 10^5
priceBoot <- numeric(B)
#set.seed(0)
for (i in 1:B)
{
  priceSample <- sample(priceDiff, n, replace = TRUE)         
  priceBoot[i] <- mean(priceSample)
}

mean(priceBoot)
quantile(priceBoot, c(0.025,0.975))     

hist(priceBoot, main = "Bootstrap distribution of prices differences",cex=.7,
     xlab="Dollars")
abline(v = mean(priceDiff), lty=2, col="red")

##-----------------------------------------------
##Section 5.5 Verizon trimmed means

#
Verizon <- read.csv("Verizon.csv")
Time.ILEC <- subset(Verizon, select=Time,Group=="ILEC", drop=T)
Time.CLEC <- subset(Verizon, select=Time,Group=="CLEC", drop=T)

n.ILEC <- length(Time.ILEC)
n.CLEC <- length(Time.CLEC)

B<-10^4
time.diff.boot<-numeric(B)

#set.seed(100)
for (i in 1:B)
{
  x.ILEC <- sample(Time.ILEC, n.ILEC, replace = TRUE)
  x.CLEC <- sample(Time.CLEC, n.CLEC, replace = TRUE)
  time.diff.boot[i] <- mean(x.ILEC, trim = .25) - mean(x.CLEC, trim = .25)
}

hist(time.diff.boot, main = "Bootstrap distribution of difference in trimmed means",
    xlab = "difference in trimmed means")
abline(v = mean(time.diff.boot),col = "red")
abline(v = mean(Time.ILEC,trim = .25) - mean(Time.CLEC, trim = .25), 
     col = "blue")

dev.new()
qqnorm(time.diff.boot)
qqline(time.diff.boot)

mean(time.diff.boot)
quantile(time.diff.boot, c(0.025,0.975))
##-----------------------------------------------------------------
#Example 5.6 Verizon ratio of means

#Time.ILEC and Time.CLEC created above
#n.ILEC, n.CLEC created above

B <- 10^4
time.ratio.boot <- numeric(B)

set.seed(100)
for (i in 1:B)
{
  ILEC.sample <- sample(Time.ILEC, n.ILEC, replace=TRUE)
  CLEC.sample <- sample(Time.CLEC, n.CLEC, replace=TRUE)
  time.ratio.boot[i] <- mean(ILEC.sample)/mean(CLEC.sample)
}


hist(time.ratio.boot, main = "Bootstrap distribution of ratio of means",
     xlab = "ratio of means")
abline(v = mean(time.ratio.boot), col = "red")
abline(v = mean(Time.ILEC)/mean(Time.CLEC), col = "blue")

dev.new()
qqnorm(time.ratio.boot)
qqline(time.ratio.boot)

mean(time.ratio.boot)
sd(time.ratio.boot)
quantile(time.ratio.boot,c(0.025,0.975))

###--------------------------------------------------------------
#Example 5.7 Relative risk example

highbp <- rep(c(1,0),c(55,3283))   #high blood pressure
lowbp <- rep(c(1,0),c(21,2655))    #low blood pressure

B <- 10^4
boot.rr <- numeric(B)
high.prop <- numeric(B)
low.prop <- numeric(B)

for (i in 1:B)
{
   x.high <- sample(highbp,3338,replace=T)
   x.low  <- sample(lowbp,2676,replace=T)
   high.prop[i] <- sum(x.high)/3338
   low.prop[i]  <- sum(x.low)/2676
   boot.rr[i] <- high.prop[i]/low.prop[i]
}

ci <- quantile(boot.rr,c(0.025,.975))

hist(boot.rr, main = "Bootstrap distribution of relative risk",
    xlab = "relative risk")
abline(v = mean(boot.rr), col="red")
abline(v = 2.12, col="blue")
legend(locator(1), legend = c("Observed RR","Mean of bootstrap dist."),
 col=c("blue","red"), lty = 1)

dev.new()
plot(range(low.prop),range(high.prop), 
    xlab = "proportion in low blood pressure group",
    ylab = "proportion in high blood pressure group",
    type = "n", xlim=c(0,.02), ylim=c(0,.03))

temp <- ifelse(high.prop < 1.31775*low.prop,1,0)

 points(low.prop[temp==1],high.prop[temp==1],pch=3,col="green")

temp2 <- ifelse(high.prop > 3.687*low.prop,1,0)
points(low.prop[temp2==1],high.prop[temp2==1],pch=3,col="green")

temp3 <- temp + temp2
 points(low.prop[temp3==0],high.prop[temp3==0],pch=16,col="blue")

abline(v = mean(low.prop), col = "red", lwd = 2)
abline(h = mean(high.prop),col = "red", lwd = 2)
abline(0,2.12, lty = 2, lwd = 2)
abline(0,ci[1],col = "blue", lwd = 2)
abline(0,ci[2], col = "blue", lwd = 2)
legend(locator(1),legend=c("Sample relative risk","bootstrap CI"),lty=c(2,1),
      lwd = c(2,2), col = c("black","blue"))
title("Scatter plot of bootstrapped proportions")

#--------------------------------------------------
