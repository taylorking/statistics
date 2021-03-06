---
Title: Exam 3 (Final Exam)
Author: Taylor King, Alyssa Nardi
---

Problem 1 
--- 
Plywood is made by gluing together several very thin sheerts of wood veneer. A cross-layering process produces a strong composite. However, like any manufactured product, there is variation in the materials. Suppose a particular brand of half-inch plywood board (i.e., plywood that is supposed to be half an inch thick) is made by layering five veneer sheets whose individual thickness are each normally distributed with a mean of .1 inches and a standard deviation of 0.005 inches. If the sheets are randomly and independently selected during assembly process and the glue itself adds negligible thickness, then the total thickness is a random variable shown below:

$Thickness_{total}\ =\ Thickness_{1}\ +\ Thickness_{2}\ +\ Thickness_{3}\ +\ Thickness_{4}\ +\ Thickness_{5}$


a. simulate the sampling distribution of the Random Variable T, the total thickness of the plywood boards.


```r
N<-10^5
res<-numeric(N)
for(i in 0:N)
{
  res[i] <- sum(rnorm(5,.1,.005))
}
hist(res)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png) 

```r
qqnorm(res)
qqline(res)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-2.png) 

b. The product is advertised as "half-inch plywood". Does this seem reasonable, given the distrobution of T? (You should consider the distrobution shape, center and spread, as well as outliers.
The Distrobution shape is symetricly distributed about the mean 
      
From our histogram the boards appear to be centered in thickness about.


```r
boxplot(res)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

```r
print(iqr<-quantile(res, .75) - quantile(res,.25))
```

```
##        75% 
## 0.01513675
```

The IQR $\approx\ .015$

```r
print(mean.data<- mean(res))
```

```
## [1] 0.4999819
```

```r
print(upper.fence <- mean.data + (.5 * iqr))
```

```
##       75% 
## 0.5075503
```

```r
print(lower.fence <- mean.data - (.5 * iqr))
```

```
##       75% 
## 0.4924136
```

```r
print(upper.outliers <- sum(res > upper.fence))
```

```
## [1] 25049
```

```r
print(lower.outliers <- sum(res < lower.fence))
```

```
## [1] 24967
```

```r
print(lower.outliers / length(res)) #this is the proprotion of the data that is an lower outlier.
```

```
## [1] 0.24967
```

```r
print(upper.outliers / length(res)) #this is the proportion of the data that is an upper outlier.
```

```
## [1] 0.25049
```

c. Suppose the company wants at least 99% of the plywood boards to be between .475 and .525 inches thick. Is it meeting this goal? if not which percentage of boards do not fall within this boundry?


```r
lower.oob<-sum(res<.475)
upper.oob<-sum(res>.525)
total.oob <- lower.oob + upper.oob
print(total.oob / N)
```

```
## [1] 0.02504
```
The company is not meeting it's goal of having 99% of boards within the range of .475 and .525. 97.47% are within the boundaries defined. 

d. Suppose I take a random sample of 100 veneer sheets and compute their average thickness. According to the central limit theorem (CLT) what would be the sampling distrobution of the mean?

```r
print(sdev <- (.005) / sqrt(100))
```

```
## [1] 5e-04
```
The data will be normally distributed with $\mu\ =\ .1$ and $\sigma\ =\ .0005$  


Problem 2.
---
Consider the flight delays data we examined in Homework 3.  Assume that the data can be considered a representative sample from a larger population of United Airlines and American airlines flights flown under certain circumstances

a.

```r
flights<-read.csv(file=url("http://www1.appstate.edu/~thomleyje/data-files/FlightDelays.csv"))
united <- subset(flights, Carrier=='UA')
american<- subset(flights, Carrier=='AA')

B<- 10 ^ 5 
my.boot <- numeric(B)
obs.stat<- max(united$Delay) - max(american$Delay)
for(i in 1:B)
{
xua<-sample(united$Delay, 1123,replace=TRUE)
my.boot[i] <- mean(xua)
}
print(qs.united <- quantile(my.boot, c(.025, .975)))
```

```
##     2.5%    97.5% 
## 13.41051 18.69012
```

```r
my.boot<-numeric(B)
for(i in 1:B)
{
  xaa<-sample(american$Delay, 2906, replace=TRUE)
  my.boot[i] <- mean(xaa)
}
print(qs.american <- quantile(my.boot, c(.0275, .975)))
```

```
##     2.75%     97.5% 
##  8.712319 11.585685
```

B. Use bootstrapping to estimate the ratio of means $\frac{\mu_{UA}}{\mu_{AA}}$ Create a histogram of your results and describe the shape of the distrobution.


```r
obs.stat <- mean(united$Delay) / mean(american$Delay)
B <- 10 ^ 5 
my.boot <- numeric(B)
for ( i in 1:B)
{
  xua<-sample(united$Delay, 1123, replace=TRUE)
  xaa<-sample(american$Delay, 2906, replace=TRUE)
  my.boot[i] <- mean(xua) / mean(xaa) 
}
hist(my.boot)
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

```r
mean(my.boot)
```

```
## [1] 1.591156
```

```r
qqnorm(my.boot)
qqline(my.boot)
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-2.png) 

The dataset does not appear to be skewed in either direction, however it does appear to be wider than the standard normal curve. 

C. Find 95% Confidence interval

```r
print(confidence <- quantile(my.boot, c(.025, .975)))
```

```
##     2.5%    97.5% 
## 1.266126 1.967210
```
Is it reasonable to assert from the mean delays for the two airlines that they are not the same? 
  
I believe it is reasonable because the delay times on united was $60{\%}$ greater than the mean of delay times on american airlines 

D. What is the bootstrap estimate of bias? What fraction of the bootstrap standard error does it represent (i.e., what is its magnitude relative to the random error)?


```r
print(obs.stat <- mean(united$Delay) / mean(american$Delay))
```

```
## [1] 1.582893
```

```r
print(bias <- mean(my.boot) - obs.stat)
```

```
## [1] 0.008262607
```

```r
print(rel.bias <- (bias/sd(my.boot)) * 100)
```

```
## [1] 4.623647
```

E.

I suspect in real life, were a plane to be delayed, it would have some affect on the delays of following flights. Some planes could be made to land later, or be made to take off later, creating a delay on the flight.
