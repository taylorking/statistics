---
Title: Class Work 12/2
Author: Taylor King
---

Today we are going to talk about an idea called bias.          

```r
my.data <- read.csv(file=url("http://www1.appstate.edu/~thomleyje/data-files/BookPrices.csv"))

hard<-subset(my.data, Area=='Math & Science')
social<-subset(my.data, Area=='Social Sciences')

print(hard.mean<-mean(hard$Price))
```

```
## [1] 156.7341
```

```r
print(hard.sd<- sd(hard$Price))
```

```
## [1] 39.14483
```

```r
print(social.mean<-mean(social$Price))
```

```
## [1] 98.99
```

```r
print(social.sd <- sd(social$Price))
```

```
## [1] 71.91385
```

```r
boxplot(hard$Price)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png) 

```r
boxplot(social$Price)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-2.png) 

```r
qqnorm(hard$Price)
qqline(hard$Price)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-3.png) 

```r
qqnorm(social$Price)
qqline(social$Price)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-4.png) 

```r
mean(hard$Price) - mean(social$Price)
```

```
## [1] 57.74407
```

```r
obs.stat<- max(hard$Price) - max(social$Price)
B<- 10^ 5
my.boot <- numeric(B)
for( i in 1:B)
{
  xhs<-sample(hard$Price, 27, replace=T )
  xss<-sample(social$Price, 17, replace=T)
  my.boot[i] <- mean(xhs) - mean(xss)
}

hist(my.boot)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-5.png) 

```r
mean(my.boot)
```

```
## [1] 57.68341
```

```r
sd(my.boot)
```

```
## [1] 18.46872
```

```r
mean(c(mean(xss), mean(xhs)))
```

```
## [1] 117.2853
```

```r
bias <- mean(my.boot) - obs.stat
rel.bias <- (bias / sd(my.boot))*100
rel.bias
```

```
## [1] 238.3132
```

```r
quantile(my.boot,c(.025, .0975))
```

```
##     2.5%    9.75% 
## 21.77646 33.66991
```


