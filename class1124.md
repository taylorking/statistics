---
Title: Class on 11/24
Author: Taylor King
---


```r
Anthropometric<-read.csv(file=url("http://www1.appstate.edu/~thomleyje/data-files/Anthropometric.csv"))
women <- subset(Anthropometric, Gender=='Female')

my.sample <- women$Height
print(length(my.sample))
```

```
## [1] 131
```

```r
b <- 10^5
my.boot <- numeric(b)

for ( i in 1 : b)
{
x <- sample(my.sample,131 , replace =T)
my.boot[i]  <- mean(x)
}

hist(my.boot)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png) 

```r
mean(my.boot)
```

```
## [1] 65.187
```

```r
sd(my.boot)
```

```
## [1] 0.2308432
```
Spread of the graph is equal to $\frac{\sigma}{\sqrt{n}}$

$\overline{x}\ =\ 65.188\ \rightarrow$ this is my best estimate of $\mu$ based on my bootstrap samples.

$\overline{\overline{x}}$ is a point estimate of $\Mu$
