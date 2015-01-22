
```r
n<-100
p<-.5
x<-c(seq(0,n,by=1))

barplot(dbinom(x,n,p),space=0)
curve(dnorm(x -.5, n * p, sqrt(n*p*(1-p))), col="red", add=T)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png) 

```r
# we sample 100 teenagers, what is the probability that 70 ofr fewer have a phone
pbinom(70,100,.78)
```

```
## [1] 0.03855434
```
