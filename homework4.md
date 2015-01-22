---
Title: Homework 4
Author: Taylor King
---

$\mu{}\ =\ np$


$\sigma{}\ =\ \sqrt{np(1-p)}$

1. (3 From the textbook) Let $A$ denote the population $\left\{ {1,3,4,5}\right\}$ and $B$ the population $\left\{ {5,7,9} \right\}$. Let $X$ be a random value from $A$, and $Y$ a random value from $B$     

  A. Find the sampling distrobution of $X\ +\ Y$
  $\left\{ { 1 + 5,\ 1 + 7,\ 1 + 9,\ 3 + 5,\ 3 + 7,\ 3 + 9,\ 4 + 5,\ 4 + 7,\ 4 + 9,\ 5 + 5,\ 5 + 7,\ 5 + 9} \right\}$
  B. In this example it does not matter whether or not you sample with or without replacement. We are only sampling 1 item at a time.
  
  C. Compute the Mean of the values of the mean of $A$ and $B$ then compute the value of the sampling distrobution of $X\ +\ Y$
  D. 
  
  ```r
      a<-c(1,3,4,5) 
      b<-c(5,7,9)
      print(mean.a <- mean(a))
  ```
  
  ```
  ## [1] 3.25
  ```
  
  ```r
      print(mean.b <- mean(b))
  ```
  
  ```
  ## [1] 7
  ```
  
  ```r
      print(mean.xy <- mean.a + mean.b)
  ```
  
  ```
  ## [1] 10.25
  ```
  
  ```r
      print(probability <- 2 / 12)
  ```
  
  ```
  ## [1] 0.1666667
  ```

2. (5 From the Textbook)
    $\int\!\frac{3}{8}y^{2}\ =\ \frac{x^{3}}{8}+c\ \frac{\frac{1}{5}^{3}}{8} - \frac{0^{3}}{8}\ =\ .001$

Taylor's problem .. (9 from the textbook)

$f(x)\ =\ \frac{3}{16}(x-4)^{2}\ \ 2\leq\ x\ \leq{} 6$

$P(x)\ =\ \frac{\overline{X}\ - \mu}{\sigma\ /\ \sqrt{n}}\ n\ =\ 244\ \ \overline{X}\ \geq\ 4.2$

  
3.

```r
  n<-800
  p <-.286
  print(mu <- n*p)
```

```
## [1] 228.8
```

```r
  print(sigma <- sqrt((n*p*(1-p))))
```

```
## [1] 12.78136
```

```r
  print(clt.aprox <- pnorm(230.5, mu, sigma) - pnorm(219.5, mu,sigma))
```

```
## [1] 0.3194833
```

```r
  print(exact.bimonial <- pbinom(230, n, p) - pbinom(219, n, p))
```

```
## [1] 0.3208302
```
5. 
If $X_{1} .., X_{n}$ are i.i.d. from Unif[0,1], how large should $n$ be so that $P(|\overline{x}\ -\ 1/2|\lt\ .05)\ \geq{}\ .90$

```r
mu <- (1 + 0) / 2 

# we are looking to find the range between .45 and .55 like we are doing in the bootstrapping section
```

6. 
Problem 15 from the Textbook. 
Let $X_{1},X_{2},...,X_{N}$ be a random sample from $N(0,1)$. Let $W\ =\ X^{2}_{1}\ +\ X^{2}_{2}\ +\ ...\ +\ X^{2}_{n}$ Describe the sampling distrobution of $W$ by running a simulation, using $n\ =\ 2$. What is the mean and varience of sampling distrobution $W$? Repeat using $n\ =\ 4,\ and\ n\ =\ 5$. What observations or conjectures do you have for $n$?

Theorem B.15 gives $\sum{Z_{i}^{2}}^{k}_{i=1}$ has a $x^2$ distribution with $k$ degrees of freedom

Sample variance can be expressed with
$S^2\ =\ \frac{1}{n-1}\sum{(X_{i}\ -\ \overline{X})^{2}}^{n}_{i=1}$

```r
# n=2
```

```r
print(n.2<-runif(2))
```

```
## [1] 0.001005282 0.684873692
```

```r
print(n.4<-runif(4))
```

```
## [1] 0.70008180 0.06898671 0.38159296 0.39419858
```

```r
print(n.5<-runif(5))
```

```
## [1] 0.2567037 0.4130033 0.6687130 0.6950826 0.6131443
```

```r
# W 
```

