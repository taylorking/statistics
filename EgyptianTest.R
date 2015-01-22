# Sampling Permutation Test
# Example of a Two-Tailed Test
# Skull Measurements from Two Eras (30 skulls per era, 4 variables each)
# Source: Thomson, A. and Randall-Maciver, R. (1905) Ancient Races of the
#         Thebaid, Oxford: Oxford University Press.
# Q: Is the maximal breadth of skulls from 200BC different than 4000BC?
# http://www.cixip.com/index.php/page/content/id/1207
# https://www.facebase.org/facial_norms/summary/measures/1/maxcranwidth
# Variables are:
#    MB: Maximal Breadth of Skull
#    BH: Basibregmatic Height of Skull
#    BL: Basialveolar Length of Skull
#    NH: Nasal Height of Skull 
# Last Updated 10/21/2014 @ 12:30pm

Skulls <- read.csv(file=url("http://www1.appstate.edu/~thomleyje/data-files/EgyptianSkullsA.csv"))

# Compute the observed difference in means in the dataset.
Skulls200<-subset(Skulls,Time==200)
Skulls4000<-subset(Skulls,Time==4000)
obs.diff <- mean(Skulls200$MB) - mean(Skulls4000$MB)    
boxplot(MB~Time, data=Skulls, horizontal=T)

# Select the variable to be tested and drop all others.
data <- subset(Skulls, select=MB, drop=T)

# -----Change Variables Above (17-23) for a Different Test-----

# Conduct the resampling permutation test for N iterations.
# Data are randomly chosen for Group 1; others become Group 2.
# The vector "index" is the data values selected to be Group 1.
# The vector "-index" is the un-selected values put in Group 2.
# The vector "result" stores the sampled null distribution; it
#     is computed the same way as the test statistic obs.diff.
N <- 10^5-1         
result <- numeric(N)
for(i in 1:N)       
{
  index <- sample(60, size=30, replace = FALSE)      
  result[i] <- mean(data[index]) - mean(data[-index])
}

# Graph the null distribution based on sampling and plot obs.diff.
hist(result, prob=T,
     main="Sampling Permutation Distribution",
     xlab="Randomly Resampled Differences")
abline(v=c(obs.diff,-obs.diff), col="blue",lty=5)

# Compute both the one-sided p-values based on the test statistic.
pvalue.upper<-(sum(result>=obs.diff)+1)/(N+1)
pvalue.lower<-(sum(result<=obs.diff)+1)/(N+1)

# Compute the two-sided p-value using the smaller one-sided value.
pvalue.two<-format(2*min(pvalue.upper,pvalue.lower),scientific=F)
print(paste0("The two-sided p-value for Ha: 200BC mean =/= 4000BC mean is ", pvalue.two),quote=F)

