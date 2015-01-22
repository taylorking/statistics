# Sampling Permutation Test
# Example of a One-Tailed Test
# Generic vs. Brand Name Batteries
# Do generic last longer than brand name?
# Observed data include 6 of each type.
# Last Updated 10/21/2014 @ 12:30pm

Batt <- read.csv(file=url("http://www1.appstate.edu/~thomleyje/data-files/BatteryLife.csv"))

# Print out observed data and a boxplot comparing the groups.
data.frame(Generic=Batt$Hours[7:12],
           Brand=Batt$Hours[1:6])
boxplot(Hours~Type, data=Batt, horizontal=T)

# How many permutations would we have in an exact test?
print(paste0("The number of exact permutations is ", choose(12,6)),quote=F)

# Compute the observed difference in means in the dataset.
# Remove label from tapply output when printing obs.diff.
# Format obs.diff output so that it reads as a sentence.
obs.diff <- tapply(Batt$Hours, Batt$Type, mean)[2]- tapply(Batt$Hours, Batt$Type, mean)[1]    
attributes(obs.diff) <- NULL
print(paste0("The observed mean difference (generic - brand) is ", round(obs.diff,4)),quote=F)

# Select the variable to be tested and drop all others.
life <- subset(Batt, select=Hours, drop=T)

# Uncomment the seed command below to get reproducible results.
# set.seed(0)

# Conduct the resampling permutation test for N iterations.
# The vector "result" stores the sampled null distribution; it
#     is computed the same way as the test statistic obs.diff.
# Every command included between the { } repeats for N times.
# Data are randomly chosen for Group 1; others become Group 2.
# The vector "index" is the data values selected to be Group 1.
# The vector "-index" is the un-selected values put in Group 2.
# Start the resampling process!
N <- 10^5-1          
result <- numeric(N)
for(i in 1:N)
{
  index <- sample(12, size=6, replace = FALSE) 
  result[i] <- mean(life[index]) - mean(life[-index])
}

# Displays resampling results (null distribution) as a histogram.
# Plots the observed difference on the null dist. for comparison.
hist(result,
     main="Sampling Permutation Distribution",
     xlab="Randomly Resampled Differences")
abline(v=obs.diff, col="blue",lty=5)       
text(obs.diff,5000,"Obs. Diff",pos=4,col="blue")

# Compute the upper-tail p-value and print it out.
pvalue.upper<-format((sum(result>=obs.diff)+1)/(N+1),scientific=F)
print(paste0("The upper-tailed p-value for Ha: generic mean > brand mean is ", pvalue.upper),quote=F)

