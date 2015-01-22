# Chi-Square Test of Independence
# Last Updated 10/263/2014 @ 08:40pm

# Create a function to compute the chi-squared test statistic
# Observed is the observed data; the variables are defined later
chisq<-function(Observed)
{ 
  Expected <- outer(rowSums(Observed),colSums(Observed))/sum(Observed)
  sum((Observed-Expected)^2/Expected)
}

# Load the dataset
Berkeley <- read.csv(file=url("http://www1.appstate.edu/~thomleyje/data-files/Berkeley.csv"))

#Select the two variables we want to test and make a table
Sex <- subset(Berkeley, select=Sex, drop=T)
Status <- subset(Berkeley, select=Status,drop=T)
Observed <- table(Sex,Status)

#Use function created above to calculate the chi-square test statistic
test.stat <- round(chisq(Observed),4)
Expected <- outer(rowSums(Observed),colSums(Observed))/sum(Observed)

#Resampling permutation test loop and graph of results
N <- 10^4-1
result<-numeric(N)
for (i in 1:N)
{
  ST.perm <-sample(Status)
  RND.table <- table(Sex, ST.perm)
  result[i]<-chisq(RND.table)
}
UL<-round(max(result),0)+1
hist(result, prob=T,
     main="Null Distribution of Chi-Square Test Statistic",
     breaks=seq(0,UL,by=0.5),
     xlab="chi-square statistic", 
     xlim=c(0,UL),
     ylim=c(0,1))
abline(v=test.stat,
       col="blue",
       lty=5)   
curve(dchisq(x, df=1), 
      from=0, 
      to=UL, 
      col="green", 
      add=T) 

# Print observed counts, expected counts, test statistic, and p-value 
print(paste0("The chi-square test statistic is ", test.stat),quote=F)
print(paste0("The p-value for the chi-square test of independence is ",format((sum(result >= test.stat) + 1)/(N + 1),scientific=F)),quote=F)
