# This line reads in the CSV from Dr Thomleys website
data <- read.csv(file=url("http://www1.appstate.edu/~thomleyje/data-files/Berkeley.csv"))

# Create the frequency table for sex, giving the numbers of responders
table(data$Sex)
# Create a proportional frequency table for sex
prop.table(table(data$Sex)) * 100
# Put this into a barplot
barplot(prop.table(table(data$Sex))*100, main="Responses By Sex", ylab="Percentage")

# Create a table for Application to different departments
table(data$Department)
# Create a proportional frequency table from this information
prop.table(table(data$Department)) * 100
# Put this data into a bar plot.
barplot(prop.table(table(data$Department)) * 100, main="Applicants by department", ylab="Percentage")

# Create a table giving the gender of each applicant by department
table(data$Department, data$Sex)
# Make this a proportional table
prop.table(table(data$Department, data$Sex),1) * 100
# Make the barchart that we want
barplot(prop.table(table(data$Sex, data$Department),1) * 100, legend=T, ylab="Percentage", xlab="Department", main="Applications by department")

# Create a table of Accepted / Rejected based on Gender
table(data$Sex,data$Status)
# Make that a proportional contingency table
round(prop.table(table(data$Sex, data$Status),1) * 100,1)

# Make the unstacked barchart to graphically display the information from 5
barplot(round(prop.table(table(data$Sex, data$Status),1) * 100,1), beside=T, legend=T, main="Application Status by Sex", ylab="Percentage", xlab="Status")

#Subset Each Department
DeptA<-subset(data, Department=='A')
DeptB<-subset(data, Department=='B')
DeptC<-subset(data, Department=='C')
DeptD<-subset(data, Department=='D')
DeptE<-subset(data, Department=='E')
DeptF<-subset(data, Department=='F')

# Display the Data For Each Department
# Dept A
round(prop.table(table(DeptA$Sex, DeptA$Status),1) * 100,1)
barplot(round(prop.table(table(DeptA$Sex, DeptA$Status),1) * 100,1), beside=T, legend=T, main="Application Status by Sex in Department A", ylab="Percentage", xlab="Status")
# Dept B
round(prop.table(table(DeptB$Sex, DeptB$Status),1) * 100,1)
barplot(round(prop.table(table(DeptB$Sex, DeptB$Status),1) * 100,1), beside=T, legend=T, main="Application Status by Sex in Department B", ylab="Percentage", xlab="Status")
# Dept C
round(prop.table(table(DeptC$Sex, DeptC$Status),1) * 100,1)
barplot(round(prop.table(table(DeptC$Sex, DeptC$Status),1) * 100,1), beside=T, legend=T, main="Application Status by Sex in Department C", ylab="Percentage", xlab="Status")
# Dept D
round(prop.table(table(DeptD$Sex, DeptD$Status),1) * 100,1)
barplot(round(prop.table(table(DeptD$Sex, DeptD$Status),1) * 100,1), beside=T, legend=T, main="Application Status by Sex in Department D", ylab="Percentage", xlab="Status")
# Dept E
round(prop.table(table(DeptE$Sex, DeptE$Status),1) * 100,1)
barplot(round(prop.table(table(DeptE$Sex, DeptE$Status),1) * 100,1), beside=T, legend=T, main="Application Status by Sex in Department E", ylab="Percentage", xlab="Status")
# Dept F
round(prop.table(table(DeptF$Sex, DeptF$Status),1) * 100,1)
barplot(round(prop.table(table(DeptF$Sex, DeptF$Status),1) * 100,1), beside=T, legend=T, main="Application Status by Sex in Department F", ylab="Percentage", xlab="Status")






