#Questions are from Keith McNulty's handbook on regression
workbook_link<- "http://peopleanalytics-regression-book.org/bin-log-reg.html"

### Chapter 3 Questions
### Chapter 3 Questions
### Chapter 3 Questions
library(peopleanalyticsdata)
library(dplyr)
library(ggplot2)
library(tidyr)
library(rmarkdown)


#working with charity donations data set.

#load date

total_donations<- read.csv("http://peopleanalytics-regression-book.org/data/charity_donation.csv")

#Quick EDA

#preview data
head(total_donations,n=5)
str(total_donations)
sum(is.na(total_donations))
summary(total_donations)
cor(total_donations[c("n_donations", "total_donations", "time_donating", "recent_donation", 
                      "last_donation", "age")])

#3.4.2.1
##Calculate the mean total_donations from the data set.
mean(total_donations$total_donations)

#3.4.2.2
##Calculate the sample variance for total_donation and convert this to a population variance.
sample_variance_donation <- var(total_donations$total_donations, na.rm = TRUE)


n <- length(na.omit(total_donations$total_donations))
population_variance_donations <- ((n-1)/n) * sample_variance_donation

sample_variance_donation
population_variance_donations
#3.4.2.3
##Calculate the sample standard deviation for total_donations and verify that it is the same as the square root of the sample variance.


sd<-sd(total_donations$total_donations, na.rm=TRUE)
sqrt<-sqrt(sample_variance_donation)
#Print values
sd
sqrt
sd==sqrt


#3.4.2.4
##Calculate the sample correlation between total_donations and time_donating. By using an appropriate hypothesis test, determine if these two variables are independent in the overall population.
#3 correlation measures

with(total_donations, cor(total_donations, time_donating))

# spearman's correlation
cor(total_donations$total_donations, total_donations$time_donating, 
    method = "spearman", use = "complete.obs")

#Plot total donations vs times donating
ggplot(total_donations, aes(x=time_donating, y=total_donations ))+
  geom_point()+
  ggtitle(("Total donations vs Times_donating"))

#hypothesis tests. Calculates once manually and once using R function
##Manual
## calculate t_star
r <- cor(total_donations$total_donations, total_donations$time_donating)
n <- nrow(total_donations)
t_star <- (r*sqrt(n - 2))/sqrt(1 - r^2)

# convert to p-value on t-distribution with n - 2 degrees of freedom
2*pt(t_star, df = n - 2, lower = FALSE)

#Function
cor.test(total_donations$total_donations, total_donations$time_donating)


#Hypothesis test results.
# p value far below .05 threshold, so we reject null. We reject the null, total donations and time donating are positively correlated.
#This makes logical sense.


#3.4.2.5
##Calculate the mean and the standard error of the mean for the first 20 entries of total_donations.

# set seed for reproducibility of sampling
set.seed(123)

# generate a sample of 20 observations
n <- length(na.omit(total_donations$total_donations))
rsample <- total_donations$total_donations[sample(1:n, 20)]

#calculate mean and standard error for 20 random entries

mean_n20<-mean(rsample)
se_n20<-sd(rsample)/sqrt(20)

mean_n20
se_n20

#3.4.2.6
##Calculate the mean and the standard error of the mean for the first 50 entries of total_donations. Verify that the standard error is less than in Exercise 5.

# generate a sample of 50 observations
rsample <- total_donations$total_donations[sample(1:n, 50)]

#calculate mean and standard error for 50 random entries

mean_n50<-mean(rsample)
se_n50<-sd(rsample)/sqrt(50)

mean_n50
se_n50

#Check that standard error is lower for the 50 samples than the 20 samples. 
se_n50<se_n20




#3.4.2.7
##By using an appropriate hypothesis test, determine if the mean age of those who made a recent donation is different from those who did not.

# Subset data into recent (1) and not recent (0) donations
recent_donations <- subset(total_donations, subset = recent_donation == 1)
past_donation <- subset(total_donations, subset = recent_donation == 0)
#print means
mean(recent_donations$age)
mean(past_donation$age)
#compare means of ages for recent and past donations
t.test(recent_donations$age, past_donation$age)

#The p value is much smaller than .05, we reject the null hypothesis.
#confirm data visually

total_donations %>% 
  mutate(recent_donation=as.character(recent_donation)) %>%
  ggplot(aes(age, fill=recent_donation))+
  geom_density(alpha=.4)+
  ggtitle("Density plot of age grouped by 'recent_donation'")+
  theme_classic()
# plot confirms the reults of t-test; recent donations are more frequently from higher age individuals


#3.4.2.8
##By using an appropriate hypothesis test, determine if there is a difference in whether or not a recent donation was made according to where people reside.

#create  table of recent vs past donations by where the individual lives
recent_reside_table <- table(total_donations$recent_donation,total_donations$reside)

#print contingency
recent_reside_table

chisq.test(recent_reside_table)
# p-value is <.05, we reject the null. The relative timing of donations vary by where the individual lives

#3.4.2.9_EXTENSION
##Extension: By using an appropriate hypothesis test, determine if the age of those who have recently donated is at least 10 years older than those who have not recently donated in the population.





#3.4.2.10_EXTENSION
##Extension: By using an appropriate hypothesis test, determine if the average donation amount is at least ten dollars higher for those who recently donated versus those who did not. Retest for twenty dollars higher.



