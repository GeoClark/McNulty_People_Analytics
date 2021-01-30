#Questions are from Keith McNulty's handbook on regression
workbook_link<- "http://peopleanalytics-regression-book.org/bin-log-reg.html"

#Regression Modeling in People Analytics by Keith McNulty
#Answers to data Exercises at the end of each chapter.
#CHAPTER 2
library(peopleanalyticsdata)
library(dplyr)
library(ggplot2)
library(tidyr)
library(GGally)
library(fastDummies)
library(ggridges)
library(rmarkdown)


#2.7.2 Data Exercises

#2.7.2.1__ 
##Create a character vector called my_names (first, middle, last as elements).

my_names<- c("John","Smith", "Doe" )

#Calculate the length of my_names.
str(my_names)

#2.7.2.2__
##Create a second numeric vector called which which corresponds to my_names. The entries should be the position of each name in the order of your full name. Verify that it has the same length as my_names.

which<-c(1, 2, 3)
#verify length of which matches the length of my_names.
str(which)
 
#2.7.2.3__
##Create a dataframe called names which consists of the two vectors my_names and which as columns. 
##Calculate the dimensions of names.
names<-data.frame(my_names, which)
dim(names)

#2.7.2.4__
##Create a new dataframe new_names with the which column converted to character type. 
##Verify that your command worked using str().
names$which<-as.character(names$which)
str(names)

#2.7.2.5__
##Use read.csv() to load the dataset of student test data found at http://peopleanalytics-regression-book.org/data/ugtests.csv into an object named ugtests. 
##Calculate the dimensions of ugtests and view the first three rows only.
ugtests<- read.csv("http://peopleanalytics-regression-book.org/data/ugtests.csv ")
dim(ugtests)
head(ugtests, n=3)

#2.7.2.6__
##View a statistical summary of all of the columns of ugtests. Determine if there are any missing values.
summary(ugtests)
sum(is.na(ugtests))

#2.7.2.7__
##View the subset of ugtests for values of Yr1 greater than 50.
subset(ugtests, Yr1>50)

#2.7.2.8__
##Install and load the package dplyr. Look up the help for the filter() function in this package and try to use it to repeat the task in the previous question.
library(dplyr)
?dplyr::filter

ugtests %>% 
  filter(Yr1>50)

#2.7.2.9__
##Write code to find the mean of the Yr1 test scores for all those who achieved Yr3 test scores greater than 100. 
##Round this mean to the nearest integer.
mean_grtr_100<-round(mean(subset(ugtests$Yr1,subset= ugtests$Yr3>100)))
mean_grtr_100 

#2.7.2.10__
##Familiarize yourself with the two functions filter() and pull() from dplyr. 
##Use these functions to try to do the same calculation in the previous question using a single unbroken piped command. 
##Be sure to namespace where necessary.
mean_grtr_100_pipe<-
  ugtests %>% 
  dplyr:: filter(Yr3>100) %>% 
  pull(Yr1) %>% 
  mean() %>% 
  round()
#print_answer
mean_grtr_100_pipe


#Alternative pipe without "pull" using sumarize function
mean_grtr_100_pipe<-
  ugtests %>% 
  dplyr:: filter(Yr3>100) %>% 
  dplyr::summarize(mean(Yr1)) %>% 
  round()
#print_answer
mean_grtr_100_pipe



### Chapter 3 Questions
### Chapter 3 Questions
### Chapter 3 Questions


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







#Chapter 5 Data Exercises
#5.5.2
#Binomial Logistic Regression
library(desc)
library(peopleanalyticsdata)
library(dplyr)
library(ggplot2)
library(tidyr)
library(GGally)
library(fastDummies)
library(ggridges)
library(rmarkdown)

#load charity donation data from Peoples analytics package.
charity_data<-
  peopleanalyticsdata::charity_donation

#Question 5.5.2.1)
#View the data and obtain statistical summaries. Ensure data types are appropriate and there is no missing data. Determine the outcome and input variables.
glimpse(charity_data)
summary(charity_data)
sum(is.na(charity_data))

# "recent donation" will be the response variable in our model

#Change "last donation" to a factor. Convert "last donation" to an ordinal variable
charity_data<- charity_data  %>% 
  mutate(  recent_donation  =  as.factor(recent_donation))   
 # mutate( last_donation  =  as.ordered(last_donation))
  


#Question 5.5.2.2)
#Using a pairplot or by plotting or correlating selected fields, try to hypothesize which variables may be significant in explaining who recently donated.

GGally::ggpairs(charity_data )

#Looking at pairs plot recent donations are likely characterized by contributors who:
     
  # are older, are domestic or domestic urban, have donated historically, and have donated larger sums in the past.
    


#Question 5.5.2.3)
#Run a binomial logistic regression model using all input fields. Determine which input variables have a significant effect on the outcome and the direction of that effect.

dummy_col<-c( "reside" , "gender")

charity_data_dummy<-charity_data %>% 
  dplyr::mutate(reside= gsub(" ","_", reside)) %>% 
  dplyr::mutate(reside= tolower(reside) ) %>% 
  fastDummies::dummy_cols(select_columns=dummy_col, remove_selected=TRUE, remove_first_dummy = TRUE) 


#view columns
glimpse(charity_data_dummy)

#build binomial logistic regression model
  full_model <- glm(formula = "recent_donation ~ .",
                    family = "binomial",
                    data = charity_data_dummy)
  
  

#Question 5.5.2.4)
#Calculate the odds ratios for the significant variables and explain their impact on the outcome
library(caret)
library(car)
  (coefs <- summary(full_model)$coefficients)
  summary(full_model)
  
  (full_coefs <- cbind(coefs[ ,c("Estimate", "Pr(>|z|)")], 
                       odds_ratio = exp(full_model$coefficients)
                       ))  
  

#Question 5.5.2.5)
#Check for collinearity or multicollinearity in your model using methods from previous chapters.

  #Check VIF to asses Collinearity. Almost everything is <2 so should be good. Time donating is 2.54, below the threshold but high. Will re-assess after re-specifying model.
  car::vif(full_model)



#Question 5.5.2.6)
#Experiment with model parsimony by reducing input variables that do not have a significant impact on the outcome. Decide on the most parsimonious model.
  charity_data_dummy2<-charity_data_dummy %>% 
    select( "recent_donation", 
           "age", "reside_rural_domestic", "gender_M", 
           "last_donation", "total_donations")
  
  simple_model <- glm(formula = "recent_donation ~ .",
                    family = "binomial",
                    data = charity_data_dummy2)
summary(simple_model)
  rm(charity_charity_data_dummy)
  

  (coefs <- summary(simple_model)$coefficients)
  summary(simple_model)
  
  (full_coefs <- cbind(coefs[ ,c("Estimate", "Pr(>|z|)")], 
                       odds_ratio = exp(simple_model$coefficients)
  ))  
  #Convert summary to a data frame and calculate odds. Extra () prints df directly.
 ( full_coeffs_df<- data.frame(full_coefs) %>% 
    dplyr::mutate(probability= odds_ratio/(odds_ratio+1))
 )
  

#Question 5.5.2.7)
#Calculate a variety of Pseudo- R2 variants for your model. How would you explain these to someone with no statistics expertise?

  #  d<-density(residuals(full_model, "pearson"))
  #  plot(d, main= "")
  
  DescTools::PseudoR2(
    full_model, 
    which = c("McFadden", "CoxSnell", "Nagelkerke", "Tjur")
  )

  
  DescTools::PseudoR2(
    simple_model, 
    which = c("McFadden", "CoxSnell", "Nagelkerke", "Tjur")
  )
  

#Question 5.5.2.8)
#Report the conclusions of your modeling exercise to the charity by writing a simple explanation that assumes no knowledge of statistics.

#We created a model to predict the likelihood that a donor would donate would donate in a given month using a set of 5 variables including: age, where the donor lived (urban, rural, or abroad), gender, months since the last donation, and the total contribution they have made to date.
  #All else being equal, a donor is more likely to donate if they are female, elderly, live domestically in a rural environment, have donated recently, and have contributed larger sums in the past.



#Question 5.5.2.9)
#Extension: Using a variety of methods of your choice, test the hypothesis that your model fits the data. How conclusive are your tests?
#
# assess goodness of fit of the model
  library(LogisticDx)
  

  # get range of goodness of fit diagnostics
  simple_model_diagnostics <- gof(simple_model, 
                                               plotROC = TRUE)
  
    # returns a list
  names(simpler_model_diagnostics)
  
  
  
  