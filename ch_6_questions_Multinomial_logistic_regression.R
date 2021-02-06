#Chapter 6 Data Exercises
#5.5.2
#multinomial Logistic Regression
library(desc)
library(peopleanalyticsdata)
library(dplyr)
library(ggplot2)
library(tidyr)
library(GGally)
library(dummies)
library(ggridges)
library(LogisticDx)

#6.5.2 Data exercises
#Use the same health_insurance data set from this chapter to answer these questions.
#

#load data from People Analytics package
health_insurance<- peopleanalyticsdata::health_insurance


#6.5.2.2
#Complete the full stratified approach to modeling the three product choices that was started in 6.2. 
#Calculate the coefficients, odds ratios and p-values in each case.

# convert product and gender to factors
health_insurance$product <- as.factor(health_insurance$product)
health_insurance$gender <- as.factor(health_insurance$gender)

# create dummies for product choice outcome
dummy_product <- dummies::dummy("product", data = health_insurance)

# combine to original set
health_insurance <- cbind(health_insurance, dummy_product)

# run a binomial model for the Product A dummy against 
# all input variables (let glm() handle dummy input variables)
A_model <- glm(
  formula = productA ~ age + gender + children + 
    position_level + tenure, 
  data = health_insurance, 
  family = "binomial"
)


B_model <- glm(
  formula = productB ~ age + gender + children + 
    position_level + tenure, 
  data = health_insurance, 
  family = "binomial"
)

C_model <- glm(
  formula = productC ~ age + gender + children + 
    position_level + tenure, 
  data = health_insurance, 
  family = "binomial"
)




# summary
summary(A_model)
summary(B_model)
summary(C_model)


#6.5.2.3
#Carefully write down your interpretation of the odds ratios from the previous question.



#6.5.2.4
#Run a multinomial logistic regression model on the product outcome using Product B as reference. Calculate the coefficients, ratios and p-values in each case.



#6.5.2.5
#Verify that the coefficients for Product C against reference Product B matches those calculated in 6.3.3.



#6.5.2.6
#Carefully write down your interpretation of the odds ratios calculated in the previous question.



#6.5.2.7
#Use the process described in 6.4.1 to simplify the multinomial model in Question 4.


