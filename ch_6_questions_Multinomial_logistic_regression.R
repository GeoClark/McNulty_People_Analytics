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
    position_level, 
  data = health_insurance, 
  family = "binomial"
)

#Fit model B. originally included "tenure." dropped due to being statistically insignificant.  
B_model <- glm(
  formula = productB ~ age + gender + children + 
    position_level, 
  data = health_insurance, 
  family = "binomial"
)
#Fit model C. Dropped "tenure" and "position_level"
C_model <- glm(
  formula = productC ~ age + gender + children, 
  data = health_insurance, 
  family = "binomial"
)

Acoef<-A_model$coefficients


# summary
summary(A_model)
summary(B_model)
summary(C_model)
#calculate odds for each model.  Combine and calculate p values
modA_diagnostics<- as.data.frame(exp(A_model$coefficients)) %>% 
  cbind(A_model$coefficients ) %>% 
  dplyr::rename( odds="exp(A_model$coefficients)") %>% 
  dplyr::rename(coefficients="A_model$coefficients") %>% 
  dplyr::mutate(model="A_model")


modB_diagnostics<- as.data.frame(exp(B_model$coefficients)) %>% 
  cbind(B_model$coefficients ) %>% 
  dplyr::rename( odds="exp(B_model$coefficients)") %>% 
  dplyr::rename(coefficients="B_model$coefficients") %>% 
  dplyr::mutate(model="B_model")



modC_diagnostics<- 
  as.data.frame(exp(C_model$coefficients)) %>% 
  cbind(C_model$coefficients ) %>% 
  dplyr::rename( odds="exp(C_model$coefficients)") %>% 
  dplyr::rename(coefficients="C_model$coefficients") %>% 
  dplyr::mutate(model="C_model")

#print diagnostics
modA_diagnostics

modB_diagnostics

modC_diagnostics

#6.5.2.3
#Carefully write down your interpretation of the odds ratios from the previous question.
#For Model A:

    #All else being equal:
modA_diagnostics
    #each year of age reduces the selection of choice A by 22%
    #being male increases selection of A 132%
    #each additional child increases the odds of selection 27%
    #each position level increases odds 36%

#
#for B:
#All else being equal:
modB_diagnostics
    #each year of age increases the selection of choice B by 6%
    #being male reduces selection of B 90%
    #each additional child decreases the odds of selection 63%
    #each position level reduces odds of selection 24%


#for C:
#All else being equal:
modC_diagnostics
#each year of age increases the selection of choice C 11%%
#being male increases selection of 207%
#each additional child increases the odds of selection 80%%


#6.5.2.4
#Run a multinomial logistic regression model on the product outcome using Product B as reference. Calculate the coefficients, ratios and p-values in each case.



#6.5.2.5
#Verify that the coefficients for Product C against reference Product B matches those calculated in 6.3.3.



#6.5.2.6
#Carefully write down your interpretation of the odds ratios calculated in the previous question.



#6.5.2.7
#Use the process described in 6.4.1 to simplify the multinomial model in Question 4.


