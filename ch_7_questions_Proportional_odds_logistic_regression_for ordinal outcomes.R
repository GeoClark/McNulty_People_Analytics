#ch_7_questions_Proportional_odds_logistic_regression_for ordinal outcomes

webpage<- "http://peopleanalytics-regression-book.org/ord-reg.html#ord-reg"

library(dplyr)
library(ggplot2)
library(peopleanalyticsdata)
library(MASS)  #masks dplyr's select function!
library(brant)
library(GGally)
library(DescTools) 
library(generalhoslem)

#Load managers dataset from "people analytics" package.
managers<-peopleanalyticsdata::managers


#data dictionary - variable definitions:
#employee_id for each manager
#     performance_group of each manager in a recent performance review: Bottom performer, Middle performer, Top performer
#     yrs_employed: Total length of time employed in years
#     manager_hire: whether or not the individual was hired directly to be a manager (Y) or promoted to manager (N)
#     test_score: score on a test given to all managers
#     group_size: the number of employees in the group they are responsible for
#     concern_flag: whether or not the individual has been the subject of a complaint by a member of their group
#     mobile_flag: whether or not the individual works mobile (Y) or in the office (N)
#     customers: the number of customer accounts the manager is responsible for
#     high_hours_flag: whether or not the manager has entered unusually high hours into their timesheet in the past year
#     transfers: the number of transfer requests coming from the manager’s group while they have been a manager
#     reduced_schedule: whether the manager works part time (Y) or full time (N)
#     city: the current office of the manager.

#7.4.2: DATA EXCERCISES
#Exercise_7.4.2.1
#Convert the outcome variable to an ordered factor of increasing performance.
#desired outcome to model: performance_group

#get structure of data.
str(managers)

# convert performance_group to ordered factor (3 level)
managers$performance_group <- ordered(managers$performance_group , 
                             levels = c("Bottom", "Middle", "Top"))

#Exercise_7.4.2.2
#Convert input variables to categorical factors as appropriate.

str(managers)
        # All categorical variables are already factors


#Exercise_7.4.2.3
#Perform any exploratory data analysis that you wish to do.
drop.cols<-c("employee_id")
managers<-managers %>% dplyr::select(-one_of(drop.cols))
GGally::ggpairs(managers)


#Exercise_7.4.2.4
#Run a proportional odds logistic regression model against all relevant input variables.

model <- polr(
  formula = performance_group ~ yrs_employed + manager_hire + test_score + 
    group_size  +  concern_flag  + mobile_flag+customers  + high_hours_flag  +  transfers  +  reduced_schedule+ city , 
  data = managers
)


#Exercise_7.4.2.5
#Construct p-values for the coefficients and consider how to simplify the model to remove variables that do not impact the outcome.

# get coefficients for manager performance model
coefficients <- summary(model)$coefficients

# calculate p-values
p_value <- (1 - pnorm(abs(coefficients[ ,"t value"]), 0, 1))*2

# bind back to coefficients matrix
coefficients <- cbind(coefficients, p_value)

model1_coefficients<-dplyr::arrange(coefficents, p_value)
model1_coefficients


#The variables "city", "reduced_schedule", "concern_flag", "customers",and "mobile" flag are not statistically significant and can be removed
simpler_model<- polr(
  formula = performance_group ~ yrs_employed + manager_hire + test_score + 
    group_size  +    + high_hours_flag  +  transfers   , 
  data = managers
)

coefficients <- summary(simpler_model)$coefficients

# calculate p-values
p_value <- (1 - pnorm(abs(coefficients[ ,"t value"]), 0, 1))*2
odds<-exp((simpler_model)$coefficients)
# bind back to coefficients matrix
coefficients_simpler_model <- cbind(coefficients, p_value, odds)
coefficients_simpler_model


#Exercise_7.4.2.6
#Calculate the odds ratios for your simplified model and write an interpretation of them.
#
coefficients_simpler_model

# All else being equal  each year employed and being hired directly as a manager reduces a manager's chances of a better performance rating
#Each additional individual in a group increases the odds of a manger being a higher performer by ~11%.
#
#Workers who work higher hours have a ~73% higher odds of being in a higher performing group.
#Each additional transfer reduces performance 22%

#Exercise_7.4.2.7
#Estimate the fit of the simplified model using a variety of metrics and perform tests to determine if the model is a good fit for the data.
DescTools::PseudoR2(
  simpler_model, 
  which = c("McFadden", "CoxSnell", "Nagelkerke", "AIC")
)
generalhoslem::lipsitz.test(simpler_model)

# The Mcfadden test is nearly significant but all other tests are not significant indicating our model fits the data well..
# 
#Exercise_7.4.2.8
#Construct new outcome variables and use a stratified binomial approach to determine if the proportional odds assumption holds for your simplified model. Are there any input variables for which you may be concerned that the assumption is violated? What would you consider doing in this case?

# create binary variable for "Mid" and "High"  versus "Bottom"
managers$middle_plus <- ifelse(managers$performance_group == "Bottom", 0, 1)

# create binary variable for "Top" versus "Middle" or "Bottom"
managers$top <- ifelse(managers$performance_group == "Top", 1, 0)

#Exercise_7.4.2.9
#Use the Brant-Wald test to support or reject the hypothesis that the proportional odds assumption holds for your simplified model.





#Exercise_7.4.2.10
#Write a full report on your model intended for an audience of people with limited knowledge of statistics.#
#