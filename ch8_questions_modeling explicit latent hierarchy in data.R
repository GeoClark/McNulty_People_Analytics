#Chapter 8
#modeling explicit latent hierarchy in data

library(dplyr)
library(peopleanalyticsdata)
library(lme4)
library(lavaan)
library(ggplot2)


#8.3.2.1
#Split the data into two sets according to the gender of the participant. Run standard binomial logistic regression models on each set to determine the relationship between the dec decision outcome and the input variables samerace, agediff, attr, intel and prob.
#load speed dating data
speed_dating<-peopleanalyticsdata::speed_dating

#split into two sets based on gender

speed_dating_male<-filter(speed_dating, gender==1)
speed_dating_fem<-filter(speed_dating, gender==0)

# run standard binomial model for males
    model_male <- glm(dec ~ agediff + samerace + attr + intel + prob, 
                      data = speed_dating_male, 
                      family = "binomial")
# run standard binomial model for males

    model_female <- glm(dec ~ agediff + samerace + attr + intel + prob, 
                      data = speed_dating_fem, 
                      family = "binomial")

#get model summaries
    summary(model_male)
    summary(model_female)

    
#8.3.2.2
#Run similar mixed models on these sets with a random intercept for iid.

  #fit mixed model with random intercept for MALES  
    iid_intercept_model_male <- lme4:::glmer(
      dec ~ agediff + samerace + attr + intel + prob + (1 | iid),
      data = speed_dating_male,
      family = "binomial"
    )    
  #fit mixed model with random intercept for FEMALES  
    iid_intercept_model_female <- lme4:::glmer(
      dec ~ agediff + samerace + attr + intel + prob + (1 | iid),
      data = speed_dating_fem,
      family = "binomial"
    )    
    
    
    #
#
#
#8.3.2.3
#What different conclusions can you make in comparing the mixed models with the standard models?
#compare models MALE 
summary(model_male )
 summary(iid_intercept_model_male)
 
#compare models FEMALE 
 summary(model_female )
 summary(iid_intercept_model_female)
    
 #intelligence in the male mixed model is much more significant than in the standard model
 #In the mixed female model all parameters became more significant.  All, are significant with the exception of age_diff which approaches the significance threshold.

 
 
 # 8.3.2.4 #
#Experiment with some random slope effects to see if they reveal anything new about the input variables.
 

 #Add random effect for age diff and attractiveness
 iid_rndm_model_male <- lme4:::glmer(
   dec ~ agediff + samerace + attr + intel + prob + (1+  intel|prob),
   data = speed_dating_male,
   family = "binomial"
 )    
 
 summary(iid_rndm_model_male)
 #experimented with multiple random effects in the male and female model. The attr and prob were the only significant values.  It appears that the random effects in the "iid" was more useful in predicting the outcome.
 
 #For exercises 5–10, load the employee_survey data set via the peopleanalyticsdata package or download
 #it from the internet
 #37. This data set contains the results of an engagement survey of employees of a 
 #technology company. Each row represents the responses of an individual to the survey and each column
 #represents a specific survey question, with responses on a Likert scale of 1 to 4, with 1 indicating
 #strongly negative sentiment and 4 indicating strongly positive sentiment. Subject matter experts have
 #grouped the items into hypothesized latent factors as follows:
 #
 
 #load employee_survey data
 employee_survey<-peopleanalyticsdata::employee_survey

 #data dictionary 
# "Happiness" is an overall measure of the employees current sentiment about their job
# Items beginning with "Ben" relate to employment benefits
# Items beginning with "Work" relate to the general work environment
# Items beginning with "Man" relate to perceptions of management
# Items beginning with "Car" relate to perceptions of career prospects
# 
#get data structure. 
str(employee_survey) 
 
#count missing data
sum(is.na(employee_survey))

#no missing values

#8.3.2.5
##Write out the proposed measurement model, defining the latent factors in terms of the measured items.
glimpse(employee_survey)

# define measurement model

employee_meas_mod <- "
# measurement model
Ben =~ Ben1 + Ben2 + Ben3
Work =~ Work1 + Work2 + Work3
Man =~ Man1 + Man2 + Man3
Car =~ Car1 + Car2+Car3+Car4
"


#8.3.2.6
##Run a confirmatory factor analysis on the proposed measurement model. Examine the fit and the factor loadings.

cfa_employee_meas_mod <- lavaan::cfa(model = employee_meas_mod, data = employee_survey)
lavaan::summary(cfa_employee_meas_mod, fit.measures = TRUE, standardized = TRUE)

#CFI and TLI do not exceed the .95 threshold.  Model does have an RMSEA of .069 (desired, >.06).

#8.3.2.7
##Experiment with the removal of measured items from the measurement model in order to improve the overall fit.

employee_meas_mod_simple <- "
# measurement model
Work =~ Work1  + Work3
Car =~ Car1 + Car2+Car4
Man =~ Man1  + Man3
Ben =~ Ben2 +Ben3

"

cfa_employee_meas_mod_sim <- lavaan::cfa(model = employee_meas_mod_simple, data = employee_survey)
lavaan::summary(cfa_employee_meas_mod_sim, fit.measures = TRUE, standardized = TRUE)
#All factor loadings from std error all column exceed .07, experimenting with removing work2, and Ben1 car3, man2, ben1
#improved fit.  Meets CFI, TLI, RMSEA, and SRMR thresholds.

#8.3.2.8
##Once satisfied with the fit of the measurement model, run a full structural equation model on the data.


# structural model

# run full SEM 
full_sem <- "
# measurement model
Work =~ Work1  + Work3
Car =~ Car1 + Car2+Car4
Man =~ Man1  + Man3
Ben =~ Ben2 +Ben3



# structural model
Happiness ~ Work + Car + Man + Ben 
"

# run full SEM 
full_model_employee <- lavaan::sem(model = full_sem, data = employee_survey)
lavaan::summary(full_model_employee, standardized = TRUE)

#8.3.2.9
##Interpret the results of the structural model. Which factors appear most related to overall employee sentiment? Approximately what proportion of the variance in overall sentiment does the model explain?

# Overall career prospects is the biggest driver for predicting employee happiness.  
#Career prospects and work conditions explain 55% of the outcome.  Manager perception and benefits explain 25% of the variance, with Man and ben explaining ~15% and 10% respectively.  



#8.3.2.10
##If you dropped measured items from your measurement model, experiment with assigning them to other factors to see if this improves the fit of the model. What statistics would you use to compare different measurement models?













