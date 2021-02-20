#Chapter 8
#modeling explicit latent hierarchy in data

library(dplyr)
library(peopleanalyticsdata)
library(lme4)
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
#
#
#  
# 8.3.2.4 #
#Experiment with some random slope effects to see if they reveal anything new about the input variables.