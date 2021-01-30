



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
  
  
  
  