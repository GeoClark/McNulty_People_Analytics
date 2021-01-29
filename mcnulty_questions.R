


#Regression Modeling in People Analytics by Keith McNulty
#Answers to data Exercises at the end of each chapter.
#CHAPTER 2
library(dplyr)
library(ggplot2)
library(tidyr)
library(GGally)
library(fastDummies)
library(ggridges)
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







##Chapter 4 Data Exercises
#use the sociological data

#load data as csv.
sociological_data<-read.csv( "http://peopleanalytics-regression-book.org/data/sociological_data.csv")
sociological_data1<-sociological_data


#4.7.2.1
#Identify the extent to which missing data is an issue
#

# explore missingness using base package
##
dim(sociological_data1)
summary(sociological_data1)

socio_na_sum<- sapply(sociological_data1, function(x) sum(is.na(x)))
#select columns with missing vaues
socio_na_sum<-socio_na_sum[socio_na_sum>0]

#arrange from most to least
socio_na_sum_desc<-sort(socio_na_sum, decreasing=TRUE)
socio_na_sum_desc


#Explore missingness using naniar package
library(naniar)
#visualize missingness. 
vis_miss(sociological_data1)



#flag columns with  missing values

# create function to flag missing values in a column with 1
miss_flag<- function(x) {
  test<-is.na(x)
  ifelse(test==TRUE, 1,0)
}


#Run function on each column with missing values.  Worte function to avoid copy pasting >3 times.

sociological_data1$an_inc_ppp_na<-miss_flag(sociological_data1$annual_income_ppp)
sociological_data1$avg_wk_hrs_na<-miss_flag(sociological_data1$average_wk_hrs)
sociological_data1$educaton_months_na<-miss_flag(sociological_data1$education_months)
sociological_data1$gender_na<-miss_flag(sociological_data1$gender)
sociological_data1$fam_size_na<-miss_flag(sociological_data1$family_size)
# same values are missing languages and work distance
sociological_data1$languages_na<-miss_flag(sociological_data1$languages)



# compare missing values by categorical variables. i.e gender, region, & work type
stats_na_gender<-sociological_data1 %>% 
  group_by(gender) %>% 
  summarize( sum(an_inc_ppp_na), sum(avg_wk_hrs_na), sum(educaton_months_na), 
             sum(gender_na), sum(fam_size_na), sum(languages_na))

#group_by region
stats_na_region<-sociological_data1 %>% 
  group_by(region) %>% 
  summarize( sum(an_inc_ppp_na), sum(avg_wk_hrs_na), sum(educaton_months_na), 
             sum(gender_na), sum(fam_size_na), sum(languages_na))

#group_by job_type
stats_na_job_type<-sociological_data1 %>% 
  group_by(job_type) %>% 
  summarize( sum(an_inc_ppp_na), sum(avg_wk_hrs_na), sum(educaton_months_na), 
             sum(gender_na), sum(fam_size_na), sum(languages_na))

#group job_type and gender
stats_na_job_gender<-sociological_data1 %>% 
  group_by(job_type, gender) %>% 
  summarize( sum(an_inc_ppp_na), sum(avg_wk_hrs_na), sum(educaton_months_na), 
             sum(gender_na), sum(fam_size_na), sum(languages_na))




#print breakdown of missing data by categorical values
stats_na_job_type
stats_na_region
stats_na_gender
stats_na_job_gender


#Plots of missing values
#barplot of missing values
barplot(socio_na_sum_desc, main="# of missing values  per column",
        xlab="Column Name")



# Conclusions on patterns in missing data:
#       Of 2618 rows, the columns with the most missing data are work_distance, languages, and family size with 412, 412, and 191 values missing respectively.
#       The rows missing work_distance are also missing languages.  The missing values are not randomly distributed. 
#       Missing information is most common in Latin America, sub-Saharan Africa, and Western Asia.  
#       Unskilled labor is missing more values than skilled labor.  Male workers are more likely to be missing family size and language data than Female. Female workers have more missing values in avg work hours.
#       Unskilled, males, are missing the most values in family_size and language. Columns for skilled females are fully populated.
#       




#plots 

#plot of avg hours vs annual income colored if missing distance or languages
#create base plot
sociological_data1 %>% 
  mutate(languages_na=as.character(languages_na)) %>% 
  #plot
  ggplot(aes(average_wk_hrs, annual_income_ppp, color=languages_na))+
  geom_point( shape=1, alpha=.7, size=2, position="jitter")
scale_colour_manual(values = c("black", "red"))+
  ggtitle("Missing languages data by Wrk hours vs annual income")+
  theme_classic()

sociological_data1 %>% 
  mutate(languages_na=as.character(languages_na)) %>% 
  #plot
  ggplot(aes(average_wk_hrs, annual_income_ppp, color=languages_na))+
  geom_point( shape=1, alpha=.7, size=2, position="jitter")
scale_colour_manual(values = c("black", "red"))+
  ggtitle("Missing languages data by Wrk hours vs annual income")+
  theme_classic()


# Data exploration for visualization
#scale wage to an hourly wage. 52 weeks in a year.
sociological_data_eda<- sociological_data %>% 
  dplyr::mutate(region= gsub(" ", "_", region)) %>% 
  dplyr::mutate(region= gsub("-", "_", region)) %>% 
  dplyr::mutate(implied_avg_hourly_wage=  annual_income_ppp/(average_wk_hrs*52)) %>% 
  dplyr::mutate(normalized_yearly_education_value= (52*40*implied_avg_hourly_wage)/ education_months) %>% 
  
  dplyr::mutate(region_simple=ifelse(grepl("nesia",region, ignore.case = TRUE), 
                                     "Pacific",
                                     
                                     ifelse(grepl("Asia", region), "Asia",
                                            
                                            ifelse( grepl("Europe", region), "Europe", 
                                                    
                                                    ifelse(grepl("America", region), "The_Americas" ,      
                                                           
                                                           ifelse(grepl("Africa", region), "Africa", 
                                                                  region
                                                                  
                                                           ))))) )  %>% 
  #rearrange column order for ease of use
  
  dplyr:: select("annual_income_ppp", "average_wk_hrs", "implied_avg_hourly_wage", "normalized_yearly_education_value","education_months", 
                 "region", "region_simple", "job_type", "gender", "family_size", "work_distance", 
                 "languages")  




# Box plot of avg_hourly wage by skill type, gender, and region_simple                                                
sociological_data_eda %>% 
  filter(., !is.na(gender)) %>% 
  ggplot(aes(x=job_type,  y=implied_avg_hourly_wage, shape=region_simple, color=region_simple ))+
  geom_boxplot()+
  facet_grid(~gender)+
  theme_bw()+
  labs(title = "Comparison of Hourly Wage ",
       subtitle = "by Skill, Gender, and Region"
       #caption = "your caption here"
       )
  
#x-plot income vs hours worked
sociological_data_eda %>% 
  ggplot(aes(x=average_wk_hrs,  y=annual_income_ppp,  color=region))+
  geom_point()+
  facet_grid(~gender)+
  theme_bw()+
  labs(title = "Comparison of Hourly Wage and hours worked ",
       subtitle = "by Gender, and Region"
       #caption = "your caption here"
  )

  # The x-plot isn't very clear.  Let's summarize the data by gender, job_type, and region
sociological_data_eda_summary<-
  sociological_data_eda %>% 
  group_by(region, region_simple, job_type, gender) %>% 
  summarize(
   mean_annual_income_ppp= mean(annual_income_ppp, na.rm=TRUE ),

   mean_average_wk_hrs=mean(average_wk_hrs, na.rm=TRUE),
   
   mean_implied_avg_hourly_wage= mean(implied_avg_hourly_wage, na.rm=TRUE),
   
   mean_education_months=mean(education_months, na.rm=TRUE),
   
   mean_family_size= mean(family_size, na.rm=TRUE)
  )

sociological_data_eda_summary %>% 
  ggplot(aes(x=mean_average_wk_hrs,  y=mean_annual_income_ppp,  color=region_simple, size=mean_implied_avg_hourly_wage))+
  geom_point()+
  facet_grid(job_type~gender)+
  theme_bw()+
  labs(title = "Comparison of Hourly Wage and hours worked ",
       subtitle = "by Gender, and Region"
       #caption = "your caption here"
  )
sociological_data_eda_summary %>% 
  ggplot(aes(x=mean_education_months,  y=mean_implied_avg_hourly_wage, group=region, color=region_simple, size=mean_implied_avg_hourly_wage))+
  geom_point(shape=1)+
  facet_grid(~gender)+
  theme_bw()+
  labs(title = "Comparison of Hourly Wage and hours worked ",
       subtitle = "by Gender, and Region"
       #caption = "your caption here"
  )


ggplot(sociological_data, aes(education_months, annual_income_ppp , size= job_type, color=region))+
  geom_point( shape=1, alpha=.7)+
  scale_size_discrete (range = c(7, 3))

#plot density plot of job_type by gender.
ggplot(sociological_data, aes( annual_income_ppp , fill=gender))+
  geom_density(alpha=.8)+facet_wrap(~job_type)


#4.7.2.2
#Determine if the data types are appropriate for analysis

#answer:
# Columns with categorical variables; region, job_type, and gender will need to be converted into "dummy" variables.  
#There are likely too many unique values in "regions" to be used as dummy variables.
#An effort will be made during subsequent exploratory data analysis (EDA) to determine how many of the regions can be lumped together.
#Gender and job_type have few enough unique values to be used as summary variables and are clearly important from the density plot and x-plot above.
#It may be problematic that there are no rows with data for "skilled" female workers.

#get structure (str) of data
glimpse(sociological_data)


#view unique values for categorical variables
unique(sociological_data$region)
unique(sociological_data$job_type)
unique(sociological_data$gender)



#4.7.2.3
#Using a correlation matrix, pairplot or alternative method, identify whether colinearity is present in the data
summary(sociological_data)
library(ggplot2)
library(GGally)

# display a pair plot of all four columns of data
sociological_data_eda %>% 
  dplyr::select(  "annual_income_ppp", "average_wk_hrs","implied_avg_hourly_wage" , "education_months", 
                  "family_size",  "work_distance", 
                  "languages", "gender","job_type"  ) %>% 
  GGally::ggpairs(. )

# Colinearity is potentially an issue (correlation coefficient >|.5| ) between our response variable, annual income_ppp, and average work hours, education months, and languages. 
# THere's also an apparent very strong relationship (r>.9) between languages and work distance.
# the strong correlation between work distance and languages doesn't make intuitive sense. 
# THe summary indicates that 95% of the data points have one language.  There are a few points pulling this correlation upward.


#4.7.2.4
#Identify and discuss anything else interesting that you see in the data
#Prepare to build a linear regression model to explain the variation in annual_income_ppp using the other data in the data set.



library(ggridges)
# create ridge plot. Effective for showing density ranges for many unique categorical variables.

sociological_data_eda %>% 
  dplyr::group_by(region_simple) %>% 
  arrange(annual_income_ppp, descending=TRUE) %>% 
  ggplot(aes(x = annual_income_ppp, y = region_simple,  fill= gender)) +
  geom_density_ridges(alpha=.6) +
  theme_ridges() +
  coord_cartesian(clip = "off")


sociological_data_eda %>% 
  arrange(annual_income_ppp, descending=TRUE) %>% 
  ggplot(aes(x = average_wk_hrs, y = region_simple, fill=gender)) +
  geom_density_ridges(alpha=.6) +
  theme_ridges() +
  coord_cartesian(clip = "off")

sociological_data_eda %>% 
  arrange(annual_income_ppp, descending=TRUE) %>% 
  ggplot(aes(x = education_months, y = region_simple, fill=gender)) +
  geom_density_ridges(alpha=.6) +
  theme_ridges() +
  coord_cartesian(clip = "off")


#xplot_ ppp income vs wrk hours







#few values of work distance are populated (unit)  Work distance is mostly "0" or NA.  Likely not useable/
table(sociological_data$work_distance)

# count values of work distance >0
n<-count(filter(sociological_data, work_distance>0))

# percent of data != 0 for populated values of work_distance
n/nrow(sociological_data %>% filter(., !is.na(work_distance)))  

# Few individuals have languages >1
table(sociological_data$languages)

#
#4.7.2.5
#Are there any fields which you believe should not be included in the model? If so, why?

#Work distance and languages could be included but it is likely they aren't going to be significant  based on the limited values that are greater than 0 and 1 respectively.  Including work_distance and languages would also include the columns with the most missing values.  THe non-random pattern of the missing vlaues observed earlier also enforces the idea of holding out this variable.


#4.7.2.6
#Would you consider imputing missing data for some or all fields where it is an issue? If so, what might be some simple ways to impute the missing data?

# Depends on the question(s) we are trying to answer.  Given the unevenness of missing values for woman in some regions if those were important we would likely consider imputing the data.

# If we are most concerned about determing the e

#

#4.7.2.7
#Which variables are categorical? Convert these variables to dummy variables using a convenient function or using your own approach.
library(fastDummies)
library(tidyr)
library(stringr)
#prepare data for modeling.  We use the drop first column argument in the function "dummy_cols" to avoid the "dummy variable trap".
# Including a dummy variable for all unique values in a column introduces multi-collinearity.

sociological_data_dummy<-  sociological_data_eda %>% 
  #dplyr::select(-region_simple) %>% 
  #remove spaces from values in "region" before creating dummy variables. SPaces cause issues in specifying the model in later steps.
  dplyr::mutate(region= str_replace(region, " ", "_")) %>% 
  #selecting all columns except work_distance and languages to reduce missingness and multicolinearity.
  dplyr::select(-work_distance, -languages) %>% 
  drop_na(.) %>% 
  fastDummies::dummy_cols(remove_first_dummy = FALSE)



#4.7.2.8
#Determine what variables are significant predictors of annual income and what is the effect of each on the outcome.

#kitchen sink model
model_1<-
  lm(data = sociological_data_dummy, formula = annual_income_ppp ~    average_wk_hrs  +  
       education_months  + job_type_Unskilled  +  gender_F+ family_size + 
       region_Central_Asia +  region_Eastern_Asia  + region_Polynesia  +  region_Eastern_Europe  +  
       region_Melanesia  +  region_Micronesia  +  region_Northern_Europe  +  region_South_eastern_Asia  +
       region_Northern_Africa  + region_Northern_America  +  region_Sub_Saharan_Africa  +
       region_Southern_Asia  +  region_Southern_Europe +  region_Western_Asia  + region_Latin_America_and_the_Caribbean 
       )


summary(model_1)
# family size, southern_Europe are not significant.  Average wk_hours is not significant but very close. It appears the region variables account for much of the variance in average wk_hourse. 
#r2 .7956



model_3<-
  lm(data = sociological_data_dummy, formula = annual_income_ppp ~        
       education_months  + job_type_Unskilled  +  gender_F + 
       region_Central_Asia +  region_Eastern_Asia  + region_Polynesia  +  region_Eastern_Europe  +  
       region_Melanesia  +  region_Micronesia  +  region_Northern_Europe  +  region_South_eastern_Asia  +
       region_Northern_Africa  + region_Northern_America  +  region_Sub_Saharan_Africa  +
       region_Southern_Asia   +  region_Western_Asia  + region_Latin_America_and_the_Caribbean  +
       gender_F * job_type_Unskilled)
 summary(model_3)
model3_summary$coefficients



#4.7.2.9
#Determine the overall fit of the model.

#The p-value for the F statistic is quite small so we conclude our  model fits better than a random model.





#4.7.2.10
#Do some simple analysis on the residuals of the model to determine if the model is safe to interpret.

#get model predictions
hist(resid(model_2))

#Plot Actual vs predictions
sociological_data_dummy_resid<-  sociological_data_dummy  %>% 
dplyr::mutate(predicted=predict(model_2)) %>% 
  dplyr::mutate(residual=resid(model_2))  
 
ggplot(sociological_data_dummy_resid, aes(x=predicted, y= annual_income_ppp, color=region))+
  geom_point(size=3,shape=1, alpha=.4)+
  labs(title="Predicted vs annual income (ppp)")+
  theme_bw()
  

plot(predict(model_2),sociological_data_dummy$annual_income_ppp , col = rep(1:2),
     xlab="predicted",ylab="actual")
abline(a=0,b=1)
#
#Plot Actual vs predictions
sociological_data_dummy_resid<-  sociological_data_dummy  %>% 
  dplyr::mutate(predicted=predict(model_3)) %>% 
  dplyr::mutate(residual=resid(model_3))  

resid_job_type<-ggplot(sociological_data_dummy_resid, aes(x=predicted, y= annual_income_ppp, color=job_type))+
  geom_point(size=3,shape=1, alpha=.4)+
  labs(title="Predicted vs annual income (ppp)",  subtitle = "colored by skill type")+
  theme_bw()

resid_gender<-ggplot(sociological_data_dummy_resid, aes(x=predicted, y= annual_income_ppp, color=gender_M))+
  geom_point(size=3,shape=1, alpha=.4)+
  labs(title="Predicted vs annual income (ppp)", subtitle = "colored by gender")+
  theme_bw()

resid_region<-ggplot(sociological_data_dummy_resid, aes(x=predicted, y= annual_income_ppp, color=region))+
  geom_point(size=3,shape=1, alpha=.4)+
  labs(title="Predicted vs annual income (ppp)", subtitle = "colored by region")+
  theme_bw()

plot(predict(model_3),sociological_data_dummy$annual_income_ppp , col = rep(1:2),
     xlab="predicted",ylab="actual")
abline(a=0,b=1)

plot(predict(model_2), resid(model_2),      
     xlab="predicted",ylab="residuals"
     )
abline(h=0)

# REsiduals are "mound-shaped" and appear to be normally distributed. Normally distributed error is one underlying assumption of linear regression

#4.7.2.11
#Experiment with improving the model fit through possible interaction terms or non-linear extensions.




#4.7.2.12
#Comment on your results. Did anything in the results surprise you? If so, what might be possible explanations for this.

#avg_wk_hrs was not not significant.  It seems that gender and job type are covaries with avg wk hours and the differences in salary observed in short and long work weeks can be explained by other variables.


#4.7.2.13
#Explain why you would or would not be comfortable using a model like this in a predictive setting - for example to help employers determine the right pay for employees.
# My comfort with this model is dependent on the use case.  Using a model such as this to determine an employees pay would be appropriate when determining pay based on location, education, and job type but would be inappropriate when considering gender.
#THe data set has minimal rows with the "skilled"  "female" employees. IN some countries adjusting a the salary of a potential employee downward because of gender could be immoral or illegal. 
#If you were working with a non-profit to assess the value of education in certain parts of the developing world, then it would be appropriate to include gender as a predictor.  






#Chapter 5

#View the data and obtain statistical summaries. Ensure data types are appropriate and there is no missing data. Determine the outcome and input variables.
#Using a pairplot or by plotting or correlating selected fields, try to hypothesize which variables may be significant in explaining who recently donated.
#Run a binomial logistic regression model using all input fields. Determine which input variables have a significant effect on the outcome and the direction of that effect.
#Calculate the odds ratios for the significant variables and explain their impact on the outcome
#Check for collinearity or multicollinearity in your model using methods from previous chapters.
#Experiment with model parsimony by reducing input variables that do not have a significant impact on the outcome. Decide on the most parsimonious model.
#Calculate a variety of Pseudo-
#  R
#2
#variants for your model. How would you explain these to someone with no statistics expertise?
#  Report the conclusions of your modeling exercise to the charity by writing a simple explanation that assumes no knowledge of statistics.
#Extension: Using a variety of methods of your choice, test the hypothesis that your model fits the data. How conclusive are your tests?
#
#