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



