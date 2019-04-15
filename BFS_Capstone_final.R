# Group Project   - CredX BFS Capstone Project
#
# Group Members   - 1.  Samiksha - itsmesamiksha@gmail.com
#                   2.  Sai Teja - saiteja.ece@gmail.com
#                   3.  Dinesh - dineshpulicharla@gmail.com
#                   4.  Sasi Bhushan- sasi.bsb@gmail.com
#
# Submission date - 21/10/2018

#Libraries
# install.packages("ggplot2")
# install.packages("MASS")
# install.packages("car")
# install.packages("ggthemes")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("Information")
# install.packages("e1071")
# install.packages("caret")
# install.packages("cowplot")
# install.packages("caTools")
# install.packages("Hmisc")
# install.packages("DMwR")
# install.packages("ROCR")
# install.packages("ranger")
# install.packages("GGally")
# install.packages("randomForest")
# install.packages("stringr")

library(ggplot2)
library(MASS)
library(car)
library(ggthemes)
library(dplyr)
library(tidyr)
library(Information)
library(e1071)
library(caret)
library(cowplot)
library(caTools)
library(ranger)
library(Hmisc)
library(ROCR)
library(DMwR)
library(GGally)
library(randomForest)
library(stringr)


##################################Business Problem Statement######################################################
#CredX is a leading credit card provider that gets thousands of credit card applicants
#every year. But in the past few years, it has experienced an increase in credit loss. 
#The CEO believes that the best strategy to mitigate credit risk is to 'acquire the 
#right customers'.
##################################################################################################################

####################################Data Understanding###########################################################
#Set Directory
#setwd("~/Downloads/BFS Project")

####################################Import Data Set#############################################################
credx_bureau <- read.csv(file = "Credit Bureau data.csv",header = TRUE,stringsAsFactors = FALSE)
credx_demo <- read.csv(file = "Demographic data.csv",header = TRUE,stringsAsFactors = FALSE)

##################################################################################################################
summary(credx_bureau)
summary(credx_demo)
##################################################################################################################
nrow(credx_bureau)
nrow(credx_demo)
ncol(credx_bureau)
ncol(credx_demo)
# summary of data understanding
#Both data set has same number of observation. There are two file demographic data and credit card user behavioral
#data.Bureau has 19 variables where as demographic has only 12 variables
#Performance.Tag is dependent variable .
#credit Bureau data has all integer values.
#credit bureau data has 18 independent variable, 1 dependent variable "Performance.Tag" .
#The demographic data has 11 independent variabe and 1 dependent variable "Performance.Tag",
#with 7 integer and 5 character variable.
str(credx_bureau)
str(credx_demo)
View(credx_demo)
View(credx_bureau)

###############################DATA CLEANSING##############################################################
#Removal of Duplicated
#removal of NA
#removal of Blank spaces
##########################################################################################################
sapply(credx_demo,function(x) sum(is.na(x)))
sapply(credx_bureau,function(x) sum(is.na(x)))
sum(is.na(credx_demo))
sum(is.na(credx_bureau))

# Demographic data has 1428 NA values and credit data has 3028 NA values
# reading Blank,NA, and empty value as NA 
credx_demo <- read.csv(file = "Demographic data.csv",header = TRUE,na.strings = c(""," ","NA",NA))

#deduplication 
x <- credx_demo[which(duplicated(credx_demo$Application.ID)),]

# Three duplicate application id exists.

View(credx_demo[credx_demo$Application.ID %in% c(765011468,653287861,671989187),])

#All the 3 observation have duplicate application id , there could be error in recording those transaction
#removing those observation from our demographic data

credx_demo <- credx_demo[-which(duplicated(credx_demo$Application.ID) == T), ]
credx_bureau <- credx_bureau[-which(duplicated(credx_bureau$Application.ID) == T), ]

#rechecking to see if credit data has any duplicate
View(credx_bureau[which(duplicated(credx_bureau$Application.ID)),])

#credx_bureau <- credx_bureau[-which(duplicated(credx_bureau$Application.ID)),]
#Creating a master file by merging both demographic and credit bureau data.
#first we will create a vector with the column name of demo and credit
#The dependent variable has 1425 NA's, which means that this population has been denied loan. 
Demographic <- colnames(credx_demo)
Credit_bureau <- colnames(credx_bureau)

##############creation of a master file which demographic and credit bureau data
Master_Data<- merge(x = unique(credx_demo), y = unique(credx_bureau), by = c("Application.ID", "Performance.Tag"))
summary(Master_Data)
str(Master_Data)

#the Performance.Tag variable has three value : 0,1, Blank.
# All the Blank data 1425 is for the user whose application has been rejected.
# we are taking them into a data frame
validation <- Master_Data[which(is.na(Master_Data$Performance.Tag)),]

#To check if the Data set is not balanced or not.
reject_rate<-nrow(validation)/nrow(Master_Data)
reject_rate*100 
approve_rate<-1-reject_rate
approve_rate*100 
#approval rate of 98%
#Rejected Rate is around 2 percent .
#It implies that the data set is imbalace.

#################################################################################################################
#removing All the data with Performance Tag Blank.
Master_Data <- Master_Data[-which(is.na(Master_Data$Performance.Tag)),]
Master_Data_copy <- Master_Data
nrow(Master_Data)
sum(is.na(Master_Data))
(sum(is.na(Master_Data))/nrow(Master_Data))*100
#If we observer, we will notice that 2.5% of data set has NA value.
#we need to first check which observation has missing values
missing_Variable <- sapply(Master_Data,function(x) sum(is.na(x)))
missing_Variable
### Gender : 2 
### Marital.Status..at.the.time.of.application. :6 
### No.of.dependents :3 
### Education:118 
### Profession : 13 
### Type.of.residence :8 
### Avgas.CC.Utilization.in.last.12.months : 1023 
### No.of.trades.opened.in.last.6.months :1 
### Presence.of.open.home.loan :272 
### Outstanding.Balance :272 
Master_Data <- Master_Data[-which(is.na(Master_Data$Gender)),]
Master_Data <- Master_Data[-which(is.na(Master_Data$Marital.Status..at.the.time.of.application.)),]
Master_Data <- Master_Data[-which(is.na(Master_Data$No.of.dependents)),]
Master_Data <- Master_Data[-which(is.na(Master_Data$Profession)),]
Master_Data <- Master_Data[-which(is.na(Master_Data$Type.of.residence)),]
Master_Data <- Master_Data[-which(is.na(Master_Data$Education)),]
# Imputing the value of NA with median when there is a considerable amount of dataset.
Master_Data$No.of.trades.opened.in.last.6.months[which(is.na(Master_Data$No.of.trades.opened.in.last.6.months)==1)]=median(Master_Data$No.of.trades.opened.in.last.6.months, na.rm = T)
Master_Data$Presence.of.open.home.loan[which(is.na(Master_Data$Presence.of.open.home.loan)==1)] = median(Master_Data$Presence.of.open.home.loan,na.rm = T)
Master_Data$Outstanding.Balance[which(is.na(Master_Data$Outstanding.Balance)==1)] = median(Master_Data$Outstanding.Balance,na.rm = T)
Master_Data$Avgas.CC.Utilization.in.last.12.months[which(is.na(Master_Data$Avgas.CC.Utilization.in.last.12.months)==1)] = 0

#All Values of NA has been removed. which is about 1.6% of total observation.
((69867 - nrow(Master_Data))/69867)*100
#Removed 1.6 % of the observation which has NA values.


###################################--------OUTLIER TREATMENT -------#####################################

## Step 1 : Age 
str(Master_Data$Age)
ggplot(Master_Data,aes(Master_Data$Age))+geom_histogram()
(quantile(Master_Data$Age,seq(0,1,0.01)))
boxplot(Master_Data$Age)
Master_Data[(which(Master_Data$Age<=18)),]$Age <- 18

#Most Users are from 30 - 50 age.
#there are some user which are below 18 , so removing all the data below 18 to 18.
# Binning the age variable and store it into "binning.age".

Master_Data$binning.Age <- as.factor(cut(Master_Data$Age, include.lowest = TRUE,
                                         breaks = c(18, 30, 40, 50, 60, 70)))

#creating bin : 18-30,30-40,40-50,50-60,60-70
#Function to create response rate 
# Writing a function "plot_response" to do the same task for each variable
plot_response <- function(cat_var, var_name){
  a <- aggregate(Performance.Tag~cat_var, Master_Data, mean)
  count <- data.frame(table(cat_var))
  count <- count[,-1]
  agg_response <- cbind(a, count)
  colnames(agg_response) <- c(var_name, "response_rate","No.of_Prospect")
  agg_response[, 2] <- format(round(agg_response[, 2], 2))
  
  ggplot(agg_response, aes(agg_response[, 1], count, label = response_rate)) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5) + xlab(var_name)
  
}
# Plotting age
plot_response(Master_Data$binning.Age,"Binning.Age")
#Removing the Age variable 
Master_Data$Age<- NULL
#-------------------------------------------#
## Step 2 : Gender 
#converting to factor variable
Master_Data$Gender<-as.factor(Master_Data$Gender)
# Plotting Gender histogram
ggplot(Master_Data,aes(Master_Data$Gender))+geom_histogram(stat="count")
# Plotting Gender response
plot_response(Master_Data$Gender,"Gender")

#Most user are from Male
#-------------------------------------------#

## Step 3 : Marital Status
#converting to factor variable
Master_Data$Marital.Status..at.the.time.of.application.<-as.factor(Master_Data$Marital.Status..at.the.time.of.application.)
ggplot(Master_Data,aes(Master_Data$Marital.Status..at.the.time.of.application.))+geom_histogram(stat="count")
plot_response(Master_Data$Marital.Status..at.the.time.of.application.,"Marital Status")

#Most user are Married.
#-------------------------------------------#
#Step 4 : No. of dependents

Master_Data$No.of.dependents<-as.factor(Master_Data$No.of.dependents)

ggplot(Master_Data,aes(Master_Data$No.of.dependents))+geom_histogram(stat="count")
plot_response(as.factor(Master_Data$No.of.dependents),"No.of.dependents")

#Most user has no of dependent with 1,2,3 .

#-------------------------------------------#
## Step 5 : Income 
ggplot(Master_Data,aes(Master_Data$Income))+geom_histogram()
(quantile(Master_Data$Income,seq(0,1,0.01)))
boxplot(Master_Data$Income)
Master_Data[(which(Master_Data$Income<=4.5)),]$Income <- 4.5
#There are outlier present in the income. 

Master_Data$binning.Income <- as.factor(cut(Master_Data$Income, include.lowest = TRUE,
                                            breaks = c(4.5, 15, 25, 35, 45, 55, 60)))
plot_response(Master_Data$binning.Income,"Binning.Income")
# Binning Income created : 4.5-15,15-25,25-35,35-45,45-55,55-60
Master_Data$Income<- NULL

#-------------------------------------------#
## Step 6 : Education
#converting to factor variable
Master_Data$Education<-as.factor(Master_Data$Education)
ggplot(Master_Data,aes(Master_Data$Education))+geom_histogram(stat="count")
plot_response(as.factor(Master_Data$Education),"Education")
#Most credit card user are professional
#-------------------------------------------#
## Step 7 : Profession
Master_Data$Profession<-as.factor(Master_Data$Profession)
ggplot(Master_Data,aes(Master_Data$Profession))+geom_histogram(stat="count")
plot_response(as.factor(Master_Data$Profession),"Profession")
#Most credit card user are salaried.
#-------------------------------------------#
## Step 7 : Type of residence
Master_Data$Type.of.residence<-as.factor(Master_Data$Type.of.residence)
ggplot(Master_Data,aes(Master_Data$Type.of.residence))+geom_histogram(stat="count")
plot_response(as.factor(Master_Data$Type.of.residence),"Type.of.residence")
#most user are having rented place.
#-------------------------------------------#

## Step 7 : No of months in current residence
ggplot(Master_Data,aes(Master_Data$No.of.months.in.current.residence))+geom_histogram()
(quantile(Master_Data$No.of.months.in.current.residence,seq(0,1,0.01)))
boxplot(Master_Data$No.of.months.in.current.residence)
Master_Data$No.of.months.in.current.residence <- as.factor(cut(Master_Data$No.of.months.in.current.residence, breaks = c(5, 9, 28, 49, 72, 97, 126)))
plot_response(Master_Data$No.of.months.in.current.residence,"No.of.months.in.current.residence")

#-------------------------------------------#
# Step 8 : No.of.months.in.current.company
ggplot(Master_Data,aes(Master_Data$No.of.months.in.current.company))+geom_histogram()
(quantile(Master_Data$No.of.months.in.current.company,seq(0,1,0.01)))
boxplot(Master_Data$No.of.months.in.current.company)
Master_Data[(which(Master_Data$No.of.months.in.current.company>74)),]$No.of.months.in.current.company <- 74
Master_Data$binning.No.of.months.in.current.company <- as.factor(cut(Master_Data$No.of.months.in.current.company, breaks = c(2, 13, 26, 33, 40, 61, 74)))
#Capping outliers, which are greater than 74. As 99 percentile is 74 and 100th percentile is 133.
# Binning the variable and store it into another variables.
plot_response(Master_Data$binning.No.of.months.in.current.company,"No.of.months.in.current.company")
Master_Data$No.of.months.in.current.company=NULL

#------------------------------------------#
# Step 9 : No.of.times.90.DPD.or.worse.in.last.6.months
#converting to factor variable
Master_Data$No.of.times.90.DPD.or.worse.in.last.6.months<-as.factor(Master_Data$No.of.times.90.DPD.or.worse.in.last.6.months)
ggplot(Master_Data,aes(Master_Data$No.of.times.90.DPD.or.worse.in.last.6.months))+geom_histogram(stat="count")
plot_response(as.factor(Master_Data$No.of.times.90.DPD.or.worse.in.last.6.months),"No.of.times.90.DPD.or.worse.in.last.6.months")
Master_Data$No.of.times.90.DPD.or.worse.in.last.6.months<-as.factor(Master_Data$No.of.times.90.DPD.or.worse.in.last.6.months)
#--------------------------------------------#
ggplot(Master_Data,aes(Master_Data$No.of.times.90.DPD.or.worse.in.last.6.months))+geom_histogram(stat="count")
plot_response(as.factor(Master_Data$No.of.times.90.DPD.or.worse.in.last.6.months),"No.of.times.90.DPD.or.worse.in.last.6.months")
#-----------------------------------------------------#
# Step 10 : No.of.times.60.DPD.or.worse.in.last.6.months
Master_Data$No.of.times.60.DPD.or.worse.in.last.6.months<-as.factor(Master_Data$No.of.times.60.DPD.or.worse.in.last.6.months)
ggplot(Master_Data,aes(Master_Data$No.of.times.60.DPD.or.worse.in.last.6.months))+geom_histogram(stat="count")
plot_response(as.factor(Master_Data$No.of.times.60.DPD.or.worse.in.last.6.months),"No.of.times.60.DPD.or.worse.in.last.6.months")
#-----------------------------------------------------#
## Step 11 : No.of.times.30.DPD.or.worse.in.last.6.months
Master_Data$No.of.times.30.DPD.or.worse.in.last.6.months<-as.factor(Master_Data$No.of.times.30.DPD.or.worse.in.last.6.months)
ggplot(Master_Data,aes(Master_Data$No.of.times.30.DPD.or.worse.in.last.6.months))+geom_histogram(stat="count")
plot_response(as.factor(Master_Data$No.of.times.30.DPD.or.worse.in.last.6.months),"No.of.times.30.DPD.or.worse.in.last.6.months")
#-----------------------------------------------------#
## Step 12 : No.of.times.90.DPD.or.worse.in.last.12.months
Master_Data$No.of.times.90.DPD.or.worse.in.last.12.months<-
  as.factor(Master_Data$No.of.times.90.DPD.or.worse.in.last.12.months)
ggplot(Master_Data,aes(Master_Data$No.of.times.90.DPD.or.worse.in.last.12.months))+
  geom_histogram(stat="count")
plot_response(as.factor(Master_Data$No.of.times.90.DPD.or.worse.in.last.12.months),"No.of.times.90.DPD.or.worse.in.last.6.months")
#-----------------------------------------------------#
# Step 13 : No.of.times.60.DPD.or.worse.in.last.12.months
Master_Data$No.of.times.60.DPD.or.worse.in.last.12.months<-
  as.factor(Master_Data$No.of.times.60.DPD.or.worse.in.last.12.months)
ggplot(Master_Data,aes(Master_Data$No.of.times.60.DPD.or.worse.in.last.12.months))+geom_histogram(stat="count")
plot_response(as.factor(Master_Data$No.of.times.60.DPD.or.worse.in.last.12.months),"No.of.times.60.DPD.or.worse.in.last.12.months")

#-----------------------------------------------------#
# Step 14 : No.of.times.30.DPD.or.worse.in.last.12.months
Master_Data$No.of.times.30.DPD.or.worse.in.last.12.months<-
  as.factor(Master_Data$No.of.times.30.DPD.or.worse.in.last.12.months)
ggplot(Master_Data,aes(Master_Data$No.of.times.30.DPD.or.worse.in.last.12.months))+geom_histogram(stat="count")
plot_response(as.factor(Master_Data$No.of.times.30.DPD.or.worse.in.last.6.months),
              "No.of.times.30.DPD.or.worse.in.last.12.months")
##-----------------------------##
## Step 15 : Avgas.CC.Utilization.in.last.12.months
ggplot(Master_Data,aes(Master_Data$Avgas.CC.Utilization.in.last.12.months))+geom_histogram()
(quantile(Master_Data$Avgas.CC.Utilization.in.last.12.months,seq(0,1,0.01)))
boxplot(Master_Data$Avgas.CC.Utilization.in.last.12.months)
#Capping outliers, which are greater than 91. 
Master_Data[(which(Master_Data$Avgas.CC.Utilization.in.last.12.months>91)),]$
  Avgas.CC.Utilization.in.last.12.months <- 91
# Binning the variable and store it into another variables.
Master_Data$binning.Avgas.CC.Utilization.in.last.12.months <- 
  as.factor(cut(Master_Data$Avgas.CC.Utilization.in.last.12.months, include.lowest = TRUE,
                breaks = c(0, 8, 14, 21, 51, 71, 91)))
plot_response(Master_Data$binning.Avgas.CC.Utilization.in.last.12.months,
              "binning.Avgas.CC.Utilization.in.last.12.months")
Master_Data$Avgas.CC.Utilization.in.last.12.months=NULL
##-----------------------------##
# step 16 :No.of.trades.opened.in.last.6.months
# Plotting No.of.trades.opened.in.last.6.months histogram
ggplot(Master_Data,aes(Master_Data$No.of.trades.opened.in.last.6.months))+geom_histogram(stat="count")
(quantile(Master_Data$No.of.trades.opened.in.last.6.months,seq(0,1,0.01)))
boxplot(Master_Data$No.of.trades.opened.in.last.6.months)
#Capping outliers, which are greater than 6. 
Master_Data[(which(Master_Data$No.of.trades.opened.in.last.6.months>6)),]$
  No.of.trades.opened.in.last.6.months <- 6
#converting to factor variable
Master_Data$No.of.trades.opened.in.last.6.months<-
  as.factor(Master_Data$No.of.trades.opened.in.last.6.months)
plot_response(as.factor(Master_Data$No.of.trades.opened.in.last.6.months),
              "No.of.trades.opened.in.last.6.months")

##-----------------------------##
# step 17 :No.of.trades.opened.in.last.12.months
ggplot(Master_Data,aes(Master_Data$No.of.trades.opened.in.last.12.months))+geom_histogram(stat="count")
(quantile(Master_Data$No.of.trades.opened.in.last.12.months,seq(0,1,0.01)))
boxplot(Master_Data$No.of.trades.opened.in.last.12.months)
#Capping outliers, which are greater than 18. 
Master_Data[(which(Master_Data$No.of.trades.opened.in.last.12.months>18)),]$
  No.of.trades.opened.in.last.12.months <- 18
# Binning the variable and store it into another variables.
Master_Data$binning.No.of.trades.opened.in.last.12.months <- 
  as.factor(cut(Master_Data$No.of.trades.opened.in.last.12.months, include.lowest = TRUE,
                breaks = c(0, 2, 5, 8, 10, 13, 18)))
plot_response(as.factor(Master_Data$binning.No.of.trades.opened.in.last.12.months),
              "No.of.trades.opened.in.last.12.months")
Master_Data$No.of.trades.opened.in.last.12.months<-NULL
# step 18 :No.of.PL.trades.opened.in.last.6.months
# Plotting No.of.PL.trades.opened.in.last.6.months histogram
ggplot(Master_Data,aes(Master_Data$No.of.PL.trades.opened.in.last.6.months))+geom_histogram(stat="count")
(quantile(Master_Data$No.of.PL.trades.opened.in.last.6.months,seq(0,1,0.01)))
boxplot(Master_Data$No.of.PL.trades.opened.in.last.6.months)
#Capping outliers, which are greater than 5. 
Master_Data[(which(Master_Data$No.of.PL.trades.opened.in.last.6.months>5)),]$
  No.of.PL.trades.opened.in.last.6.months <- 5
Master_Data$No.of.PL.trades.opened.in.last.6.months<-
  as.factor(Master_Data$No.of.PL.trades.opened.in.last.6.months)
plot_response(as.factor(Master_Data$No.of.PL.trades.opened.in.last.6.months),
              "No.of.PL.trades.opened.in.last.6.months")

##--------------------------------------------##
# step 19 :No.of.PL.trades.opened.in.last.12.months
# Plotting No.of.PL.trades.opened.in.last.12.months histogram
ggplot(Master_Data,aes(Master_Data$No.of.PL.trades.opened.in.last.12.months))+geom_histogram(stat="count")
(quantile(Master_Data$No.of.PL.trades.opened.in.last.12.months,seq(0,1,0.01)))
boxplot(Master_Data$No.of.PL.trades.opened.in.last.12.months)
#Capping outliers, which are greater than 9. 
Master_Data[(which(Master_Data$No.of.PL.trades.opened.in.last.12.months>9)),]$
  No.of.PL.trades.opened.in.last.12.months <- 9
# Binning the variable and store it into another variables.
Master_Data$binning.No.of.PL.trades.opened.in.last.12.months <- 
  as.factor(cut(Master_Data$No.of.PL.trades.opened.in.last.12.months, include.lowest = TRUE,
                breaks = c(0, 1, 2, 3, 5, 6, 9)))
plot_response(as.factor(Master_Data$binning.No.of.PL.trades.opened.in.last.12.months),
              "No.of.PL.trades.opened.in.last.12.months")
Master_Data$No.of.PL.trades.opened.in.last.12.months<-NULL

##------------------------------------------##
# step 20 :No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.
ggplot(Master_Data,aes(Master_Data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.))+geom_histogram(stat="count")
(quantile(Master_Data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.,seq(0,1,0.01)))
boxplot(Master_Data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)
#Capping outliers, which are greater than 7. 
Master_Data[(which(Master_Data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.>7)),]$
  No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. <- 7
Master_Data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.<-
  as.factor(Master_Data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)
plot_response(as.factor(Master_Data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.),
              "No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.")

##-----------------------------##
# step 21 :No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.
ggplot(Master_Data,aes(Master_Data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.))+geom_histogram(stat="count")
(quantile(Master_Data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,seq(0,1,0.01)))
boxplot(Master_Data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)
#Capping outliers, which are greater than 12. 
Master_Data[(which(Master_Data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.>12)),]$
  No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. <- 12
# Binning the variable and store it into another variables.
Master_Data$binning.No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. <- 
  as.factor(cut(Master_Data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., 
                include.lowest = TRUE, breaks = c(0, 1, 2, 4, 6, 8, 12)))
# Plotting response
plot_response(as.factor(Master_Data$binning.No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.),
              "binning.No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.")


Master_Data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.<-NULL
##-----------------------------##


## Step 22 : Presence.of.open.auto.loan
Master_Data$Presence.of.open.auto.loan<-
  as.factor(Master_Data$Presence.of.open.auto.loan)

ggplot(Master_Data,aes(Master_Data$Presence.of.open.auto.loan))+geom_histogram(stat="count")
plot_response(as.factor(Master_Data$Presence.of.open.auto.loan),
              "Presence.of.open.auto.loan")

##-----------------------------##
## Step 23 : Presence.of.open.home.loan
Master_Data$Presence.of.open.home.loan<-
  as.factor(Master_Data$Presence.of.open.home.loan)
ggplot(Master_Data,aes(Master_Data$Presence.of.open.home.loan))+geom_histogram(stat="count")
plot_response(as.factor(Master_Data$Presence.of.open.home.loan),
              "Presence.of.open.home.loan")
##-----------------------------##
# step 24 :Total.No.of.Trades
# Plotting Total.No.of.Trades histogram
ggplot(Master_Data,aes(Master_Data$Total.No.of.Trades))+geom_histogram(stat="count")
(quantile(Master_Data$Total.No.of.Trades,seq(0,1,0.01)))
boxplot(Master_Data$Total.No.of.Trades)
#Capping outliers, which are greater than 21. 
Master_Data[(which(Master_Data$Total.No.of.Trades>21)),]$
  Total.No.of.Trades <- 21
# Binning the variable and store it into another variables.
Master_Data$binning.Total.No.of.Trades <- 
  as.factor(cut(Master_Data$Total.No.of.Trades, include.lowest = TRUE,
                breaks = c(0, 2, 4, 6, 10, 19, 21)))
plot_response(as.factor(Master_Data$binning.Total.No.of.Trades),
              "binning.Total.No.of.Trades")
Master_Data$Total.No.of.Trades<-NULL
##-----------------------------##
# step 25 :Outstanding.Balance
# Plotting Outstanding.Balance histogram
ggplot(Master_Data,aes(Master_Data$Outstanding.Balance))+geom_histogram(stat="count")
# Let's check the outlier in the variables 
(quantile(Master_Data$Outstanding.Balance,seq(0,1,0.01)))
boxplot(Master_Data$Outstanding.Balance)
#Capping outliers, which are greater than 4251446 
Master_Data[(which(Master_Data$Outstanding.Balance>4251446)),]$
  Outstanding.Balance <- 4251446
# Binning the variable and store it into another variables.
Master_Data$binning.Outstanding.Balance <- 
  as.factor(cut(Master_Data$Outstanding.Balance, include.lowest = TRUE,
                breaks = c(0,584,6852,25604,386878,585402,774165,972299,1357216,2960909,3282409,4251446)))
# Plotting response
plot_response(as.factor(Master_Data$binning.Outstanding.Balance),
              "binning.Outstanding.Balance")
Master_Data$Outstanding.Balance<-NULL
##-----------------------------##

#checking the structure of dataframe
summary(Master_Data)
str(Master_Data)
#######################################################################################
#######################################################################################
###########End of univariate EDA##################################################
###############################################################################
#Finding correlation between variables
library(corrplot)

non_numeric_column <- c("Gender","Marital.Status..at.the.time.of.application." 
               ,"Education","Profession","Type.of.residence")

numeric_column <-c('Age','Income','No.of.months.in.current.residence','No.of.months.in.current.company'
                ,'Total.No.of.Trades','Outstanding.Balance','Avgas.CC.Utilization.in.last.12.months'
                ,'No.of.times.90.DPD.or.worse.in.last.6.months','No.of.times.60.DPD.or.worse.in.last.6.months','No.of.times.30.DPD.or.worse.in.last.6.months'
                ,'No.of.times.90.DPD.or.worse.in.last.12.months','No.of.times.60.DPD.or.worse.in.last.12.months','No.of.times.30.DPD.or.worse.in.last.12.months'
                ,'No.of.trades.opened.in.last.6.months','No.of.trades.opened.in.last.12.months'
                ,'No.of.PL.trades.opened.in.last.6.months','No.of.PL.trades.opened.in.last.6.months'
                ,'No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.'
                ,'No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.'
                ,'No.of.PL.trades.opened.in.last.12.months','Presence.of.open.home.loan','Presence.of.open.auto.loan')

dataframe_for_correlation <- Master_Data_copy[,numeric_column]
# # Correlation matrix of variables
corr_mat = round(cor(dataframe_for_correlation, use = "pairwise.complete.obs"), 2)

corrplot(cor(dataframe_for_correlation, use = "pairwise.complete.obs"), type = "upper", tl.pos = "td",
         method = "shade", tl.cex = .001, tl.col = 'black',
         order = "hclust", diag = FALSE)

View(corr_mat)
#######Analysis after correlation
#"Total.No.of.Trades"  has a positive corelation with "Outstanding.Balance"                                            
# there iss27% correlation in number of months in current residence and avg credit utilisation
###############################################################################

#creating a copy of cleaned master data
Master_Data_copy2<-Master_Data

#We need to remove the Application ID since its a key variables to access each data points, and its not useful
#for model building
Applicant <- Master_Data[,1]
Master_Data <- Master_Data[,2:29]
#checking column names of the variable
colnames(Master_Data)
################################################################################  
#######################EDA USING WOE AND IV####################################


#Weight Of Evidence and IV Analysis:
str(Master_Data)
#Creating a Information value table using create_infotables command
IV <- create_infotables(data = Master_Data,y = "Performance.Tag",parallel = TRUE)
info_val<-IV
info_val$Tables
info_val$Summary

grid.table(IV$Summary[seq(from=1,to=20,by=1),], rows=NULL)

plotFrame <- IV$Summary[order(-IV$Summary$IV), ]
plotFrame$Variable <- factor(plotFrame$Variable, levels = plotFrame$Variable[order(-plotFrame$IV)])
ggplot(plotFrame, aes(x = Variable, y = IV)) +
  geom_bar(width = .35, stat = "identity", color = "blue", fill = "blue") +
  ggtitle("Information Value") +
  theme_bw() +
  theme(plot.title = element_text(size = 10)) +
  theme(axis.text.x = element_text(angle = 90))

plotFrame

# We see that "No of Inquiries in last 12 months excluding home auto loans", 
#"Avgas CC Utilization in last 12 months", "No of PL trades
# opened in last 12 months", "No of trades opened in last 12 months" 
#has IV more than 0.3 which indicates that these variable have Strong
# predictive Power where as "Outstanding Balance" 
#and "Total No of Trade" has Medium predictive Power.
#Variable           IV
#23                          binning.Avgas.CC.Utilization.in.last.12.months 0.2944024528
#24                           binning.No.of.trades.opened.in.last.12.months 0.2918215132
#25                        binning.No.of.PL.trades.opened.in.last.12.months 0.2572140727
#26 binning.No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. 0.2491086513
#28                                             binning.Outstanding.Balance 0.2466235727
#11                            No.of.times.30.DPD.or.worse.in.last.6.months 0.2447735367
#27                                              binning.Total.No.of.Trades 0.2265574955
#16                                 No.of.PL.trades.opened.in.last.6.months 0.2228041793
#14                           No.of.times.30.DPD.or.worse.in.last.12.months 0.2181404894
#12                           No.of.times.90.DPD.or.worse.in.last.12.months 0.2159543700
#10                            No.of.times.60.DPD.or.worse.in.last.6.months 0.2115643766
#17          No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. 0.2080386172
#15                                    No.of.trades.opened.in.last.6.months 0.1891432219
#13                           No.of.times.60.DPD.or.worse.in.last.12.months 0.1880632521
#9                             No.of.times.90.DPD.or.worse.in.last.6.months 0.1626709730
#8                                        No.of.months.in.current.residence 0.0785210431
#21                                                          binning.Income 0.0344673784
#22                                 binning.No.of.months.in.current.company 0.0199623224
#18                                              Presence.of.open.home.loan 0.0167695330
#4                                                         No.of.dependents 0.0025982773
#6                                                               Profession 0.0022940841
#19                                              Presence.of.open.auto.loan 0.0016073931
#1                                                           Application.ID 0.0013805793
#20                                                             binning.Age 0.0011887691
#7                                                        Type.of.residence 0.0008773352
#5                                                                Education 0.0008008174
#2                                                                   Gender 0.0003225663
#3                              Marital.Status..at.the.time.of.application. 0.0001106485
View(IV$Tables)
#Checking to see the distribution of the age:woe values
plot_infotables(IV,"binning.Age")
plot_infotables(IV,"Gender")
plot_infotables(IV,"Marital.Status..at.the.time.of.application.")
plot_infotables(IV,"No.of.dependents")
plot_infotables(IV,"binning.Income")
plot_infotables(IV,"Education")
plot_infotables(IV,"Profession")
plot_infotables(IV,"Type.of.residence")
plot_infotables(IV,"No.of.months.in.current.residence")
plot_infotables(IV,"binning.No.of.months.in.current.company")
plot_infotables(IV,"No.of.times.90.DPD.or.worse.in.last.6.months")
plot_infotables(IV,"No.of.times.60.DPD.or.worse.in.last.6.months")
plot_infotables(IV,"No.of.times.30.DPD.or.worse.in.last.6.months")
plot_infotables(IV,"binning.Avgas.CC.Utilization.in.last.12.months")
plot_infotables(IV,"No.of.trades.opened.in.last.6.months")
plot_infotables(IV,"binning.No.of.trades.opened.in.last.12.months")
plot_infotables(IV,"No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.")
plot_infotables(IV,"binning.No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.")
plot_infotables(IV,"Presence.of.open.home.loan")
plot_infotables(IV,"binning.Outstanding.Balance")
plot_infotables(IV,"binning.Total.No.of.Trades")
plot_infotables(IV,"Presence.of.open.auto.loan")
#########################Analysis after woe plot ####################
#In comparison to female male has the high chances of default.
#we see that the age group between 51 to 53 has the high chances of default since their WOE is very less.
#Married has the high chances of default, since their percentage of good customers were very less in comparison
# to their bad customers
#Applicant who has 2 dependents has the higher chances of default.
#Applicant who has "Income" group between 49 to 60 has the higher chances of default.
#Applicant whose Education is masters have equal number of good and bad customers and others have the high evidence
#Applicant whose Profession is SAL has the higher chances of default.
#Applicant whose Type.of.residence is others has the higher chances of default.
#Applicant whose No.of.months.in.current.residence is between 6 to 9 months has the higher chances of default.
#Applicant whose No.of.months.in.current.company is between 41 to 61 months has the higher chances of default.
#Applicant whose No.of.times.90.DPD.or.worse.in.last.6.months is 0 has the higher chances of default.
#Applicant whose No.of.times.60.DPD.or.worse.in.last.6.months is 0 has the higher chances of default.
#Applicant whose No.of.times.30.DPD.or.worse.in.last.6.months is 0 has the higher chances of default and same goes for
#12 months also.
#Applicant whose Avgas.CC.Utilization.in.last.12.months is between 0 to 14 has the higher chances of default.
#Applicant whose No.of.trades.opened.in.last.6.months is between 0 to 1 has the higher chances of default.
#Applicant whose No.of.trades.opened.in.last.12.months is between 0 to 2 has the higher chances of default.
#Applicant whose No.of.PL.trades.opened.in.last.6 and 12.months is 0 has the higher chances of default.
#Applicant whose No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. is 0 has the higher chances of default.
#Applicant whose No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. is 0 has the higher chances of default.
#Applicant whose Presence.of.open.home.loan is 1 has the higher chances of default.
#Applicant whose Outstanding.Balance is between 0 to 56065 and between 1362889 to 3289931 has the higher chances of default.
#Applicant whose Total.No.of.Trades is between 1 to 4 has the higher chances of default.
#Applicant whose Presence.of.open.auto.loan is between 1 has the higher chances of default.
######################################################################################################################### 
#--------------------------------------------------------------------------
#Removing unimportant variables whose iv is less than 0.02  
#important variables 
# write.csv(IV$Summary,"iv_values.csv")
# Based on information value (IV), a variable can be trated as:
# Useless if IV is less than 0.02
# Weak if IV is  in between 0.02 and 0.1
# Medium if IV is in between 0.1 and 0.3
# Strong if IV is in between 0.3 and 0.5
# suspicious if IV is greater than 0.5
#The following variables have IV of less than 0.02. So, they can be removed.
#Marital.Status..at.the.time.of.application., Gender, Education, 
#Type.of.residence, binning.Age, Presence.of.open.auto.loan, Profession, 
#No.of.dependents

Master_Data_imp_var<-subset(Master_Data,select = -c(Marital.Status..at.the.time.of.application.,
                                                    Gender, Education,Type.of.residence, binning.Age, Presence.of.open.auto.loan, Profession,
                                                    No.of.dependents) )
# install.packages("fuzzyjoin")
library(fuzzyjoin)

IV_final <-
  Information::create_infotables(data = Master_Data_imp_var,
                                 y = "Performance.Tag",
                                 parallel =
                                   TRUE)


# Let's replace the variables with woe value for model building
IV<-IV_final
woe_replace <- function(df_orig, IV) {
  df <- cbind(df_orig)
  df_clmtyp <- data.frame(clmtyp = sapply(df, class))
  df_col_typ <-
    data.frame(clmnm = colnames(df), clmtyp = df_clmtyp$clmtyp)
  for (rownm in 1:nrow(df_col_typ)) {
    colmn_nm <- toString(df_col_typ[rownm, "clmnm"])
    if(colmn_nm %in% names(IV$Tables)){
      column_woe_df <- cbind(data.frame(IV$Tables[[toString(df_col_typ[rownm, "clmnm"])]]))
      if (df_col_typ[rownm, "clmtyp"] == "factor" | df_col_typ[rownm, "clmtyp"] == "character") {
        df <-
          dplyr::inner_join(
            df,
            column_woe_df[,c(colmn_nm,"WOE")],
            by = colmn_nm,
            type = "inner",
            match = "all"
          )
        df[colmn_nm]<-NULL
        colnames(df)[colnames(df)=="WOE"]<-colmn_nm
      } else if (df_col_typ[rownm, "clmtyp"] == "numeric" | df_col_typ[rownm, "clmtyp"] == "integer") {
        column_woe_df$lv<-as.numeric(str_sub(
          column_woe_df[,colmn_nm],
          regexpr("\\[", column_woe_df[,colmn_nm]) + 1,
          regexpr(",", column_woe_df[,colmn_nm]) - 1
        ))
        column_woe_df$uv<-as.numeric(str_sub(
          column_woe_df[,colmn_nm],
          regexpr(",", column_woe_df[,colmn_nm]) + 1,
          regexpr("\\]", column_woe_df[,colmn_nm]) - 1
        ))
        column_woe_df[colmn_nm]<-NULL
        column_woe_df<-column_woe_df[,c("lv","uv","WOE")]
        colnames(df)[colnames(df)==colmn_nm]<-"WOE_temp2381111111111111697"
        df <-
          fuzzy_inner_join(
            df,
            column_woe_df[,c("lv","uv","WOE")],
            by = c("WOE_temp2381111111111111697"="lv","WOE_temp2381111111111111697"="uv"),
            match_fun=list(`>=`,`<=`)
          )
        df["WOE_temp2381111111111111697"]<-NULL
        df["lv"]<-NULL
        df["uv"]<-NULL
        colnames(df)[colnames(df)=="WOE"]<-colmn_nm
      }}
  }
  return(df)
}
# #
#Function Call:-
final_df <- woe_replace(Master_Data_imp_var, IV)

#checking the structure of final df
summary(final_df)
str(final_df)

#making a copy of final df
final_df_copy<-final_df
#########################################################################################################
#######################MODEL CREATION####################################################################
#
######################## For unbalanced Data ############################################################
#########################################################################################################
################ Model building steps - Without balancing the data ######################################
################ 1.Logistic regression model ############################################################

# Creating a copy of dataset

master_woe_imbalance <- final_df
# Let's remove applicant id from the data
length(master_woe_imbalance$Performance.Tag)
set.seed(1000)
split_indices <- sample.split(master_woe_imbalance$Performance.Tag, SplitRatio =  0.70)
# Train Dataset 
train <- master_woe_imbalance[split_indices, ]
#sum(train$Performance.Tag.x)/nrow(train)
# Test Dataset
test <- master_woe_imbalance[!split_indices, ]
#sum(test$Performance.Tag.x)/nrow(test)
#  Tag= 1 implies default, 0 implies good
# Logistic model
initial_model = glm(Performance.Tag ~ ., data = train, family = "binomial")
# Summary initial model
summary(initial_model)
# Run stepwise feature selection to remove the insignificant independent variables 
best_model_1 = stepAIC(initial_model, direction = "both")
summary(best_model_1)
# Note that the coefficients are negative because we have converted to woe values
# such that a higher woe value indicates 'good' customer, thus, p(bad) should go down 
# with increasing woe (or increasing good customers)

best_model12 <- glm(formula = Performance.Tag ~ No.of.times.30.DPD.or.worse.in.last.6.months + 
                      binning.Avgas.CC.Utilization.in.last.12.months + binning.No.of.trades.opened.in.last.12.months + 
                      binning.No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                      binning.Total.No.of.Trades + binning.Outstanding.Balance, 
                    family = "binomial", data = train)

# Checking the variance inflation factor to detect the highly correlated independent variable.

vif(best_model12)

# Let's remove "binning.Total.No.of.Trades:

best_model_2 <- glm(formula = Performance.Tag ~ No.of.times.30.DPD.or.worse.in.last.6.months + 
                      binning.Avgas.CC.Utilization.in.last.12.months + binning.No.of.trades.opened.in.last.12.months + 
                      binning.No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                       binning.Outstanding.Balance, family = "binomial", data = train)

# Summary "best_model_2"
summary(best_model_2)

#  VIF
vif(best_model_2)



# Let's remove binning.No.of.trades.opened.in.last.12.months :

best_model_3 <- glm(formula = Performance.Tag ~ No.of.times.30.DPD.or.worse.in.last.6.months + 
                      binning.Avgas.CC.Utilization.in.last.12.months +
                      binning.No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                      binning.Outstanding.Balance, family = "binomial", data = train)


summary(best_model_3)

vif(best_model_3)


# Let's remove "No.of.trades.opened.in.last.12.months" variable:

#best_model_4 <- glm(formula = Performance.Tag ~ No.of.times.30.DPD.or.worse.in.last.6.months + 
 #                                           binning.No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
#                      binning.Outstanding.Balance, 
#                    family = "binomial", data = train)


#summary(best_model_4)

#vif(best_model_4)

final_model_unbalance<- best_model_3

summary(final_model_unbalance)


###################################################################################################
############################# Model Evaluation ####################################################
str(test)

test$Performance.Tag <- as.factor(test$Performance.Tag) 

predictions_logit_unbalace <- predict(final_model_unbalance, newdata = test[, -16], type = "response")

summary(predictions_logit_unbalace)# returns p(bad)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.01211 0.01715 0.03571 0.04226 0.06376 0.10500 

# ################################################################################

# Let's find out the optimal probablility cutoff 

summary(test$Performance.Tag)


perform_fn <- function(cutoff) {
  predicted_response <- as.factor(ifelse(predictions_logit_unbalace >= cutoff, "1", "0"))
  
  conf <- confusionMatrix(predicted_response,test$Performance.Tag, positive = "1")
  
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc)))
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

#######################################################################################

#-------------------------------------------------------------------------------------

# Creating cutoff values from 0.014 to 0.11 for plotting and initiallizing a matrix of 1000 X 4.

s = seq(0.014,0.10 ,length=100)

OUT = matrix(0,100,3)


for(i in 1:100){
  OUT[i,] = perform_fn(s[i])
}

# #######################################################################################
# 
# # plotting cutoffs 
# 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,0.2,length=5),seq(0,0.2,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0.08,.70, col=c(1,"darkgreen", 2,"darkred"),lwd=c(1,1,1,1),c("Sensitivity","Specificity","Accuracy"))


cutoff_logistic_unbalance <- s[which(abs(OUT[,1]-OUT[,2])<0.025)]

cutoff_logistic_unbalance <- cutoff_logistic_unbalance[1]#4.7
# the optimal cutoff seems to be somewhere around 4.7 to 4.8%

#---------------------------------------------------------    
# ### Confusion Matrix
predicted_response_unbalance <- as.factor(ifelse(predictions_logit_unbalace >= cutoff_logistic_unbalance, 1, 0))


conf_logistic_unbalance <- confusionMatrix(predicted_response_unbalance,test$Performance.Tag, positive = "1")

conf_logistic_unbalance

# Accuracy :    0.62  
# Sensitivity : 0.63         
# Specificity : 0.62   

#####################################################################
# Let's also see the  KS statistic for test data. 

actual_test<- ifelse(test$Performance.Tag=="1",1,0)

predicted_response_unbalance <- ifelse(predicted_response_unbalance=="1",1,0)

model_unbalance_test_logistic <- prediction(predicted_response_unbalance,actual_test)

model_perf_test_unbalance_logistic <- performance(model_unbalance_test_logistic, "tpr", "fpr")

plot(model_perf_test_unbalance_logistic,col = "red", lab = c(10,10,10))


#KS Statistics

ks_table <- attr(model_perf_test_unbalance_logistic, "y.values")[[1]] - (attr(model_perf_test_unbalance_logistic, "x.values")[[1]])

ks_test_unbalance_logistic = max(ks_table)

# Maximum KS
ks_test_unbalance_logistic
#0.2607553, neither too bad nor good
####################################################################
#
# Let's also see the KS statistic on master data. 

predicted_prob_unbalace_master <- predict(final_model_unbalance, newdata = master_woe_imbalance[, -16], type = "response")

predicted_response_unbalance_master <- ifelse(predicted_prob_unbalace_master>=cutoff_logistic_unbalance,1,0)


model_unbalance_logistic_master <- prediction(predicted_response_unbalance_master,master_woe_imbalance$Performance.Tag )

model_perf_unbalance_logistic_master <- performance(model_unbalance_logistic_master, "tpr", "fpr")

plot(model_perf_unbalance_logistic_master,col = "red", lab = c(10,10,10))


#KS Statistics

ks_table <- attr(model_perf_unbalance_logistic_master, "y.values")[[1]] - (attr(model_perf_unbalance_logistic_master, "x.values")[[1]])

ks_train_unbalance_logistic_master = max(ks_table)

# Maximum KS
ks_train_unbalance_logistic_master
# 0.2582

# Evaluation is poor for logistic model
######################################################################################
# Let's try another model: Random forest
#######################################################################################
############Let's build the randomForrest model on unbalanced Dataset
#######################################################################################
### Random Forest ###

### Data preparation for modelling

RF_data_imbalance <- final_df


# Spliting the bank data in 70:30 ratio

RF_data_imbalance$Performance.Tag <- as.factor(ifelse(RF_data_imbalance$Performance.Tag==1,"yes","no"))

set.seed(1010)
split_indices <- sample.split(RF_data_imbalance$Performance.Tag, SplitRatio = 0.70)

train_rf <- RF_data_imbalance[split_indices, ]

test_rf <- RF_data_imbalance[!split_indices, ]

#### Modelling ############################### 
# install.packages("randomForest")
library(randomForest)

sum(is.na(train_rf)==TRUE)

nrow(train_rf)




rf_model <- randomForest( Performance.Tag~., data = train_rf, proximity = F, do.trace = T, mtry = 5,ntree=500)



rf_pred <- predict(rf_model, test_rf[, -1], type = "prob")

summary(rf_pred)

perform_fn_rf <- function(cutoff) {
  predicted_response <- as.factor(ifelse(rf_pred[, 2] >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test_rf$Performance.Tag, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT_rf <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT_rf) <- c("sensitivity", "specificity", "accuracy")
  return(OUT_rf)
}


s = seq(0.01,0.6 ,length=100)

OUT_rf = matrix(0,100,3)


for(i in 1:100){
  OUT_rf[i,] = perform_fn_rf(s[i])
}

#######################################################################################
# plotting cutoffs

plot(s, OUT_rf[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_rf[,2],col="darkgreen",lwd=2)
lines(s,OUT_rf[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

# Let's find the cutoff 
cutoff_unbalance_rf <- s[which(abs(OUT_rf[,1]-OUT_rf[,2])<0.045)]
cutoff_unbalance_rf

predicted_response_rf <- as.factor(ifelse(rf_pred[, 2] >= 0.03979798, "yes", "no"))
conf_unbalance_rf <- confusionMatrix(predicted_response_rf, test_rf$Performance.Tag, positive = "yes")

conf_unbalance_rf

# Model results: 
# Sensitivity = around 63%
# Specificity = around 60%
# Accuracy =    around 60%


###########################################################################################
################ Model building steps - With balancing the data ###########################
##########################################################################################

####### 1 Logistic regression model 

# Data preparation for model building


final_df$Performance.Tag <- as.factor(final_df$Performance.Tag)

set.seed(1000)
split_indices <- sample.split(final_df$Performance.Tag, SplitRatio = 0.70)

# Train data
train <- final_df[split_indices, ]

#  test data
test <- final_df[!split_indices, ]

#Since the data is highly imbalanced,It is required to balance the data.SMOTE function is used to balance the same

train_swote <- SMOTE(Performance.Tag ~ ., train, perc.over = 100, perc.under=200)

summary(train_swote$Performance.Tag)

##################################################################################################
# Modelling : Logistic Regresion

# Tag= 1 implies default, 0 implies good
initial_model = glm(Performance.Tag ~ ., data = train_swote, family = "binomial")
summary(initial_model)


best_model_1<- stepAIC(initial_model, direction="both")
summary(best_model_1)

# Removing multicollinearity through VIF check
vif(best_model_1)

#Excluding No.of.times.30.DPD.or.worse.in.last.6.months

best_model_2<- glm(formula = Performance.Tag ~ No.of.months.in.current.residence + 
      No.of.times.60.DPD.or.worse.in.last.12.months + 
      No.of.PL.trades.opened.in.last.6.months + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
      binning.No.of.months.in.current.company + binning.Avgas.CC.Utilization.in.last.12.months + 
      binning.No.of.trades.opened.in.last.12.months + binning.No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., 
    family = "binomial", data = train_swote)

summary(best_model_2)

vif(best_model_2)


#Excluding binning.No.of.trades.opened.in.last.12.months 

best_model_3<- glm(formula = Performance.Tag ~ No.of.months.in.current.residence + 
                     No.of.times.60.DPD.or.worse.in.last.12.months + 
                     No.of.PL.trades.opened.in.last.6.months + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                     binning.No.of.months.in.current.company + binning.Avgas.CC.Utilization.in.last.12.months + 
                     binning.No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., 
                   family = "binomial", data = train_swote)

summary(best_model_3)

vif(best_model_3)

#Excluding No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. 

best_model_4<- glm(formula = Performance.Tag ~ No.of.months.in.current.residence + 
                     No.of.times.60.DPD.or.worse.in.last.12.months + 
                     No.of.PL.trades.opened.in.last.6.months + 
                     binning.No.of.months.in.current.company + binning.Avgas.CC.Utilization.in.last.12.months + 
                     binning.No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., 
                   family = "binomial", data = train_swote)

summary(best_model_4)

vif(best_model_4)


#Excluding No.of.months.in.current.residence 
 
best_model_5<- glm(formula = Performance.Tag ~ No.of.times.60.DPD.or.worse.in.last.12.months + 
                     No.of.PL.trades.opened.in.last.6.months + 
                     binning.No.of.months.in.current.company + binning.Avgas.CC.Utilization.in.last.12.months + 
                     binning.No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., 
                   family = "binomial", data = train_swote)

summary(best_model_5)

vif(best_model_5)

#since all the variables have less vif and p values, all are significant
#so, this is the final model


# Final Model for Prediction
final_model_balanced <- best_model_5

summary(final_model_balanced)
########################################################################################################
######################################## Model Evaluation: test Data #################################
predictions_logit_balance <- predict(final_model_balanced, newdata = test[, -1], type = "response")

summary(predictions_logit_balance)# returns p(bad)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.2275  0.2722  0.4523  0.4536  0.6088  0.7344 

# ################################################################################
# Let's find out the optimal probablility cutoff 

summary(test$Performance.Tag)


perform_fn <- function(cutoff) {
  predicted_response <- as.factor(ifelse(predictions_logit_balance >= cutoff, "1", "0"))
  
  conf <- confusionMatrix(predicted_response,test$Performance.Tag, positive = "1")
  
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc)))
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

#######################################################################################

#-------------------------------------------------------------------------------------

# Creating cutoff values from 0.014 to 0.11 for plotting and initiallizing a matrix of 1000 X 4.

s = seq(0.3,0.7 ,length=100)

OUT = matrix(0,100,3)


for(i in 1:100){
  OUT[i,] = perform_fn(s[i])
}

# #######################################################################################
# 
# # plotting cutoffs 
# 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,0.2,length=5),seq(0,0.2,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0.08,.70, col=c(1,"darkgreen", 2,"darkred"),lwd=c(1,1,1,1),c("Sensitivity","Specificity","Accuracy"))


cutoff_logistic_balance <- s[which(abs(OUT[,1]-OUT[,2])<0.011)]

cutoff_logistic_balance <- cutoff_logistic_balance[1]
cutoff_logistic_balance
# the optimal cutoff seems to be somewhere around 0.53
  
#---------------------------------------------------------    
# ### Confusion Matrix
predicted_response_balance <- 
  as.factor(ifelse( predictions_logit_balance >= cutoff_logistic_balance, 1, 0))


conf_logistic_balance <- confusionMatrix(predicted_response_balance,test$Performance.Tag, positive = "1")

conf_logistic_balance

#Accuracy - 0.63
#sensitivity - 0.63307
#specificity - 0.62537

######################################################################################################
# Apply model on master Data: 


predictions_logit_balance_master <- 
  predict(final_model_balanced, newdata = final_df[, -1], type = "response")

summary(predictions_logit_balance_master)# returns p(bad)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.2275  0.2744  0.4560  0.4550  0.6100  0.7344 


predicted_master_response <- factor(ifelse(predictions_logit_balance_master>=cutoff_logistic_balance,1,0))

conf_logistic_balance_master <- confusionMatrix(predicted_master_response,final_df$Performance.Tag, positive = "1")

conf_logistic_balance_master

#Accuracy - 0.6209
#sensitivity - 0.63222
#specificity - 0.62039

#####################################################################################################
# Let's build Random forest model on the balance data 
#####################################################################################################
#####################################################################################################
### Random Forest ###


### Data preparation for modelling

RF_data <- final_df


# Spliting the bank data in 70:30 ratio

RF_data$Performance.Tag <- 
  as.factor(ifelse(RF_data$Performance.Tag==1,"yes","no"))

set.seed(1000)
split_indices <- sample.split(RF_data$Performance.Tag, SplitRatio = 0.70)

train_rf <- RF_data[split_indices, ]

test_rf <- RF_data[!split_indices, ]


RF_train_swote <- train_swote

str(RF_train_swote)

summary(RF_train_swote$Performance.Tag)

library(randomForest)

rf_model <- randomForest( Performance.Tag~., data = RF_train_swote, 
                          proximity = F, do.trace = T, mtry = 5,ntree=500, na.action=na.omit)

# Variables importance 
rf_imp_var <- data.frame(rf_model$importance)
rf_imp_var$MeanDecreaseGini <- round(rf_imp_var$MeanDecreaseGini,1)

# Test

str(test_rf)

rf_pred <- predict(rf_model, test_rf[,-1] , type = "prob")

summary(rf_pred)

##
# table(rf_pred,test_rf$Performance.Tag)

perform_fn_rf <- function(cutoff) {
  predicted_response <- as.factor(ifelse(rf_pred[, 2] >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test_rf$Performance.Tag, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT_rf <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT_rf) <- c("sensitivity", "specificity", "accuracy")
  return(OUT_rf)
}


s = seq(0.01,0.8,length=100)

OUT_rf = matrix(0,100,3)


for(i in 1:100){
  OUT_rf[i,] = perform_fn_rf(s[i])
}

#######################################################################################
# plotting cutoffs

plot(s, OUT_rf[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_rf[,2],col="darkgreen",lwd=2)
lines(s,OUT_rf[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


# Choosing cutoff 
cutoff_rf_balance <- s[which(abs(OUT_rf[,1]-OUT_rf[,2])<0.015)]
cutoff_rf_balance 
# Somewhere around 0.401 
# lets take cutoff 0.40

predicted_rf_balance <- as.factor(ifelse(rf_pred[, 2] >= 0.40, "yes", "no"))

# evaluating the rf model on the test data, which is also imbalanced
summary(test_rf$Performance.Tag)
conf_rf_balance <- confusionMatrix(predicted_rf_balance, test_rf$Performance.Tag, positive = "yes")

conf_rf_balance
#Accuracy - 0.61
#Sensitvity -0.63
#specificity - 0.61

predicted_response_rf <- ifelse(predicted_rf_balance=="yes",1,0)

actual_response_rf <- ifelse(test_rf$Performance.Tag=="yes",1,0)

model_score_test_rf <- prediction(predicted_response_rf,actual_response_rf )

model_perf_test_rf <- performance(model_score_test_rf, "tpr", "fpr")

plot(model_perf_test_rf,col = "red", lab = c(10,10,10))

#KS Statistics

ks_table <- attr(model_perf_test_rf, "y.values")[[1]] - (attr(model_perf_test_rf, "x.values")[[1]])

ks = max(ks_table)

ks # approx.0.2371, this is much better than the previous models (which were trained on imbalanced data)

##################################################################################################
# evaluate the model on the entire master data: 
View(final_df)
master_pred <- predict(rf_model, final_df[,-1], type = "prob")

# Summarise the probabilities

summary(master_pred)

master_response_rf <- as.factor(ifelse(master_pred[,2] >=0.40 , 1, 0))

conf_rf <- confusionMatrix(master_response_rf,final_df$Performance.Tag, positive = "1")

conf_rf # this looks quite good; all three metric > 60%
#Accuracy - 0.64
#sensitivity - 0.84
#Specificity - 0.63

################ Let's see the KS ##########################

actual_master_default<- ifelse(final_df$Performance.Tag=="1",1,0)

predicted_master_default <- ifelse(master_response_rf=="1",1,0)

model_score_master <- prediction(predicted_master_default,actual_master_default)

model_perf_master <- performance(model_score_master, "tpr", "fpr")

plot(model_perf_master,col = "red", lab = c(10,10,10))


#KS Statistics

ks_table_master <- attr(model_perf_master, "y.values")[[1]] - (attr(model_perf_master, "x.values")[[1]])

ks_master = max(ks_table_master)

ks_master # 0.47

## apart from the master dataset, we should also evaluate the model on a randomly chosen 
## subset (say with 20% data)  - this will help us validate the stability further


set.seed(1010)
sample_indices <- sample.split(final_df$Performance.Tag, SplitRatio = 0.30)

sample_master <- final_df[sample_indices,]

sample_master_pred <- predict(rf_model, sample_master[,-1], type = "prob")

# Summarise the probabilities

summary(sample_master_pred)

sample_master_response_rf <- as.factor(ifelse(sample_master_pred[,2] >=0.40 , 1, 0))

sample_conf_rf <- confusionMatrix(sample_master_response_rf,sample_master$Performance.Tag, positive = "1")

sample_conf_rf # this looks quite good; all three metric > 70%
#Accuracy - 0.65
#sensitivity - 0.85
#specificity -0.64

################ Let's see the KS ##########################

actual_sample_master_default<- ifelse(sample_master$Performance.Tag=="1",1,0)

predicted_master_default <- ifelse(sample_master_response_rf=="1",1,0)

model_score_sample_master <- prediction(predicted_master_default,actual_sample_master_default)

model_perf_sample_master <- performance(model_score_sample_master, "tpr", "fpr")

plot(model_perf_sample_master,col = "red", lab = c(10,10,10))


#KS Statistics

ks_table_sample_master <- attr(model_perf_sample_master, "y.values")[[1]] - (attr(model_perf_sample_master, "x.values")[[1]])

ks_master = max(ks_table_sample_master)

ks_master # 0.48



######################################################################################################



######################################################################################################

# Application Scorecard on master dataset
# For master Dataset: Let's create a new dataframe which contains "Applicant_id","Actual_response","Predicted_Response" & "Predicted_prob" from the test data

final_df_copy$Application.ID<-Applicant
str(final_df_copy)
score_data <- final_df_copy[,c(1,21)]
# Lets store probabilities of bad in store_data
score_data$predicted_prob_bad <- master_pred[,2]


# append "probabilies of good" to store_data
score_data$predicted_prob_good <- master_pred[,1]

score_data$odds <- score_data$predicted_prob_good/score_data$predicted_prob_bad

score_data$log_odds <- log(score_data$odds)

#Points to double the odds i.e.20

PDO<-20

#Base Score=400 & odds = 10

BaseScore<-400

Odds<-10

#Calaculating Factor & Offset

Factor=PDO/log(2)

Offset=BaseScore-(Factor*log(Odds))

score_data$score <- Offset+(Factor*score_data$log_odds)

# Rounding to the near integer
score_data$score <- round(score_data$score,0)
str(score_data$score)
###########################################################
#### Now because of 0 odds, log of odds returns Infinity 

score_data <- score_data[-which(score_data$score=="Inf"|score_data$score=="-Inf"),]


# Summary "score"

summary(score_data$score)

#######################################################
# Let's sort the "score_data" with "score"

score_data <- score_data[order(score_data$score,decreasing = T),]

score_data$Performance.Tag <- ifelse(score_data$Performance.Tag=="1",1,0)

score_data$predicted_default <- ifelse(score_data$predicted_prob_bad>=0.40,1,0)

# Finding the score optimal value
score_data$score[which(score_data$predicted_prob_bad==0.40)]
# optimal value is 345


g<- ggplot(score_data , aes(score,predicted_prob_bad),colour="red")+
  geom_line()+  ggtitle("Score v/s probability of bad")+
  geom_hline(yintercept=0.48, col="red")+geom_vline(xintercept= 345)

g

###########################################
# Scores percentile
quantile(score_data$score,seq(0,1,0.1))
# 40% customers are defaulters if the optimal score is 345

# Score distribution

ggplot(score_data,aes(score))+geom_histogram() +geom_vline(xintercept= 345,col="blue")

######################################################################

## Modelling on Rejected dataset. 
## Now we will calculate the predicted scores and thus the performance (0/1) for the 
## rejected population

## This is a test of model's 'population' stability - it should predict that a high fraction of the rejected
## population is 'bad'

# Replacing variables with woe values: 
summary(validation) 

###################
# Additional copy of "rejected file"

rejected<-validation
str(validation)
#rejected<- rejected_1
################### Data Preparation ############################
#################################################################
# View(rejected)
rejected <- rejected[-which(is.na(rejected$Profession)),]
rejected$Profession <- factor(rejected$Profession)
rejected$Type.of.residence <- factor(rejected$Type.of.residence)
rejected <- rejected[-which(is.na(rejected$Education)),]
rejected$Education <- factor(rejected$Education)
rejected$Gender <- factor(rejected$Gender)
rejected$Marital.Status..at.the.time.of.application. <- factor(rejected$Marital.Status..at.the.time.of.application.)
rejected <- rejected[-which(is.na(rejected$Avgas.CC.Utilization.in.last.12.months)),]
rejected$Avgas.CC.Utilization.in.last.12.months <- factor(rejected$Avgas.CC.Utilization.in.last.12.months)

sapply(rejected, function(x) sum(is.na(x)))
#Changing colnames of rejected to match with final df
colnames(rejected)
colnames(final_df_copy)
colnames(final_df)

colnames(rejected)[colnames(rejected)=="Income"]<-"binning.Income"
colnames(rejected)[colnames(rejected)=="No.of.months.in.current.company"]<-"binning.No.of.months.in.current.company"
colnames(rejected)[colnames(rejected)=="Avgas.CC.Utilization.in.last.12.months"]<-"binning.Avgas.CC.Utilization.in.last.12.months"
colnames(rejected)[colnames(rejected)=="No.of.trades.opened.in.last.12.months"]<-"binning.No.of.trades.opened.in.last.12.months"
colnames(rejected)[colnames(rejected)=="No.of.PL.trades.opened.in.last.12.months"]<-"binning.No.of.PL.trades.opened.in.last.12.months"
colnames(rejected)[colnames(rejected)=="No.of.Inquiries.in.last.12.months..excluding.home...auto.loans."]<-"binning.No.of.Inquiries.in.last.12.months..excluding.home...auto.loans."
colnames(rejected)[colnames(rejected)=="Total.No.of.Trades"]<-"binning.Total.No.of.Trades"
colnames(rejected)[colnames(rejected)=="Outstanding.Balance"]<-"binning.Outstanding.Balance"

######################################################################

## Considering only important variables
col_replace <- which(colnames(rejected) %in% colnames(final_df_copy) )
length(col_replace)
rejected_copy<-rejected
rejected <- rejected[,col_replace]

colnames(rejected)
#Removing application id, Performance.Tag
col_replace <- which(!colnames(rejected) %in% c("Application.ID","Performance.Tag"))

# col_replace <- which(!colnames(rejected) %in% c("Application.ID"))
col_replace

#Making a copy of rejected
rejected_copy<-rejected
str(rejected_copy)
rejected <- rejected[,col_replace]
View(IV$Tables)
#changing the order of names of rejected to the order of IV names
rejected_final<-rejected[,c(names(IV$Tables))]
str(rejected_final)
#########--------REJECTED DATA BINNING and OUTLIER TREATMENT for woe bins extraction-------#####################################
str(rejected_final)
#-------------------------------------------#
## Income 
rejected_final[(which(rejected_final$binning.Income<=4.5)),]$rejected_final$binning.Income <- 4.5
summary(rejected_final$binning.Income)
rejected_final$binning.Income <- as.factor(cut(rejected_final$binning.Income, include.lowest = TRUE,
                                               breaks = c(4.5, 15, 25, 35, 45, 55, 60)))

# Binning Income created : 4.5-15,15-25,25-35,35-45,45-55,55-60

##No of months in current residence
rejected_final$No.of.months.in.current.residence <- as.factor(cut(rejected_final$No.of.months.in.current.residence, breaks = c(5, 9, 28, 49, 72, 97, 126)))
#-------------------------------------------#
# No.of.months.in.current.company
rejected_final[(which(rejected_final$binning.No.of.months.in.current.company>74)),]$binning.No.of.months.in.current.company <- 74
rejected_final$binning.No.of.months.in.current.company <- as.factor(cut(rejected_final$binning.No.of.months.in.current.company, breaks = c(2, 13, 26, 33, 40, 61, 74)))
#Capping outliers, which are greater than 74. As 99 percentile is 74 and 100th percentile is 133.
# Binning the variable and store it into another variables.

#------------------------------------------#
# No.of.times.90.DPD.or.worse.in.last.6.months
#converting to factor variable
rejected_final$No.of.times.90.DPD.or.worse.in.last.6.months<-as.factor(rejected_final$No.of.times.90.DPD.or.worse.in.last.6.months)

#-----------------------------------------------------#
# No.of.times.60.DPD.or.worse.in.last.6.months
rejected_final$No.of.times.60.DPD.or.worse.in.last.6.months<-as.factor(rejected_final$No.of.times.60.DPD.or.worse.in.last.6.months)
#-----------------------------------------------------#
## No.of.times.30.DPD.or.worse.in.last.6.months
rejected_final$No.of.times.30.DPD.or.worse.in.last.6.months<-as.factor(rejected_final$No.of.times.30.DPD.or.worse.in.last.6.months)
#-----------------------------------------------------#
##No.of.times.90.DPD.or.worse.in.last.12.months
rejected_final$No.of.times.90.DPD.or.worse.in.last.12.months<-
  as.factor(rejected_final$No.of.times.90.DPD.or.worse.in.last.12.months)
#-----------------------------------------------------#
# No.of.times.60.DPD.or.worse.in.last.12.months
rejected_final$No.of.times.60.DPD.or.worse.in.last.12.months<-
  as.factor(rejected_final$No.of.times.60.DPD.or.worse.in.last.12.months)
#-----------------------------------------------------#
# No.of.times.30.DPD.or.worse.in.last.12.months
rejected_final$No.of.times.30.DPD.or.worse.in.last.12.months<-
  as.factor(rejected_final$No.of.times.30.DPD.or.worse.in.last.12.months)
##-----------------------------##
## Avgas.CC.Utilization.in.last.12.months
#Capping outliers, which are greater than 91. 
rejected_final$binning.Avgas.CC.Utilization.in.last.12.months<- as.integer(rejected_final$binning.Avgas.CC.Utilization.in.last.12.months)
rejected_final[(which(rejected_final$binning.Avgas.CC.Utilization.in.last.12.months>91)),]$
  binning.Avgas.CC.Utilization.in.last.12.months <- 91
# Binning the variable and store it into another variables.
rejected_final$binning.Avgas.CC.Utilization.in.last.12.months <- 
  as.factor(cut(rejected_final$binning.Avgas.CC.Utilization.in.last.12.months, include.lowest = TRUE,
                breaks = c(0, 8, 14, 21, 51, 71, 91)))
##-----------------------------##
# No.of.trades.opened.in.last.6.months
#Capping outliers, which are greater than 6. 
rejected_final[(which(rejected_final$No.of.trades.opened.in.last.6.months>6)),]$
  No.of.trades.opened.in.last.6.months <- 6
#converting to factor variable
rejected_final$No.of.trades.opened.in.last.6.months<-
  as.factor(rejected_final$No.of.trades.opened.in.last.6.months)
##-----------------------------##
# No.of.trades.opened.in.last.12.months
rejected_final[(which(rejected_final$binning.No.of.trades.opened.in.last.12.months>18)),]$
  binning.No.of.trades.opened.in.last.12.months <- 18
# Binning the variable and store it into another variables.
rejected_final$binning.No.of.trades.opened.in.last.12.months <- 
  as.factor(cut(rejected_final$binning.No.of.trades.opened.in.last.12.months, include.lowest = TRUE,
                breaks = c(0, 2, 5, 8, 10, 13, 18)))
##------------------------------##
# No.of.PL.trades.opened.in.last.6.months
#Capping outliers, which are greater than 5. 
rejected_final[(which(rejected_final$No.of.PL.trades.opened.in.last.6.months>5)),]$
  No.of.PL.trades.opened.in.last.6.months <- 5
rejected_final$No.of.PL.trades.opened.in.last.6.months<-
  as.factor(rejected_final$No.of.PL.trades.opened.in.last.6.months)

##--------------------------------------------##
# No.of.PL.trades.opened.in.last.12.months
#Capping outliers, which are greater than 9.
rejected_final[(which(rejected_final$binning.No.of.PL.trades.opened.in.last.12.months>9)),]$
  binning.No.of.PL.trades.opened.in.last.12.months <- 9
# Binning the variable and store it into another variables.
rejected_final$binning.No.of.PL.trades.opened.in.last.12.months <- 
  as.factor(cut(rejected_final$binning.No.of.PL.trades.opened.in.last.12.months, include.lowest = TRUE,
                breaks = c(0, 1, 2, 3, 5, 6, 9)))
##------------------------------------------##
# No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.
#Capping outliers, which are greater than 7. 
rejected_final[(which(rejected_final$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.>7)),]$
  No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. <- 7
rejected_final$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.<-
  as.factor(rejected_final$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)
##-----------------------------##
# No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.
#Capping outliers, which are greater than 12. 
rejected_final[(which(rejected_final$binning.No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.>12)),]$
  binning.No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. <- 12
# Binning the variable and store it into another variables.
rejected_final$binning.No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. <- 
  as.factor(cut(rejected_final$binning.No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., 
                include.lowest = TRUE, breaks = c(0, 1, 2, 4, 6, 8, 12)))
##-----------------------------##
## Presence.of.open.home.loan
rejected_final$Presence.of.open.home.loan<-
  as.factor(rejected_final$Presence.of.open.home.loan)
##-----------------------------##
# Total.No.of.Trades
#Capping outliers, which are greater than 21. 
rejected_final[(which(rejected_final$binning.Total.No.of.Trades>21)),]$
  Total.No.of.Trades <- 21
# Binning the variable and store it into another variables.
rejected_final$binning.Total.No.of.Trades <- 
  as.factor(cut(rejected_final$binning.Total.No.of.Trades, include.lowest = TRUE,
                breaks = c(0, 2, 4, 6, 10, 19, 21)))
##-----------------------------##
# Outstanding.Balance
#Capping outliers, which are greater than 4251446 
rejected_final[(which(Master_Data$binning.Outstanding.Balance>4251446)),]$
  binning.Outstanding.Balance <- 4251446
# Binning the variable and store it into another variables.
rejected_final$binning.Outstanding.Balance <- 
  as.factor(cut(rejected_final$binning.Outstanding.Balance, include.lowest = TRUE,
                breaks = c(0,584,6852,25604,386878,585402,774165,972299,1357216,2960909,3282409,4251446)))
##-----------------------------##

#checking the structure of dataframe
summary(rejected_final)
str(rejected_final)
#######################################################################################

rejected_df <- woe_replace(rejected_final, IV)
View(rejected_df)
rejected_prob <- predict(rf_model, rejected_df, type = "prob")

#####################################################################################
################# Scoring for rejected applications #################################

rejected$predicted_prob_bad <- rejected_prob[,2]
rejected$predicted_prob_good <- rejected_prob[,1]
rejected$odds <- rejected$predicted_prob_good/rejected$predicted_prob_bad
rejected$log_odds <- log(rejected$odds)

rejected$score <- Offset+(Factor*rejected$log_odds)

# Rounding it to 0
rejected$score <- round(rejected$score,0)

# Let's create a derived metric "Default" if the score is greater than 345.

rejected$defaults <- ifelse(rejected$score>345,0,1)

# find the percentage of correctly identified out of rejected population:

percentage_correctly_identified <- (sum(rejected$defaults)*100)/nrow(rejected)

percentage_correctly_identified

# 90.52 correctly identified, i.e. 90% are predicted to be 'bad'
###########################################################
###################################################################
# let's compare the score of rejected and approved datasets

approved_pop <- data.frame(score = score_data$score,decision = rep("approved",nrow(score_data)))

rejected_pop <- data.frame(score = rejected$score,decision = rep("rejected",nrow(rejected)))

# Let's bind "approved_pop" and "rejected_pop"

combined_scores <-  rbind(approved_pop, rejected_pop)


# Let see the difference
ggplot(combined_scores, aes(x=factor(decision), y=score))+geom_boxplot()


###########################################################
#########################################################################################




#################### financial benefit analysis###########################

nrow(validation)
rejection_rate <- nrow(validation)/nrow(Master_Data)
rejection_rate*100
approval_rate <- (1-rejection_rate)
approval_rate*100   # 98% approval rate

approval_rate
# current approval rate : 98%

#Confusion matrix of master_data
conf_rf

#                      Reference
# Prediction          0       1
#               0   41464    462
#               1   24338   2432

#In the confusion matrix, 0 represents good and 1 represents bad

#As per the confusion matrix, we derive the below predictions of our model

actual_good_cust_predicted_as_good<- 41464
actual_good_cust_predicted_as_bad <-24338
actual_bad_cust_predicted_as_good<-462
actual_bad_cust_predicted_as_bad<-2432

##Calculating credit loss:

#calculating credit loss without model
credit_loss_without_model<-(actual_bad_cust_predicted_as_good+actual_bad_cust_predicted_as_bad)/nrow(Master_Data)

credit_loss_without_model*100
#4.2
#Without model, the company would have issued cards to 4.2 percent of bad customers

credit_loss_with_model<-actual_bad_cust_predicted_as_good/nrow(Master_Data)

credit_loss_with_model*100
#0.6
#Our model predicted only 0.6 percent of total customers as bad.

credit_loss_saved_with_model<-credit_loss_without_model-credit_loss_with_model
credit_loss_saved_with_model*100
#credit loss saved=(4.2-0.6)=3.5
#By implementing the model, the company can save credit loss of 3.5 percent


##Calculating revenue loss:
percent_of_good_cust_predicted_as_bad<-actual_good_cust_predicted_as_bad/(actual_good_cust_predicted_as_good+
                                                                            actual_good_cust_predicted_as_bad)
percent_of_good_cust_predicted_as_bad*100
#Around 36 percent of good customers are predicted as bad.
#So, the revenue loss will be around 36 percent


#Assesing finanacial benefit with model:

#When there was no model:
#Financial Benefit without model= (65802 * revenue per customer) - (2894 * loss per customer) 
#Financial Benefit with model= (41464 * revenue per customer) - (462 * loss per customer) 

#Model will be beneficial if,    FB with model  >  FB without model 
#Model is beneficial if, 
# (41464 * revenue per customer) - (462 * loss per customer) >(65802 * revenue per customer) - (2894 * loss per customer)
# 2432 * loss per customer > 24330 * revenue per customer
#After solving the above inequality, we will be left with the below:
#loss per customer > 10 * revenue per customer
# SO, The model is beneficial, when loss per customer is 10 times the revenue per customer. 
 