#==============================================================================
#   1. Setup section
#==============================================================================
rm(list = ls())

library(foreign)
library(car)
library(tidyverse)
library(readstata13)
library(sandwich)
library(stargazer)
library(WDI)
library(haven)
library(ggplot2)

setwd("~/Household_Income")

# turn off scientific notation except for big numbers
options(scipen = 9)
# function to calculate corrected SEs for regression 
cse = function(reg) {
  rob = sqrt(diag(vcovHC(reg, type = "HC1")))
  return(rob)
}

#==============================================================================
#   2. Data section
#==============================================================================
# Read data

df <- read.csv("http://www.stern.nyu.edu/~wgreene/Text/Edition7/TableF5-1.csv")

df <- df %>% rename(labor_force_part=LFP)
#==============================================================================
#   3. Analysis section
#==============================================================================
#Descriptive Statistics of Dataset
stargazer(df, type="text", median=TRUE, 
          digits=2, title="Household Statistics")

#Plot 1
ggplot(data=df, aes(x=FAMINC)) +
  geom_histogram()+
  labs(x="Family Income",title="Family Income Distribution")

#Plot 2
ggplot(data=df, aes(x=factor(labor_force_part), y=FAMINC)) +
  geom_boxplot()+
  labs(x="Wife's Labor Participation", y = "Family Income", 
       title="Family income if wife works VS. wife doesn't work") 

#Plot 3
ggplot(data=df, aes(x=factor(KL6), y=FAMINC)) +
  geom_boxplot()+
  labs(x="Number of kids below 6 years old", y = "Family Income", 
       title="Kids below 6 years old on Family Income") 

#Plot 4
ggplot(data=df, aes(x=factor(K618), y=FAMINC)) +
  geom_boxplot()+
  labs(x="Number of kids between 6 years old and 18 years old", y = "Family Income", 
       title="Kids between 6 and 18 on Family Income") 

#Plot 5
ggplot(data=df, aes(x=factor(WE), y=FAMINC)) +
  geom_boxplot()+
  labs(x="Wife Education Level in Years", y = "Family Income", 
       title="Wife's Education on Family Income") 

#Plot 6
ggplot(data=df, aes(x=factor(HE), y=FAMINC)) +
  geom_boxplot()+
  labs(x="Husband Education Level in Years", y = "Family Income", 
       title="Husband Education on Family Income") 

#Regressions for regression table 1
reg1 = lm(data=subset(df, labor_force_part==0),log(FAMINC)~KL6+K618)
reg2 = lm(data=subset(df, labor_force_part==0),log(FAMINC)~KL6+K618+HE)
reg3 = lm(data=subset(df, labor_force_part==0),log(FAMINC)~KL6+K618+HE+UN)
reg4 = lm(data=subset(df, labor_force_part==0),log(FAMINC)~KL6+K618+HE+UN+CIT)

#Regression table 1
stargazer(reg1, reg2, reg3,reg4,
          se=list(cse(reg1),cse(reg2),cse(reg3),cse(reg4)), 
          title="Log-linear Multivariable Linear Regression with labor_force_part=0", type="text", 
          df=FALSE, digits=4)

#Regressions for regression table 2
reg5 = lm(data=subset(df, labor_force_part==1),log(FAMINC)~KL6+K618+HE+WE+CIT)
reg6 = lm(data=subset(df, labor_force_part==1),log(FAMINC)~KL6+K618+HE+WE+CIT+AX+WE*AX)

#Regression table 2
stargazer(reg5, reg6,
          se=list(cse(reg5),cse(reg6)), 
          title="Log-linear  Multivariable Linear Regression with labor_force_part=1 including Interaction Term", type="text", 
          df=FALSE, digits=4)

# unemployment rate is useless because everyone is employed, they will have family income, sample selection bias

