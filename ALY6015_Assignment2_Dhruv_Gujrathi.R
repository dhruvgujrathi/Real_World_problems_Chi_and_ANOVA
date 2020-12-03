#ALY6015_Assignment2_Dhruv_Gujrathi
#Section 11-1
#Blood Types
chisq.test(x=c(12,8,24,6),
           p=c(0.2,0.28,0.36,0.16)) #Conducting the Chi-sq test
qchisq(0.10, 3, lower.tail = FALSE)
#Section 11-1
#On-time Performance by Airlines
chisq.test(x=c(125,10,25,40),
           p=c(0.708,0.082,0.09,0.12)) #Conducting the Chi-sq test
qchisq(0.05, 3, lower.tail = FALSE)
#Section 11-2
#Ethinicity and Movie Admissions
dat<-matrix(c(724,370,
              335,292,
              174,152,
              107,140),
            nrow=2) #assigning the matrix to a single variable

chisq.test(dat) #Conducting the Chi-sq test
qchisq(0.05, 3, lower.tail = FALSE)
#Section 11-2
#Women in the Military
data<-matrix(c(10791,62491,
               7816,42750,
               932,9525,
               11819,54344),
             nrow=2) #Assigning the matrix to a single variable

chisq.test(data) #Conducting the Chi-sq test
qchisq(0.05, 3, lower.tail = FALSE)
#Section 12-1
#Sodium Contents of Foods
install.packages("tidyverse") #Installing 'tidyverse' package for Data Manipulation & Visualization
install.packages("Hmisc") #Installing 'Hmisc' package for Imputing missing values, high level graphics, etc.
library(tidyverse)
library(Hmisc)
data2<-tibble(y=c(270,130,230,180,80,70,200,260,220,290,290,200,320,140,100,180,250,250,300,360,300,160)) #Assigning required values to tibble.
type=c(rep("Condiments", 7),
       rep("Cereals",7),
       rep("Desserts",8)) #Determining the dependant and independant variables

data2
mod_5<-lm(y~type,data=data2)
summary(aov(mod_5)) #Performing the ANOVA test
qf(0.95,2,19) #Determining the critical value

#Section 12-2
#Sales for leading companies
data3<-tibble(y=c(578,320,264,249,237,311,106,109,125,173,261,185,302,689), #Assigning required values to tibble.
              type=c(rep("Cereals",5),
                     rep("Chocolate candy",5),
                     rep("Coffee",4))) #Determining the dependant and independant variables
data3
model_6<-lm(y~type,data=data3)
summary(aov(model_6)) #Performing the ANOVA test
qf(0.99,2,11) #Determining the critical value

#Section 12-2
#Per Pupil Expenditure
dat<-tibble(y=c(4946,5953,6202,7243,6113,6149,7451,6000,6479,5282,8605,6528,6911), #Assigning required values to tibble.
            type=c(rep("Eastern third",5),
                   rep("Middle third",4),
                   rep("Westernthirs",4))) #Determining the dependant and independant variables
dat
model_7<-lm(y~type,data=dat) #Performing the ANOVA test
summary(aov(model_7)) #Determining the critical value

#Section 12-3
#Increasing Plant Growth
data4<-matrix(c(9.2, 9.4, 8.9,
               8.5, 9.2, 8.9,
               7.1, 7.2, 8.5,
               5.5, 5.8, 7.6),
             nrow=2) #Assigning the matrix to a variable

chisq.test(data4) #Performing the chi square test
qchisq(0.05, 5, lower.tail = FALSE)
data5 <- read.csv("baseball.csv") #Importing baseball.csv dataset
install.packages("funModeling") #Installing 'funModeling' package
library(tidyverse)
library(funModeling)
library(Hmisc)
basic_eda <- function(data5) #Performing exploratory data analysis
{
  glimpse(data5)
  print(status(data5))
  freq(data5) 
  print(profiling_num(data5))
  plot_num(data5)
  describe(data5)
}
basic_eda(data5)

install.packages("dplyr") #Installing 'dplyr' package
library(dplyr)
# Extract decade from year
data5$Decade <- data5$Year - (data5$Year %% 10)
# Create a wins table by summing the wins by decade
wins <- data5 %>%
  group_by(Decade) %>%
  summarise(wins = sum(W)) %>%
  as_tibble()
wins
summary(wins) 
chisq.test(wins) #Performing the chi square test
qchisq(0.95,5) #Determining the critical value

data6 <- read.csv("crop_data.csv") #Importing baseball.csv dataset
str(data6) #Checking the structure of the dataset
table(data6$fertilizer,data6$density) #Creating frequency table for dependant variables
res.aov2 <- aov(yield ~ fertilizer + density, data = data6) #Conducting the two way ANOVA test
summary(res.aov2) #Summarizing the analysis of variance model
plot(res.aov2)
