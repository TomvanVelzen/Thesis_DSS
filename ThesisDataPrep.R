### Loading in all required packages -------------------------------------------
install.packages("mice")
install.packages("mitools")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("caret")
install.packages("kernlab")
install.packages("data.table")
install.packages("foreign")
install.packages("factoextra")
install.packages("corrplot")
install.packages("VIM")
library(mice)
library(mitools)
library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)
library(kernlab)
library(data.table)
library(foreign)
library(factoextra)
library(VIM)
library(corrplot)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(dendextend) # for comparing two dendrograms

### Setting up our working directory and loading in the required files ---------

setwd('C:/Users/tom_v/Dropbox/MSC DSS/Thesis/Rstudio/rwb')
dataDir <- "../data/"

X2019_Health <- read.spss(paste0(dataDir, '2019_Health.sav'), #Nov 2019
                          to.data.frame=TRUE)
X2020_Health <- read.spss(paste0(dataDir, '2020_Health.sav'), #May 2019
                          to.data.frame=TRUE)
X2019_Personality <- read.spss(paste0(dataDir, '2019_Personality.sav'), #Nov 2020 
                               to.data.frame=TRUE)
X2020_Personality <- read.spss(paste0(dataDir, '2020_Personality.sav'), #May 2020
                               to.data.frame=TRUE)


sum(!complete.cases(X2019_Health))
sum(!complete.cases(X2020_Health))
sum(!complete.cases(X2019_Personality))
sum(!complete.cases(X2020_Personality))
### Controlling for errors while importorting

### 2020Health Errors: 
### ch20m006, ch20m007, ch20m008, ch20m009, ch20m010 ,ch20m264,
### ch20m250, ch20m251, ch20m252, ch20m253, ch20m254

class(X2020_Health$ch20m010)
table(X2020_Health$ch20m010)

### 2019Health Errors: 
### ch19l006, ch19l007, ch19l008, ch19l009, ch19l010, ch19l264,
### ch19l250, ch19l251, ch19l252, ch19l253, ch19l254

class(X2019_Health$ch19l252)
table(X2019_Health$ch19l252)

### 2020Personality: 
### cp20l184, cp20l185, cp20l186, cp20l187, cp20l188

class(X2020_Health$ch20m010)
table(X2020_Health$ch20m010)

### 2019Personality: 
### cp19k184, cp19k185, cp19k186, cp19k187, cp19k188

class(X2020_Health$ch20m250)
table(X2020_Health$ch20m)

### After individually controlling all errors the following seem the problem:
### If R imports a variable where the answers could start with a letter AND a 
### number, R doesn't know what the order of these answers is. Therefor, 
### resulting in an 'Undeclared level' error.

### For this research the variables that gave a undeclared level error aren't 
### needed. Therefor they won't be threated until needed. 

### Non-Undeclared-level row:

class(X2020_Personality$cp20l010)
table(X2020_Personality$cp20l010)
plot(X2020_Personality$cp20l010)

### Above you can see that R can 'order' these answers. 

#### Data Preparation ----------------------------------------------------------

### Since the Expert (Brobert) said it will be very difficult without face-to-face
### interviews to determine a differential between depression and anxiety, we will
### create a new Dataset with all the relevant questions regarding 'mental health'.
### The variables that will be used can be found in the paper. 

### 2020 Data

X2020_MentalHealth_P <- data.frame(ID = X2020_Personality$nomem_encr,
                                 Cp20l010 = X2020_Personality$cp20l010,
                                 Cp20l011 = X2020_Personality$cp20l011,
                                 Cp20l012 = X2020_Personality$cp20l012,
                                 Cp20l015 = X2020_Personality$cp20l015,
                                 Cp20l016 = X2020_Personality$cp20l016,
                                 Cp20l023 = X2020_Personality$cp20l023,
                                 Cp20l028 = X2020_Personality$cp20l028,
                                 Cp20l033 = X2020_Personality$cp20l033,
                                 Cp20l063 = X2020_Personality$cp20l063,
                                 Cp20l070 = X2020_Personality$cp20l070, #Could be changed to 070 - 079
                                 #Cp20l145 = X2020_Personality$cp20l145,
                                 Cp20l151 = X2020_Personality$cp20l151,
                                 Cp20l154 = X2020_Personality$cp20l154,
                                 Cp20l155 = X2020_Personality$cp20l155,
                                 Cp20l156 = X2020_Personality$cp20l156,
                                 #Cp20l157 = X2020_Personality$cp20l157,
                                 #Cp20l159 = X2020_Personality$cp20l159,
                                 Cp20l199 = X2020_Personality$cp20l199,
                                 Cp20l202 = X2020_Personality$cp20l202)
                                 #Cp20l203 = X2020_Personality$cp20l203)

X2020_MentalHealth_H <- data.frame(ID = X2020_Health$nomem_encr,
                                   Gender = X2020_Health$ch20m001,
                                   Age = X2020_Health$ch20m002,
                                   Ch20m014 = X2020_Health$ch20m014,
                                   #Ch20m017 = X2020_Health$ch20m017,
                                   Ch20m021 = X2020_Health$ch20m021,
                                   Ch20m075 = X2020_Health$ch20m075,
                                   Ch20m076 = X2020_Health$ch20m076,
                                   Ch20m077 = X2020_Health$ch20m077,
                                   #Ch20m159 = X2020_Health$ch20m159,
                                   #Ch20m160 = X2020_Health$ch20m160,
                                   #Ch20m161 = X2020_Health$ch20m161,
                                   #Ch20m162 = X2020_Health$ch20m162,
                                   #Ch20m163 = X2020_Health$ch20m163,
                                   #Ch20m177 = X2020_Health$ch20m177,
                                   Ch20m178 = X2020_Health$ch20m178)
  
X2020_MentalHealth = merge(X2020_MentalHealth_H, X2020_MentalHealth_P, 
                           by.x = "ID")

# Environment cleanup
X2020_MentalHealth_H <- NULL
X2020_MentalHealth_P <- NULL
X2020_Health <- NULL
X2020_Personality <- NULL

### 2019 Data

X2019_MentalHealth_P <- data.frame(ID = X2019_Personality$nomem_encr,
                                 Cp19k010 = X2019_Personality$cp19k010,
                                 Cp19k011 = X2019_Personality$cp19k011,
                                 Cp19k012 = X2019_Personality$cp19k012,
                                 Cp19k015 = X2019_Personality$cp19k015,
                                 Cp19k016 = X2019_Personality$cp19k016,
                                 Cp19k023 = X2019_Personality$cp19k023,
                                 Cp19k028 = X2019_Personality$cp19k028,
                                 Cp19k033 = X2019_Personality$cp19k033,
                                 Cp19k063 = X2019_Personality$cp19k063,
                                 Cp19k070 = X2019_Personality$cp19k070, #Could be changed to 070 - 079
                                 #Cp19k145 = X2019_Personality$cp19k145,
                                 Cp19k151 = X2019_Personality$cp19k151,
                                 Cp19k154 = X2019_Personality$cp19k154,
                                 Cp19k155 = X2019_Personality$cp19k155,
                                 Cp19k156 = X2019_Personality$cp19k156,
                                 #Cp19k157 = X2019_Personality$cp19k157,
                                 #Cp19k159 = X2019_Personality$cp19k159,
                                 Cp19k199 = X2019_Personality$cp19k199,
                                 Cp19k202 = X2019_Personality$cp19k202)
                                 #Cp19k203 = X2019_Personality$cp19k203

X2019_MentalHealth_H <- data.frame(ID = X2019_Health$nomem_encr,
                                   Gender = X2019_Health$ch19l001,
                                   Age = X2019_Health$ch19l002,
                                   Ch19l014 = X2019_Health$ch19l014,
                                   #Ch19l017 = X2019_Health$ch19l017,
                                   Ch19l021 = X2019_Health$ch19l021,
                                   Ch19l075 = X2019_Health$ch19l075,
                                   Ch19l076 = X2019_Health$ch19l076,
                                   Ch19l077 = X2019_Health$ch19l077,
                                   #Ch19l159 = X2019_Health$ch19l159,
                                   #Ch19l160 = X2019_Health$ch19l160,
                                   #Ch19l161 = X2019_Health$ch19l161,
                                   #Ch19l162 = X2019_Health$ch19l162,
                                   #Ch19l163 = X2019_Health$ch19l163,
                                   #Ch19l177 = X2019_Health$ch19l177,
                                   Ch19l178 = X2019_Health$ch19l178)

X2019_MentalHealth = merge(X2019_MentalHealth_H, X2019_MentalHealth_P, 
                           by.x = "ID")

# Environment cleanup
X2019_MentalHealth_H <- NULL
X2019_MentalHealth_P <- NULL
X2019_Health <- NULL
X2019_Personality <- NULL

### Missing Data & Missing Data Threatment -------------------------------------

# Absolute values of missing data per dataset.

X2019_Missing_Ab <- colSums(is.na(X2019_MentalHealth))
X2020_Missing_Ab <- colSums(is.na(X2020_MentalHealth))

# Percentage Missing per Variable

X2019_Missing_Pr <- colMeans(is.na(X2019_MentalHealth))
X2020_Missing_Pr <- colMeans(is.na(X2020_MentalHealth))

# Filled in cases

X2019_NotMissing <- colSums(!is.na(X2019_MentalHealth))
X2020_NotMissing <- colSums(!is.na(X2020_MentalHealth))

cc <- md.pairs(X2019_MentalHealth)$rr/nrow(X2019_MentalHealth)
cc

### Recoding the Data ----------------------------------------------------------
# The higher the score, the higher chances of mental illnesses are. 


# Mentalhealth Database 2019
#Gender = X2019_Health$ch19l001,
#Age = X2019_Health$ch19l002

#Gender
X2019_MentalHealth$Gender <- recode(X2019_MentalHealth$Gender,
                                    '1' = 'Male', '2' = 'Female')

# This past month I felt depressed and gloomy
X2019_MentalHealth$Ch19l014 <- recode(X2019_MentalHealth$Ch19l014, 
                                      never = 0 , seldom = 1 , sometimes = 2 ,
                                      often = 3 , mostly = 4 , continuously = 5)

#Not needed, weight in kilo's.
X2019_MentalHealth$Ch19l017 

#To what extent did your physical health or emotional problems hinder your social activities over the past month?
X2019_MentalHealth$Ch19l021 <- recode(X2019_MentalHealth$Ch19l021,
                                      "not at all" = 0, "hardly" = 1, "a bit" = 2,
                                      "quite a lot" = 3, "very much" = 4)

#Do you regularly suffer from: headache
X2019_MentalHealth$Ch19l075 <- recode(X2019_MentalHealth$Ch19l075,
                                      "no" = 0, "yes" = 1)
#Do you regularly suffer from: fatigue
X2019_MentalHealth$Ch19l076 <- recode(X2019_MentalHealth$Ch19l076,
                                      "no" = 0, "yes" = 1)

#Do you regularly suffer from: sleeping problems
X2019_MentalHealth$Ch19l077 <- recode(X2019_MentalHealth$Ch19l077,
                                      "no" = 0, "yes" = 1)

#Did you use one or more of the following substances over the past month:
# Sedatives (such as valium)
X2019_MentalHealth$Ch19l159 <- recode(X2019_MentalHealth$Ch19l159,
                                      "never" = 0, "sometimes" = 1, "regularly" = 2)

# Soft drugs (such as hashish or marijuana)
X2019_MentalHealth$Ch19l160 <- recode(X2019_MentalHealth$Ch19l160,
                                      "never" = 0, "sometimes" = 1, "regularly" = 2)

# XTC
X2019_MentalHealth$Ch19l161 <- recode(X2019_MentalHealth$Ch19l161,
                                      "never" = 0, "sometimes" = 1, "regularly" = 2)

# Hallucinogens (Such as LSD, Magic Mushrooms)
X2019_MentalHealth$Ch19l162 <- recode(X2019_MentalHealth$Ch19l162,
                                      "never" = 0, "sometimes" = 1, "regularly" = 2)

# Hard drugs (such as stimulants, cocaine, heroin)
X2019_MentalHealth$Ch19l163 <- recode(X2019_MentalHealth$Ch19l163,
                                      "never" = 0, "sometimes" = 1, "regularly" = 2)

# Are you currently taking medicine at least once a week for:
# Sleeping problems
X2019_MentalHealth$Ch19l177 <- recode(X2019_MentalHealth$Ch19l177,
                                      "no" = 0, "yes" = 1)

# Anxiety or depression
#X2019_MentalHealth$Ch19l178 <- recode(X2019_MentalHealth$Ch19l178,
#                                      "no" = 0, "yes" = 1)

#10 On the whole, how happy would oyu say you are?
X2019_MentalHealth$Cp19k010 <- recode(X2019_MentalHealth$Cp19k010,
                                      "10 totally happy" = 0, '9' = 1, '8' = 2, 
                                      '7' = 3, '6' = 4, '5' = 5, '4' = 6, 
                                      '3' = 7, '2' = 8, '1' = 9, "0 totally unhappy" = 10)
#11 How satisfied are you with the life oyu lead at the moment?
X2019_MentalHealth$Cp19k011 <- recode(X2019_MentalHealth$Cp19k011,
                                      "10 completely satisfied" = 0, '9' = 1, '8' = 2, 
                                      '7' = 3, '6' = 4, '5' = 5, '4' = 6, 
                                      '3' = 7, '2' = 8, '1' = 9, "0 not at all satisfied" = 10)
#12 How do you feel at the moment?
X2019_MentalHealth$Cp19k012 <- recode(X2019_MentalHealth$Cp19k012,
                                      "7 very good" = 0, '6' = 1, '5' = 2, 
                                      '4' = 3, '3' = 4, '2' = 5, '1 very bad' = 6)
#15 The conditions of my life are excellent
X2019_MentalHealth$Cp19k015 <- recode(X2019_MentalHealth$Cp19k015,
                                      "7 strongly agree" = 0, '6 agree' = 1, 
                                      '5 slightly agree' = 2, '4 neither agree nor disagree' = 3, 
                                      '3 slightly disagree' = 4, '2 disagree' = 5, 
                                      '1 strongly disagree' = 6)
#16 I am Satisfied with my life
X2019_MentalHealth$Cp19k016 <- recode(X2019_MentalHealth$Cp19k016,
                                      "7 strongly agree" = 0, '6 agree' = 1, 
                                      '5 slightly agree' = 2, '4 neither agree nor disagree' = 3, 
                                      '3 slightly disagree' = 4, '2 disagree' = 5, 
                                      '1 strongly disagree' = 6)
#23 Get stressed out easily
X2019_MentalHealth$Cp19k023 <- recode(X2019_MentalHealth$Cp19k023,
                                      "very inaccurate" = 0, "moderately inaccurate" = 1,
                                      "neither inaccurate nor accurate" = 2, "moderately accurate" = 3,
                                      "very accurate" = 4)
#28 Am relaxed most of the time
X2019_MentalHealth$Cp19k028 <- recode(X2019_MentalHealth$Cp19k028,
                                      "very inaccurate" = 4, "moderately inaccurate" = 3,
                                      "neither inaccurate nor accurate" = 2, "moderately accurate" = 1,
                                      "very accurate" = 0)

#33 Worry about things
X2019_MentalHealth$Cp19k033 <- recode(X2019_MentalHealth$Cp19k033,
                                      "very inaccurate" = 0, "moderately inaccurate" = 1,
                                      "neither inaccurate nor accurate" = 2, "moderately accurate" = 3,
                                      "very accurate" = 4)

#63 Get irritated easily
X2019_MentalHealth$Cp19k063 <- recode(X2019_MentalHealth$Cp19k063,
                                      "very inaccurate" = 0, "moderately inaccurate" = 1,
                                      "neither inaccurate nor accurate" = 2, "moderately accurate" = 3,
                                      "very accurate" = 4)

#70 I feel that I am a person of worth, at least on an equal plane with others
X2019_MentalHealth$Cp19k070 <- recode(X2019_MentalHealth$Cp19k070,
                                      "1 totally disagree" = 6, "2" = 5, "3" = 4,
                                      "4" = 3, "5" = 2, "6" = 1, "7 totally agree" = 0)

#145 I am sometimes irritated by people who ask favors from me
X2019_MentalHealth$Cp19k145 <- recode(X2019_MentalHealth$Cp19k145,
                                      "False" = 0, "True" = 1)

#151 Indicate to what extend you feel, right now, at the present moment; guilty
X2019_MentalHealth$Cp19k151 <- recode(X2019_MentalHealth$Cp19k151,
                                      "1 not at all" = 0, "2" = 1, "3" = 2, "4" = 3,
                                      "5" = 4, "6" = 5, "7 extremely" = 6)

#154 Indicate to what extend you feel, right now, at the present moment; enthusiastic
X2019_MentalHealth$Cp19k154 <- recode(X2019_MentalHealth$Cp19k154,
                                      "1 not at all" = 6, "2" = 5, "3" = 4, "4" = 3,
                                      "5" = 2, "6" = 1, "7 extremely" = 0)

#155 Indicate to what extend you feel, right now, at the present moment; proud
X2019_MentalHealth$Cp19k155 <- recode(X2019_MentalHealth$Cp19k155,
                                      "1 not at all" = 6, "2" = 5, "3" = 4, "4" = 3,
                                      "5" = 2, "6" = 1, "7 extremely" = 0)

#156 Indicate to what extend you feel, right now, at the present moment; irritable
X2019_MentalHealth$Cp19k156 <- recode(X2019_MentalHealth$Cp19k156,
                                      "1 not at all" = 0, "2" = 1, "3" = 2, "4" = 3,
                                      "5" = 4, "6" = 5, "7 extremely" = 6)

#157 Indicate to what extend you feel, right now, at the present moment; alert
X2019_MentalHealth$Cp19k157 <- recode(X2019_MentalHealth$Cp19k157,
                                      "1 not at all" = 6, "2" = 5, "3" = 4, "4" = 3,
                                      "5" = 2, "6" = 1, "7 extremely" = 0)

#159 Indicate to what extend you feel, right now, at the present moment; inspired
X2019_MentalHealth$Cp19k159 <- recode(X2019_MentalHealth$Cp19k159,
                                      "1 not at all" = 6, "2" = 5, "3" = 4, "4" = 3,
                                      "5" = 2, "6" = 1, "7 extremely" = 0)

#199 Its easy for me to relax
X2019_MentalHealth$Cp19k199 <- recode(X2019_MentalHealth$Cp19k199,
                                      "strongly agree" = 0, "agree" = 1, "neutral" = 2,
                                      "disagree" = 3, "strongly disagree" = 4)

#202 I enjoy my friends a lot
X2019_MentalHealth$Cp19k202 <- recode(X2019_MentalHealth$Cp19k202,
                                      "strongly agree" = 0, "agree" = 1, "neutral" = 2,
                                      "disagree" = 3, "strongly disagree" = 4)

#203 Its important for me to keep busy
X2019_MentalHealth$Cp19k203 <- recode(X2019_MentalHealth$Cp19k203,
                                      "strongly agree" = 0, "agree" = 1, "neutral" = 2,
                                      "disagree" = 3, "strongly disagree" = 4)


# Sanity check

X2019_Missing_Pr_AfterRecode <- colMeans(is.na(X2019_MentalHealth))
X2019_Missing_Pr_AfterRecode
X2019_Missing_Pr
identical(X2019_Missing_Pr, X2019_Missing_Pr_AfterRecode)

### Mentalhealth Database 2020 ------------------------------------------------- 

#1 Gender

X2020_MentalHealth$Gender <- recode(X2020_MentalHealth$Gender,
                                    '1' = 'Male', '2' = 'Female')

#14 I felt depressed and gloomy
X2020_MentalHealth$Ch20m014 <- recode(X2020_MentalHealth$Ch20m014, 
                                      never = 0 , seldom = 1 , sometimes = 2 ,
                                      often = 3 , mostly = 4 , continuously = 5)

#17 Weight in kilo's
X2020_MentalHealth$Ch20m017 #Not needed to recode.

#21 To what extent did you physical health or emotional problems hinder your social activities over the past month?
X2020_MentalHealth$Ch20m021 <- recode(X2020_MentalHealth$Ch20m021,
                                      "not at all" = 0, "hardly" = 1, "a bit" = 2,
                                      "quite a lot" = 3, "very much" = 4)

#75 Do you regularly suffer from headache?
X2020_MentalHealth$Ch20m075 <- recode(X2020_MentalHealth$Ch20m075,
                                      "no" = 0, "yes" = 1)

#76 Do you regularly suffer from fatigue?
X2020_MentalHealth$Ch20m076 <- recode(X2020_MentalHealth$Ch20m076,
                                      "no" = 0, "yes" = 1)

#77 Do you regularly suffer from sleeping problems?
X2020_MentalHealth$Ch20m077 <- recode(X2020_MentalHealth$Ch20m077,
                                      "no" = 0, "yes" = 1)

#Did you use one or more of the following substances over the past month?
#159 Sedatives valium
X2020_MentalHealth$Ch20m159 <- recode(X2020_MentalHealth$Ch20m159,
                                      "never" = 0, "sometimes" = 1, "regularly" = 2)

#160 Softdrugs (Hashish, marijuana)
X2020_MentalHealth$Ch20m160 <- recode(X2020_MentalHealth$Ch20m160,
                                      "never" = 0, "sometimes" = 1, "regularly" = 2)

#161 XTC (MDMA)
X2020_MentalHealth$Ch20m161 <- recode(X2020_MentalHealth$Ch20m161,
                                      "never" = 0, "sometimes" = 1, "regularly" = 2)

#162 Hallucigens (LSD, Mushrooms)
X2020_MentalHealth$Ch20m162 <- recode(X2020_MentalHealth$Ch20m162,
                                      "never" = 0, "sometimes" = 1, "regularly" = 2)

#163 Hard drugs (Stimulants, cocaine, heroin)
X2020_MentalHealth$Ch20m163 <- recode(X2020_MentalHealth$Ch20m163,
                                      "never" = 0, "sometimes" = 1, "regularly" = 2)

#Are you currently taking medicine at least once a week for:
#177 Sleeping problems
X2020_MentalHealth$Ch20m177 <- recode(X2020_MentalHealth$Ch20m177,
                                      "no" = 0, "yes" = 1)
#178 Anxiety or depression
#X2020_MentalHealth$Ch20m178 <- recode(X2020_MentalHealth$Ch20m178,
#                                      "no" = 0, "yes" = 1)

# Personality Database 2020
#10 On the whole, how happy would you say you are?
X2020_MentalHealth$Cp20l010 <- recode(X2020_MentalHealth$Cp20l010,
                                      "10 totally happy" = 0, '9' = 1, '8' = 2, 
                                      '7' = 3, '6' = 4, '5' = 5, '4' = 6, 
                                      '3' = 7, '2' = 8, '1' = 9, "0 totally unhappy" = 10)
#11 How satisfied are you with the life you lead at the moment?
X2020_MentalHealth$Cp20l011 <- recode(X2020_MentalHealth$Cp20l011,
                                      "10 completely satisfied" = 0, '9' = 1, '8' = 2, 
                                      '7' = 3, '6' = 4, '5' = 5, '4' = 6, 
                                      '3' = 7, '2' = 8, '1' = 9, "0 not at all satisfied" = 10)
#12 How do you feel at the moment?
X2020_MentalHealth$Cp20l012 <- recode(X2020_MentalHealth$Cp20l012,
                                      "7 very good" = 0, '6' = 1, '5' = 2, 
                                      '4' = 3, '3' = 4, '2' = 5, '1 very bad' = 6)
#15 The conditions of my life are excellent
X2020_MentalHealth$Cp20l015 <- recode(X2020_MentalHealth$Cp20l015,
                                      "7 strongly agree" = 0, '6 agree' = 1, 
                                      '5 slightly agree' = 2, '4 neither agree nor disagree' = 3, 
                                      '3 slightly disagree' = 4, '2 disagree' = 5, 
                                      '1 strongly disagree' = 6)
#16 I am Satisfied with my life
X2020_MentalHealth$Cp20l016 <- recode(X2020_MentalHealth$Cp20l016,
                                      "7 strongly agree" = 0, '6 agree' = 1, 
                                      '5 slightly agree' = 2, '4 neither agree nor disagree' = 3, 
                                      '3 slightly disagree' = 4, '2 disagree' = 5, 
                                      '1 strongly disagree' = 6)
#23 Get stressed out easily
X2020_MentalHealth$Cp20l023 <- recode(X2020_MentalHealth$Cp20l023,
                                      "very inaccurate" = 0, "moderately inaccurate" = 1,
                                      "neither inaccurate nor accurate" = 2, "moderately accurate" = 3,
                                      "very accurate" = 4)
#28 Am relaxed most of the time
X2020_MentalHealth$Cp20l028 <- recode(X2020_MentalHealth$Cp20l028,
                                      "very inaccurate" = 4, "moderately inaccurate" = 3,
                                      "neither inaccurate nor accurate" = 2, "moderately accurate" = 1,
                                      "very accurate" = 0)

#33 Worry about things
X2020_MentalHealth$Cp20l033 <- recode(X2020_MentalHealth$Cp20l033,
                                      "very inaccurate" = 0, "moderately inaccurate" = 1,
                                      "neither inaccurate nor accurate" = 2, "moderately accurate" = 3,
                                      "very accurate" = 4)

#63 Get irritated easily
X2020_MentalHealth$Cp20l063 <- recode(X2020_MentalHealth$Cp20l063,
                                      "very inaccurate" = 0, "moderately inaccurate" = 1,
                                      "neither inaccurate nor accurate" = 2, "moderately accurate" = 3,
                                      "very accurate" = 4)

#70 I feel that I am a person of worth, at least on an equal plane with others
X2020_MentalHealth$Cp20l070 <- recode(X2020_MentalHealth$Cp20l070,
                                      "1 totally disagree" = 6, "2" = 5, "3" = 4,
                                      "4" = 3, "5" = 2, "6" = 1, "7 totally agree" = 0)

#145 I am sometimes irritated by people who ask favors from me
X2020_MentalHealth$Cp20l145 <- recode(X2020_MentalHealth$Cp20l145,
                                      "False" = 0, "True" = 1)

#151 Indicate to what extend you feel, right now, at the present moment; guilty
X2020_MentalHealth$Cp20l151 <- recode(X2020_MentalHealth$Cp20l151,
                                      "1 not at all" = 0, "2" = 1, "3" = 2, "4" = 3,
                                      "5" = 4, "6" = 5, "7 extremely" = 6)

#154 Indicate to what extend you feel, right now, at the present moment; enthusiastic
X2020_MentalHealth$Cp20l154 <- recode(X2020_MentalHealth$Cp20l154,
                                      "1 not at all" = 6, "2" = 5, "3" = 4, "4" = 3,
                                      "5" = 2, "6" = 1, "7 extremely" = 0)

#155 Indicate to what extend you feel, right now, at the present moment; proud
X2020_MentalHealth$Cp20l155 <- recode(X2020_MentalHealth$Cp20l155,
                                      "1 not at all" = 6, "2" = 5, "3" = 4, "4" = 3,
                                      "5" = 2, "6" = 1, "7 extremely" = 0)

#156 Indicate to what extend you feel, right now, at the present moment; irritable
X2020_MentalHealth$Cp20l156 <- recode(X2020_MentalHealth$Cp20l156,
                                      "1 not at all" = 0, "2" = 1, "3" = 2, "4" = 3,
                                      "5" = 4, "6" = 5, "7 extremely" = 6)

#157 Indicate to what extend you feel, right now, at the present moment; alert
X2020_MentalHealth$Cp20l157 <- recode(X2020_MentalHealth$Cp20l157,
                                      "1 not at all" = 6, "2" = 5, "3" = 4, "4" = 3,
                                      "5" = 2, "6" = 1, "7 extremely" = 0)

#159 Indicate to what extend you feel, right now, at the present moment; inspired
X2020_MentalHealth$Cp20l159 <- recode(X2020_MentalHealth$Cp20l159,
                                      "1 not at all" = 6, "2" = 5, "3" = 4, "4" = 3,
                                      "5" = 2, "6" = 1, "7 extremely" = 0)

#199 Its easy for me to relax
X2020_MentalHealth$Cp20l199 <- recode(X2020_MentalHealth$Cp20l199,
                                      "strongly agree" = 0, "agree" = 1, "neutral" = 2,
                                      "disagree" = 3, "strongly disagree" = 4)

#202 I enjoy my friends a lot
X2020_MentalHealth$Cp20l202 <- recode(X2020_MentalHealth$Cp20l202,
                                      "strongly agree" = 0, "agree" = 1, "neutral" = 2,
                                      "disagree" = 3, "strongly disagree" = 4)

#203 Its important for me to keep busy
X2020_MentalHealth$Cp20l203 <- recode(X2020_MentalHealth$Cp20l203,
                                      "strongly agree" = 0, "agree" = 1, "neutral" = 2,
                                      "disagree" = 3, "strongly disagree" = 4)


sum(!complete.cases(X2019_MentalHealth))
sum(!complete.cases(X2020_MentalHealth))
X2019_CMentalHealth <- X2019_MentalHealth[complete.cases(X2019_MentalHealth),]
X2020_CMentalHealth <- X2020_MentalHealth[complete.cases(X2020_MentalHealth),]
x2019_Missing <- X2019_MentalHealth[!complete.cases(X2019_MentalHealth),]

# Making Categories out of Age & Gender

# Gender
X2019_CMentalHealth$Gender <- factor(X2019_CMentalHealth$Gender, labels = c("Female", "Male"))
X2020_CMentalHealth$Gender <- factor(X2020_CMentalHealth$Gender, labels = c("Female", "Male"))

# Age
# Categories : 0-30, 31-60, 61-102
X2019_CMentalHealth$AgeCat <- cut(X2019_CMentalHealth$Age, breaks = c(min(X2019_CMentalHealth$Age),
                                                                        31,65,max(X2019_CMentalHealth$Age+1)), 
                                                                        right = FALSE)

X2020_CMentalHealth$AgeCat <- cut(X2020_CMentalHealth$Age, breaks = c(min(X2020_CMentalHealth$Age),
                                                                        31,65,max(X2020_CMentalHealth$Age+1)), 
                                                                        right = FALSE)
# Rewriting and replacement in Dataframe
X2019_CMentalHealth$AgeCat <- recode(X2019_CMentalHealth$AgeCat,
                                     "[16,31)" = "Young People (16 - 30)", "[31,65)" = 'Middle Aged People (31 - 64)', 
                                     "[65,103)" = "Elderly People (65+)")
X2019_CMentalHealth <- subset(X2019_CMentalHealth, select=c(ID,Gender,Ch19l178,AgeCat,Age:Cp19k202))
X2019_CMentalHealth$Ch19l178.1 <- NULL

X2020_CMentalHealth$AgeCat <- recode(X2020_CMentalHealth$AgeCat,
                                     "[16,31)" = "Young People (16 - 30)", "[31,65)" = 'Middle Aged People (31 - 64)', 
                                     "[65,104)" = "Elderly People (65+)")
X2020_CMentalHealth <- subset(X2020_CMentalHealth, select=c(ID,Gender,Ch20m178,AgeCat,Age:Cp20l202))
X2020_CMentalHealth$Ch20m178.1 <- NULL

# Plotting the new categorical variable AgeCat
# As can be seen in the plot, the dataset doesn't contain too much observation from 16 - 30.
plot(X2019_CMentalHealth$AgeCat)  
plot(X2020_CMentalHealth$AgeCat)

summary(X2019_CMentalHealth)
summary(X2020_CMentalHealth)

### Splitting the Dataset based on Age Groups -----------------------------------

YoungPeople_2019 <- filter(X2019_CMentalHealth, AgeCat == "Young People (16 - 30)")
MiddlePeople_2019 <- filter(X2019_CMentalHealth, AgeCat == "Middle Aged People (31 - 64)")
OldPeople_2019 <- filter(X2019_CMentalHealth, AgeCat == "Elderly People (65+)")

YoungPeople_2020 <- filter(X2020_CMentalHealth, AgeCat == "Young People (16 - 30)")
MiddlePeople_2020 <- filter(X2020_CMentalHealth, AgeCat == "Middle Aged People (31 - 64)")
OldPeople_2020 <- filter(X2020_CMentalHealth, AgeCat == "Elderly People (65+)")

### Normalizing the data -------------------------------------------------------
# Done using scale() function --> https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/scale

# Whole 2019 Dataset
X2019_N_CMentalHealth <- data.frame(scale(X2019_CMentalHealth[,c(6:26)], center = TRUE, scale = TRUE))
X2019_N_CMentalHealth$ID <- X2019_CMentalHealth$ID
X2019_N_CMentalHealth$Gender <- X2019_CMentalHealth$Gender
X2019_N_CMentalHealth$AgeCat <- X2019_CMentalHealth$AgeCat
X2019_N_CMentalHealth$Age <- X2019_CMentalHealth$Age
X2019_N_CMentalHealth$Ch19l178 <- X2019_CMentalHealth$Ch19l178
X2019_N_CMentalHealth <- subset(X2019_N_CMentalHealth, select=c(ID, Gender, AgeCat, Age, Ch19l178, Ch19l014:Cp19k202))
head(X2019_N_CMentalHealth)
summary(X2019_N_CMentalHealth)

# Whole 2020 Dataset
X2020_N_CMentalHealth <- data.frame(scale(X2020_CMentalHealth[,c(6:26)], center = TRUE, scale = TRUE))
X2020_N_CMentalHealth$ID <- X2020_CMentalHealth$ID
X2020_N_CMentalHealth$Gender <- X2020_CMentalHealth$Gender
X2020_N_CMentalHealth$AgeCat <- X2020_CMentalHealth$AgeCat
X2020_N_CMentalHealth$Age <- X2020_CMentalHealth$Age
X2020_N_CMentalHealth$Ch20m178 <- X2020_CMentalHealth$Ch20m178
X2020_N_CMentalHealth <- subset(X2020_N_CMentalHealth, select=c(ID, Gender, AgeCat, Age, Ch20m178, Ch20m014:Cp20l202))
head(X2019_N_CMentalHealth)
summary(X2019_N_CMentalHealth)

# 2019 Age Groups
# Yung '19
X2019_YN_CMentalHealth <- data.frame(scale(YoungPeople_2019[,c(6:26)], center = TRUE, scale = TRUE))
X2019_YN_CMentalHealth$ID <- YoungPeople_2019$ID
X2019_YN_CMentalHealth$Gender <- YoungPeople_2019$Gender
X2019_YN_CMentalHealth$AgeCat <- YoungPeople_2019$AgeCat
X2019_YN_CMentalHealth$Age <- YoungPeople_2019$Age
X2019_YN_CMentalHealth$Ch19l178 <- YoungPeople_2019$Ch19l178
X2019_YN_CMentalHealth <- subset(X2019_YN_CMentalHealth, select=c(ID, Gender, AgeCat, Age, Ch19l178, Ch19l014:Cp19k202))
head(X2019_YN_CMentalHealth)
summary(X2019_YN_CMentalHealth)
# Middle '19
X2019_MN_CMentalHealth <- data.frame(scale(MiddlePeople_2019[,c(6:26)], center = TRUE, scale = TRUE))
X2019_MN_CMentalHealth$ID <- MiddlePeople_2019$ID
X2019_MN_CMentalHealth$Gender <- MiddlePeople_2019$Gender
X2019_MN_CMentalHealth$AgeCat <- MiddlePeople_2019$AgeCat
X2019_MN_CMentalHealth$Age <- MiddlePeople_2019$Age
X2019_MN_CMentalHealth$Ch19l178 <- MiddlePeople_2019$Ch19l178
X2019_MN_CMentalHealth <- subset(X2019_MN_CMentalHealth, select=c(ID, Gender, AgeCat, Age, Ch19l178, Ch19l014:Cp19k202))
head(X2019_MN_CMentalHealth)
summary(X2019_MN_CMentalHealth)
# Elder '19
X2019_EN_CMentalHealth <- data.frame(scale(OldPeople_2019[,c(6:26)], center = TRUE, scale = TRUE))
X2019_EN_CMentalHealth$ID <- OldPeople_2019$ID
X2019_EN_CMentalHealth$Gender <- OldPeople_2019$Gender
X2019_EN_CMentalHealth$AgeCat <- OldPeople_2019$AgeCat
X2019_EN_CMentalHealth$Age <- OldPeople_2019$Age
X2019_EN_CMentalHealth$Ch19l178 <- OldPeople_2019$Ch19l178
X2019_EN_CMentalHealth <- subset(X2019_EN_CMentalHealth, select=c(ID, Gender, AgeCat, Age, Ch19l178, Ch19l014:Cp19k202))
head(X2019_EN_CMentalHealth)
summary(X2019_EN_CMentalHealth)

# 2020 Age Groups
# Young '20
X2020_YN_CMentalHealth <- data.frame(scale(YoungPeople_2020[,c(6:26)], center = TRUE, scale = TRUE))
X2020_YN_CMentalHealth$ID <- YoungPeople_2020$ID
X2020_YN_CMentalHealth$Gender <- YoungPeople_2020$Gender
X2020_YN_CMentalHealth$AgeCat <- YoungPeople_2020$AgeCat
X2020_YN_CMentalHealth$Age <- YoungPeople_2020$Age
X2020_YN_CMentalHealth$Ch20m178 <- YoungPeople_2020$Ch20m178
X2020_YN_CMentalHealth <- subset(X2020_YN_CMentalHealth, select=c(ID, Gender, AgeCat, Age, Ch20m178, Ch20m014:Cp20l202))
head(X2020_YN_CMentalHealth)
summary(X2020_YN_CMentalHealth)
# Middle '20
X2020_MN_CMentalHealth <- data.frame(scale(MiddlePeople_2020[,c(6:26)], center = TRUE, scale = TRUE))
X2020_MN_CMentalHealth$ID <- MiddlePeople_2020$ID
X2020_MN_CMentalHealth$Gender <- MiddlePeople_2020$Gender
X2020_MN_CMentalHealth$AgeCat <- MiddlePeople_2020$AgeCat
X2020_MN_CMentalHealth$Age <- MiddlePeople_2020$Age
X2020_MN_CMentalHealth$Ch20m178 <- MiddlePeople_2020$Ch20m178
X2020_MN_CMentalHealth <- subset(X2020_MN_CMentalHealth, select=c(ID, Gender, AgeCat, Age, Ch20m178, Ch20m014:Cp20l202))
head(X2020_MN_CMentalHealth)
summary(X2020_MN_CMentalHealth)
# Elder '20
X2020_EN_CMentalHealth <- data.frame(scale(OldPeople_2020[,c(6:26)], center = TRUE, scale = TRUE))
X2020_EN_CMentalHealth$ID <- OldPeople_2020$ID
X2020_EN_CMentalHealth$Gender <- OldPeople_2020$Gender
X2020_EN_CMentalHealth$AgeCat <- OldPeople_2020$AgeCat
X2020_EN_CMentalHealth$Age <- OldPeople_2020$Age
X2020_EN_CMentalHealth$Ch20m178 <- OldPeople_2020$Ch20m178
X2020_EN_CMentalHealth <- subset(X2020_EN_CMentalHealth, select=c(ID, Gender, AgeCat, Age, Ch20m178, Ch20m014:Cp20l202))
head(X2020_EN_CMentalHealth)
summary(X2020_EN_CMentalHealth)

####################### Principal Component Analysis

# Creating Correlation Matrixes for the different Datasets ---------------------
# Using Corrplot()

# Whole Dataset

Corr_Matr19 <- cor(X2019_N_CMentalHealth[c(6:26)])
Corr_Matr20 <- cor(X2020_N_CMentalHealth[c(6:26)])

corrplot(Corr_Matr19, method = "circle",  cl.pos = "b", title = "Correlation Matrix 2019", mar = c(0,0,1,0))
corrplot(Corr_Matr20, method = "circle",  cl.pos = "b", title = "Correlation Matrix 2020", mar = c(0,0,1,0))

#Age Groups
Corr_Matr19_Y <- cor(X2019_YN_CMentalHealth[c(6:26)])
Corr_Matr19_M <- cor(X2019_MN_CMentalHealth[c(6:26)])
Corr_Matr19_E <- cor(X2019_EN_CMentalHealth[c(6:26)])
Corr_Matr20_Y <- cor(X2020_YN_CMentalHealth[c(6:26)])
Corr_Matr20_M <- cor(X2020_MN_CMentalHealth[c(6:26)])
Corr_Matr20_E <- cor(X2020_EN_CMentalHealth[c(6:26)])

corrplot(Corr_Matr19_Y, method = "circle",  cl.pos = "b", title = "Correlation Matrix 2019, Young People", mar = c(0,0,1,0))
corrplot(Corr_Matr19_M, method = "circle",  cl.pos = "b", title = "Correlation Matrix 2019, Middle People", mar = c(0,0,1,0))
corrplot(Corr_Matr19_E, method = "circle",  cl.pos = "b", title = "Correlation Matrix 2019, Elder People", mar = c(0,0,1,0))
corrplot(Corr_Matr20_Y, method = "circle",  cl.pos = "b", title = "Correlation Matrix 2020, Young People", mar = c(0,0,1,0))
corrplot(Corr_Matr20_M, method = "circle",  cl.pos = "b", title = "Correlation Matrix 2020, Middle People", mar = c(0,0,1,0))
corrplot(Corr_Matr20_E, method = "circle",  cl.pos = "b", title = "Correlation Matrix 2020, Elder People", mar = c(0,0,1,0))

### Principal Component Analysis -------------------------------------------------
# PCA done using the prcomp() function: https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/prcomp
# Visualization done with Screeplot() function: https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/screeplot

#2019 & 2020 Complete
X2019_PCA_C <- prcomp(X2019_N_CMentalHealth[c(6:26)])
summary(X2019_PCA_C)
X2020_PCA_C <- prcomp(X2020_N_CMentalHealth[c(6:26)])
summary(X2020_PCA_C)

X2019_PCAL_C <- pcomp

#2019 AgeGroups
X2019_PCA_Y <- prcomp(X2019_YN_CMentalHealth[c(6:26)])
X2019_PCA_M <- prcomp(X2019_MN_CMentalHealth[c(6:26)])
X2019_PCA_E <- prcomp(X2019_EN_CMentalHealth[c(6:26)])
summary(X2019_PCA_Y)
summary(X2019_PCA_M)
summary(X2019_PCA_E)

#2020 AgeGroups
X2020_PCA_Y <- prcomp(X2020_YN_CMentalHealth[c(6:26)])
X2020_PCA_M <- prcomp(X2020_MN_CMentalHealth[c(6:26)])
X2020_PCA_E <- prcomp(X2020_EN_CMentalHealth[c(6:26)])
summary(X2020_PCA_Y)
summary(X2020_PCA_M)
summary(X2020_PCA_E)
# Screeplots to visualize the PCA Variance Explained
# Source: https://towardsdatascience.com/principal-component-analysis-pca-101-using-r-361f4c53a9ff

# 2019 & 2020
screeplot(X2019_PCA_C, type = "b", npcs = 15, main = "Screeplot of the first 15 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)
Cumpro_2019 <- cumsum(X2019_PCA_C$sdev^2 / sum(X2019_PCA_C$sdev^2))
plot(Cumpro_2019[0:15], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot 2019")
abline(v = 7, col="blue", lty=5)
abline(h = 0.70, col="blue", lty=5)
legend("bottomright", legend=c("Cut-off @ PC5"),
       col=c("blue"), lty=5, cex=0.6)

screeplot(X2020_PCA_C, type = "l", npcs = 15, main = "Screeplot of the first 15 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)
Cumpro_2020 <- cumsum(X2020_PCA_C$sdev^2 / sum(X2020_PCA_C$sdev^2))
plot(Cumpro_2020[0:15], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot 2020")
abline(v = 7, col="blue", lty=5)
abline(v = 3, col= "red", lty=5 )
abline(h = 0.7, col = "red", lty =5)
abline(h = 0.7, col="blue", lty=5)
legend("bottomright", legend=c("Cut-off @ PC5", "Cut-off @ PC3"),
       col=c("blue", "red"), lty=5, cex=0.6)

### 2019 & 2020 PCA Data-frame - Complete
compX_2019C <- data.frame(X2019_PCA_C$x[,1:21],X2019_CMentalHealth$Ch19l178, X2019_CMentalHealth$AgeCat)
compX_2020C <- data.frame(X2020_PCA_C$x[,1:21],X2020_CMentalHealth$Ch20m178, X2020_CMentalHealth$AgeCat)

compX_2019CY <- filter(compX_2019C, X2019_CMentalHealth.AgeCat == "Young People (16 - 30)")
compX_2019CM <- filter(compX_2019C, X2019_CMentalHealth.AgeCat == "Middle Aged People (31 - 64)")
compX_2019CE <- filter(compX_2019C, X2019_CMentalHealth.AgeCat == "Elderly People (65+)")
compX_2020CY <- filter(compX_2020C, X2020_CMentalHealth.AgeCat == "Young People (16 - 30)")
compX_2020CM <- filter(compX_2020C, X2020_CMentalHealth.AgeCat == "Middle Aged People (31 - 64)")
compX_2020CE <- filter(compX_2020C, X2020_CMentalHealth.AgeCat == "Elderly People (65+)")



# Check
plot(compX_2019C, pch=16, col=rgb(0,0,0,0.5))
plot3d(compX_2019C$PC1, compX_2019C$PC2, compX_2019C$PC3, col =as.integer(compX_2019C$X2019_CMentalHealth.Ch19l178),pch=1)

table(compX_2019C$X2019_CMentalHealth.Ch19l178)

# Young 2019 vs 2020
# 2019
compX_2019Y <- data.frame(X2019_PCA_Y$x[,1:21],X2019_YN_CMentalHealth$Ch19l178, X2019_YN_CMentalHealth$AgeCat)
plot3d(compX_2019Y$PC1, compX_2019Y$PC2, compX_2019Y$PC3, col = as.integer(compX_2019Y$X2019_YN_CMentalHealth.Ch19l178))
# 2020
compX_2020Y <- data.frame(X2020_PCA_Y$x[,1:21],X2020_YN_CMentalHealth$Ch20m178, X2020_YN_CMentalHealth$AgeCat)
plot3d(compX_2020Y$PC1, compX_2020Y$PC2, compX_2020Y$PC3, col = as.integer(compX_2020Y$X2020_YN_CMentalHealth.Ch20m178))

# Middle 2019 vs 2020
# 2019
compX_2019M <- data.frame(X2019_PCA_M$x[,1:21],X2019_MN_CMentalHealth$Ch19l178, X2019_MN_CMentalHealth$AgeCat)
plot3d(compX_2019M$PC1, compX_2019M$PC2, compX_2019M$PC3, col = as.integer(compX_2019M$X2019_MN_CMentalHealth.Ch19l178))
# 2020
compX_2020M <- data.frame(X2020_PCA_M$x[,1:21],X2020_MN_CMentalHealth$Ch20m178, X2020_MN_CMentalHealth$AgeCat)
plot3d(compX_2020M$PC1, compX_2020M$PC2, compX_2020M$PC3, col = as.integer(compX_2020M$X2020_MN_CMentalHealth.Ch20m178))

# Elder 2019 vs 2020
# 2019
compX_2019E <- data.frame(X2019_PCA_E$x[,1:21])
compX_2019E <- data.frame(X2019_PCA_E$x[,1:21],X2019_EN_CMentalHealth$Ch19l178, X2019_EN_CMentalHealth$AgeCat)
plot3d(compX_2019E$PC1, compX_2019E$PC2, compX_2019E$PC3, col = as.integer(compX_2019E$X2019_EN_CMentalHealth.Ch19l178))
# 2020
compX_2020E <- data.frame(X2020_PCA_E$x[,1:21],X2020_EN_CMentalHealth$Ch20m178, X2020_EN_CMentalHealth$AgeCat)
plot3d(compX_2020E$PC1, compX_2020E$PC2, compX_2020E$PC3, col = as.integer(compX_2020E$X2020_EN_CMentalHealth.Ch20m178))

### Visualizing the PCA's
library(ggbiplot)

#Complete
BIPLOT2019 <- ggbiplot(X2019_PCA_C, groups = X2019_CMentalHealth$Ch19l178, obs.scale = 1) + 
                  labs(x = "PC1 (34.5% explained var.)", y = "PC2 (9.1% explained var.)", colour = "Medication") 
                  + theme(legend.position = "none")
BIPLOT2020 <- ggbiplot(X2020_PCA_C, groups = X2020_CMentalHealth$Ch20m178, obs.scale = 1) +
                  labs(x = "PC1 (33.6% explained var.)", y = "PC2 (8.9% explained var.)", colour = "Medication")

annotate_figure(ggarrange(BIPLOT2019, BIPLOT2020, nrow=1, ncol=2, align = "hv", common.legend = TRUE, legend = "right"), 
                          top = "Scaling of Variables, Left = 2019, Right = 2020")

#Youth
BIPLOT_Y_2019 <- ggbiplot(X2019_PCA_Y, groups = X2019_YN_CMentalHealth$Ch19l178, obs.scale = 1) + 
                  labs(x = "PC1 (33.1% explained var.)", y = "PC2(9.8% explained var.)", colour = "Medication") +
                  xlim(-10, 7.5) +
                  ylim(-10,6) 
BIPLOT_Y_2020 <- ggbiplot(X2020_PCA_Y, groups = X2020_YN_CMentalHealth$Ch20m178, obs.scale = 1) + 
                  labs(x = "PC1 (32.9% explained var.)", y = "PC2(9.4% explained var.)", colour = "Medication") +
                  xlim(-10, 7.5) +
                  ylim(-10,6)

annotate_figure(ggarrange(BIPLOT_Y_2019, BIPLOT_Y_2020, nrow=1, ncol=2, align = "hv", common.legend = TRUE, legend = "right"), 
                          top = "Scaling of Variables, Left = 2019, Right = 2020")

#Elder
BIPLOT_E_2019 <- ggbiplot(X2019_PCA_E, groups = X2019_EN_CMentalHealth$Ch19l178, obs.scale = 1) + 
                  labs(x = "PC1 (32.5% explained var.)", y = "PC2(8.7% explained var.)", colour = "Medication") +
                  xlim(-7, 12) +
                  ylim(-7.5,7.5) 
BIPLOT_E_2020 <- ggbiplot(X2020_PCA_E, groups = X2020_EN_CMentalHealth$Ch20m178, obs.scale = 1) + 
                  labs(x = "PC1 (31.6% explained var.)", y = "PC2(8.7% explained var.)", colour = "Medication") +
                  xlim(-7, 12) +
                  ylim(-7.5,7.5)

annotate_figure(ggarrange(BIPLOT_E_2019, BIPLOT_E_2020, nrow=1, ncol=2, align = "hv", common.legend = TRUE, legend = "right"), 
                          top = "Scaling of Variables, Left = 2019, Right = 2020")

### Visualizations of Scaling of the PC's

#Complete
PCAPLOT_2019 <- data.frame('Medication' = X2019_CMentalHealth$Ch19l178, X2019_PCA_C$x[,1:2])
PCAPLOT_2020 <- data.frame('Medication' = X2020_CMentalHealth$Ch20m178, X2020_PCA_C$x[,1:2])

BIPLOT20202 <- ggplot(data = PCAPLOT_2020) + 
  geom_point(aes(x = PC1, y = PC2, col = Medication)) + 
  theme_minimal() 
BIPLOT20192 <- ggplot(data = PCAPLOT_2019) +
  geom_point(aes(x = PC1, y = PC2, col = Medication)) +
  theme_minimal()

annotate_figure(ggarrange(BIPLOT20192, BIPLOT20202, nrow=2, ncol=1, align = "hv", common.legend = TRUE, legend = "bottom"), top = "Control Variable inside the PC plots. Top = 2019. Bottom = 2020")

#Youth
PCAPLOT_2019Y <- data.frame('Medication' = X2019_YN_CMentalHealth$Ch19l178, X2019_PCA_Y$x[,1:2])
PCAPLOT_2020Y <- data.frame('Medication' = X2020_YN_CMentalHealth$Ch20m178, X2020_PCA_Y$x[,1:2])

BIPLOT2020_Y <- ggplot(data = PCAPLOT_2020Y) + 
  geom_point(aes(x = PC1, y = PC2, col = Medication)) + 
  theme_minimal() +
  xlim(-10, 7.5) +
  ylim(-10, 6)
BIPLOT2019_Y <- ggplot(data = PCAPLOT_2019Y) +
  geom_point(aes(x = PC1, y = PC2, col = Medication)) +
  theme_minimal() +
  xlim(-10, 7.5) +
  ylim(-10, 6)

annotate_figure(ggarrange(BIPLOT2019_Y, BIPLOT2020_Y, nrow=2, ncol=1, align = "hv", common.legend = TRUE, legend = "bottom"), top = "Control Variable inside the 'Youth' PC plots. Top = 2019. Bottom = 2020")

#Elder
PCAPLOT_2019E <- data.frame('Medication' = X2019_EN_CMentalHealth$Ch19l178, X2019_PCA_E$x[,1:2])
PCAPLOT_2020E <- data.frame('Medication' = X2020_EN_CMentalHealth$Ch20m178, X2020_PCA_E$x[,1:2])

BIPLOT2020_E <- ggplot(data = PCAPLOT_2020E) + 
  geom_point(aes(x = PC1, y = PC2, col = Medication)) + 
  theme_minimal() +
  xlim(-7, 12) +
  ylim(-7.5, 7.5)
BIPLOT2019_E <- ggplot(data = PCAPLOT_2019E) +
  geom_point(aes(x = PC1, y = PC2, col = Medication)) +
  theme_minimal() +
  xlim(-7, 12) +
  ylim(-7.5, 7.5)

annotate_figure(ggarrange(BIPLOT2019_E, BIPLOT2020_E, nrow=2, ncol=1, align = "hv", common.legend = TRUE, legend = "bottom"), top = "Control Variable inside the 'Elder' PC plots. Top = 2019. Bottom = 2020")
