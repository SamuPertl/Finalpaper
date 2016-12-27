############ Overview of this script 
# This script loads the the packages & data and further recodes all necessary variables
# Finally I create a new data frame which I use for my visual and statistical analysis as well as for the regression diagnostic 


########## Stepts 
# 1) load the packages 
# 2) load the data 
# 3) recode the variables
# 4) create a new data frame with all variables and exclude all rows where my response variable is NA


# install all the packages
list.of.packages <- c("car","readstata13", "dplyr", "ggplot2","VGAM", "broom", "lmtest",  "tseries", "tikzDevice", "scales", "stargazer", "bstats", "faraway", "gvlma", "MASS")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)>0){ install.packages(new.packages)
} else print("All required packages installed")

#install.packages("VGAM")
#install.packages("broom")
#install.packages("lmtest")
#install.packages("tseries")
#install.packages("tikzDevice")
#install.packages("scales")
#install.packages("stargazer")
#install.packages("bstats")
#install.packages("faraway")
#install.packages("gvlma")
#install.packages("MASS")
#install.packages("car")
#install.packages("readstata13")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("tseries")


# load all the packages 
library(car)
library(readstata13)
library(dplyr)
library(ggplot2)
library(VGAM)
library(broom)
library(lmtest)
library(tseries)
library(tikzDevice)
library(scales)
library(stargazer)
library(faraway)
library(gvlma)
library(MASS)



# create working directory
# set the working directory to my project 
#setwd("/Users/Samuel/Dropbox/ZeppelinUniversita\314\210t/Git Hub/Finalpaper/R-Code/")

# create working directory 
wkdir <- getwd()


# create my folders 
folders <- c("figures", "data", "diagnostic")


#create the folders
for(i in 1:length(folders)){
  if(file.exists(folders[i]) == "FALSE") {
    dir.create(file.path(wkdir, folders[i]))
  } else print("Folder Already Exists")}

# code the path to the folders 
p.data <- paste(wkdir, "/data/", sep="")
p.figures <- paste(wkdir, "/figures/", sep = "")
p.diagnostic <- paste(wkdir, "/diagnostic/", sep = "")



# load the data set
pre <- data.frame(read.dta13(paste(p.data, "pre_2005_1_dt.dta", sep = ""), convert.factors = F))




########### code all the variables 

# Gender as dummy variable, male = 1 female = 0
male = Recode(pre$sex, "2=0; -1=NA; -2=NA; -3=NA") # recode from the car package
# in the original data set female = 2; I recode is at a 0 so we have a dummy variable that is easy to interpret

# Recode variable year of birth
gebdat = Recode(pre$gebdat, "-1=NA; -2=NA; -3=NA")

# create variable age: age = 2005 (year of the experiment) - year of birth 
age = 2005 - gebdat

# religious affiliation (dummy) 1=member of a religious community = 1;  0= no member of a religious community 
religion = Recode(pre$f02601, "1=1; 2=1; 3=1; 4=1; 5=1; 6=0; -1=NA; -2=NA; -3=NA")

# religious communities 
relicom = Recode(pre$f02601, "1 = 'Katholisch'; 2 = 'Protestantisch'; 3 = 'Andere christliche Gemeinschaft'; 4 = 'Islam'; 5 = 'Andere religioese Gemeinschaft'; 6 = 'keine Religionszugehoerigkeit'; -1=NA; -2=NA; -3=NA")

# religious intensity (dummy) 1 = intense (at least once a week/month); 0 = not intense (les often/never)
reliintens <- Recode(pre$f02509, "1=1; 2=1; 3=0; 4=0; -1=NA")


# Income

# monthly income 
income1 = Recode(pre$hnetto, "-1=NA; -2=NA; -3=NA")

# income reported in intervals 
income2 = Recode(pre$znetto, "-1=NA; -2=NA; -3=NA; 21=NA; 31=NA; 41=NA; 51=NA; 1=375; 2=1125; 3=2000; 4=3000; 5=4250; 6=7500")

# replace all NA´s in income1 with values from income2 
income1[which(is.na(income1))] <- income2[is.na(income1)]

# take the log of income1 
logincom = log(income1)

# Liquid constraint (dummy variable) 1 = no constraint; 0 = constraint 
constraint = Recode(pre$f093, "-1=NA; 2=0" )

# Education (dummy variable for different educational degrees)

# Abitur (dummy) 1 = high school degree  0 = no high-school degree   
abi = Recode(pre$f00802, "1=0; 2=0; 3=1; 4=1; 5=0; 6=0; 0=NA; -1=NA -2=NA; -3=NA")

# replace all NA´s with 0 
abi[which(is.na(abi))] <- 0

#Realschule (dummy) 1= secondary school degree, 0= no secondary school degree  
real = Recode(pre$f00802, "1=0; 2=1; 3=0; 4=0; 5=0; 6=0; -1=NA")

# replace all NA´s with 0
real[which(is.na(real))] <- 0 

tablereal <- with(pre, table(real))
tablereal

# no degree (dummy) 1 = no school leaving degree , 0 = degree 
nodegree = Recode(pre$f00802, "1=0; 2=0; 3=0; 4=0; 5=0; 6=1; -1=NA")

# replace all NA´s with zero 
nodegree[which(is.na(nodegree))] <- 0

tablenodegree <- with(pre, table(nodegree))
tablenodegree

# other degree (dummy) 1 = other degree, 0 = herkömmlicher Abschluss 
otherdegree = Recode(pre$f00802, "1=0; 2=0; 3=0; 4=0; 5=1; 6=0; -1=NA")

# replace all NA´s with zero 
otherdegree[which(is.na(otherdegree))] <- 0

tableother <- with(pre, table(otherdegree))
tableother

# enrolled in school (dummy) 1= still in school, 0= graduated 
enrolled = Recode(pre$f00801, "0=0; 1=1; -1=NA")

# replace all NA`s with zero 
enrolled[which(is.na(enrolled))] <- 0


# intertemporal choice 
# switching line = T012 
T012 = pre$z1spaet

# create a variable for the return or internal rate of return 
return = Recode(pre$z1spaet, "1=2.5; 2=5; 3=7.5; 4=10; 5=12.5; 6=15; 7=17.5; 8=20; 9=22.5; 10=25; 11=27.5; 12=30; 13=32.5; 14=35; 15=37.5; 16=40; 17=42.5; 18=45; 19=47.5; 20=50; 21=52.5")

# Big 5 psychology traits 

# Conscientiousness
G1 = Recode(pre$f02201, "-1=NA; -2=NA; -3=NA")
# reverse the polarity 
G2 = Recode(pre$f02207, "-1=NA; -2=NA; -3=NA; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1")
G3 = Recode(pre$f02211, "-1=NA; -2=NA; -3=NA")

# Extraversion
E1 = Recode(pre$f02202, "-1=NA; -2=NA; -3=NA")
E2 = Recode(pre$f02208, "-1=NA; -2=NA; -3=NA")
#reverse the polarity 
E3 = Recode(pre$f02212, "-1=NA; -2=NA; -3=NA; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1")

# Agreeableness
#reverse the polarity 
V1 = Recode(pre$f02203, "-1=NA; -2=NA; -3=NA; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1")
V2 = Recode(pre$f02206, "-1=NA; -2=NA; -3=NA")
V3 = Recode(pre$f02213, "-1=NA; -2=NA; -3=NA")

# Openness 
O1 = Recode(pre$f02204, "-1=NA; -2=NA; -3=NA")
O2 = Recode(pre$f02209, "-1=NA; -2=NA; -3=NA")
O3 = Recode(pre$f02214, "-1=NA; -2=NA; -3=NA")

#Neuroticism 
N1 = Recode(pre$f02205, "-1=NA; -2=NA; -3=NA")
N2 = Recode(pre$f02210, "-1=NA; -2=NA; -3=NA")
N3 = Recode(pre$f02215, "-1=NA; -2=NA; -3=NA; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1")



# for each Big Five add all three questions to an overall score 
# than scale it/ z-transformation so it can interpreted in comparison to the population
# Conscientiousness 
big_g <- G1 + G2 + G3

scaledbig_g = scale(big_g, center = TRUE, scale = TRUE)

# Extraversion 
big_e = E1 + E2 + E3

scaledbig_e = scale(big_e, center = TRUE, scale = TRUE)

# Agreeableness 
big_v = V1 + V2 + V3

scaledbig_v = scale(big_v, center = TRUE, scale = TRUE)

# Opennness  
big_o = O1 + O2 + O3

scaledbig_o = scale(big_o, center = TRUE, scale = TRUE)

# Neurotizismus 
big_n = N1 + N2 + N3

scaledbig_n = scale(big_n, center = TRUE, scale = TRUE)

# cognitive ability 

# word fluency test | I have to check this again 
wordtest = Recode(pre$f96t30g, "0=NA; -1=NA; -2=NA; -3=NA; -4=NA")
# mention that 0 = na and why

scaledwordtest = scale(wordtest, center = TRUE, scale = TRUE)

# symbole-digit correspondence test 
symtest = Recode(pre$f99z90r, "0=NA; -1=NA; -2=NA; -3=NA; -4=NA")

scaledsymtest = scale(symtest, center = TRUE, scale = TRUE)

# region (dummy) 1 = West, 0 = Ost 
west = Recode(pre$westost, "-1=NA; -2=NA; -3=NA; 0=1; 1=0")

# create new data frame with all the variables that I created 
# create new dataset 
dt <-data.frame(abi, age, big_e, big_g, big_n, big_o, big_v, constraint, enrolled, gebdat, logincom, male, nodegree, otherdegree, real, relicom, religion, symtest, T012, return, west, wordtest, reliintens, scaledbig_g, scaledbig_e, scaledbig_n, scaledbig_o, scaledbig_v, scaledsymtest, scaledwordtest)


#final dataset where I exclude all the rows where T012 has NA´s 
dtnew <- dt[!is.na(dt$T012),]
