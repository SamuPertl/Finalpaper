install.packages("VGAM")
install.packages("broom")
install.packages("lmtest")
install.packages("tseries")
install.packages("tikzDevice")
install.packages("scales")
install.packages("stargazer")
install.packages("QuantPsyc")
install.packages("bstats")
install.packages("AER")
install.packages("faraway")
install.packages("gvlma")
install.packages("MASS")



library(car)
library(readstata13)
library(dplyr)
library(ggplot2)
library(data.table)
library(VGAM)
library(broom)
library(lmtest)
library(tseries)
library(tikzDevice)
library(scales)
library(stargazer)
library(QuantPsyc)
library(AER)
library(faraway)
library(gvlma)
library(MASS)

pre <- read.dta13(file = "pre_2005_1_dt.dta", convert.factors = F)



# Gender as dummy variable, male = 1 female = 
male = Recode(pre$sex, "2=0; -1=NA; -2=NA; -3=NA")

# age in years
gebdat = Recode(pre$gebdat, "-1=NA; -2=NA; -3=NA")
age = 2005 - gebdat

# religious affiliation (dummy) 1=member of a religious community = 1;  0= no member of a religious community 
religion = Recode(pre$f02601, "1=1; 2=1; 3=1; 4=1; 5=1; 6=0; -1=NA; -2=NA; -3=NA")

testreli = Recode(pre$f02601, "6=1; 5=0; 4=0; 3=0; 2=0; 1=0; -1=NA")


# religious communities 
relicom = Recode(pre$f02601, "1 = 'Katholisch'; 2 = 'Protestantisch'; 3 = 'Andere christliche Gemeinschaft'; 4 = 'Islam'; 5 = 'Andere religioese Gemeinschaft'; 6 = 'keine Religionszugehoerigkeit'; -1=NA; -2=NA; -3=NA")

# religious intensity (dummy) 1 = intense (at least once a week/month); 0 = not intense (les often/never)

reliintens <- Recode(pre$f02509, "1=1; 2=1; 3=0; 4=0; -1=NA")


# Income 
income1 = Recode(pre$hnetto, "-1=NA; -2=NA; -3=NA")

income2 = Recode(pre$znetto, "-1=NA; -2=NA; -3=NA; 21=NA; 31=NA; 41=NA; 51=NA; 1=375; 2=1125; 3=2000; 4=3000; 5=4250; 6=7500")

# replace all NA´s in income1 with values from income2 
income1[which(is.na(income1))] <- income2[is.na(income1)]

logincom = log(income1)

# Liquid constraint (dummy variable) 1 = no constraint; 0 = constraint 
constraint = Recode(pre$f093, "-1=NA; 2=0" )

# Education (dummy variable for different educational degrees)

# Abitur (dummy) 1 = Hochschulreife 0 = keine Hochschulreife  
abi = Recode(pre$f00802, "1=0; 2=0; 3=1; 4=1; 5=0; 6=0; 0=NA; -1=NA -2=NA; -3=NA")
abi[which(is.na(abi))] <- 0

#Realschule (dummy) 1= Realschulabschluss, 0=kein Realschulabschluss 
real = Recode(pre$f00802, "1=0; 2=1; 3=0; 4=0; 5=0; 6=0; -1=NA")
real[which(is.na(real))] <- 0 

tablereal <- with(pre, table(real))
tablereal


# no degree (dummy) 1 = kein Abschluss, 0 = Abschluss 
nodegree = Recode(pre$f00802, "1=0; 2=0; 3=0; 4=0; 5=0; 6=1; -1=NA")
nodegree[which(is.na(nodegree))] <- 0

tablenodegree <- with(pre, table(nodegree))
tablenodegree

# other degree (dummy) 1 = other degree, 0 = herkömmlicher Abschluss 
otherdegree = Recode(pre$f00802, "1=0; 2=0; 3=0; 4=0; 5=1; 6=0; -1=NA")
otherdegree[which(is.na(otherdegree))] <- 0

tableother <- with(pre, table(otherdegree))
tableother

# enrolled in school (dummy) 1= still in school, 0= graduated 
enrolled = Recode(pre$f00801, "0=0; 1=1; -1=NA")
enrolled[which(is.na(enrolled))] <- 0

# Height in cm 
height = Recode(pre$f073, "-1=NA; -2=NA; -3=NA")

# intertemporal choice 
# switching line  

# T012 
T012 = pre$z1spaet
return = Recode(pre$z1spaet, "1=2.5; 2=5; 3=7.5; 4=10; 5=12.5; 6=15; 7=17.5; 8=20; 9=22.5; 10=25; 11=27.5; 12=30; 13=32.5; 14=35; 15=37.5; 16=40; 17=42.5; 18=45; 19=47.5; 20=50; 21=52.5")

# T06 
#T06 = pre$z2spaet
#return06 = Recode(pre$z2spaet, "1=2.5; 2=5; 3=7.5; 4=10; 5=12.5; 6=15; 7=17.5; 8=20; 9=22.5; 10=25; 11=27.5; 12=30; 13=32.5; 14=35; 15=37.5; 16=40; 17=42.5; 18=45; 19=47.5; 20=50; 21=52.5")

# T612
#T612 = pre$z3spaet
#return612 = Recode(pre$z3spaet, "1=2.5; 2=5; 3=7.5; 4=10; 5=12.5; 6=15; 7=17.5; 8=20; 9=22.5; 10=25; 11=27.5; 12=30; 13=32.5; 14=35; 15=37.5; 16=40; 17=42.5; 18=45; 19=47.5; 20=50; 21=52.5")

# Big 5
G1 = Recode(pre$f02201, "-1=NA; -2=NA; -3=NA")
G2 = Recode(pre$f02207, "-1=NA; -2=NA; -3=NA; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1")
G3 = Recode(pre$f02211, "-1=NA; -2=NA; -3=NA")

E1 = Recode(pre$f02202, "-1=NA; -2=NA; -3=NA")
E2 = Recode(pre$f02208, "-1=NA; -2=NA; -3=NA")
E3 = Recode(pre$f02212, "-1=NA; -2=NA; -3=NA; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1")

V1 = Recode(pre$f02203, "-1=NA; -2=NA; -3=NA; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1")
V2 = Recode(pre$f02206, "-1=NA; -2=NA; -3=NA")
V3 = Recode(pre$f02213, "-1=NA; -2=NA; -3=NA")

O1 = Recode(pre$f02204, "-1=NA; -2=NA; -3=NA")
O2 = Recode(pre$f02209, "-1=NA; -2=NA; -3=NA")
O3 = Recode(pre$f02214, "-1=NA; -2=NA; -3=NA")

N1 = Recode(pre$f02205, "-1=NA; -2=NA; -3=NA")
N2 = Recode(pre$f02210, "-1=NA; -2=NA; -3=NA")
N3 = Recode(pre$f02215, "-1=NA; -2=NA; -3=NA; 1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1")

# Gewissenhaftigkeit 
big_g = G1 + G2 + G3

scaledbig_g = scale(big_g, center = TRUE, scale = TRUE)

# Extraversion 
big_e = E1 + E2 + E3

scaledbig_e = scale(big_e, center = TRUE, scale = TRUE)

# Verträglichkeit 
big_v = V1 + V2 + V3

scaledbig_v = scale(big_v, center = TRUE, scale = TRUE)

# Offenheit für Erfahrungen 
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

#children in HH -> not available

# region (dummy) 1 = West, 0 = Ost 
west = Recode(pre$westost, "-1=NA; -2=NA; -3=NA; 0=1; 1=0")


# create new dataset 
dt <-data.frame(abi, age, big_e, big_g, big_n, big_o, big_v, constraint, enrolled, gebdat, height, logincom, male, nodegree, otherdegree, real, relicom, religion, symtest, T012, return, west, wordtest, reliintens, testreli, scaledbig_g, scaledbig_e, scaledbig_n, scaledbig_o, scaledbig_v, scaledsymtest, scaledwordtest)


#final dataset 
dtnew <- dt[!is.na(dt$T012),]



# try permutation test 
# prepare the dataset 
select_(dtnew, logincom)

permut <- dtnew %>% select_(T012)
  

A <- dtnew[dtnew$religion == 1,]
B <- dtnew[dtnew$religion == 0, ]


# store 
mean.stor <- rep(NA, ncol(dtnew))


A.t <- dtnew[dtnew$religion == 1 , ]
B.t <- dtnew[dtnew$religion == 0 , ]

for(i in 1:ncol(dtnew)){
  print(i)  # print iteration number to console
  # not neded if everything goes well, but you can track progress this way
  # i <- 1    # little 'hack' so you can test if all goes well for a specific
  # value of i. not neded if all goes well
  A.tm <- mean(A.t[,i]) # get mean of A
  B.tm <- mean(B.t[,i]) # get mean of B
  # take the differnce between A and B and square it. Store in vector
  mean.stor[i - 1] <- (A.tm - B.tm)^2
}


real.test.statistic <- sum(mean.stor) 