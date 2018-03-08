###########################################################
# Analysis of Train Accidents in USA for the past 16 years
###########################################################

## READ THE README FILE BEFORE GOING THROUGH THE CODE

# Load Data
setwd("D:/UVA Courses/SYS6021/R Code")
source("D:/UVA Courses/SYS6021/R Code/AccidentInput.R")

path <- "D:/UVA Courses/SYS6021/R Code/Train Data"
acts <- file.inputl(path)

sapply(acts, dim)
dim(acts[[12]])

setdiff(colnames(acts[[1]]), colnames(acts[[8]]))
comvar <- intersect(colnames(acts[[1]]),colnames(acts[[8]]))

totacts <- combine.data(acts, comvar)

# For Casualties (TOTINJ + TOTKLD)
totacts$Casualties <- totacts$TOTINJ + totacts$TOTKLD

# Setup categorical variables
totacts$Cause <- rep(NA, nrow(totacts))
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "M")] <- "M"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "T")] <- "T"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "S")] <- "S"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "H")] <- "H"
totacts$Cause[which(substr(totacts$CAUSE, 1, 1) == "E")] <- "E"

####################################
##### With the entire data set #####
####################################

# Build a data frame with only extreme accidents for ACCDMG
dmgbox <- boxplot(totacts$ACCDMG)
xdmg <- totacts[totacts$ACCDMG > dmgbox$stats[5],]

# Remove duplicates from xdmg and call new data frame xdmgnd
xdmgnd <- xdmg[!(duplicated(xdmg[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])),]
xdmgndH <- xdmgnd[xdmgnd$Cause == "H",]

# Boxplots
install.packages("lattice")
library(lattice)
bwplot(as.factor(Cause)~ACCDMG, data = totacts, main = "Box Plots of Accident Damage", xlab = "Damage ($)", ylab = "Cause")
bwplot(as.factor(Cause)~Casualties, data = totacts, main = "Box Plots of Total Casualties", xlab = "Casualties", ylab = "Cause")

############################################
##### With just Human Factors data set #####
############################################

# Framing accidents with Human Factors
install.packages("dplyr")
library(dplyr)
HFacts <- totacts %>% filter(Cause == "H") %>% mutate(CauseHF = factor(CAUSE))

#H8 is deemed insufficient for separate category (Only 5 events caused by H8). We are adding H8 to H9

##Creating a new variable for sub categories of Human factor cause
HFacts$SUBCAUSE <- rep(NA, nrow(HFacts))
HFacts$SUBCAUSE[which(substr(HFacts$CAUSE, 1, 2) == "H0")] <- "H0"
#HFacts$SUBCAUSE[which(substr(HFacts$CAUSE, 1, 2) == "H1")] <- "H1"
HFacts$SUBCAUSE[which(substr(HFacts$CAUSE, 1, 2) == "H2")] <- "H2"
#HFacts$SUBCAUSE[which(substr(HFacts$CAUSE, 1, 2) == "H3")] <- "H3"
HFacts$SUBCAUSE[which(substr(HFacts$CAUSE, 1, 2) == "H4")] <- "H4"
HFacts$SUBCAUSE[which(substr(HFacts$CAUSE, 1, 2) == "H5")] <- "H5"
HFacts$SUBCAUSE[which(substr(HFacts$CAUSE, 1, 2) == "H6")] <- "H6"
HFacts$SUBCAUSE[which(substr(HFacts$CAUSE, 1, 2) == "H7")] <- "H7"
HFacts$SUBCAUSE[which(substr(HFacts$CAUSE, 1, 2) == "H9" | substr(HFacts$CAUSE, 1, 2) == "H8" | substr(HFacts$CAUSE, 1, 2) == "H3" | substr(HFacts$CAUSE, 1, 2) == "H1")] <- "H9"

#Converting SUBCAUSE to factors
HFacts <- HFacts %>% mutate(SUBCAUSE= factor(SUBCAUSE))
plot(HFacts$SUBCAUSE)

# Finding the extreme accidents for ACCDMG of Human Factors
HFdmgbox <- boxplot(HFacts$ACCDMG)
xdmgHF <- HFacts[HFacts$ACCDMG > HFdmgbox$stats[5],]
xdmgndHF <- xdmgHF[!(duplicated(xdmgHF[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])),]
summary(xdmgndHF$SUBCAUSE)
plot(xdmgndHF$SUBCAUSE, xdmgndHF$ACCDMG)

# Finding the extreme accidents for Casualties of Human Factors
xcasHF <- HFacts[HFacts$Casualties > 1,]
xcasndHF <- xcasHF[!(duplicated(xcasHF[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])),]
plot(xcasndHF$SUBCAUSE, xcasndHF$Casualties)

#Summary for each severity based on frequency
summary(xdmgndHF)
summary(xcasndHF)
plot(xdmgndHF$SUBCAUSE)
plot(xcasndHF$SUBCAUSE)

#Box plots for each severity
bwplot(as.factor(SUBCAUSE)~ACCDMG, data = xdmgndHF, main = "Box Plots of Severe Accident Damage", xlab = "Damage ($)", ylab = "Cause")
bwplot(as.factor(SUBCAUSE)~Casualties, data = xcasndHF, main = "Box Plots of Severe Casualties", xlab = "Number of Casualties", ylab = "Cause")

###############################################
# Narrowing down to top 10 most serve accidents
xHFACCDMG <- xdmgndHF[xdmgndHF$ACCDMG > 10000000,]
xHFCasualties <- xcasndHF[xcasndHF$Casualties > 54,]

summary(xHFACCDMG)
summary(xHFCasualties)
###############################################

##Scatter Plot to Motivate Quantitative Variables
xyplot(ACCDMG~CARS | SUBCAUSE, data = xdmgndHF, main = "Severe ACCDMG vs HAZMAT Cars on Subcauses")
xyplot(Casualties~CARS | SUBCAUSE, data = xcasndHF, main = "Severe Casualties vs HAZMAT Cars on Subcauses")

xyplot(ACCDMG~TRNSPD | SUBCAUSE, data = xdmgndHF, main = "Severe ACCDMG vs Train Speed on Subcauses")
xyplot(Casualties~TRNSPD | SUBCAUSE, data = xcasndHF, main = "Severe Casualties vs Train Speed on Subcauses")

xyplot(ACCDMG~LOADF1 | SUBCAUSE, data = xdmgndHF, main = "Severe ACCDMG vs Freight Cars on Subcauses")
xyplot(Casualties~LOADF1 | SUBCAUSE, data = xcasndHF, main = "Severe Casualties vs Freight Cars on Subcauses")

xyplot(ACCDMG~LOADP1 | SUBCAUSE, data = xdmgndHF, main = "Severe ACCDMG vs Passenger Cars on Subcauses")
xyplot(Casualties~LOADP1 | SUBCAUSE, data = xcasndHF, main = "Severe Casualties vs Passenger Cars on Subcauses")

###############################################
##### Models for ACCDMG #######################
###############################################

#Linear Regression for severe ACCDMG
lmdmg1 <- lm(ACCDMG~SUBCAUSE+TRNSPD+LOADF1+LOADP1, data = xdmgndHF)
summary(lmdmg1)

#Interaction Plots between each quantitative predictors
lmdmg2 <- lm(ACCDMG~(SUBCAUSE+TRNSPD+LOADF1+LOADP1)^2,data=xdmgndHF)
summary(lmdmg2)

lmdmg2.step <- step(lmdmg2)
summary(lmdmg2.step)

#Complete Second Order Model
#lmdmg3 <- lm(ACCDMG~(SUBCAUSE+CARS+TRNSPD+LOADF1+LOADP1)^2+I(CARS^2)+I(TRNSPD^2)+I(LOADF1^2)+I(LOADP1),data=xdmgndHF)
#summary(lmdmg4)

#lmdmg3.step <- step(lmdmg3)
#summary(lmdmg3.step)

#Partial F-test
anova(lmdmg1,lmdmg2.step)

#Adjusted R^2
summary(lmdmg1)$adj.r.squared
summary(lmdmg2.step)$adj.r.squared

#AIC
AIC(lmdmg1)
AIC(lmdmg2.step)

#PMSE
source("D:/UVA Courses/SYS6021/R Code/TestSet.R")

pmse1.result <- NULL;
pmse2.result <- NULL;

for (i in c(1:20)){
  test.size <- 1/4
  xdmgndHF.data <- test.set(xdmgndHF, test.size)
  lmdmg1.train <- lm(ACCDMG~SUBCAUSE+TRNSPD+LOADF1+LOADP1, data = xdmgndHF.data$train)
  lmdmg2.train <- lm(ACCDMG~(SUBCAUSE+TRNSPD+LOADP1)^2,data=xdmgndHF.data$train)

  lmdmg1.pred <- predict(lmdmg1.train, newdata = xdmgndHF.data$test)
  lmdmg2.pred <- predict(lmdmg2.train, newdata = xdmgndHF.data$test)

  pmse.lm1 <- mse(lmdmg1.pred, xdmgndHF.data$test$ACCDMG)
  pmse.lm2 <- mse(lmdmg2.pred,xdmgndHF.data$test$ACCDMG)
  
  pmse1.result <- c(pmse1.result,pmse.lm1)
  pmse2.result <- c(pmse2.result,pmse.lm2)
}


plot(pmse1.result,type='b',col='blue',xlab= "Index", ylab= "PMSE")
lines(pmse2.result, type = 'b',col ='red')
title(main = 'Model Comparison Based on PMSE for ACCDMG')

summary(pmse1.result)
summary(pmse2.result)

# Paired t test:
t.test(pmse1.result,pmse2.result,paired=T)

###############################################
##### Models for Casualties ###################
###############################################

#Linear Regression for severe Casualties
lmcas1 <- lm(Casualties~SUBCAUSE+CARS+TRNSPD+LOADF1+LOADP1, data = xcasndHF)
summary(lmcas1)

#Interaction Plots between each quantitative predictors
lmcas2 <- lm(Casualties~(SUBCAUSE+TRNSPD+LOADF1+LOADP1)^2,data=xcasndHF)
summary(lmcas2)
lmcas2.step <- step(lmcas2)
summary(lmcas2.step)

#Complete Second Order Model
#lmcas3 <- lm(Casualties~(SUBCAUSE+CARS+TRNSPD+LOADF1+LOADP1)^2+I(CARS^2)+I(TRNSPD^2)+I(LOADF1^2)+I(LOADP1),data=xcasndHF)
#summary(lmcas3)

#lmcas3.step <- step(lmcas3)
#summary(lmcas3.step)

#Partial F-test
anova(lmcas1, lmcas2.step)

#Adjusted R^2
summary(lmcas1)$adj.r.squared
summary(lmcas2.step)$adj.r.squared

#AIC
AIC(lmcas1)
AIC(lmcas2.step)

#PMSE
source("D:/UVA Courses/SYS6021/R Code/TestSet.R")

pmse1.result <- NULL;
pmse2.result <- NULL;

for (i in c(1:20)){
  test.size <- 1/4
  xcasndHF.data <- test.set(xcasndHF, test.size)
  lmcas1.train <- lm(Casualties~SUBCAUSE+CARS+TRNSPD+LOADF1+LOADP1, data = xcasndHF.data$train)
  lmcas2.train <- lm(Casualties~SUBCAUSE+TRNSPD+LOADF1+LOADP1+SUBCAUSE*TRNSPD+SUBCAUSE*LOADP1+TRNSPD*LOADP1,data=xcasndHF.data$train)
  
  lmcas1.pred <- predict(lmcas1.train, newdata = xcasndHF.data$test)
  lmcas2.pred <- predict(lmcas2.train, newdata = xcasndHF.data$test)
  
  pmse.lm1 <- mse(lmcas1.pred, xcasndHF.data$test$Casualties)
  pmse.lm2 <- mse(lmcas2.pred,xcasndHF.data$test$Casualties)
  
  pmse1.result <- c(pmse1.result,pmse.lm1)
  pmse2.result <- c(pmse2.result,pmse.lm2)
}

plot(pmse1.result,type='b',col='blue',xlab= "Index", ylab= "PMSE")
lines(pmse2.result, type = 'b',col ='red')
title(main = 'Model Comparison Based on PMSE for Casualties')

summary(pmse1.result)
summary(pmse2.result)

# Paired t test:
t.test(pmse1.result,pmse2.result,paired=T)

###########################################
######## Diagnostic Plots for ACCDMG ######
###########################################

plot(lmdmg2.step)

###########################################
#### Diagnostic Plots for Causalties ######
###########################################

plot(lmcas2.step)

###########################################
######### Box-Cox Plot for ACCDMG #########
###########################################

# load the MASS library 
install.packages("MASS")
library(MASS)
boxcox(lmdmg2.step) #box-cox plot
boxcox(lmdmg2.step, plotit=T, lambda=seq(-2,2,by=0.5))

#get x and y values without plotting
boxcox(lmdmg2.step,plotit=F) #values

# find y value for maximum lambda
max(boxcox(lmdmg2.step, plotit = F)$y)

# find best lamda value
boxcox(lmdmg2.step, plotit = F)$x[which.max(boxcox(lmdmg2.step, plotit = F)$y)] 

#get the best lambda and store in L
L<-boxcox(lmdmg2.step, plotit = F)$x[which.max(boxcox(lmdmg2.step, plotit = F)$y)] 
summary(L)

#model with the best lambda
lmdmg2.boxcox<-lm(ACCDMG^L~(SUBCAUSE+TRNSPD+LOADP1)^2,data=xdmgndHF)
boxcox(lmdmg2.boxcox)
plot(lmdmg2.boxcox)

# Display regression results for boxcox model
summary(lmdmg2.boxcox)

# Let's replot our density function for our response variable ACCDMG
plot(density((xdmgndHF$ACCDMG^L)))

# Logarithm Transformation
plot((density(log(xdmgndHF$ACCDMG))))

###############################################
######### Box-Cox Plot for Casualties #########
###############################################

# load the MASS library 
boxcox(lmcas2.step) #box-cox plot
boxcox(lmcas2, plotit=T, lambda=seq(-2,2,by=0.5))

#get x and y values without plotting
boxcox(lmcas2,plotit=F) #values

# find y value for maximum lambda
max(boxcox(lmcas2, plotit = F)$y)

# find best lamda value
boxcox(lmcas2, plotit = F)$x[which.max(boxcox(lmcas2, plotit = F)$y)] 

#get the best lambda and store in L
L<-boxcox(lmcas2, plotit = F)$x[which.max(boxcox(lmcas2, plotit = F)$y)] 
summary(L)

#model with the best lambda
lmcas2.boxcox<-lm(Casualties^L~SUBCAUSE+TRNSPD+LOADF1+LOADP1+SUBCAUSE*TRNSPD+SUBCAUSE*LOADP1+TRNSPD*LOADP1,data=xcasndHF)
boxcox(lmcas2.boxcox)
plot(lmcas2.boxcox)

# Display regression results for boxcox model
summary(lmcas2.boxcox)

# Let's replot our density function for our response variable ACCDMG
plot(density((xdmgndHF$Casualties^L)))

# Logarithm Transformation
plot((density(log(xcasndHF$Casualties))))

#########################################
########## Interaction Plots ############
#########################################

# Interaction Plot for ACCDMG
trnspdbox<-boxplot(xdmgndHF$TRNSPD)
TRNSPD.factor<-xdmgndHF$TRNSPD
TRNSPD.factor[which(xdmgndHF$TRNSPD<50)]<-'low train speed'
TRNSPD.factor[which(xdmgndHF$TRNSPD>=50)]<-'high train speed'
TRNSPD.factor <- factor(TRNSPD.factor)

Humanfactor <- rep(0, nrow(xdmgndHF))
Humanfactor[which(xdmgndHF$SUBCAUSE == "H0")] <- "H0"
Humanfactor[which(xdmgndHF$SUBCAUSE != "H0")] <- "Rest"
Humanfactor <- as.factor(Humanfactor)
contrasts(Humanfactor)

# Create an interaction plot for TRNSPD and SubCause
interaction.plot(TRNSPD.factor,Humanfactor, xdmgndHF$ACCDMG,xlab = "Train Speed", ylab = "Mean of Accident Damage")

# Interaction Plot for Casualties
trnspdbox<-boxplot(xcasndHF$TRNSPD)
TRNSPD.factor<-xcasndHF$TRNSPD
TRNSPD.factor[which(xcasndHF$TRNSPD<50)]<-'low train speed'
TRNSPD.factor[which(xcasndHF$TRNSPD>=50)]<-'high train speed'
TRNSPD.factor <- factor(TRNSPD.factor)

Humanfactor <- rep(0, nrow(xcasndHF))
Humanfactor[which(xcasndHF$SUBCAUSE == "H0")] <- "H0"
Humanfactor[which(xcasndHF$SUBCAUSE != "H0")] <- "Rest"
Humanfactor <- as.factor(Humanfactor)
contrasts(Humanfactor)

# Create an interaction plot for TRNSPD and SubCause
interaction.plot(TRNSPD.factor,Humanfactor, xcasndHF$Casualties,xlab = "Train Speed", ylab = "Mean of Casualties")




