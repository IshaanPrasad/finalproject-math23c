rm(list=ls())
set.seed(123)

#Change this to location of your data
#setwd("/Users/ishaanprasad/math23c/finalproject-math23c")
setwd("/Users/abhishekmalani/Desktop/Math 23C/finalproject-math23c")

if (!require(foreign)) install.packages("foreign"); library(foreign)
if (!require(haven)) install.packages("haven"); library(haven)
if (!require(randomForest)) install.packages("randomForest"); library(randomForest)
if (!require(rpart)) install.packages("rpart"); library(rpart)
if (!require(stats4)) install.packages("stats4"); library(stats4)
if (!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)

census <- read.csv("finalproject.csv")
head(census)
index <- which(!is.na(census$Income) & !is.na(census$CensusTract) & !is.na(census$State)
               & !is.na(census$County) & !is.na(census$TotalPop) & !is.na(census$Citizen)& !is.na(census$Men)
               & !is.na(census$Women) & !is.na(census$Hispanic) & !is.na(census$White) & !is.na(census$Black)
               & !is.na(census$Native) & !is.na(census$Asian) & !is.na(census$Pacific) & !is.na(census$IncomeErr)
               & !is.na(census$IncomePerCap) & !is.na(census$IncomePerCapErr) & !is.na(census$Poverty) 
               & !is.na(census$ChildPoverty) & !is.na(census$Professional) & !is.na(census$Service) 
               & !is.na(census$Office) & !is.na(census$Construction) & !is.na(census$Production)
               & !is.na(census$Drive) & !is.na(census$Carpool) & !is.na(census$Transit) & !is.na(census$Walk)
               & !is.na(census$OtherTransp) & !is.na(census$WorkAtHome) & !is.na(census$MeanCommute)
               & !is.na(census$Employed) & !is.na(census$PrivateWork) & !is.na(census$PublicWork) 
               & !is.na(census$SelfEmployed) & !is.na(census$FamilyWork) & !is.na(census$Unemployment)               )
clean <- census[index,]
head(clean)
write.csv(clean,"clean.csv")  #look at this in Excel or as a text file


#Permutation Test to see if differences between % of people using public transportation between MA & IL is significant
#Calculate the observed beer consumption difference by State

sum(clean$State == "Massachusetts")

IlAvg <- sum(clean$Transit*(clean$State == "Illinois"))/sum(clean$State == "Illinois"); IlAvg
MAAvg <- sum(clean$Transit*(clean$State == "Massachusetts"))/sum(clean$State == "Massachusetts"); MAAvg
observed <- MAAvg - IlAvg; observed

#Now replace all the states with a random sample of all the data
State <- sample(clean$State); State   #permuted state column
sum(State == "Massachusetts")  #still 1453 but each will match up with random transit percentage
IlAvg <- sum(clean$Transit*(State == "Illinois"))/sum(State == "Illinois"); IlAvg
MAAvg <- sum(clean$Transit*(State == "Massachusetts"))/sum(State == "Massachusetts"); MAAvg
MAAvg - IlAvg   #as likely to be negative or positive
#Repeat 10000 times
N <- 10000
diffs <- numeric(N)
for (i in 1:N){
  State <- sample(clean$State); State  #permuted State column
  IlAvg <- sum(clean$Transit*(State == "Illinois"))/sum(State == "Illinois"); IlAvg
  MAAvg <- sum(clean$Transit*(State == "Massachusetts"))/sum(State == "Massachusetts"); MAAvg
  diffs[i] <- MAAvg - IlAvg    #as likely to be negative or positive
}
mean(diffs) #should be close to zero
hist(diffs, breaks = "FD")
#Now display the observed difference on the histogram
abline(v = observed, col = "red")
#What is the probability (the P value) that a difference this large
#could have arisen with a random subset?
pvalue <- 2*(1- (sum(diffs >= observed)+1)/(N+1)); pvalue #two sided significance test
#Not statistically significant because P-Value is greater than 0.05

# Chiacgo vs. Boston for Transit through Permutation Test

Chicago <- data.frame(clean$CensusTract, clean$County, clean$State, clean$Transit)
ChicagoConditional <- (clean$County == "Cook" & clean$State == "Illinois")
Chicago <- Chicago[ChicagoConditional,]
Chicago <- Chicago[sample(nrow(Chicago), 191), ]

Boston <- data.frame(clean$CensusTract, clean$County, clean$State, clean$Transit)
BostonConditional <- (clean$County == "Suffolk" & clean$State == "Massachusetts")
Boston <- Boston[BostonConditional,]
Overall <- rbind(Chicago,Boston)

ChicagoAvg <- sum(Overall$clean.Transit*(Overall$clean.County == "Cook"))/sum(Overall$clean.County == "Cook"); ChicagoAvg
BostonAvg <- sum(Overall$clean.Transit*(Overall$clean.County == "Suffolk"))/sum(Overall$clean.County == "Suffolk"); BostonAvg
observed <- BostonAvg - ChicagoAvg; observed

#Now replace Massachusetts with a random sample of all the data
County <- sample(Overall$clean.County); County   #permuted State column
sum(County == "Cook")  #still 15 men but they will match up with random beer consumption
ChicagoAvg <- sum(Overall$clean.Transit*(County == "Cook"))/sum(County == "Cook"); ChicagoAvg
BostonAvg <- sum(Overall$clean.Transit*(County == "Suffolk"))/sum(County == "Suffolk"); BostonAvg
BostonAvg - ChicagoAvg  #as likely to be negative or positive
#Repeat 10000 times
N <- 10000
diffs <- numeric(N)
for (i in 1:N){
  Chicago <- data.frame(clean$CensusTract, clean$County, clean$State, clean$Transit)
  ChicagoConditional <- (clean$County == "Cook" & clean$State == "Illinois")
  Chicago <- Chicago[ChicagoConditional,]
  Chicago <- Chicago[sample(nrow(Chicago), 191), ]
  
  Boston <- data.frame(clean$CensusTract, clean$County, clean$State, clean$Transit)
  BostonConditional <- (clean$County == "Suffolk" & clean$State == "Massachusetts")
  Boston <- Boston[BostonConditional,]
  Overall <- rbind(Chicago,Boston)
  
  County <- sample(Overall$clean.County); County   #permuted State column
  ChicagoAvg <- sum(Overall$clean.Transit*(County == "Cook"))/sum(County == "Cook"); ChicagoAvg
  BostonAvg <- sum(Overall$clean.Transit*(County == "Suffolk"))/sum(County == "Suffolk"); BostonAvg
  diffs[i] <- BostonAvg - ChicagoAvg    #as likely to be negative or positive
}
mean(diffs) #should be close to zero
hist(diffs, xlim=c(-6,13), breaks = "FD")
#Now display the observed difference on the histogram
abline(v = observed, col = "red")
#What is the probability (the P value) that a difference this large
#could have arisen with a random subset?
pvalue <- 2*(sum(diffs >= observed)+1)/(N+1); pvalue #two sided test
#Difference between Boston and Chicago is statistically significant! Boston has a statistically significant higher
#percentage of people using public transport compared to Chicago

#Storing predictor variables
vars <- colnames(clean[18:ncol(clean)])
vars

#Randomly sampling 70% of the data for a training set and the other 30% is testing
clean2 <- data.frame(clean); head(clean2)
length(clean2)
training <- clean2[sample(nrow(clean2), (0.7*length(clean2))), ]

#OLS Regression
to_hat <- with(clean[clean$Income>0,], lm(reformulate(vars, "Income")))
summary(to_hat)
rank_hat_ols = predict(to_hat, newdata=clean)
summary(rank_hat_ols)
#Poverty, ChildPoverty, MeanCommute, Employed, Private Work, and Unemployment are all statistically significant 
#correlates at the 5% level

#Decision Tree or Regression Tree
one_tree <- rpart(reformulate(vars, "Income")
                  , data=clean
                  , control = rpart.control(xval = 5)) ## this sets the number of folds for cross validation.

one_tree #Text Representation of Tree
#Decision tree shows that the best predictors 
rank_hat_tree <- predict(one_tree, newdata=clean)
table(rank_hat_tree)
hist(rank_hat_tree, xlab="Predicted Rates - Single Tree")

plot(one_tree) # plot tree
text(one_tree) # add labels to tree, download as PDF to see full version (looks cramped on screen)
# print complexity parameter table using cross validation
printcp(one_tree)
# 
# #Random Forest from 500 Bootstrapped Samples
forest_hat <- randomForest(reformulate(vars, "Income"), ntree=100, mtry=20, maxnodes=50
                           ,importance=TRUE, do.trace=25, data=clean[clean$Income>0,])
getTree(forest_hat, 100, labelVar = TRUE) #Text Representation of Tree
rank_hat_forest <- predict(forest_hat, newdata=clean,type="response")
summary(rank_hat_forest); hist(rank_hat_forest, xlab="Predicted Rates - Random Forest")

plot(forest_hat) # plot tree
text(forest_hat) # add labels to tree
printcp(forest_hat)
