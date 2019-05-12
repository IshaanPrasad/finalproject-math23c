rm(list=ls())
set.seed(123)

#Change this to location of your data
#Can use drop down menu in R studio: file->import data set-> from stata and find stata data set
setwd("/Users/abhishekmalani/Desktop/Math 23C/Data")

if (!require(foreign)) install.packages("foreign"); library(foreign)
if (!require(haven)) install.packages("haven"); library(haven)
if (!require(randomForest)) install.packages("randomForest"); library(randomForest)
if (!require(rpart)) install.packages("rpart"); library(rpart)

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
median(clean$Income)
length(clean$Income)
hist(clean$Income)
h = hist(clean$Income, probability = TRUE) # or hist(x,plot=FALSE) to avoid the plot of the histogram
h$density = h$counts/sum(h$counts)
plot(h,freq=FALSE)

#To find the best fitting normal distribution, compute the mean and variance of the data
mu <- mean(clean$Income); mu
sigma <- sd(clean$Income)     #estimates square root of the population variance
curve(dnorm(x, mu, sigma), from = 0, to = 250000, add = TRUE, col = "red")

#Storing predictor variables
#Order data in stata so all predictors appear in right-most columns
vars <- colnames(clean[18:ncol(clean)])
vars
summary(vars)

#OLS Regression
to_hat <- with(clean[clean$Income>0,], lm(reformulate(vars, "Income")))
summary(to_hat)
rank_hat_ols = predict(to_hat, newdata=clean)
summary(rank_hat_ols); hist(rank_hat_ols, xlab="Predicted Rates - OLS")
vars

#Decision Tree or Regression Tree
one_tree <- rpart(reformulate(vars, "Income")
                  , data=clean
                  , control = rpart.control(xval = 10)) ## this sets the number of folds for cross validation.

one_tree #Text Representation of Tree
rank_hat_tree <- predict(one_tree, newdata=clean)
table(rank_hat_tree)
hist(rank_hat_tree, xlab="Predicted Rates - Single Tree")

plot(one_tree) # plot tree
text(one_tree) # add labels to tree
# print complexity parameter table using cross validation
printcp(one_tree)

#Random Forest from 1000 Bootstrapped Samples
forest_hat <- randomForest(reformulate(vars, "Income"), ntree=1000, mtry=11, maxnodes=100
                           ,importance=TRUE, do.trace=25, data=clean[clean$Income>0,])
getTree(forest_hat, 250, labelVar = TRUE) #Text Representation of Tree
rank_hat_forest <- predict(forest_hat, newdata=clean,type="response")
summary(rank_hat_forest); hist(rank_hat_forest, xlab="Predicted Rates - Random Forest")
