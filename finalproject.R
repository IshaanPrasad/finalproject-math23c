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

#Median income across all census tracts
median(clean$Income)

#Histogram with Normal Distribution added
hist(clean$Income)
h = hist(clean$Income)
h$density = h$counts/sum(h$counts)
plot(h,freq=FALSE)
hist(clean$Income, breaks = "FD", probability = TRUE)

#To find the best fitting normal distribution, compute the mean and variance of the data
mu <- mean(clean$Income); mu
var <- var(clean$Income)
sigma <- sd(clean$Income)     #estimates square root of the population variance
curve(dnorm(x, mu, sigma), from = 0, to = 250000, add = TRUE, col = "red")

#Visualizng the percentage of a race in a population w.r.t. the average income of the population
income <-  clean$Income
# ggplotting the data (not finished)
df <- data.frame(
  percentA <- clean$Asian / 100,       # Asian
  percentB <- clean$Black / 100,       # Black
  percentH <- clean$Hispanic / 100,    # Hispanic
  percentN <- clean$Native / 100,      # Native
  percentP <- clean$Pacific / 100,     # Pacific
  percentW <- clean$White / 100,       # White
  income <-  clean$Income
)
ggplot(df, aes(percentA, income)) + 
  geom_point(color = "red", size = 0.5) + 
  geom_smooth(method = "lm", color ="blue") +
  xlab("Percent of Asian People in Tract") +
  ylab("Average Income of Tract")

ggplot(df, aes(percentB, income)) + 
  geom_point(color = "red", size = 0.5) + 
  geom_smooth(method = "lm", color ="blue") +
  xlab("Percent of Black People in Tract") +
  ylab("Average Income of Tract")

ggplot(df, aes(percentH, income)) + 
  geom_point(color = "red", size = 0.5) + 
  geom_smooth(method = "lm", color ="blue") +
  xlab("Percent of Hispanic People in Tract") +
  ylab("Average Income of Tract")

ggplot(df, aes(percentN, income)) + 
  geom_point(color = "red", size = 0.5) + 
  geom_smooth(method = "lm", color ="blue") +
  xlab("Percent of Native People in Tract") +
  ylab("Average Income of Tract")

ggplot(df, aes(percentP, income)) + 
  geom_point(color = "red", size = 0.5) + 
  geom_smooth(method = "lm", color ="blue") +
  xlab("Percent of Pacific People in Tract") +
  ylab("Average Income of Tract")

ggplot(df, aes(percentW, income)) + 
  geom_point(color = "red", size = 0.5) + 
  geom_smooth(method = "lm", color ="blue") +
  xlab("Percent of White People in Tract") +
  ylab("Average Income of Tract")

# Linear regression on the above scatter plots
Percent <- c(0,1)
Income <- c(0, 200000)
plot(Percent, Income, pch=".") # To clear plot space but keep axes in tact
abline(lm(income ~ percentH), col = "red") # Hispanic
abline(lm(income ~ percentW), col = "orange") # White
abline(lm(income ~ percentB), col = "yellow") # Black
abline(lm(income ~ percentN), col = "green") # Native
abline(lm(income ~ percentA), col = "blue") # Asian
abline(lm(income ~ percentP), col = "violet") # Pacific


#Trying to do Permutation Test here 
sum(clean$County == "Suffolk"); sum(clean$County == "Cook")
#Not an equal amount so we will randomly sample 1172 counties from MA

MaSample <- sample (clean$State == "Massachusetts", size=1172, replace =F)
length(MaSample)

#Calculate the observed beer consumption difference by State

IlAvg <- sum(clean$Transit*(State == "Illinois"))/sum(State == "Illinois"); IlAvg
MAAvg <- sum(clean$Transit*(State == "Massachusetts"))/sum(State == "Massachusetts"); MAAvg
observed <- IlAvg - MAAvg; observed

#Now replace Massachusetts with a random sample of all the data
State <- sample(clean$State); State   #permuted State column
sum(State == "New York")  #still 15 men but they will match up with random beer consumption
IlAvg <- sum(clean$Transit*(State == "Illinois"))/sum(State == "Illinois"); IlAvg
MAAvg <- sum(clean$Transit*(State == "Massachusetts"))/sum(State == "Massachusetts"); MAAvg
IlAvg - MAAvg   #as likely to be negative or positive
#Repeat 10000 times
N <- 1000
diffs <- numeric(N)
for (i in 1:N){
  State <- sample(clean$State); State   #permuted State column
  IlAvg <- sum(clean$Transit*(State == "Illinois"))/sum(State == "Illinois"); IlAvg
  MAAvg <- sum(clean$Transit*(State == "Massachusetts"))/sum(State == "Massachusetts"); MAAvg
  diffs[i] <- IlAvg - MAAvg    #as likely to be negative or positive
}
mean(diffs) #should be close to zero
hist(diffs, breaks = "FD")
#Now display the observed difference on the histogram
abline(v = observed, col = "red")
#What is the probability (the P value) that a difference this large
#could have arisen with a random subset?
pvalue <- 2*(1 -(sum(diffs >= observed)+1)/(N+1)); pvalue #two sided significance test


ChicagoAvg <- sum(clean$Transit*(clean$County == "Cook"))/sum(clean$County == "Cook"); ChicagoAvg
BostonAvg <- sum(clean$Transit*(clean$County == "Suffolk"))/sum(clean$County == "Suffolk"); BostonAvg
observed <- ChicagoAvg - BostonAvg; observed

#Now replace Massachusetts with a random sample of all the data
County <- sample(clean$County); County   #permuted State column
sum(State == "New York")  #still 15 men but they will match up with random beer consumption
ChicagoAvg <- sum(clean$Transit*(County == "Cook"))/sum(County == "Cook"); ChicagoAvg
BostonAvg <- sum(clean$Transit*(County == "Suffolk"))/sum(County == "Suffolk"); BostonAvg
ChicagoAvg - BostonAvg  #as likely to be negative or positive
#Repeat 10000 times
N <- 1000
diffs <- numeric(N)
for (i in 1:N){
  County <- sample(clean$County); County   #permuted State column
  ChicagoAvg <- sum(clean$Transit*(County == "Cook"))/sum(County == "Cook"); ChicagoAvg
  BostonAvg <- sum(clean$Transit*(County == "Suffolk"))/sum(County == "Suffolk"); BostonAvg
  diffs[i] <- ChicagoAvg - BostonAvg    #as likely to be negative or positive
}
mean(diffs) #should be close to zero
hist(diffs, xlim=c(-6,6), breaks = "FD")
#Now display the observed difference on the histogram
abline(v = observed, col = "red")
#What is the probability (the P value) that a difference this large
#could have arisen with a random subset?
pvalue <- 2*(sum(diffs >= observed)+1)/(N+1); pvalue #two sided test


#Storing predictor variables
#Order data in stata so all predictors appear in right-most columns
vars <- colnames(clean[18:ncol(clean)])
vars

#OLS Regression
to_hat <- with(clean[clean$Income>0,], lm(reformulate(vars, "Income")))
summary(to_hat)
rank_hat_ols = predict(to_hat, newdata=clean)
summary(rank_hat_ols); hist(rank_hat_ols, xlab="Predicted Rates - OLS")

# 
# 
#Decision Tree or Regression Tree
one_tree <- rpart(reformulate(vars, "Income")
                   , data=clean
                   , control = rpart.control(xval = 5)) ## this sets the number of folds for cross validation.
 
 one_tree #Text Representation of Tree
 rank_hat_tree <- predict(one_tree, newdata=clean)
 table(rank_hat_tree)
 hist(rank_hat_tree, xlab="Predicted Rates - Single Tree")
 
 plot(one_tree) # plot tree
 text(one_tree) # add labels to tree
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

 ################## Massimo ################## 
 
 ### Bar Plots ###
 income = clean$Income
 hispanic = which(clean$Hispanic>50);  length(hispanic)
 white = which(clean$White>50); length(white)
 black = which(clean$Black>50);  length(black)
 native = which(clean$Native>50); length(native)
 asian = which(clean$Asian>50); length(asian)
 pacific = which(clean$Pacific>50); length(pacific)
 
 #income
 mostlywhite = clean$Income[white]
 mostlyblack = clean$Income[black]
 mostlyhispanic = clean$Income[hispanic]
 mostlyasian = clean$Income[asian]
 mostlynative = clean$Income[native]
 mostlypacific = clean$Income[pacific]
 
 whiteincome = mean(mostlywhite)
 blackincome = mean(mostlyblack)
 hispanicincome = mean(mostlyhispanic)
 asianincome = mean(mostlyasian)
 nativeincome = mean(mostlynative)
 pacificincome = mean(mostlypacific)
 
 whiteincome; blackincome;hispanicincome;asianincome;nativeincome;pacificincome
 barplot(c(whiteincome, blackincome, hispanicincome, asianincome, nativeincome, pacificincome), names.arg = c("white", "black", "hispanic", "asian", "native", "pacific"),main = "Income")
 
 #Poverty
 mostlywhite = clean$Poverty[white]
 mostlyblack = clean$Poverty[black]
 mostlyhispanic = clean$Poverty[hispanic]
 mostlyasian = clean$Poverty[asian]
 mostlynative = clean$Poverty[native]
 mostlypacific = clean$Poverty[pacific]
 
 whitepoverty = mean(mostlywhite)
 blackpoverty = mean(mostlyblack)
 hispanicpoverty = mean(mostlyhispanic)
 asianpoverty = mean(mostlyasian)
 nativepoverty = mean(mostlynative)
 pacificpoverty = mean(mostlypacific)
 
 whitepoverty; blackpoverty;hispanicpoverty;asianpoverty;nativepoverty;pacificpoverty
 barplot(c(whitepoverty,blackpoverty,hispanicpoverty,asianpoverty,nativepoverty,pacificpoverty), names.arg = c("white", "black", "hispanic", "asian", "native", "pacific"),main = "Poverty")
 
 
 ### 95% Confidence Interval ###
 #Point 20 - confidence interval
 commute = clean$MeanCommute 
 µ = mean(commute); µ  #population mean
 sigma = sd(commute); sigma  #population standard deviation
 hist(commute, probability = T)  #looks approximately normal, just a little bit skewed to the right
 f = function(x) dnorm(x,µ, sigma)
 curve(f, add=T, col = "blue")
 
 N = length(commute); N
 n = 5000
 
 sample = sample(N,n) #Point 2 - drew random samples from large population (72727 entries) 
 xbar = mean(commute[sample])  #sample mean
 s = sd(commute[sample]);s  #sample standard deviation
 
 lower = xbar - 1.96*s/sqrt(n); lower 
 upper = xbar + 1.96*s/sqrt(n); upper
 
 counter <- 0
 plot(x =c(µ-2,µ+2), y = c(1,100), type = "n", xlab = "", ylab = "") 
 for (i in 1:100) {
   sample = sample(N,n)
   xbar = mean(commute[sample])
   s = sd(commute[sample])
   lower = xbar - 1.96*s/sqrt(n)
   upper = xbar + 1.96*s/sqrt(n)
   if (lower < µ && µ < upper) counter <- counter + 1 
   if(i <= 100) {
     points(lower, i, pch= 22)
     points(upper, i, pch= 23)
     segments(lower, i, upper, i)
   }    
 }
 abline (v = µ, col = "red") #vertical line at population mean
 #What percentage of the time does the interval contain the population mean?
 counter/100  #around 95% most of the time
 
 ### Logistic Regression ###
 #Point 15 - logistic regression curve
 income = clean$Income
 hist(income)
 mass = which(clean$State == "Massachusetts") #look at just tracts in Massachusetts
 professional = clean$Professional[mass] #independent variable is the percentage of workers employed in management, business, science, and arts
 hist(professional)
 
 
 wealthy <- (as.numeric(income[mass]>=50000)); head(wealthy) #threshold for being a "wealthy" county is average income above $50,000
 plot(professional,wealthy)  
 
 MLL<- function(alpha, beta) {
   -sum( log( exp(alpha+beta*professional)/(1+exp(alpha+beta*professional)) )*wealthy
         + log(1/(1+exp(alpha+beta*professional)))*(1-wealthy) )
 }
 results<-mle(MLL, start = list(alpha = 0, beta = 0)) #an initial guess is required
 results@coef
 curve( exp(results@coef[1]+results@coef[2]*x)/ (1+exp(results@coef[1]+results@coef[2]*x)),col = "blue", add=TRUE)
 abline(v=20, col = "red"); abline(v=40, col = "green")
 index <- which(professional >= 19 & professional <= 21); head(index)   #evaluated in a small range of values around 20, because very few tracts were exactly equal to 20 and might not give an accurate estimate
 a = mean(wealthy[index]); a   #21% wealthy tracts 
 abline(h=a, col = "red")  #not bad
 index <- which(professional >= 39 & professional <= 41); head(index)   #evaluated in a small range of values around 40, because very few tracts were exactly equal to 40 and might not give an accurate estimate
 b = mean(wealthy[index]); b   #87% wealthy tracts
 abline(h=b, col = "green")  #pretty good
 
 ################## end Massimo ################## 



