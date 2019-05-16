rm(list=ls())
set.seed(123)

################## BEGIN Set Up ################## 
## Change this to location of your data
  setwd("/Users/ishaanprasad/math23c/finalproject-math23c")

## Check for required packages
  if (!require(foreign)) install.packages("foreign"); library(foreign)
  if (!require(haven)) install.packages("haven"); library(haven)
  if (!require(randomForest)) install.packages("randomForest"); library(randomForest)
  if (!require(rpart)) install.packages("rpart"); library(rpart)
  if (!require(stats4)) install.packages("stats4"); library(stats4)
  if (!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)

## Clean up data and export to new "clean.csv"
  #Point 1 - A data set with lots of columns, allowing comparison of many different variables.
  #Point 2 - A data set that is so large that it can be used as a population from which samples are taken.
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
  write.csv(clean,"clean.csv") 
##################   END Set up   ################## 

################## BEGIN Analysis ################## 
## Median income across all census tracts
  median(clean$Income)

## Histogram with Normal Distribution added
  # Display Histogram
  hist(clean$Income)
  h = hist(clean$Income)
  h$density = h$counts/sum(h$counts)
  plot(h,freq=FALSE)
  hist(clean$Income, breaks = "FD", probability = TRUE, col="light blue", xlab = "Income (USD)", main = "Histogram of Income Distribution")
  # To find the best fitting normal distribution, compute the mean and variance of the data
  mu <- mean(clean$Income); mu
  var <- var(clean$Income)
  sigma <- sd(clean$Income)     #estimates square root of the population variance
  curve(dnorm(x, mu, sigma), from = 0, to = 250000, add = TRUE, col = "red")
  
## Visualizng the percentage of a race in a population w.r.t. the median income of the population
  # Point 11 - Nicely labeled graphics using ggplot, with good use of color, line styles, etc., that tell a convincing story.
  # Point 14 - Use of linear regression
  # Setting up the relevant percentages for races
  percentA <- clean$Asian / 100       # Asian
  percentB <- clean$Black / 100       # Black
  percentH <- clean$Hispanic / 100    # Hispanic
  percentN <- clean$Native / 100      # Native
  percentP <- clean$Pacific / 100     # Pacific
  percentW <- clean$White / 100       # White
  income <-  clean$Income
  # ggplotting the data: fulfilling "Nicely labeled graphics using ggplot, with good use of color, line styles, etc., that tell a convincing story"
  df <- data.frame(
    percentA,      # Asian
    percentB,      # Black
    percentH,   # Hispanic
    percentN,    # Native
    percentP,  # Pacific
    percentW,       # White
    income
  )
  # Asian
  ggplot(df, aes(percentA, income)) + 
    geom_point(color = "red", size = 0.5) + 
    geom_smooth(method = "lm", color ="blue") +
    xlab("Percent of Asian People in Tract") +
    ylab("Average Income of Tract") + 
    ggtitle("Percent Asians vs. Average Income (in tract) ")
  # Black
  ggplot(df, aes(percentB, income)) + 
    geom_point(color = "orange", size = 0.5) + 
    geom_smooth(method = "lm", color ="blue") +
    xlab("Percent of Black People in Tract") +
    ylab("Average Income of Tract") +
    ggtitle("Percent Blacks vs. Average Income (in tract) ")
  # Hispanic
  ggplot(df, aes(percentH, income)) + 
    geom_point(color = "yellow", size = 0.5) + 
    geom_smooth(method = "lm", color ="blue") +
    xlab("Percent of Hispanic People in Tract") +
    ylab("Average Income of Tract") + 
    ggtitle("Percent Hispanics vs. Average Income (in tract) ")
  # Native
  ggplot(df, aes(percentN, income)) + 
    geom_point(color = "green", size = 0.5) + 
    geom_smooth(method = "lm", color ="blue") +
    xlab("Percent of Native People in Tract") +
    ylab("Average Income of Tract") +
    ggtitle("Percent Natives vs. Average Income (in tract) ")
  # Pacific
  ggplot(df, aes(percentP, income)) + 
    geom_point(color = "dodgerblue2", size = 0.5) + 
    geom_smooth(method = "lm", color ="blue") +
    xlab("Percent of Pacific People in Tract") +
    ylab("Average Income of Tract") +
    ggtitle("Percent Pacifics vs. Average Income (in tract) ")
  # White
  ggplot(df, aes(percentW, income)) + 
    geom_point(color = "violet", size = 0.5) + 
    geom_smooth(method = "lm", color ="blue") +
    xlab("Percent of White People in Tract") +
    ylab("Average Income of Tract") + 
    ggtitle("Percent Whites vs. Average Income (in tract) ")
  # Linear regression on the above data
  Percent <- c(0,1)
  Income <- c(0, 200000)
  plot(Percent, Income, pch=".") # To clear plot space but keep axes in tact
  abline(lm(income ~ percentH), col = "red") # Hispanic
  abline(lm(income ~ percentW), col = "orange") # White
  abline(lm(income ~ percentB), col = "yellow") # Black
  abline(lm(income ~ percentN), col = "green") # Native
  abline(lm(income ~ percentA), col = "blue") # Asian
  abline(lm(income ~ percentP), col = "violet") # Pacific



## Permutation Test: To see if differences between % of people using public transportation between MA & IL is significant
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
  hist(diffs, breaks = "FD", col="light blue")
  #Now display the observed difference on the histogram
  abline(v = observed, col = "red")
  #What is the probability (the P value) that a difference this large
  #could have arisen with a random subset?
  pvalue <- 2*(1- (sum(diffs >= observed)+1)/(N+1)); pvalue #two sided significance test
  #Not statistically significant because P-Value (0.3731627) is greater than 0.05

## Follow-Up Permutation Test: To see if differences between % of people using public transportation between Boston & Chicago is significant
  # Point 8 - A convincing demonstration of a relationship that might not have been statistically significant but that turns out to be so.
  # Especially given the results from above, we expect that the difference of the percentage of people using public tranpsort in Boston vs. Chicacgo will not be significant
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
  sum(County == "Cook") 
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
  } #takes about a minute
  mean(diffs) #should be close to zero
  hist(diffs, xlim=c(-6,13), breaks = "FD", col="light green")
  #Display of the observed difference on the histogram
  abline(v = observed, col = "red")
  #What is the probability (the P value) that a difference this large
  #could have arisen with a random subset?
  pvalue <- 2*(sum(diffs >= observed)+1)/(N+1); pvalue #two sided test
  #Difference between Boston and Chicago is statistically significant (p-value is less than 0.05)! Boston has a 
  #statistically significant higher percentage of people using public transport compared to Chicago
  
  #Contingency Table & Chi-Squared
  
  Chicago <- data.frame(clean$CensusTract, clean$County, clean$State, clean$Transit)
  ChicagoConditional <- (clean$County == "Cook" & clean$State == "Illinois")
  Chicago <- Chicago[ChicagoConditional,]
  
  Boston <- data.frame(clean$CensusTract, clean$County, clean$State, clean$Transit)
  BostonConditional <- (clean$County == "Suffolk" & clean$State == "Massachusetts")
  Boston <- Boston[BostonConditional,]
  Overall <- rbind(Chicago,Boston)
  
  #the greater than 25 percent use of public transportation is an arbitrary cutoff determined by the group
  Overall$LotsOfTransit <- Overall$clean.Transit > 25
  Overall$Chicago <- Overall$clean.County == "Cook"
  
  #This is the contingency table looking at if you are in Cook County (chicago) on if your public transportation 
  # usage is greater than 25%
  tbl <- table(Overall$Chicago, Overall$LotsOfTransit); tbl
  
  #Compare with the table that would be expected if the factors were independent
  tbl
  Expected <- outer(rowSums(tbl), colSums(tbl))/sum(tbl); Expected
  #These table look quite different. It suggests that the difference in public transit usage could be statistically
  # significant between Boston and Chicago. Is the difference significant?
  
  #Using a chi-squared test, we can determine the probability that this difference between the contingency table
  # and the expected table occur by chance and use this p-value to determine if the difference is significant
  chisq.test(Overall$Chicago,Overall$LotsOfTransit)
  #The low p-value means there is about 1 chance in 10,000,000,000,000,000 that it arose by chance. Boston has a 
  # statistically significant higher percent of population using of public transportation 
  

## Machine Learning (OLS, Decision Tree, Random Forest)
  # Point 16 - Appropriate use of covariance or correlation.
  #Storing predictor variables
  vars <- colnames(clean[18:ncol(clean)])
  vars
  
  #Randomly sampling 70% of the data for a training set and the other 30% is testing
  insample <- c(rep(T, 50908),rep(F,21819))
  clean2 <- data.frame(clean, insample); head(clean2)
  nrow(clean2)
  insample <- sample(clean2$insample) #permuting to randomize which data points are selected for in-sample vs out-of-sample
  clean2 <- data.frame(clean, insample); head(clean2)
  
  #OLS Regression
  to_hat <- with(clean2[clean2$insample==1,], lm(reformulate(vars, "Income")))
  summary(to_hat)
  rank_hat_ols = predict(to_hat, newdata=clean2)
  summary(rank_hat_ols)
  #Poverty, ChildPoverty, MeanCommute, Employed, Private Work, and Unemployment are all statistically significant 
  #correlates at the 5% level
  #With an R^2 value of 0.71, we can see that 71 percent of the change income is explained by the model
  #Because this is a multivariable regression, the R value of 0.84 is less insightful as you don't know which variables
  # exactly had which effect.
  
  #Decision Tree or Regression Tree
  one_tree <- rpart(reformulate(vars, "Income")
                    , data=clean2
                    , subset = clean2$insample==1
                    , control = rpart.control(xval = 5)) ## this sets the number of folds for cross validation.
  
  one_tree #Text Representation of Tree
  #Decision tree shows that the best predictors 
  rank_hat_tree <- predict(one_tree, newdata=clean2)
  table(rank_hat_tree)
  hist(rank_hat_tree, xlab="Predicted Rates - Single Tree")
  
  plot(one_tree) # plot tree
  text(one_tree) # add labels to tree, download as PDF to see full version (looks cramped on screen)
  # print complexity parameter table using cross validation
  printcp(one_tree)
  
  # #Random Forest from 500 Bootstrapped Samples
  forest_hat <- randomForest(reformulate(vars, "Income"), ntree=500, mtry=20, maxnodes=50
                             ,importance=TRUE, do.trace=25, data=clean2[clean2$insample==1,])
  getTree(forest_hat, 100, labelVar = TRUE) #Text Representation of Tree
  rank_hat_forest <- predict(forest_hat, newdata=clean2,type="response")
  summary(rank_hat_forest); hist(rank_hat_forest, xlab="Predicted Rates - Random Forest")
  
  
  #Out-of-Sample Validation
  if (!require(stargazer)) install.packages("stargazer"); library(stargazer)
  
  #Merge training data with test data
  clean2$rank_hat_ols <- rank_hat_ols
  clean2$rank_hat_tree <- rank_hat_tree
  clean2$rank_hat_forest <- rank_hat_forest
  
  predictions <- clean2[clean2$insample==0,]
  predictions <- predictions[,-c(4:13)]
  predictions <- predictions[,-c(5:7)]
  predictions <- predictions[,-c(5:25)]
  
  predictions$ols_error <- predictions$Income - predictions$rank_hat_ols
  predictions$tree_error <- predictions$Income - predictions$rank_hat_tree
  predictions$forest_error <- predictions$Income - predictions$rank_hat_forest
  
  mean(predictions$ols_error)^2
  mean(predictions$tree_error)^2
  mean(predictions$forest_error)^2
  #random forest has the lowest out-of-sample error and is followed by the OLS Regression
  #There doesn't seem to be any indication of overfitting of the in-sample data.
  
  write.csv(predictions, "predictions.csv") #Save data as an excel .csv file

 
## Bar Plots
  # Setup variables (considering tracts that have >50% of one race)
  income = clean$Income
  hispanic = which(clean$Hispanic>50);  length(hispanic)
  white = which(clean$White>50); length(white)
  black = which(clean$Black>50);  length(black)
  native = which(clean$Native>50); length(native)
  asian = which(clean$Asian>50); length(asian)
  pacific = which(clean$Pacific>50); length(pacific)
  
  ## Income
    #Setup income variables
    mostlywhite = clean$Income[white]
    mostlyblack = clean$Income[black]
    mostlyhispanic = clean$Income[hispanic]
    mostlyasian = clean$Income[asian]
    mostlynative = clean$Income[native]
    mostlypacific = clean$Income[pacific]
    # Get mean incomes
    whiteincome = mean(mostlywhite); whiteincome
    blackincome = mean(mostlyblack); blackincome
    hispanicincome = mean(mostlyhispanic); hispanicincome
    asianincome = mean(mostlyasian); asianincome
    nativeincome = mean(mostlynative); nativeincome
    pacificincome = mean(mostlypacific); pacificincome
    # Visualize Mean Income by Race
    barplot(c(whiteincome, blackincome, hispanicincome, asianincome, nativeincome, pacificincome), names.arg = c("white", "black", "hispanic", "asian", "native", "pacific"),main = "Income by Predominant Racial Group in Tract", xlab = "Race", ylab = "Income ($)", col = rgb(0,0.3,0.7,0.4))
    
    #Predominantly Asian tracts have the highest average income
    #Is it significantly greater than each other race?
    w = asianincome - whiteincome
    b = asianincome - blackincome
    h = asianincome - hispanicincome
    n = asianincome - nativeincome
    p = asianincome - pacificincome
    
    s.w = var(mostlywhite)/length(mostlywhite)
    s.b = var(mostlyblack)/length(mostlyblack)
    s.h = var(mostlyhispanic)/length(mostlyhispanic)
    s.a = var(mostlyasian)/length(mostlyasian)
    s.n = var(mostlynative)/length(mostlynative)
    s.p = var(mostlypacific)/length(mostlypacific)
    
    t.w = w/sqrt(s.w+s.a)
    t.b = b/sqrt(s.b+s.a)
    t.h = h/sqrt(s.h+s.a)
    t.n = n/sqrt(s.n+s.a)
    t.p = p/sqrt(s.p+s.a)
    
    p.w = 1 - pnorm(t.w, 0, 1)
    p.b = 1 - pnorm(t.b, 0, 1)
    p.h = 1 - pnorm(t.h, 0, 1)
    p.n = 1 - pnorm(t.n, 0, 1)
    p.p = 1 - pnorm(t.p, 0, 1)
    
    p.w; p.b; p.h; p.n; p.p  #All of the differences are significant
    
    
  ## Poverty
    #Setup poverty variables
    mostlywhite = clean$Poverty[white]
    mostlyblack = clean$Poverty[black]
    mostlyhispanic = clean$Poverty[hispanic]
    mostlyasian = clean$Poverty[asian]
    mostlynative = clean$Poverty[native]
    mostlypacific = clean$Poverty[pacific]
    # Get mean poverrty rates
    whitepoverty = mean(mostlywhite); whitepoverty
    blackpoverty = mean(mostlyblack); blackpoverty
    hispanicpoverty = mean(mostlyhispanic); hispanicpoverty
    asianpoverty = mean(mostlyasian); asianpoverty
    nativepoverty = mean(mostlynative); nativpoverty
    pacificpoverty = mean(mostlypacific); pacificpoverty
    # Visualize Mean Poverty by Race
    barplot(c(whitepoverty,blackpoverty,hispanicpoverty,asianpoverty,nativepoverty,pacificpoverty), names.arg = c("white", "black", "hispanic", "asian", "native", "pacific"),main = "Poverty Rate by Predominant Racial Group in Tract", xlab = "Race", ylab = "Income ($)", col = rgb(0,0.7,0.3,0.4))
    
 
 
## Calculating a 95% Confidence Interval
  #Point 20 - Calculation of a confidence interval.
  commute = clean$MeanCommute 
  µ = mean(commute); µ  #population mean
  sigma = sd(commute); sigma  #population standard deviation
  hist(commute, probability = T, breaks = "fd", main = "Commute Time Distribution", xlab = "Commute Time (min)", ylab = "Density", col = rgb(.75,0,.25,.35))  #looks approximately normal, just a little bit skewed to the right
  f = function(x) dnorm(x,µ, sigma)
  curve(f, add=T, col = "blue")
  
  N = length(commute); N
  n = 5000
  
  sample = sample(N,n) #Point 2 - we drew random samples from a large population (72727 entries) 
  xbar = mean(commute[sample])  #sample mean
  s = sd(commute[sample]);s  #sample standard deviation
  
  lower = xbar - 1.96*s/sqrt(n); lower 
  upper = xbar + 1.96*s/sqrt(n); upper
  
  counter <- 0
  plot(x =c(µ-2,µ+2), y = c(1,100), type = "n",main = "95% Confidence Intervals", xlab = "Commute Time (min)", ylab = "Sample Number") 
  for (i in 1:100) {
   sample = sample(N,n)
   xbar = mean(commute[sample])
   s = sd(commute[sample])
   lower = xbar - 1.96*s/sqrt(n)
   upper = xbar + 1.96*s/sqrt(n)
   if (lower < µ && µ < upper) counter <- counter + 1 
   if(i <= 100) {
     points(lower, i, pch= 22, col = "blue")
     points(upper, i, pch= 23, col = "blue")
     segments(lower, i, upper, i, col = "blue")
   }    
  }
  abline (v = µ, col = "red") #vertical line at population mean
  #What percentage of the time does the interval contain the population mean?
  counter/100  #around 95% most of the time
  
  ### Logistic Regression ###
  #Point 15 - Calculation and display of a logistic regression curve.
  income = clean$Income
  mass = which(clean$State == "Massachusetts") #look at just tracts in Massachusetts
  professional = clean$Professional[mass] #independent variable is the percentage of workers employed in management, business, science, and arts
  hist(professional, breaks = "fd", main = "Distribution of Percentage of Professional Workers in Tract", xlab = "Percent Professional Workers", col = rgb(.4,.6,0,.5))
  
  
  wealthy <- (as.numeric(income[mass]>=50000)); head(wealthy) #threshold for being a "wealthy" county is average income above $50,000
  plot(professional,wealthy,xlab = "Percent Professional Workers", ylab = "Wealthy", main = "Wealthy by Percent Professional")  
  
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


## Walking in Chicago and Los Angeles
  #Point 9 - A convincing demonstration of a relationship that might have been statistically significant but that turns out not to be so.
  #We expect more people to walk to work in Los Angeles than in Chicago due to climate differences.
  chicago = which(clean$County == "Cook" & clean$State == "Illinois"); length(chicago)
  LA = which(clean$County == "Los Angeles"); length(LA)
  mean(clean$Walk[chicago]); mean(clean$Walk[LA]) 
  obs = mean(clean$Walk[LA]) - mean(clean$Walk[chicago]); obs
  
  #We can't just permute the County column, because Cook county exists in multiple states
  #So we create a new city column
  is.chicago = as.numeric(clean$County == "Cook" & clean$State == "Illinois"); head(is.chicago)
  length(is.chicago); sum(is.chicago)  #Seems right
  is.LA = as.numeric(clean$County == "Los Angeles", clean$State == "California"); head(is.LA)
  length(is.LA); sum(is.LA)  #Seems right
  city = is.chicago + 2*is.LA; head(city) #Since no tract is both in Chicago and Los Angeles, there is no overlap. Chicago tracts will be a 1, Los Angeles tracts are a 2, and everything else is 0
  sum(city == 1); sum(city == 2)  #Perfect
  
  #Now replace Chicago with a random sample of all the data
  permute <- sample(city); head(permute)   #permuted city column
  ChicagoAvg <- sum(clean$Walk*(permute == 1))/sum(permute == 1); ChicagoAvg
  LosAvg <- sum(clean$Walk*(permute == 2))/sum(permute == 2); LosAvg
  LosAvg - ChicagoAvg    #as likely to be negative or positive
  
  #Repeat 10000 times
  N <- 10000
  diffs <- numeric(N)
  for (i in 1:N){
   permute <- sample(city); head(permute)   
   ChicagoAvg <- sum(clean$Walk*(permute == 1))/sum(permute == 1)
   LosAvg <- sum(clean$Walk*(permute == 2))/sum(permute == 2)
   diffs[i] <- LosAvg - ChicagoAvg     
  } #takes about a minute
  
  mean(diffs) 
  hist(diffs, breaks = "FD")
  hist(diffs, breaks = "FD", xlim = c(-1.5,1.5), col = rgb(0.2,0.2,0.6,0.4), main = "Histogram of Predicted Differences", xlab = "Difference", probability = T, ylab = "Density")
  #Now display the observed difference on the histogram
  abline(v = obs, col = "red")
  #What is the probability (the P value) that a difference this large could have arisen with a random subset?
  p <- (sum(diffs >= obs)+1)/(N+1); p #greater than 0.05
  #The proportion of people who walk to work in Los Angeles is not statistically greater than the proportion of people who walk to work in Chicago.
  1-p
  #In fact, the opposite (Chicago > Los Angeles) is statistically significant surprisingly (1-p < 0.05).



## Skewness and Kurtosis of Drive
  #Point 13 - Appropriate use of novel statistics (e.g. trimmed mean, maximum or minimum, skewness, ratios)
  drive = clean$Drive
  hist(drive, breaks = "fd", main = "Distribution of Percentage of People Driving to Work in Tracts", xlab = "Percentage of People Who Drive to Work", col = "orange") #looks like it's skewed pretty heavily to the left
  N = length(drive)
  mu = mean(drive)
  s = sd(drive)
  skewness = sum(((drive - mu)^3)/(N*s^3)); skewness #skewness is -2.19
  
  kurtosis = sum(((drive - mu)^4)/(N*s^4))-3; kurtosis #kurtosis is 5.74

##################   END Analysis   ##################