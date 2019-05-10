census <- read.csv("finalproject.csv")
head(census)
index <- which(!is.na(census$Income) & !is.na(census$CensusTract) & !is.na(census$State)
               & !is.na(census$County) & !is.na(census$TotalPop) & !is.na(census$Citizen))
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




curve(dbeta(x, 2,5), from = 0, to = 250000, add = TRUE, col = "blue")
