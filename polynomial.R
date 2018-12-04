#Polynomial Regression

#Importing data set
#http://archive.ics.uci.edu/ml/machine-learning-databases/abalone/ 
ds=read.csv('C:/Users/91944/Documents/R/praxis/Machine Learning/abalone.csv')

head(ds)              #Top 6 observations
dim(ds)               #Dimensions of dataset
sapply(ds,class)      #Check Data type
summary(ds)           #Calculate summary of attributes
colSums(is.na(ds))    #Checking the number of missing values in columns

nums <- unlist(lapply(ds, is.numeric))  #Storing numerical seperately


correlation=cor(ds[,nums],method="pearson")  #Correlation values
print(correlation)
library(corrplot)
# create correlation plot
corrplot(correlation, method="circle")

#Scatter plot mattrix
pairs(ds[,nums])

#Ignore Height as low correlation

#Whole-Weight  = Shucked weight+ Viscera weight	+ Shell weight  #Almost equal and corelations are same

fit=lm(ds$Rings~Whole.weight,data=ds) #Residual standard error: 2.713 #Adjusted R-squared:  0.2919
summary(fit)

fit2=lm(ds$Rings~Length,data=ds)   #Residual standard error: 2.679    #Adjusted R-squared:  0.3098
summary(fit2)

fit3=lm(ds$Rings~Diameter,data=ds) #Residual standard error: 2.639  #Adjusted R-squared:  0.3301
summary(fit3)

#Less R.Std.error and good Adjusted R-square so considering Diameter

rand = sample(1:nrow(ds),3600)  #nrow(...) means length,, 1 to that length take 20 random #it takes index
train = ds[rand, ]      # 88% in train
test = ds[-rand, ]      # other than that remaining in test

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7
#=============================================================================================
#Sample size 20
n1=20
rand1 = sample(1:nrow(train),n1)
trainrand1=train[rand1, ]
m1 <- lm(Rings ~ Diameter + I(Diameter^2) + I(Diameter^3) + I(Diameter^4) + I(Diameter^5) + I(Diameter^6) 
          + I(Diameter^7), trainrand1)
m1

#PLOTTING THE MODEL OVER THE DATA
plot(trainrand1$Diameter,trainrand1$Rings, pch=19, cex=0.5)   #pch= symbols, cex=label size
lines(sort(trainrand1$Diameter), fitted(m1)[order(trainrand1$Diameter)], col='red', type='l') 

#TRAIN AND TEST ACCURACY
sum(m1$residuals^2)
pred = predict(m1, newdata=test)
e1=sum((pred-test$Rings)^2)


#Sample size 80
n2=80
rand1 = sample(1:nrow(train),n2)
trainrand1=train[rand1, ]
m1 <- lm(Rings ~ Diameter + I(Diameter^2) + I(Diameter^3) + I(Diameter^4) + I(Diameter^5) + I(Diameter^6) 
         + I(Diameter^7), trainrand1)
m1

#PLOTTING THE MODEL OVER THE DATA
plot(trainrand1$Diameter,trainrand1$Rings, pch=19, cex=0.5)   #pch= symbols, cex=label size
lines(sort(trainrand1$Diameter), fitted(m1)[order(trainrand1$Diameter)], col='red', type='l') 

#TRAIN AND TEST ACCURACY
sum(m1$residuals^2)
pred = predict(m1, newdata=test)
e2=sum((pred-test$Rings)^2)


#Sample size 200
n3=200
rand1 = sample(1:nrow(train),n3)
trainrand1=train[rand1, ]
m1 <- lm(Rings ~ Diameter + I(Diameter^2) + I(Diameter^3) + I(Diameter^4) + I(Diameter^5) + I(Diameter^6) 
         + I(Diameter^7), trainrand1)
m1

#PLOTTING THE MODEL OVER THE DATA
plot(trainrand1$Diameter,trainrand1$Rings, pch=19, cex=0.5)   #pch= symbols, cex=label size
lines(sort(trainrand1$Diameter), fitted(m1)[order(trainrand1$Diameter)], col='red', type='l') 

#TRAIN AND TEST ACCURACY
sum(m1$residuals^2)
pred = predict(m1, newdata=test)
e3=sum((pred-test$Rings)^2)


#Sample size 500
n4=500
rand1 = sample(1:nrow(train),n4)
trainrand1=train[rand1, ]
m1 <- lm(Rings ~ Diameter + I(Diameter^2) + I(Diameter^3) + I(Diameter^4) + I(Diameter^5) + I(Diameter^6) 
         + I(Diameter^7), trainrand1)
m1

#PLOTTING THE MODEL OVER THE DATA
plot(trainrand1$Diameter,trainrand1$Rings, pch=19, cex=0.5)   #pch= symbols, cex=label size
lines(sort(trainrand1$Diameter), fitted(m1)[order(trainrand1$Diameter)], col='red', type='l') 

#TRAIN AND TEST ACCURACY
sum(m1$residuals^2)
pred = predict(m1, newdata=test)
e4=sum((pred-test$Rings)^2)

#Sample size 800
n5=800
rand1 = sample(1:nrow(train),n5)
trainrand1=train[rand1, ]
m1 <- lm(Rings ~ Diameter + I(Diameter^2) + I(Diameter^3) + I(Diameter^4) + I(Diameter^5) + I(Diameter^6) 
         + I(Diameter^7), trainrand1)
m1

#PLOTTING THE MODEL OVER THE DATA
plot(trainrand1$Diameter,trainrand1$Rings, pch=19, cex=0.5)   #pch= symbols, cex=label size
lines(sort(trainrand1$Diameter), fitted(m1)[order(trainrand1$Diameter)], col='red', type='l') 

#TRAIN AND TEST ACCURACY
sum(m1$residuals^2)
pred = predict(m1, newdata=test)
e5=sum((pred-test$Rings)^2)

#Sample size 1400
n6=1400
rand1 = sample(1:nrow(train),n6)
trainrand1=train[rand1, ]
m1 <- lm(Rings ~ Diameter + I(Diameter^2) + I(Diameter^3) + I(Diameter^4) + I(Diameter^5) + I(Diameter^6) 
         + I(Diameter^7), trainrand1)
m1

#PLOTTING THE MODEL OVER THE DATA
plot(trainrand1$Diameter,trainrand1$Rings, pch=19, cex=0.5)   #pch= symbols, cex=label size
lines(sort(trainrand1$Diameter), fitted(m1)[order(trainrand1$Diameter)], col='red', type='l') 

#TRAIN AND TEST ACCURACY
sum(m1$residuals^2)
pred = predict(m1, newdata=test)
e6=sum((pred-test$Rings)^2)

#Sample size 1900
n7=1900
rand1 = sample(1:nrow(train),n7)
trainrand1=train[rand1, ]
m1 <- lm(Rings ~ Diameter + I(Diameter^2) + I(Diameter^3) + I(Diameter^4) + I(Diameter^5) + I(Diameter^6) 
         + I(Diameter^7), trainrand1)
m1

#PLOTTING THE MODEL OVER THE DATA
plot(trainrand1$Diameter,trainrand1$Rings, pch=19, cex=0.5)   #pch= symbols, cex=label size
lines(sort(trainrand1$Diameter), fitted(m1)[order(trainrand1$Diameter)], col='red', type='l') 

#TRAIN AND TEST ACCURACY
sum(m1$residuals^2)
pred = predict(m1, newdata=test)
e7=sum((pred-test$Rings)^2)

#Sample size 2500
n8=2500
rand1 = sample(1:nrow(train),n8)
trainrand1=train[rand1, ]
m1 <- lm(Rings ~ Diameter + I(Diameter^2) + I(Diameter^3) + I(Diameter^4) + I(Diameter^5) + I(Diameter^6) 
         + I(Diameter^7), trainrand1)
m1

#PLOTTING THE MODEL OVER THE DATA
plot(trainrand1$Diameter,trainrand1$Rings, pch=19, cex=0.5)   #pch= symbols, cex=label size
lines(sort(trainrand1$Diameter), fitted(m1)[order(trainrand1$Diameter)], col='red', type='l') 

#TRAIN AND TEST ACCURACY
sum(m1$residuals^2)
pred = predict(m1, newdata=test)
e8=sum((pred-test$Rings)^2)

#Sample size 3000
n9=3000
rand1 = sample(1:nrow(train),n9)
trainrand1=train[rand1, ]
m1 <- lm(Rings ~ Diameter + I(Diameter^2) + I(Diameter^3) + I(Diameter^4) + I(Diameter^5) + I(Diameter^6) 
         + I(Diameter^7), trainrand1)
m1

#PLOTTING THE MODEL OVER THE DATA
plot(trainrand1$Diameter,trainrand1$Rings, pch=19, cex=0.5)   #pch= symbols, cex=label size
lines(sort(trainrand1$Diameter), fitted(m1)[order(trainrand1$Diameter)], col='red', type='l') 

#TRAIN AND TEST ACCURACY
sum(m1$residuals^2)
pred = predict(m1, newdata=test)
e9=sum((pred-test$Rings)^2)

#Sample size 3600
n10=3600
rand1 = sample(1:nrow(train),n10)
trainrand1=train[rand1, ]
m1 <- lm(Rings ~ Diameter + I(Diameter^2) + I(Diameter^3) + I(Diameter^4) + I(Diameter^5) + I(Diameter^6) 
         + I(Diameter^7), trainrand1)
m1

#PLOTTING THE MODEL OVER THE DATA
plot(trainrand1$Diameter,trainrand1$Rings, pch=19, cex=0.5)   #pch= symbols, cex=label size
lines(sort(trainrand1$Diameter), fitted(m1)[order(trainrand1$Diameter)], col='red', type='l') 

#TRAIN AND TEST ACCURACY
sum(m1$residuals^2)
pred = predict(m1, newdata=test)
e10=sum((pred-test$Rings)^2)

samples=c(n1,n2,n3,n4,n5,n6,n7,n8,n9,n10)
errors=c(e1,e2,e3,e4,e5,e6,e7,e8,e9,e10)
plot(samples,errors, pch=19, cex=0.5,type='b',col='red')

samples
errors


