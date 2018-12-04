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

#Fixed sample size= 70 (take randomly 4 times i.e N1,N2,N3,N4)
#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF N1
#=============================================================================================
ntest=4177-800
set.seed(0)
#ORDER-1
rand = sample(1:nrow(ds),800)  #nrow(...) means length,, 1 to that length take 70 random #it takes index
train = ds[rand, ]      # 800 in train
test = ds[-rand, ]      # other than that remaining in test

m1 <- lm(Rings ~ Diameter, train)
m1

#TRAIN AND TEST ACCURACY
sum(m1$residuals^2)
pred = predict(m1, newdata=test)
e1o1=sqrt(sum((pred-test$Rings)^2)/ntest)   #RMSE
e1o1

#ORDER-2

m1 <- lm(Rings ~ Diameter+I(Diameter^2), train)
m1

#TRAIN AND TEST ACCURACY
sum(m1$residuals^2)
pred = predict(m1, newdata=test)
e1o2=sqrt(sum((pred-test$Rings)^2)/ntest)       #RMSE
e1o2


#ORDER-7

m1 <- lm(Rings ~ Diameter+I(Diameter^2)+I(Diameter^3)+I(Diameter^4)+I(Diameter^5)+I(Diameter^6)+I(Diameter^7), train)
m1

#TRAIN AND TEST ACCURACY
sum(m1$residuals^2)
pred = predict(m1, newdata=test)
e1o7=sqrt(sum((pred-test$Rings)^2)/ntest)       #RMSE
e1o7

#ORDER-8

m1 <- lm(Rings ~ Diameter+I(Diameter^2)+I(Diameter^3)+I(Diameter^4)+I(Diameter^5)+I(Diameter^6)+I(Diameter^7)+I(Diameter^8), train)
m1

#TRAIN AND TEST ACCURACY
sum(m1$residuals^2)
pred = predict(m1, newdata=test)
e1o8=sqrt(sum((pred-test$Rings)^2)/ntest)       #RMSE
e1o8


#ORDER-9

m1 <- lm(Rings ~ Diameter+I(Diameter^2)+I(Diameter^3)+I(Diameter^4)+I(Diameter^5)+I(Diameter^6)+I(Diameter^7)+I(Diameter^8)+I(Diameter^9), train)
m1

#TRAIN AND TEST ACCURACY
sum(m1$residuals^2)
pred = predict(m1, newdata=test)
e1o9=sqrt(sum((pred-test$Rings)^2)/ntest)       #RMSE
e1o9


#ORDER-10

m1 <- lm(Rings ~ Diameter+I(Diameter^2)+I(Diameter^3)+I(Diameter^4)+I(Diameter^5)+I(Diameter^6)+I(Diameter^7)+I(Diameter^8)+I(Diameter^9)+I(Diameter^10), train)
m1

#TRAIN AND TEST ACCURACY
sum(m1$residuals^2)
pred = predict(m1, newdata=test)
e1o10=sqrt(sum((pred-test$Rings)^2)/ntest)       #RMSE
e1o10

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF N2
#=============================================================================================
ntest=4177-70
set.seed(78)
#ORDER-1
rand = sample(1:nrow(ds),70)  #nrow(...) means length,, 1 to that length take 70 random #it takes index
train = ds[rand, ]      # 70 in train

m1 <- lm(Rings ~ Diameter, train)
m1

#TRAIN AND TEST ACCURACY
sum(m1$residuals^2)
pred = predict(m1, newdata=test)
e2o1=sqrt(sum((pred-test$Rings)^2)/ntest)   #RMSE
e2o1

#ORDER-2

m1 <- lm(Rings ~ Diameter+I(Diameter^2), train)
m1

#TRAIN AND TEST ACCURACY
sum(m1$residuals^2)
pred = predict(m1, newdata=test)
e2o2=sqrt(sum((pred-test$Rings)^2)/ntest)       #RMSE
e2o2


#ORDER-7

m1 <- lm(Rings ~ Diameter+I(Diameter^2)+I(Diameter^3)+I(Diameter^4)+I(Diameter^5)+I(Diameter^6)+I(Diameter^7), train)
m1

#TRAIN AND TEST ACCURACY
sum(m1$residuals^2)
pred = predict(m1, newdata=test)
e2o7=sqrt(sum((pred-test$Rings)^2)/ntest)       #RMSE
e2o7

#ORDER-8

m1 <- lm(Rings ~ Diameter+I(Diameter^2)+I(Diameter^3)+I(Diameter^4)+I(Diameter^5)+I(Diameter^6)+I(Diameter^7)+I(Diameter^8), train)
m1

#TRAIN AND TEST ACCURACY
sum(m1$residuals^2)
pred = predict(m1, newdata=test)
e2o8=sqrt(sum((pred-test$Rings)^2)/ntest)       #RMSE
e2o8


#ORDER-9

m1 <- lm(Rings ~ Diameter+I(Diameter^2)+I(Diameter^3)+I(Diameter^4)+I(Diameter^5)+I(Diameter^6)+I(Diameter^7)+I(Diameter^8)+I(Diameter^9), train)
m1

#TRAIN AND TEST ACCURACY
sum(m1$residuals^2)
pred = predict(m1, newdata=test)
e2o9=sqrt(sum((pred-test$Rings)^2)/ntest)       #RMSE
e2o9


#ORDER-10

m1 <- lm(Rings ~ Diameter+I(Diameter^2)+I(Diameter^3)+I(Diameter^4)+I(Diameter^5)+I(Diameter^6)+I(Diameter^7)+I(Diameter^8)+I(Diameter^9)+I(Diameter^10), train)
m1

#TRAIN AND TEST ACCURACY
sum(m1$residuals^2)
pred = predict(m1, newdata=test)
e2o10=sqrt(sum((pred-test$Rings)^2)/ntest)       #RMSE
e2o10


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF N3
#=============================================================================================
ntest=4177-70
set.seed(378)
#ORDER-1
rand = sample(1:nrow(ds),70)  #nrow(...) means length,, 1 to that length take 70 random #it takes index
train = ds[rand, ]      # 70 in train

m1 <- lm(Rings ~ Diameter, train)
m1

#TRAIN AND TEST ACCURACY
sum(m1$residuals^2)
pred = predict(m1, newdata=test)
e3o1=sqrt(sum((pred-test$Rings)^2)/ntest)   #RMSE
e3o1

#ORDER-2

m1 <- lm(Rings ~ Diameter+I(Diameter^2), train)
m1

#TRAIN AND TEST ACCURACY
sum(m1$residuals^2)
pred = predict(m1, newdata=test)
e3o2=sqrt(sum((pred-test$Rings)^2)/ntest)       #RMSE
e3o2


#ORDER-7

m1 <- lm(Rings ~ Diameter+I(Diameter^2)+I(Diameter^3)+I(Diameter^4)+I(Diameter^5)+I(Diameter^6)+I(Diameter^7), train)
m1

#TRAIN AND TEST ACCURACY
sum(m1$residuals^2)
pred = predict(m1, newdata=test)
e3o7=sqrt(sum((pred-test$Rings)^2)/ntest)       #RMSE
e3o7

#ORDER-8

m1 <- lm(Rings ~ Diameter+I(Diameter^2)+I(Diameter^3)+I(Diameter^4)+I(Diameter^5)+I(Diameter^6)+I(Diameter^7)+I(Diameter^8), train)
m1

#TRAIN AND TEST ACCURACY
sum(m1$residuals^2)
pred = predict(m1, newdata=test)
e3o8=sqrt(sum((pred-test$Rings)^2)/ntest)       #RMSE
e3o8


#ORDER-9

m1 <- lm(Rings ~ Diameter+I(Diameter^2)+I(Diameter^3)+I(Diameter^4)+I(Diameter^5)+I(Diameter^6)+I(Diameter^7)+I(Diameter^8)+I(Diameter^9), train)
m1

#TRAIN AND TEST ACCURACY
sum(m1$residuals^2)
pred = predict(m1, newdata=test)
e3o9=sqrt(sum((pred-test$Rings)^2)/ntest)       #RMSE
e3o9


#ORDER-10

m1 <- lm(Rings ~ Diameter+I(Diameter^2)+I(Diameter^3)+I(Diameter^4)+I(Diameter^5)+I(Diameter^6)+I(Diameter^7)+I(Diameter^8)+I(Diameter^9)+I(Diameter^10), train)
m1

#TRAIN AND TEST ACCURACY
sum(m1$residuals^2)
pred = predict(m1, newdata=test)
e3o10=sqrt(sum((pred-test$Rings)^2)/ntest)       #RMSE
e3o10


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF N4
#=============================================================================================
ntest=4177-70
set.seed(1687)
#ORDER-1
rand = sample(1:nrow(ds),70)  #nrow(...) means length,, 1 to that length take 70 random #it takes index
train = ds[rand, ]      # 70 in train

m1 <- lm(Rings ~ Diameter, train)
m1

#TRAIN AND TEST ACCURACY
sum(m1$residuals^2)
pred = predict(m1, newdata=test)
e4o1=sqrt(sum((pred-test$Rings)^2)/ntest)   #RMSE
e4o1

#ORDER-2

m1 <- lm(Rings ~ Diameter+I(Diameter^2), train)
m1

#TRAIN AND TEST ACCURACY
sum(m1$residuals^2)
pred = predict(m1, newdata=test)
e4o2=sqrt(sum((pred-test$Rings)^2)/ntest)       #RMSE
e4o2


#ORDER-7

m1 <- lm(Rings ~ Diameter+I(Diameter^2)+I(Diameter^3)+I(Diameter^4)+I(Diameter^5)+I(Diameter^6)+I(Diameter^7), train)
m1

#TRAIN AND TEST ACCURACY
sum(m1$residuals^2)
pred = predict(m1, newdata=test)
e4o7=sqrt(sum((pred-test$Rings)^2)/ntest)       #RMSE
e4o7

#ORDER-8

m1 <- lm(Rings ~ Diameter+I(Diameter^2)+I(Diameter^3)+I(Diameter^4)+I(Diameter^5)+I(Diameter^6)+I(Diameter^7)+I(Diameter^8), train)
m1

#TRAIN AND TEST ACCURACY
sum(m1$residuals^2)
pred = predict(m1, newdata=test)
e4o8=sqrt(sum((pred-test$Rings)^2)/ntest)       #RMSE
e4o8


#ORDER-9

m1 <- lm(Rings ~ Diameter+I(Diameter^2)+I(Diameter^3)+I(Diameter^4)+I(Diameter^5)+I(Diameter^6)+I(Diameter^7)+I(Diameter^8)+I(Diameter^9), train)
m1

#TRAIN AND TEST ACCURACY
sum(m1$residuals^2)
pred = predict(m1, newdata=test)
e4o9=sqrt(sum((pred-test$Rings)^2)/ntest)       #RMSE
e4o9


#ORDER-10

m1 <- lm(Rings ~ Diameter+I(Diameter^2)+I(Diameter^3)+I(Diameter^4)+I(Diameter^5)+I(Diameter^6)+I(Diameter^7)+I(Diameter^8)+I(Diameter^9)+I(Diameter^10), train)
m1

#TRAIN AND TEST ACCURACY
sum(m1$residuals^2)
pred = predict(m1, newdata=test)
e4o10=sqrt(sum((pred-test$Rings)^2)/ntest)       #RMSE
e4o10

n1=c(e1o1,e1o2,e1o7,e1o8,e1o9,e1o10)
n2=c(e2o1,e2o2,e2o7,e2o8,e2o9,e2o10)
n3=c(e3o1,e3o2,e3o7,e3o8,e3o9,e3o10)
n4=c(e4o1,e4o2,e4o7,e4o8,e4o9,e4o10)

o=c(1,2,7,8,9,10)

plot(o,n1, pch=19, cex=0.7,type='b',col="red")   #pch= symbols, cex=symbol size
plot(o,n2, pch=19, cex=0.7,type='b',col="green")   #pch= symbols, cex=symbol size
plot(o,n3, pch=19, cex=0.7,type='b',col="magenta")   #pch= symbols, cex=symbol size
plot(o,n4, pch=19, cex=0.7,type='b',col="blue")   #pch= symbols, cex=symbol size


n1
n2
n3
n4
