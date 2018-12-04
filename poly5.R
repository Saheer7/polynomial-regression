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


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 1
#=============================================================================================
#Sample size  600
set.seed(7)
ntest=4177-600
rand = sample(1:nrow(ds),600)  #nrow(...) means length,, 1 to that length take 70 random #it takes index
train = ds[rand, ]
test = ds[-rand, ]      # other than that remaining in test

m1 <- lm(Rings ~ Diameter, train)
m1

e1=sqrt(sum(m1$residuals^2)/600)
pred = predict(m1, newdata=test)
t1=sqrt(sum((pred-test$Rings)^2)/ntest)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 2
#=============================================================================================

m2 <- lm(Rings ~ Diameter+I(Diameter^2), train)
m2

e2=sqrt(sum(m2$residuals^2)/600)
pred = predict(m2, newdata=test)
t2=sqrt(sum((pred-test$Rings)^2)/ntest)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 3
#=============================================================================================

m3 <- lm(Rings ~ Diameter+I(Diameter^2)+I(Diameter^3), train)
m3

e3=sqrt(sum(m3$residuals^2)/600)
pred = predict(m3, newdata=test)
t3=sqrt(sum((pred-test$Rings)^2)/ntest)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 4
#=============================================================================================

m4 <- lm(Rings ~ Diameter+I(Diameter^2)+I(Diameter^3)+I(Diameter^4), train)
m4

e4=sqrt(sum(m4$residuals^2)/600)
pred = predict(m4, newdata=test)
t4=sqrt(sum((pred-test$Rings)^2)/ntest)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 5
#=============================================================================================

m5 <- lm(Rings ~ Diameter+I(Diameter^2)+I(Diameter^3)+I(Diameter^4)+I(Diameter^5), train)
m5

e5=sqrt(sum(m5$residuals^2)/600)
pred = predict(m5, newdata=test)
t5=sqrt(sum((pred-test$Rings)^2)/ntest)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 6
#=============================================================================================

m6 <- lm(Rings ~ Diameter+I(Diameter^2)+I(Diameter^3)+I(Diameter^4)+I(Diameter^5)+I(Diameter^6), train)
m6

e6=sqrt(sum(m6$residuals^2)/600)
pred = predict(m6, newdata=test)
t6=sqrt(sum((pred-test$Rings)^2)/ntest)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7
#=============================================================================================

m7 <- lm(Rings ~ Diameter+I(Diameter^2)+I(Diameter^3)+I(Diameter^4)+I(Diameter^5)+I(Diameter^6)+I(Diameter^7), train)
m7

e7=sqrt(sum(m7$residuals^2)/600)
pred = predict(m7, newdata=test)
t7=sqrt(sum((pred-test$Rings)^2)/ntest)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 8
#=============================================================================================

m8 <- lm(Rings ~ Diameter+I(Diameter^2)+I(Diameter^3)+I(Diameter^4)+I(Diameter^5)+I(Diameter^6)+I(Diameter^7)+I(Diameter^8), train)
m8

e8=sqrt(sum(m8$residuals^2)/600)
pred = predict(m8, newdata=test)
t8=sqrt(sum((pred-test$Rings)^2)/ntest)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 9
#=============================================================================================

m9 <- lm(Rings ~ Diameter+I(Diameter^2)+I(Diameter^3)+I(Diameter^4)+I(Diameter^5)+I(Diameter^6)+I(Diameter^7)+I(Diameter^8)+I(Diameter^9), train)
m9

e9=sqrt(sum(m9$residuals^2)/600)
pred = predict(m9, newdata=test)
t9=sqrt(sum((pred-test$Rings)^2)/ntest)

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 10
#=============================================================================================


m10 <- loess.smooth(Rings ~ Diameter+I(Diameter^2)+I(Diameter^3)+I(Diameter^4)+I(Diameter^5)+I(Diameter^6)+I(Diameter^7)+I(Diameter^8)+I(Diameter^9)+I(Diameter^10), train)
m10

e10=sqrt(sum(m10$residuals^2)/600)
pred = predict(m10, newdata=test)
t10=sqrt(sum((pred-test$Rings)^2)/ntest)

#PLOTTING THE MODEL OVER THE DATA
#PLOTTING THE MODEL OVER THE DATA


o=c(1,2,3,4,5,6,7,8,9,10)
n1=c(e1,e2,e3,e4,e5,e6,e7,e8,e9,e10)  #TRAIN RMSE
tes=c(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10) #test RMSE

plot(o,n1, pch=19, cex=0.7,type='b',col="red",ylim=c(0,7000))   #pch= symbols, cex=symbol size

n1#TRAIN RMSE
tes#test RMSE
par(new=TRUE)
plot(o,tes, pch=19, cex=0.7,type='b',col="blue",axes=FALSE,ylab='')   #pch= symbols, cex=symbol size

legend("topleft", legend=c("Train RMSE", "Test RMSE"),
       col=c("red", "blue"), lty=1:2, cex=0.8,box.lty=2)
