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
# FITTING A POLYNOMIAL REGRESSION OF ORDER 1
#=============================================================================================
#Sample size  78
n=78
rand1 = sample(1:nrow(train),n)
trainrand1=train[rand1, ]
m1 <- lm(Rings ~ Diameter, trainrand1)
m1


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 2
#=============================================================================================

m2 <- lm(Rings ~ Diameter+I(Diameter^2), trainrand1)
m2

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 3
#=============================================================================================

m3 <- lm(Rings ~ Diameter+I(Diameter^2)+I(Diameter^3), trainrand1)
m3

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 4
#=============================================================================================

m4 <- lm(Rings ~ Diameter+I(Diameter^2)+I(Diameter^3)+I(Diameter^4), trainrand1)
m4

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 5
#=============================================================================================

m5 <- lm(Rings ~ Diameter+I(Diameter^2)+I(Diameter^3)+I(Diameter^4)+I(Diameter^5), trainrand1)
m5

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 6
#=============================================================================================

m6 <- lm(Rings ~ Diameter+I(Diameter^2)+I(Diameter^3)+I(Diameter^4)+I(Diameter^5)+I(Diameter^6), trainrand1)
m6

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7
#=============================================================================================

m7 <- lm(Rings ~ Diameter+I(Diameter^2)+I(Diameter^3)+I(Diameter^4)+I(Diameter^5)+I(Diameter^6)+I(Diameter^7), trainrand1)
m7

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 8
#=============================================================================================

m8 <- lm(Rings ~ Diameter+I(Diameter^2)+I(Diameter^3)+I(Diameter^4)+I(Diameter^5)+I(Diameter^6)+I(Diameter^7)+I(Diameter^8), trainrand1)
m8

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 9
#=============================================================================================

m9 <- lm(Rings ~ Diameter+I(Diameter^2)+I(Diameter^3)+I(Diameter^4)+I(Diameter^5)+I(Diameter^6)+I(Diameter^7)+I(Diameter^8)+I(Diameter^9), trainrand1)
m9

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 10
#=============================================================================================


m10 <- loess.smooth(Rings ~ Diameter+I(Diameter^2)+I(Diameter^3)+I(Diameter^4)+I(Diameter^5)+I(Diameter^6)+I(Diameter^7)+I(Diameter^8)+I(Diameter^9)+I(Diameter^10), trainrand1)
m10

#PLOTTING THE MODEL OVER THE DATA
#PLOTTING THE MODEL OVER THE DATA
plot(trainrand1$Diameter,trainrand1$Rings, pch=19, cex=0.1)   #pch= symbols, cex=symbol size


lines(sort(trainrand1$Diameter), fitted(m1)[order(trainrand1$Diameter)], col='red', type='l',lwd=2) 
lines(sort(trainrand1$Diameter), fitted(m2)[order(trainrand1$Diameter)], col='blue', type='l',lwd=2) 
lines(sort(trainrand1$Diameter), fitted(m3)[order(trainrand1$Diameter)], col='green', type='l',lwd=2) 
lines(sort(trainrand1$Diameter), fitted(m4)[order(trainrand1$Diameter)], col='yellow', type='l',lwd=2) 
lines(sort(trainrand1$Diameter), fitted(m5)[order(trainrand1$Diameter)], col='pink', type='l',lwd=2) 
lines(sort(trainrand1$Diameter), fitted(m6)[order(trainrand1$Diameter)], col='violet', type='l',lwd=2) 
lines(sort(trainrand1$Diameter), fitted(m7)[order(trainrand1$Diameter)], col='orange', type='l',lwd=2) 
lines(sort(trainrand1$Diameter), fitted(m8)[order(trainrand1$Diameter)], col='magenta', type='l',lwd=2) 
lines(sort(trainrand1$Diameter), fitted(m9)[order(trainrand1$Diameter)], col='purple', type='l',lwd=2) 
lines(sort(trainrand1$Diameter), fitted(m10)[order(trainrand1$Diameter)], col='grey', type='l',lwd=2) 

legend("topleft", legend=c("Degree 1", "Degree 2","Degree 3","Degree 4","Degree 5","Degree 6","Degree 7","Degree 8","Degree 9","Degree 10"),
       col=c("red", "blue","green","yellow","pink","violet","orange","magenta","purple","grey"), lty=1:2, cex=0.41,box.lty=2)
