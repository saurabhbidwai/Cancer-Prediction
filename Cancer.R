#Set working directory where my dataset resides
setwd("E:/Aegis/Machine Learning/Data")

#Read the .csv file of cancer dataset
cancer=read.csv("cancer.csv")

#checking the structure of data
head(cancer)

#checking summary of data
summary(cancer)

#Checking the Correlation of the cancer dataset's features
library("corrplot")
corrplot(cor(cancer))

#Ploting the first column i.e age of cancer dataset
plot(cancer[ ,1])

#Histogram of age column
hist(cancer[ ,1])

#plot grade vs age column of cancer dataset
plot(cancer[ ,"grade"],cancer[,"age"])

#Correlation of age and grade of cancer dataset
cor(cancer[ ,"grade"],cancer[,"age"])

#Histogram of ethnicity column of cancer dataset
hist(cancer[ ,2])

#correlation of ethnicity and grade of cancer dataset
cor(cancer[ ,2],cancer[,"grade"])

#checking is there any NA data in row  
anyNA(cancer)

#if any NA then its row and column number i.e its position
which(is.na(cancer),T) 

#Divide the grade 1 and grade 0 data
class1=subset(cancer,grade==1)
class0=subset(cancer,grade==0)

#taking sample of 70% grade 1 and grade 0 data
ind0=sample(1:nrow(class0),round(0.70*(nrow(class0))))
ind1=sample(1:nrow(class1),round(0.70*(nrow(class1))))

train1=class1[ind1,]
train0=class0[ind0,]
test1=class1[-ind1,]
test0=class0[-ind0,]

#creating final train and test data
train=rbind(train1,train0)
test=rbind(test1,test0)

#logistic model of cancer data
fit1=glm(grade ~ sqrt(age) + ethnicity + ER + PR + RT + CT + HT + factor(tumorStage) + tumorSize ,family=binomial("logit"),train)#71.651

#step function applied on fit1 model
step(fit1)

#created the new logistic model according to step() function
fit=glm(grade ~ sqrt(age) + RT + factor(tumorStage) + tumorSize ,family=binomial("logit"),train)#63.269

#plot of logistic model
plot(fit)

#checking is there any multicolinearity in logistic model
library("car")
vif(fit)

#predicting the grade of our test data 
out=predict(fit,test,type="response")

#checking summary of our logistic model
summary(fit)

#Rounding of the grade vaue
out=ifelse(out>0.5,1,0)
out

#checking the accuracy
count=0
accuracy=0
for(i in 1:nrow(test)){
  if(out[i]==test[i,11]){
    count=count+1
  }
accuracy=c(accuracy,count/nrow(test))
}
accuracy

#checking root mean square error
RMSE=sqrt(mean((out-test["grade"])^2))
RMSE

