library(randomForest)
library(ggplot2)
library(gmodels)
library(dummies)
install.packages("Metrics")
library(Metrics)
getwd()
setwd("I:\\R programming\\data science\\practice")
train=read.csv("train.csv")
test=read.csv("test.csv")
head(train)
head(test)
str(train)
names(train)
names(test)
#target variable is "Item_Outlet_Sales"
colSums(is.na(train))
#missing values in Item_Weight
colSums(is.na(test))
#data cleaning 
median(train$Item_Weight,na.rm = TRUE)
median(test$Item_Weight,na.rm = TRUE)
#check for the condition target variable must fallow the normal distribution
hist(log(train$Item_Outlet_Sales))
hist(sqrt(train$Item_Outlet_Sales))
#it approxy matly fallow the normal distribution
#adding the target varable to  the test data
test$Item_Outlet_Sales=NA
ncol(test)
names(test)

#murging the data
data.full=rbind(train,test)
str(data.full)
colSums(is.na(data.full))
weight=median(data.full$Item_Weight,na.rm = TRUE)

#repalce the missing value with median
data.full$Item_Weight[is.na(data.full$Item_Weight)]=weight
mean(data.full$Item_Weight)

#spliit the trrain and test data
set.seed(1234)
train12=data.full[1:8523,]
test12=data.full[-c(1:8523),]

#building the model random forest
?randomForest
names(train12)
str(train12)
table(train12$Item_Identifier)

#convert them to dummy variables
#train12$Item_Identifier=dummy(train12$Item_Identifier)
train12$Item_Identifier=as.numeric(train12$Item_Identifier)
model12=randomForest(train12$Item_Outlet_Sales~.,data = train12,ntree=500)
varImpPlot(model12)
#prediticting the values
test12$Item_Identifier=as.numeric(test12$Item_Identifier)
test12$Item_Outlet_Sales=predict(model12,newdata = test12)
head(test12$Item_Outlet_Sales)

test12$Item_Identifier21=as.factor(test12$Item_Identifier)

result=data.frame(Item_Identifier=test$Item_Identifier,Outlet_Identifier=test12$Item_Outlet_Sales)

#creating csv file
write.csv(result,file = "submission.csv",row.names = FALSE)
