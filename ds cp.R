setwd("C:/Users/HP EK0025TX/OneDrive/Desktop/ds cp")

#CaloriePrediction 

f1<-read.csv("calories.csv")
f2<-read.csv("exercise.csv")
calories<-f1$Calories
f1<-cbind(f2,calories)

#importing libraries
library(ggplot2)
library(dplyr)
library(rpart)
library(tidyverse)
library(randomForest)
library(modelr)
library(e1071)
library(caret)
library(xgboost)

#attach(f1)
str(f1)
summary(f1)


f1<-subset(f1, select = -c(User_ID))

hist(f1$calories,main='Distribution of Calories',xlab='Calories',ylab='Frequency',col=blues9)
hist(f1$Heart_Rate,main='Distribution of Heart Rate',xlab = 'Heart Rate',ylab = 'Frequency',col=blues9)
hist(f1$Duration,main='Distribution w.r.t. Duration',xlab='Duration',ylab='Frequency',col = blues9,)

plot(f1$Duration,f1$calories,main = 'Calories wrt Duration',ylab='Calories',xlab='Duration',col='blue', pch=20)
plot(f1$Heart_Rate,f1$calories,main='Calories wrt Heart Rate',ylab='Calories',xlab='Heart Rate',col='blue',pch=20)
plot(f1$calories~Age,data=f1,col='blue',main='Calories wrt Age',xlab='Age',ylab='Calories',pch=20)

set.seed(1)

sample<-sample(c(TRUE,FALSE),nrow(f1),replace = TRUE,prob=c(0.7,0.3))
train <- f1[sample, ]
test <- f1[!sample, ]


#SimpleLinearRegression (First model)

slr = lm(calories~Duration,train)
summary(slr)
rsquareforslr<-summary(slr)$r.squared
p=ggplot(data=train,aes(x=calories,y=Duration))+geom_point()+geom_smooth(method = "lm", formula = y~x, color="red2",se=F)
print(p)
result<-predict(slr)
print(result)
train1=cbind(train,result)
pred_result=predict(slr,test)
comp_result<-as.data.frame(cbind(actual = test$calories, predicted = pred_result))
error1 = comp_result$actual - comp_result$predicted
comp_result<-cbind(comp_result,error1)
rmse1 = sqrt(mean(comp_result$error1^2))
print(rmse1)
predictbySLR<-comp_result$predicted


#SimpleLinearRegression (Second model)

slr2 = lm(calories~Heart_Rate,train)
summary(slr)
p1=ggplot(data=train,aes(x=calories,y=Heart_Rate))+geom_point()+geom_smooth(method = "lm", formula = y~x, color="red2",se=F)
print(p1)
result2<-predict(slr2)
print(result2)
train2=cbind(train,result2)

pred_result2=predict(slr2,test)
comp_result2<-as.data.frame(cbind(actual = test$calories, predicted = pred_result2))
error2 = comp_result2$actual - comp_result2$predicted
comp_result2<-cbind(comp_result2,error2)
rmse2 = sqrt(mean(comp_result2$error2^2))
print(rmse2)
predictbySLR2<-comp_result2$predicted



# Mulitiple Linear Regression

mlr=lm(calories~.,train)
summary(mlr)
rsquareformlr<-summary(mlr)$r.squared
result3=predict(mlr)
print(result3)
train3=cbind(train,result3)

pred_result3=predict(mlr,test)
comp_result3<-as.data.frame(cbind(actual = test$calories, predicted = pred_result3))
error3 = comp_result3$actual - comp_result3$predicted
comp_result3<-cbind(comp_result3,error3)
rmse3 = sqrt(mean(comp_result3$error3^2))
print(rmse3)

predictbyMLR=comp_result3$predicted

ggplot(data=test, aes(x=predict(mlr,test), y= calories)) +geom_point(color="blue") +
  geom_abline(intercept=0, slope=1,color="red")+labs(x='Predicted Values', y='Actual Values', title='Predicted vs. Actual Values with MLR')



#DecisionTree

fit<- rpart(calories~.,data=train)
plot(fit,uniform=TRUE)
text(fit,cex=0.7)

head(f1)
predict(fit,head(f1))
mae(model = fit, data = head(f1))

pred_result4=predict(fit,test)
comp_result4<-as.data.frame(cbind(actual = test$calories, predicted = pred_result4))
error4 = comp_result4$actual - comp_result4$predicted
comp_result4<-cbind(comp_result4,error4)
rmse4 = sqrt(mean(comp_result4$error4^2))
print(rmse4)

predictbyDTree=comp_result4$predicted

ggplot(data=test, aes(x=predict(fit,test), y= calories)) +geom_point(color="blue") +
  geom_abline(intercept=0, slope=1,color="red")+labs(x='Predicted Values', y='Actual Values', title='Predicted vs. Actual Values with Decision Tree')



#RandomForestAlgorithm

set.seed(1)
fit4<- randomForest(calories~., data= train, na.action=na.exclude)

pred_result5=predict(fit4,test)
comp_result5<-as.data.frame(cbind(actual = test$calories, predicted = pred_result5))
error5 = comp_result5$actual - comp_result5$predicted
comp_result5<-cbind(comp_result5,error5)
rmse5 = sqrt(mean(comp_result5$error5^2))
print(rmse5)

predictbyRF=comp_result5$predicted

ggplot(data=test, aes(x=predict(fit4,test), y= calories)) +geom_point(color="blue") +
  geom_abline(intercept=0, slope=1,color="red")+labs(x='Predicted Values', y='Actual Values', title='Predicted vs. Actual Values with Random Forest')



#SVR

set.seed(1)
svm_model = svm(calories~.,train)
summary(svm_model)

result6 = predict(svm_model)
print(result6)
train6 = cbind(train,result6)

pred_result6 = predict(svm_model,test)
comp_results6 = as.data.frame(cbind(actual = test$calories, predicted = pred_result6))
error6 = comp_results6$actual - comp_results6$predicted

comp_result6<-cbind(comp_results6,error6)
rmse6 = sqrt(mean(comp_result6$error6^2))  
print(rmse6)

predictbySVM<-comp_result6$predicted

ggplot(data=test, aes(x=predict(svm_model,test), y= calories)) +geom_point(color="blue") +
  geom_abline(intercept=0, slope=1,color="red")+labs(x='Predicted Values', y='Actual Values', title='Predicted vs. Actual Values with SVM')



#KNNRegression

set.seed(1)
model<-train(calories~.,data=train,method='knn',preProcess=c("center","scale"))
model
pred_result7<-predict(model,test)
comp_result7<-as.data.frame(cbind(actual = test$calories, predicted = pred_result6))
error7 = comp_result7$actual - comp_result7$predicted
comp_result7<-cbind(comp_result7,error7)
rmse7 = sqrt(mean(comp_result7$error7^2))
print(rmse7)

predictByKNN=comp_result7$predicted

ggplot(data=test, aes(x=predict(model,test), y= calories)) +geom_point(color="blue") +
  geom_abline(intercept=0, slope=1,color="red")+labs(x='Predicted Values', y='Actual Values', title='Predicted vs. Actual Values with KNN Regression')



#XGBoost

train7=data.matrix(train)
test7=data.matrix(test)

xgb<- xgboost(data=train7,label=train$calories,max.depth=2,nrounds=1000)
pred_result8<-predict(xgb,test7)
comp_result8<-as.data.frame(cbind(actual = test$calories, predicted = pred_result8))
error8 = comp_result8$actual - comp_result8$predicted
comp_result8<-cbind(comp_result8,error8)
rmse8 = sqrt(mean(comp_result8$error8^2))
print(rmse8)


predictByXGB=comp_result8$predicted

ggplot(data=test, aes(x=predict(xgb,test7), y= calories)) +geom_point(color="blue") +
  geom_abline(intercept=0, slope=1,color="red")+labs(x='Predicted Values', y='Actual Values', title='Predicted vs. Actual Values with XGBoost')



# Model evaluation

mae1<-mae(model=slr,data=test)
cat("\nThe Mean Absolute Error with SLR:",mae1)
cat("\nThe Root Mean Square Error with SLR:",rmse1)
mae7<-mae(model=slr2,data=test)
cat("\nThe Mean Absolute Error with SLR:",mae7)
cat("\nThe Root Mean Square Error with SLR:",rmse2)
mae2<-mae(model=mlr,data=test)
cat("\nThe Mean Absolute Error with MLR:",mae2)
cat("\nThe Root Mean Square Error with MLR:",rmse3)
mae3<-mae(model=fit,data=test)
cat("\nThe Mean Absolute Error with DTree:",mae3)
cat("\nThe Root Mean Square Error with DTree:",rmse4)
mae5<-mae(model=fit4,data=test)
cat("\nThe Mean Absolute Error with RF:",mae5)
cat("\nThe Root Mean Square Error with RF:",rmse5)
mae6<-mae(model=svm_model,data=test)
cat("\nThe Mean Absolute Error with SVM:",mae6)
cat("\nThe Root Mean Square Error with SVM:",rmse6)

cat("\nThe Root Mean Square Error with KNN:",rmse7)
cat("\nThe Root Mean Square Error with XGB:",rmse8)


comparision<-data.frame(test$calories,predictbySLR,predictbyMLR,predictbyDTree,predictbyRF,predictbySVM,predictByKNN,predictByXGB)
print(comparision)

actual <- comparision$test.calories
predictedRF <- comparision$predictbyRF
rsquareforRF <- 1 - (sum((actual-predictedRF)^2)/sum((actual-mean(actual))^2))

predictedbyDTree <- comparision$predictbyDTree
rsquareforDTree <- 1 - (sum((actual-predictedbyDTree)^2)/sum((actual-mean(actual))^2))

predictedbySVM <- comparision$predictbySVM
rsquareforSVM <- 1 - (sum((actual-predictedbySVM)^2)/sum((actual-mean(actual))^2))

predictedbyKNN <- comparision$predictByKNN
rsquareforKNN <- 1 - (sum((actual-predictedbyKNN)^2)/sum((actual-mean(actual))^2))

predictedbyXGB <- comparision$predictByXGB
rsquareforXGB <- 1 - (sum((actual-predictedbyXGB)^2)/sum((actual-mean(actual))^2))

Algorithms<-c("SLR","MLR","DTree","RF","SVM","KNN","XGB")
RSquare<- c(rsquareforslr,rsquareformlr,rsquareforDTree,rsquareforRF,rsquareforSVM,rsquareforKNN,rsquareforXGB)
RMSE<- c(rmse1,rmse3,rmse4,rmse5,rmse6,rmse7,rmse8)
MAE<- c(mae1,mae2,mae3,mae5,mae6,0,0)

performance<- data.frame(Algorithms,RSquare,RMSE,MAE)