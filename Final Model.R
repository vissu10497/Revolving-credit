library(lazyeval)
library(readr)
library(plyr)
library(dplyr)
library(readxl)
library(ggplot2)
library(funModeling)
library(scales)
library(tidyverse)
library(corrplot)
library(GGally)
library(caret)
library(rpart)
library(randomForest)
library(pROC)
library(gbm)
library(choroplethr)
library(choroplethrMaps)
library(microbenchmark)
library(doParallel)
library(e1071)
library(ggthemes)
library(RColorBrewer)
library(funModeling)
library(dplyr)
library(DataExplorer)
library(tidyverse)
library(Hmisc)
library(ggplot2)
library(mvinfluence)
library(car)
library(rgl)
#install.packages("xgboost")
library(xgboost)
# read data file 
data <- read.csv("C:/Users/srivi/OneDrive/Desktop/Excelr/Project/data.csv")
# See dimension of dataset
dim(data)
summary(data)

##Selecting the important variables
credit <- data %>%
  select(loan_amnt,terms,Rate_of_intrst,annual_inc,delinq_2yrs,numb_credit,total.revol_bal,total_credits,total_rec_int,total_rec_late_fee,tot_colle_amt,tot_curr_bal)
summary(credit)
#--------------------------------------DATA CLEANING---------------------------------------------------
#------------------------------------removing NA values------------------------------------------------
credit1 <- credit %>%
  filter(!is.na(annual_inc) ,!is.na(tot_curr_bal),
         !is.na(total_rec_int),!is.na(delinq_2yrs),!is.na(numb_credit),!is.na(total_credits),!is.na(tot_colle_amt))
summary(credit1)
str(credit1)

###Encoding categorical variables
credit1$terms <- factor(credit1$terms,
                        levels=c('36 months','60 months'),
                        labels=c(1,2))
str(credit1)

#---------------------------converting variables to numeric--------------------------------------------------

credit1_2 <- transform(credit1, loan_amnt = as.numeric(loan_amnt),
                       terms=as.numeric(terms),
                       total.revol_bal = as.numeric(total.revol_bal),
                       delinq_2yrs = as.numeric(delinq_2yrs),
                       numb_credit=as.numeric(numb_credit),
                       total_credits=as.numeric(total_credits),
                       tot_colle_amt=as.numeric(tot_colle_amt)
)
str(credit1_2)


#---------------------------spliting Data into train & Test Data----------------------------------------------
set.seed(101);splitC <- sample(nrow(credit1_2),nrow(credit1_2)*.7,F)
trainC <- credit1_2[splitC,]
testC <- credit1_2[-splitC,]
summary(trainC)
str(trainC)
summary(testC)

#----------------------Converting variable to list----------------------------------------------------------
ls_credit1 = list("loan_amnt" = table(credit1$loan_amnt),"Rate_of_intrst"=table(credit1$Rate_of_intrst),"annual_inc"=table(credit1$annual_inc),
                  "total_revol_bal"=table(credit1$total.revol_bal),"total_rec_int"=table(credit1$total_rec_int),
                  "tot_curr_bal"=table(credit1$tot_curr_bal),"delinq_2yrs"=table(credit1$delinq_2yrs))
summary(ls_credit1)

#---------------------------------Model-1------------------------------------------------------------------
#---------------------------------Linear Model-------------------------------------------------------------
#Linear regression is used to predict the value of an outcome variable Y based on one or more input predictor variables X.
#The aim is to establish a linear relationship (a mathematical formula) between the predictor variable(s) and 
#the response variable, so that, we can use this formula to estimate the value of the response Y, 
#when only the predictors (Xs) values are known.

model_credit1 <-lm(total.revol_bal~loan_amnt+terms+Rate_of_intrst+
                     annual_inc+delinq_2yrs+numb_credit+
                     total_credits+total_rec_int+total_rec_late_fee+tot_colle_amt+tot_curr_bal,data = trainC)
summary(model_credit1)
####from this model,i see Adjusted r squared value as 0.2564
####RMSE 19890
vif(model_credit1)
pred_C_1 <- predict(model_credit1,newdata =trainC)
cor(pred_C_1,trainC$total.revol_bal)
######collinearity-0.5064043
trainrmse_C_1 <- mean(model_credit1$residuals^2)^.5
summary(data.frame(trainrmse_C_1))
######RMSE=19892

######testing my model
model_credit1_test <-lm(total.revol_bal~loan_amnt+terms+Rate_of_intrst+
                          annual_inc+delinq_2yrs+numb_credit+
                          total_credits+total_rec_int+total_rec_late_fee+tot_colle_amt+tot_curr_bal,data = testC)
summary(model_credit1_test)
####from this model,i see Adjusted r squared value as 0.2527
####RMSE 18640
vif(model_credit1_test)
pred_C_1 <- predict(model_credit1_test,newdata =testC)
cor(pred_C_1,testC$total.revol_bal)
######collinearity-0.525735
testrmse_C_2 <- mean(model_credit1_test$residuals^2)^.5
summary(data.frame(testrmse_C_2))


######RMSE=18640

X <- data.frame(rbind(trainrmse_C_1,testrmse_C_2))
View(X)


#-------------------------------------------MODEL-2--------------------------------------------------------
#--------------------------------------XG BOOST------------------------------------------------------------
#XGBoost belongs to a family of boosting algorithms that convert weak learners into strong learners.
#A weak learner is one which is slightly better than random guessing
#Boosting is a sequential process; i.e., trees are grown using the information from a previously grown tree one after the other. 
#This process slowly learns from data and tries to improve its prediction in subsequent iterations.


######deselecting  the target variable

train.treat <-  select(trainC,-c(7))
test.treat <- select(testC,-c(7))

####Convertingdata frame into matrix

trainC_1 <- as.matrix(train.treat)
testC_2 <- as.matrix(test.treat)
y_train<- as.matrix(trainC$total.revol_bal)
y_test <-as.matrix(testC$total.revol_bal) 

dtrain <- xgb.DMatrix(trainC_1,label=y_train)
dtest <- xgb.DMatrix(testC_2,label=y_test)

set.seed(500)

#---------------------------Creating Watch list----------------------------------------------
w <- list(train=dtrain,test= dtest)

#--------------------------------------Model--------------------------------------------------------------
#Boostergbtree parameter- a tree is grown one after other and attempts to reduce misclassification rate in subsequent iterations.
#In this, the next tree is built by giving a higher weight to misclassified points by the previous tree (as explained above)


#---------------------------Booster Parameters------------------------------------------------
#As mentioned above, parameters for tree and linear boosters are different. Let's understand each one of them:

#Parameters for Tree Booster

#nrounds[default=100]
#It controls the maximum number of iterations. For classification, it is similar to the number of trees to grow.
#Should be tuned using CV

#eta[default=0.3][range: (0,1)]-It controls the learning rate, i.e., the rate at which our model learns patterns in data. After every round, it shrinks the feature weights to reach the best optimum.
#Lower eta leads to slower computation. It must be supported by increase in nrounds.
#Typically, it lies between 0.01 - 0.3


#gamma[default=0][range: (0,Inf)]
#It controls regularization (or prevents overfitting). The optimal value of gamma depends on the data set and other parameter values.
#Higher the value, higher the regularization. Regularization means penalizing large coefficients which don't improve the model's performance. default = 0 means no regularization.

#max_depth[default=6][range: (0,Inf)]
#It controls the depth of the tree.
#Larger the depth, more complex the model; higher chances of overfitting. There is no standard value for max_depth. Larger data sets require deep trees to learn the rules from data.
#Should be tuned using CV

#eval_metric [no default, depends on objective selected]
#These metrics are used to evaluate a model's accuracy on validation data.
#For regression, default metric is RMSE. For classification, default metric is error

xgb_model1 <- xgb.train(data=dtrain,
                        booster='gbtree',
                        nrounds=80,
                        max_depth=6,
                        eval_metric='rmse',
                        eta=0.135,
                        watchlist=w,
                        early_stopping_rounds = 30)
####train-rmse:18246.822266
###test-rmse:19462.431641

#######Model2
xgb_model2 <- xgb.train(data=dtrain,
                        booster='gbtree',
                        nrounds=80,
                        max_depth=6,
                        eval_metric='rmse',
                        eta=0.1,
                        watchlist=w,
                        early_stopping_rounds = 30) 

#####train-rmse:18182.523438	
#####test-rmse:19499.806641

######Model3
xgb_model3 <- xgb.train(data=dtrain,
                        booster='gbtree',
                        nrounds=200,
                        max_depth=8,
                        eval_metric='rmse',
                        eta=0.3,
                        watchlist=w,
                        early_stopping_rounds = 30)
####train-rmse:17715.822266	
####test-rmse:19278.767578



######Model4
xgb_model4 <- xgb.train(data=dtrain,
                        booster='gbtree',
                        nrounds=400,
                        max_depth=6,
                        eval_metric='rmse',
                        eta=0.135,
                        watchlist=w,
                        early_stopping_rounds = 30)

#####train-rmse:18246.822266
#####test-rmse:19462.431641

####MODEL2 is the best model
best_model<- xgb.train(data=dtrain,
                       booster='gbtree',
                       nrounds=20,
                       max_depth=6,
                       eval_metric='rmse',
                       eta=0.1,
                       watchlist=w)

###train RMSE-18182.523438
###test RMSE-19499.806641

#####prediction of test data

pred_revolvingbalnc <- predict(best_model,newdata = dtest,class='response')
pred_revolvingbalnc <- round(pred_revolvingbalnc)
prediction <- data.frame(head(pred_revolvingbalnc))


