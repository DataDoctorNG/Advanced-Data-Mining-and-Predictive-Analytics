rm(list = ls())
library(caret)
library(ada)
set.seed(2018)

data(GermanCredit)

Index=sample(nrow(GermanCredit),nrow(GermanCredit)*.8)
Train=GermanCredit[Index,]
Test=GermanCredit[-Index,]

Model<-ada(Class~.,data=Train, iter=500)
Predictions=predict(Model,Test)
table(Predictions,Test$Class)


#---------------------------------------------------------------
Model_caret<-train(Class~.,data=Train, method='ada')
Predictions_caret=predict(Model_caret,Test)
table(Predictions_caret,Test$Class)

Model_caret2<-train(Class~.,data=Train, method='ada',tuneGrid=expand.grid(maxdepth = c(3,4,6),iter=c(150,300,500), nu=c(0.1,0.05)))
Predictions_caret2=predict(Model_caret2,Test)
table(Predictions_caret2,Test$Class)