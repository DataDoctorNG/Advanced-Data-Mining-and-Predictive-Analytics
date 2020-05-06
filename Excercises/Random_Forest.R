rm(list = ls())
library(caret)
library(rpart)
set.seed(2018)

data(GermanCredit)

Index=sample(nrow(GermanCredit),nrow(GermanCredit)*.8)
Train=GermanCredit[Index,]
Test=GermanCredit[-Index,]

Model<-rpart(Class~.,data=Train, method='class')
Predictions=predict(Model,Test,type="class")
table(Predictions,Test$Class)


#---------------------------------------------------------------

library(randomForest)

Train=GermanCredit[Index,]
Model_forest <- randomForest(Class ~ ., data = Train,ntree=500)
pred<-predict(Model_forest,Test)
table(pred,Test$Class)


Train=GermanCredit[Index,]
Model_forest2 <- randomForest(Class ~ ., data = Train,ntree=40)
pred2<-predict(Model_forest2,Test)
table(pred2,Test$Class)

#######################################
library(caret)
Model_forest_caret <- train(Class ~ ., data = Train,method='rf', 
                            trControl=trainControl(method = "oob"),
                            tuneGrid=expand.grid(mtry = 10:50))
pred_caret<-predict(Model_forest_caret,Test)
table(pred_caret,Test$Class)

##

Model_forest_caret2 <- train(Class ~ ., data = Train,method='rf', 
                            trControl=trainControl(method = "oob"),
                            tuneGrid=expand.grid(mtry = 10:50, ntree=10000))
pred_caret<-predict(Model_forest_caret,Test)
table(pred_caret,Test$Class)


