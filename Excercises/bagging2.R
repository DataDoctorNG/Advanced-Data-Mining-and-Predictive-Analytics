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

#######################################################################
Pred_matrix<-NULL

for (n in 1:1000){ # Creat 1000 Tress
  Train=GermanCredit[Index,]
  Index_row=sample(nrow(Train),round(nrow(Train)*0.6)) # Each tree only sees 60% of records
  Train<-Train[Index_row,]
  
  Model<-rpart(Class~.,data=Train, method='class')
  Predictions=predict(Model,Test) 
  Pred_matrix=cbind(Pred_matrix,Predictions[,2])# add prediction probs to the list, only one column is needed 
}

Pred_avg=apply(Pred_matrix,1,mean) # Average probobilities

Pred_avg_factor<-Pred_avg
Pred_avg_factor[Pred_avg<0.5]<-'Bad' # Determine good ans bad cases
Pred_avg_factor[Pred_avg>0.5]<-'Good'

table(Pred_avg_factor,Test$Class)

#---------------------------------------------------------------