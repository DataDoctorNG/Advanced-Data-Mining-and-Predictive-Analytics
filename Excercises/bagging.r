rm(list = ls())

set.seed(10)
y<-c(1:1000)
x1<-c(1:1000)*runif(1000,min=0,max=2)
x2<-c(1:1000)*runif(1000,min=0,max=2)
x3<-c(1:1000)*runif(1000,min=0,max=2)

all_data<-data.frame(y,x1,x2,x3)
index <- sample(nrow(all_data),size=floor((nrow(all_data)/4)*3))
training<- all_data[index,]
testing<- all_data[-index,]

################# Simple lm model ##############
lm_fit<-lm(y~x1+x2+x3,data=training)
predictions<-predict(lm_fit,newdata=testing)
error<-sqrt((sum((testing$y-predictions)^2))/nrow(testing))

error
################# Bagging ##############

predictions_bagging<-NULL

# We will build 1000 lm models each based on 66% of data records randomly selected
for (n in 1:1000){
  sub_index=sample(nrow(training),round(nrow(training)*.66))
  sub_model=lm(y~x1+x2+x3,data=training[sub_index,]) # build model
  predictions<-predict(sub_model,testing)# predict
  predictions_bagging=cbind(predictions_bagging,predictions) #add the predictions as a new column
}
  
predictions_bagging_final=apply(predictions_bagging,1,mean) # average all predictions , average over rows i.e. dimention 1
error2<-sqrt((sum((testing$y-predictions)^2))/nrow(testing)) # calculate errors
error2