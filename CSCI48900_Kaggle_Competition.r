
#setting up working directory for workspace to access files
setwd("C:\\Users\\Austin\\Desktop\\CSCI48900 Data Science")
getwd()

#files provided with a template for target results
Test_File <- read.csv(file='test.csv', header=T, stringsAsFactors=T)
Train_File <- read.csv(file='train.csv', header=T, stringsAsFactors=T)

head(Train_File)


#glm model
#attempted this code with various parameter combination.
#block is listed once due to only changing the modeling parameter where the rest of the code is the same


glm_model = glm(pstr~SEX+ageyear+higheduc, data=Train_File)
Test_File$pstr=predict(glm_model, Test_File)
Test_File$pstr[is.na(Test_File$pstr)] <-0
write.csv(Test_File[,c("test_id", "pstr")], "submission_glm_ageyear_higheduc_SEX1.csv",row.names=F)


glm_model2 = glm(pstr~SEX+ageyear+higheduc+interview_age+hincome, data=Train_File)
Test_File$pstr=predict(glm_model2, Test_File)
Test_File$pstr[is.na(Test_File$pstr)] <-0
write.csv(Test_File[,c("test_id", "pstr")], "submission_glm_ageyear_higheduc_SEX1_interviewage_hincome.csv",row.names=F)


#GBM model
#attempted this code with various parameter combination
#working with this model, it was observed that it provided the best results compared to other
#models used in the competition

#block is listed once due to only changing the modeling parameter where the 
#rest of the code is the same repeated.
library(gbm)
set.seed(102)
gbm_model2 = gbm(pstr~SEX+ageyear+higheduc+interview_age+hincome+child_social_media_time_cv, data=Train_File, distribution = "gaussian")
Test_File$pstr=predict(gbm_model2, Test_File)
Test_File$pstr[is.na(Test_File$pstr)] <-0
write.csv(Test_File[,c("test_id", "pstr")], "submission_gbm_ageyear_higheduc_SEX_interviewage_hincomechild_social_media_time_cv.csv",row.names=F)

#randomForest
#attempted this code with various parameter combination
#block is listed once due to only changing the modeling parameter where the 
#rest of the code is the same repeated.
library(randomForest)
set.seed(888)
RF_model = randomForest(Train_File$pstr~SEX+ageyear+interview_age+hincome+child_social_media_time_cv, data=Train_File, mtry = 3, importance=TRUE, na.action=na.omit)
Test_File$pstr=predict(RF_model, Test_File)
Test_File$pstr[is.na(Test_File$pstr)] <-0
write.csv(Test_File[,c("test_id", "pstr")], "submission_RF_ageyear_SEX_interviewage+hincome+child_social_media_time_cv.csv",row.names=F)


#glmnet
#attempted to use a glmnet but could not export the data for analysis
#the results obtained works but the competition produced fail upload

library(tidyverse)
library(glmnet)

Test_File1 = read.csv(file='test.csv', header=T, stringsAsFactors=T)
Train_File1 = read.csv(file='train.csv', header=T, stringsAsFactors=T)

a = Train_File1$pstr
b = Train_File1[,3:84]
b$pstr = NULL
b$SEX = as.numeric(as.factor(b$SEX))
b_table = Train_File1 %>% group_by(higheduc) %>% summarise(mnpstr = mean(pstr))

b_join = left_join(b, b_table, by = "higheduc")
b_join$higheduc=NULL

b_join[is.na(b_join)]=0
b_join = as.matrix(b_join)

dim(b_join)

b_join = b_join[,1:80]
b_glmn = glmnet(b_join, a, alpha=1)

Test_File$pstr=predict(b_glmn, Test_File)

Test_File1$pstr[is.na(Test_File1$pstr)] <-0
write.csv(Test_File1[,c("test_id", "pstr")], "submission_glmnet_ageyear_higheduc_SEX1_interviewage.csv",row.names=F)



#neural network
#provided results that flowed well for the competition
#with and without dropout makes a difference when submitting

library(keras)
library(imager)
library(tensorflow)
install_tensorflow()

b_join[,1]=(b_join[,1]-mean(b_join[,1]))/sd(b_join[,1])
dim(b_join)

#with dropout
model_bjoin <- keras_model_sequential() 
model_bjoin %>% 
  layer_dense(units = 256, activation = 'relu', input_shape = c(80)) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 1, activation = 'linear')


model_bjoin %>% compile(
  loss = 'MeanSquaredError',
  optimizer = optimizer_adam(),
  metrics = c('MeanSquaredError')
)

# fit model
history_bjoin <- model_bjoin %>% fit(
  b_join, a, 
  epochs = 30, batch_size = 128, 
  validation_split = 0.2
)

# can predict on new data
model_predict = model_bjoin %>% predict(b_join)

#Test_File1$pstr[is.na(Test_File1$pstr)] <-0
write.csv(model_predict, "submission_neuralnetwork_ageyear_higheduc_SEX1_interviewage.csv")


#without dropout
model_bjoin1 <- keras_model_sequential() 
model_bjoin1 %>% 
  layer_dense(units = 256, activation = 'relu', input_shape = c(80)) %>% 
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dense(units = 1, activation = 'linear')


model_bjoin1 %>% compile(
  loss = 'MeanSquaredError',
  optimizer = optimizer_adam(),
  metrics = c('MeanSquaredError')
)

# fit model
history_bjoin1 <- model_bjoin1 %>% fit(
  b_join, a, 
  epochs = 30, batch_size = 128, 
  validation_split = 0.2
)

#predicting new data
model_predict1 = model_bjoin1 %>% predict(b_join)

Test_File1$pstr[is.na(Test_File1$pstr)] <-0
write.csv(model_predict1, "submission_NN_ageyear_higheduc_SEX1_interviewage2.csv")

