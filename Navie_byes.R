####Navie Byes  Algorithm on CTG Dataset
###install library e1071

ctg<-read.csv("E:/datasets/CTG.csv")
ctg$NSP<-factor(ctg$NSP)

##Apply Sampling
ctg_sample=sample(2,nrow(ctg),replace = TRUE,prob = c(0.8,.2))
ctg_train=ctg[ctg_sample==1,]
ctg_test=ctg[ctg_sample==2,]

#Apply  Navie Byes Function
library(dplyr)
library(e1071)
ctg_NB<-naiveBayes(NSP~.,data=ctg_train)
pred_NB<-predict(ctg_NB,ctg_test)

tab<-table(pred_NB,ctg_test$NSP)
tab

####To Calculate accuracy
acc<-sum(diag(tab))*100/sum(tab)
acc

##Apply Over Sampling(increase the record of class which is under represented) & execute code again from model buliding

ctg_train1<-ctg_train[ctg_train$NSP==2,]
ctg_train2<-ctg_train[ctg_train$NSP==3,]

dim (ctg_train1)
dim(ctg_train2)
ctg_train=rbind(ctg_train,ctg_train1,ctg_train2,ctg_train2 , ctg_train2, ctg_train2 , ctg_train2)
table(ctg_train$NSP)


