
train<-sample(891,600)
hadoop.train<-hadoop[train,]
hadoop.test<-hadoop[-train,]
boost=gbm(Survived~.,data=hadoop.train[,c(2,3,4,5,8,9)], distribution="bernoulli",n.trees =5000 , interaction.depth =5, verbose=F,cv.folds=6,bag.fraction=.5,n.minobsinnode=12)
boost_predictions<-predict(boost,hadoop.test[,c(3,4,5,8,9)],n.trees=5000,type="response")
boost_predictions<-round(boost_predictions)
(291-abs(sum(boost_predictions-hadoop.test[,2])))/291

#boost=gbm.fit(hadoop[,-c(2,7,8)],hadoop[,2], distribution="bernoulli",n.trees =5000 , interaction.depth =3,bag.fraction=.5,n.minobsinnode=10,verbose=F,cv.folds=6)
boost=gbm(Survived~.,data=sub[,c(2,3,4,5,9,10)], distribution="bernoulli",n.trees =5000 , interaction.depth =5, verbose=F,cv.folds=6,bag.fraction=.5,n.minobsinnode=12)
boost_predictions<-predict(boost,original_test[,c(2,4,5,9,10)],n.trees=5000,type="response",verbose=F)
boost_predictions<-round(boost_predictions)
boost_submission<-cbind(test2$PassengerId,boost_predictions)
colnames(boost_submission)<-c("PassengerId","Survived")
#boost_submission[,2]<-boost_submission[,2]-rep(1,418)
write.csv(boost_submission, "boost.csv", row.names=F)

boost=gbm.fit(train.final[,-1],train.final[,1], distribution="bernoulli",n.trees =5000 , interaction.depth =5, verbose=F,cv.folds=6)


bound<-matrix(c(3,0,100),nrow=1)
imputed_train.master<-amelia(test[master.vec,-c(1,3,4)],m=1,bounds=bound)
imputed_train.mr<-amelia(test[mr.vec,c(2,5,6,7)],m=1,bounds=bound)
train.fixed.mr<-imputed_train.mr$imputations[[1]]
imputed_train.miss<-amelia(test[miss.vec,c(2,5,6,7)],m=1,bounds=bound)
train.fixed.miss<-imputed_train.miss$imputations[[1]]
imputed_train.mrs<-amelia(test[mrs.vec,c(2,5,6,7)],m=1,bounds=bound)
train.fixed.mrs<-imputed_train.mrs$imputations[[1]]





train.final<-rbind(train.fixed.master,train.fixed.mr,train.fixed.miss,train.fixed.mrs)

summary(train.final)
summary(test)



boost_predictions<-predict(boost,test,n.trees=5000,type="response",verbose=F)
head(boost_predictions)
boost_submission<-cbind(test2$PassengerId,round(boost_predictions))


