setwd("Documents/GitHub/omsi/ml-100k")
ratings <- read.table('u.data',header=F)
names(ratings) <- c('usernum','movienum','rating','transID')
demog<-read.table('u.user',sep='|')
names(demog)<-c('usernum','age','gender','occ','ZIP')
u.big<-merge(ratings,demog,by.x=1,by.y=1)

movies <- read.csv('u.item',sep='|',header=F)
genre <- read.csv('u.genre',sep='|',header=F)

nr <- nrow(movies)
singles <- as.data.frame(matrix(0,ncol=20,nrow=nr))
singles[,1] <- movies$V1
names(singles)<-c('movienum',as.character(genre$V1))
singles[,2:20] <- movies[,6:24]
u.big2<-merge(u.big,singles)


test<-sample(1:nrow(u.big2),5000)
utest<-u.big2[test,]
utrain<-u.big2[-test,]
colnames(u.big2)<-c('movienum','usernum','rating','transID','age','gender','occ','ZIP','unknown','Action','Adventure','Animation','Children','Comedy','Crime','Documentary','Drama','Fantasy','Noir','Horror','Musical','Mystery','Romance','SciFi','Thriller','War','Western')
u.big2
lmout<-lm(rating~age+gender+occ+unknown+Action+Adventure+Animation+Children+Comedy+Crime+Documentary+Drama+Fantasy+Noir+Horror+Musical+Mystery+Romance+SciFi+Thriller+War+Western,data=utrain)
preds<-predict(lmout,utest)
mean(abs(preds-utest$rating))    

#new term when lm()rr/20
u.big2[,"new"]<-NA
mmin<-min(u.big2[,2])
mmax<-max(u.big2[,2])

for(i in (mmin:mmax)){
  uu<-u.big2[u.big2[,2]==i,]
  rr<-t(as.matrix(colMeans(uu[,9:27])))%*%t(as.matrix(uu[,9:27]))
  rr<-rr/19
  u.big2[u.big2[,2]==i,28]<-t(rr)
}


test<-sample(1:nrow(u.big2),5000)
utest<-u.big2[test,]
utrain<-u.big2[-test,]
lmout1<-lm(rating~age+gender+occ+unknown+Action+Adventure+Animation+Children+Comedy+Crime+Documentary+Drama+Fantasy+Noir+Horror+Musical+Mystery+Romance+SciFi+Thriller+War+Western+new,data=utrain)
preds1<-predict(lmout1,utest)
mean(abs(preds1-utest$rating))

u.big2[,"age_gender"]<-(u.big2$age)*(u.big2$gender)
u.big2

test<-sample(1:nrow(u.big2),5000)
utest<-u.big2[test,]
utrain<-u.big2[-test,]
lmout1<-lm(rating~gender+occ+unknown+Action+Adventure+Animation+Children+Comedy+Crime+Documentary+Drama+Fantasy+Noir+Horror+Musical+Mystery+Romance+SciFi+Thriller+War+Western+new+age_square,data=utrain)
preds1<-predict(lmout1,utest)
mean(abs(preds1-utest$rating))

gender_dummy<-factorToDummies(u.big2$gender,"gender")
u.big2[,"gender_dummy"]<-gender_dummy
u.big2[,"age_gender"]<-u.big2[,"gender_dummy"]*u.big2[,"age"]
test<-sample(1:nrow(u.big2),5000)
utest<-u.big2[test,]
utrain<-u.big2[-test,]
lmout1<-lm(rating~age+gender+occ+unknown+Action+Adventure+Animation+Children+Comedy+Crime+Documentary+Drama+Fantasy+Noir+Horror+Musical+Mystery+Romance+SciFi+Thriller+War+Western+new+age_square+age_gender,data=utrain)
preds1<-predict(lmout1,utest)
mean(abs(preds1-utest$rating))

#lasso
mm<-factorToDummies(u.big2$occ,"occ")
u.big2[,colnames(mm)]<-mm
x<-as.matrix(u.big2[,c('gender_dummy','age','unknown','Action','Adventure','Animation','Children','Comedy','Crime','Documentary','Drama','Fantasy','Noir','Horror','Musical','Mystery','Romance','SciFi','Thriller','War','Western',colnames(mm))])
test<-sample(1:nrow(x),5000)
xtest<-x[test,]
xtrain<-x[-test,]
fit.lasso<-lars(xtrain,u.big2$rating[-test],type="lasso",trace = FALSE)
pred<-predict.lars(object = fit.lasso,newx=xtest,s=0.1,mode="lambda") 
mean(abs(pred$fit-u.big2$rating[test]))  #0.89

#NMF
library(recosystem)
library(regtools)
r=Reco()
test<-sample(1:nrow(u.big2))
train<-u.big2[-test,]
test<-u.big2[test,]
train_set=data_memory(user_index=train$userid,
                      item_index = train$movieid,
                      rating = train$rating,
                      index1=TRUE)

opts=r$tune(train_set,opts=list(dim=100,niter=10,nmf=TRUE))
r$train(train_set,opts=c(opts$min,nthread=1,niter=10,nmf=TRUE))

test_set=data_memory(test$userid,test$movieid,index1=TRUE)
pred$rating=r$predict(test_set,out_memory())
error<-mean(abs(pred$rating-test$rating))

train_set<-data.frame(train$userid,train@movieid,train$rating)
rec<-trainReco(trainset,rnk=100,nmf=TRUE)













