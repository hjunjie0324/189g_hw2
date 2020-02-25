lmFinalModel <- function(u.big2){
  library(recosystem)
  library(regtools)
 # u.big <- read.delim("u.big.tst",header = TRUE, sep="\t",dec = ".")
  colnames(u.big2)<-c('movienum','usernum','rating','transID','age','gender','occ','ZIP',
                     'unknown','Action','Adventure','Animation','Children','Comedy',
                     'Crime','Documentary','Drama','Fantasy','Noir','Horror','Musical',
                     'Mystery','Romance','SciFi','Thriller','War','Western')
  
  #new term when lm()rr/20
  u.big2[,"new"]<-NA
  mmin<-min(u.big2[,2])
  mmax<-max(u.big2[,2])
  
  newFea<-matrix(0L,nrow=mmax,ncol=19)
  
  for(i in (mmin:mmax)){
    uu<-u.big2[u.big2[,2]==i,]
    for(j in (9:27)){
      mean<-mean(uu[uu[,j]==1,]$rating)
      newFea[i,j-8]<-mean
    }
  }
  newFea[is.nan(newFea)]<-0
  for(i in (1:nrow(u.big2))){
    user_num<-u.big2[i,2]
    rr<-newFea[user_num,]%*%t(as.matrix(u.big2[i,9:27]))
    rr<-rr/19
    u.big2[i,28]<-rr
  }
  
  user_mean<-tapply(u.big2$rating,u.big2$usernum,mean)
  movie_mean<-tapply(u.big2$rating,u.big2$movienum,mean)
  
  u.big2[,"user_mean"]<-NA
  u.big2[,"movie_mean"]<-NA
  for(i in (1:nrow(u.big2))){
    userId<-u.big2[i,2]
    movieId<-u.big2[i,1]
    u.big2[i,29]<-user_mean[userId]
    u.big2[i,30]<-movie_mean[movieId]
  }
  
  gender_dummy<-factorToDummies(u.big2$gender,"gender")
  u.big2[,"gender_dummy"]<-gender_dummy
  u.big2[,"age_gender"]<-u.big2[,"gender_dummy"]*u.big2[,"age"]
  
  
  zip <- as.character(u.big2$ZIP)
  u.big2$ZIP_first2 <- substr(zip,1,2)  #take first 2 digits
  test<-sample(1:nrow(u.big2),5000)
  utest<-u.big2[test,]
  utrain<-u.big2[-test,]
  lmout1<-lm(rating~occ+unknown+Action+Adventure+Animation+Children+Comedy+Crime+Documentary+Drama+Fantasy+Noir+Horror+Musical+Mystery+Romance+SciFi+Thriller+War+Western+new+user_mean+movie_mean+age_gender+ZIP_first2,data=utrain)
  preds1<-predict(lmout1,utest)
  error<-MAPE(preds1,utest$rating)
  return(error)
  #mean(abs(preds1-utest$rating))  #0.697
}

nmfFinalModel <- function(u.big2){
library(recosystem)
library(regtools)
#u.big2 <- u.big
colnames(u.big2)<-c('movienum','usernum','rating','transID','age','gender','occ','ZIP','unknown','Action','Adventure','Animation','Children','Comedy','Crime','Documentary','Drama','Fantasy','Noir','Horror','Musical','Mystery','Romance','SciFi','Thriller','War','Western')
  #0.91

#new term when lm()rr/20
u.big2[,"new"]<-NA
mmin<-min(u.big2[,2])
mmax<-max(u.big2[,2])

newFea<-matrix(0L,nrow=mmax,ncol=19)

for(i in (mmin:mmax)){
  uu<-u.big2[u.big2[,2]==i,]
  for(j in (9:27)){
    mean<-mean(uu[uu[,j]==1,]$rating)
    newFea[i,j-8]<-mean
  }
}
newFea[is.nan(newFea)]<-0
for(i in (1:nrow(u.big2))){
  user_num<-u.big2[i,2]
  rr<-newFea[user_num,]%*%t(as.matrix(u.big2[i,9:27]))
  rr<-rr/19
  u.big2[i,28]<-rr
}

 #0.79

user_mean<-tapply(u.big2$rating,u.big2$usernum,mean)
movie_mean<-tapply(u.big2$rating,u.big2$movienum,mean)

u.big2[,"user_mean"]<-NA
u.big2[,"movie_mean"]<-NA
for(i in (1:nrow(u.big2))){
  userId<-u.big2[i,2]
  movieId<-u.big2[i,1]
  u.big2[i,29]<-user_mean[userId]
  u.big2[i,30]<-movie_mean[movieId]
}



gender_dummy<-factorToDummies(u.big2$gender,"gender")
u.big2[,"gender_dummy"]<-gender_dummy
u.big2[,"age_gender"]<-u.big2[,"gender_dummy"]*u.big2[,"age"]

zip <- as.character(u.big2$ZIP)
u.big2$ZIP_first2 <- substr(zip,1,2)  #take first 2 digits
test<-sample(1:nrow(u.big2),5000)
utest<-u.big2[test,]
utrain<-u.big2[-test,]
lmout1<-lm(rating~gender+occ+unknown+Action+Adventure+Animation+Children+Comedy+Crime+Documentary+Drama+Fantasy+Noir+Horror+Musical+Mystery+Romance+SciFi+Thriller+War+Western+new+user_mean+movie_mean+age_gender+ZIP_first2,data=utrain)
preds1<-predict(lmout1,utest)
mean(abs(preds1-utest$rating))  #0.689
minresid <- min(lmout1$residuals)
utrain[,3]<- lmout1$residuals-minresid


#lasso
mm<-factorToDummies(u.big2$occ,"occ")
u.big2[,colnames(mm)]<-mm

#NMF
library(recosystem)
library(regtools)
r=Reco()
test<-sample(1:nrow(u.big2))
train<-u.big2[-test,]
test<-u.big2[test,]
train_set=data_memory(user_index=utrain$usernum,
                      item_index = utrain$movienum,
                      rating = utrain$rating,
                      index1=TRUE)

opts=r$tune(train_set,opts=list(dim=40,niter=10,nmf=TRUE))
r$train(train_set,opts=c(opts$min,nthread=1,niter=10,nmf=TRUE))

test_set=data_memory(utest$usernum,utest$movienum,index1=TRUE)
pred=r$predict(test_set,out_memory())
tmp <- preds1+minresid+pred
error<-mean(abs(tmp-utest$rating))

return(error)
#error<-mean(abs(tmp-utest$rating)
}
