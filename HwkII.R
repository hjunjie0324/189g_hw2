setwd('D:/UCD/ECS189G/omsi-master/ml-100k')
ratings <- read.table('u.data')
names(ratings) <- c('userID','movieID','rating','transID')
head(ratings)
demog <- read.table('u.user',sep='|')
names(demog) <- c('userID','age','gender','occ','ZIP')
head(demog)
u.big <- merge(ratings,demog)
class(u.big$userID)
u.big$ZIP <- as.character(u.big$ZIP)
u.big$ZIP <- substr(u.big$ZIP,1,2)  #take first 2 digits
u.big$ZIP <- as.factor(u.big$ZIP)
head(u.big)

movies <- read.csv('u.item',sep='|',header=F)
head(movies)
movies <- movies[,-(2:5)]
genre <- read.csv('u.genre',sep='|',header=F)
head(genre)
nr <- nrow(movies)
colnames(movies)<-c('movieID',as.character(genre$V1))
movies <- movies[,-2]
head(movies)

library(data.table)
movies <- data.table(movies)
u.big <- data.table(u.big)

u.bignew <- merge(u.big,movies)
head(u.bignew)

rat_age <- tapply(u.bignew$rating,u.bignew$age,mean)
barplot(rat_age)
rat_gen <- tapply(u.bignew$rating,u.bignew$gender,mean) 
rat_ZIP <- tapply(u.bignew$rating,u.bignew$ZIP,mean)
barplot(rat_ZIP,ylim=c(0,5))
rat_occ <- tapply(u.bignew$rating,u.bignew$occ,mean) 
barplot(rat_occ,ylim=c(0,5))


u <- u.bignew[,-4]
u$movieID <- as.factor(u$movieID)
u$userID <- as.factor(u$userID)
head(u)
test <- sample(1:nrow(u),5000)
utest <- u[test,]
utrain <- u[-test,]

# fit linear model
lmout <- lm (rating ~ age + gender + occ + ZIP , data=utrain)

preds <- predict(lmout,utest)
mean(abs(preds-utest$rating))


library(`rectools-master`)
rat <- ratings[,-4]
trainReco(rat,rnk=10,nmf=TRUE)




library(recosystem)
library(regtools)

lmFinalModel <- function(u.big.tst){
  u.big <- read.delim("u.big.tst",header = TRUE, sep="\t",dec = ".")
  u.big$ZIP <-as.character(u.big$ZIP)
  u.big$ZIP<-substr(u.big$ZIP,start=1, stop =2)
  u.big$ZIP<-as.factor(u.big$ZIP)
  
  colnames(u.big)<-c('movienum','usernum','rating','transID','age','gender','occ','ZIP',
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
  error
  #mean(abs(preds1-utest$rating))  #0.697
}

nmfFinalModel <- function(u.big.tst){
  
}
