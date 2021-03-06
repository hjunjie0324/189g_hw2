lmFinalModel<-function(u.big2){
  colnames(u.big2)<-c('movienum','usernum','rating','transID','age','gender','occ','ZIP','unknown','Action','Adventure','Animation','Children','Comedy','Crime','Documentary','Drama','Fantasy','Noir','Horror','Musical','Mystery','Romance','SciFi','Thriller','War','Western')
  
  #add new term that connect user and item
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
  
  #add two new features: user_mean and movie_mean
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
  
  #age*gender term
  gender_dummy<-factorToDummies(u.big2$gender,"gender")
  u.big2[,"gender_dummy"]<-gender_dummy
  u.big2[,"age_gender"]<-u.big2[,"gender_dummy"]*u.big2[,"age"]
  
  #zip term
  zip <- as.character(u.big2$ZIP)
  u.big2$ZIP_first2 <- substr(zip,1,2)  #take first 2 digits
  lmout1<-lm(rating~gender+occ+unknown+Action+Adventure+Animation+Children+Comedy+Crime+Documentary+Drama+Fantasy+Noir+Horror+Musical+Mystery+Romance+SciFi+Thriller+War+Western+new+user_mean+movie_mean+age_gender+ZIP_first2,data=utrain)
  lmout1
}