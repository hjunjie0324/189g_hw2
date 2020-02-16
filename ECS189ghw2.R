setwd('C:/omsi-master/ml-100k')
ratings <- read.table('u.data')
names(ratings) <- c('userId','movieId','rating','timestamp')
#head(ratings)

m<-read.csv('movies.csv',sep=',',header=T)
head(m)

demog <- read.table('u.user',sep='|')
names(demog)<-c('usernum','age','gender','occ','ZIP')
#head(demog)

u.big<-merge(ratings,demog,by.x=1,by.y=1)
head(u.big)

u.new<-merge(u.big,m,by.x=1,by.y=1)
head(u.new)

#class(u.big$age)

#lmout1<-lm(rating ~ age+gender,data=u.big)
#coef(lmout1)

#lmout2<-lm(rating ~ movieId+userId,data=u.big)
#coef(lmout2)

#lmoutNew<-lm(rating~age+occ,data=u.big)
#coef(lmoutNew)

#lmout<-lm(age~occ,data=u.big)
#coef(lmout)


#p<-tapply(u.big$rating,u.big$occ,mean)
#barplot(p)

#u.big$age <- as.factor(u.big$age)
u.big$gender <- as.factor(u.big$gender)
u.big$movieId <- as.factor(u.big$movieId)
u.big$userId <- as.factor(u.big$userId)


#lmout3<-lm(rating ~ age+gender,data=u.big)
#coef(lmout3)

#lmout3<-lm(rating ~ age+gender,data=u.big)
#coef(lmout3)

u.big$ZIP <-as.character(u.big$ZIP)
substr(u.big$ZIP,start=1, stop =2)
#u.big$ZIP<-(as.numeric(u.big$ZIP))%/%1000
u.big$ZIP<-as.factor(u.big$ZIP)

lmout<-lm(rating~gender+occ+ZIP+genres,data = u.new)
head(u.new)

#getPE()
pe1<-u.new[,c(3,5,6,7,8,10)]
head(pe1)

#set.seed(9999)
testidxs<-sample(1:nrow(pe1),5000)
testset<-pe1[testidxs,]
trainset<-pe1[-testidxs,]
lmout<-lm(rating~.,data=pe1)
predvals<-predict(lmout,testset[,-1])
mean(abs(predvals-testset[,1]))

#u.big$age <- as.factor(u.big$age)
#u.big$age <- as.factor(u.big$age)
