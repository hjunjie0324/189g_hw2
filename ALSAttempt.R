library(recosystem)
library(regtools)
library(rectools)
setwd('C:/omsi-master/ml-100k')

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

colnames(u.big2)<-c('movienum','usernum','rating','transID','age',
                    'gender','occ','ZIP','unknown','Action','Adventure',
                    'Animation','Children','Comedy','Crime','Documentary',
                    'Drama','Fantasy','Noir','Horror','Musical','Mystery','Romance',
                    'SciFi','Thriller','War','Western')
u.big2

u.big2$ZIP <-as.character(u.big2$ZIP)
u.big2$ZIP<-substr(u.big2$ZIP,start=1, stop =2)
u.big2$ZIP<-as.factor(u.big2$ZIP)


r = Reco()

test <-sample(1:nrow(u.big2),5000)

train<-u.big2[-test,]
testS <- u.big2[test,]

train_set = data.frame(train$usernum,train$movienum,train$rating)

test_set = data.frame(testS$age,testS$gender,testS$occ,testS$unknown,testS$Action,
                      testS$Adventure,testS$Animation,testS$Children,testS$Comedy,testS$Crime,
                      testS$Documentary,testS$Drama,testS$Fantasy,testS$Noir,testS$Horror,testS$Musical,
                      testS$Mystery,testS$Romance,testS$SciFi,testS$Thriller,testS$War,testS$Western)

reco<-trainReco(train_set,rnk=10,nmf=TRUE)
pre<-predict.RecoS3(reco,test_set)
err1<-MAPE(pred,testS$rating)
err1




##################
#  ALS with all genres with rank 100
#
#################
test_set = data.frame(testS$age,testS$gender,testS$occ,testS$ZIP,testS$unknown,testS$Action,
                      testS$Adventure,testS$Animation,testS$Children,testS$Comedy,testS$Crime,
                      testS$Documentary,testS$Drama,testS$Fantasy,testS$Noir,testS$Horror,testS$Musical,
                      testS$Mystery,testS$Romance,testS$SciFi,testS$Thriller,testS$War,testS$Western)

reco<-trainReco(train_set,rnk=50,nmf=TRUE)
pre<-predict.RecoS3(reco,test_set)
err1<-MAPE(pred,testS$rating)
err1#1.049593


##################
#  ALS with all genres with rank 50
#
#################
test_set = data.frame(testS$age,testS$gender,testS$occ,testS$ZIP,testS$unknown,testS$Action,
                      testS$Adventure,testS$Animation,testS$Children,testS$Comedy,testS$Crime,
                      testS$Documentary,testS$Drama,testS$Fantasy,testS$Noir,testS$Horror,testS$Musical,
                      testS$Mystery,testS$Romance,testS$SciFi,testS$Thriller,testS$War,testS$Western)

reco1<-trainReco(train_set,rnk=50,nmf=TRUE)
pre<-predict.RecoS3(reco1,test_set)
err2<-MAPE(pred,testS$rating)
err2#1.076098


##################
#  ALS withOUT genres with rank 100
#
#################
test_set = data.frame(testS$age,testS$gender,testS$occ,testS$ZIP)

reco<-trainReco(train_set,rnk=100,nmf=TRUE)
pre<-predict.RecoS3(reco,test_set)
err3<-MAPE(pred,testS$rating)
err3#1.050084

##################
#  ALS withOUT genres with rank 50
#
#################
test_set = data.frame(testS$age,testS$gender,testS$occ,testS$ZIP)

reco<-trainReco(train_set,rnk=50,nmf=TRUE)
pre<-predict.RecoS3(reco,test_set)
err4<-MAPE(pred,testS$rating)
err4#1.044001


##################
#  ALS withOUT genres with rank 500
#
#################
test_set = data.frame(testS$age,testS$gender,testS$occ,testS$ZIP)

reco<-trainReco(train_set,rnk=500,nmf=TRUE)
pre<-predict.RecoS3(reco,test_set)
err3<-MAPE(pred,testS$rating)
err3#1.059114
