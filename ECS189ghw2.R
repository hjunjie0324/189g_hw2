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

head(u.big2)


u.big2$ZIP <-as.character(u.big2$ZIP)
u.big2$ZIP<-substr(u.big2$ZIP,start=1, stop =2)
u.big2$ZIP<-as.factor(u.big2$ZIP)

colnames(u.big2)<-c('movienum','usernum','rating','transID','age','gender','occ','ZIP',
                   'unknown','Action','Adventure','Animation','Children','Comedy',
                   'Crime','Documentary','Drama','Fantasy','Noir','Horror','Musical',
                   'Mystery','Romance','SciFi','Thriller','War','Western')

u<-u.big2[,c(3,5:27)]

test<-sample(1:nrow(u),5000)
utest<-u[test,]
utrain<-u[-test,]

lmout<-lm(rating~.,data=utrain)
preds<-predict(lmout,utest)
preds
errorLM<-MAPE(preds,utest$rating)
errorLM#0.8891619
