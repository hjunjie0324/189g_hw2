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