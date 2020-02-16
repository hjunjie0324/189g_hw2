library(recosystem)
library(regtools)
library(rectools)
ratings <- read.table("/Users/xianliu/ECS189/hw1/u.data")
ratings<-ratings[,-(4)]
item <- read.csv("/Users/xianliu/ECS189/ml-100k/u.item",sep="|",header = F)
item <-item[,-(2:5)]
genre <- read.csv("/Users/xianliu/ECS189/ml-100k/u.genre",sep="|")
genre<-data.frame(genre)
colnames(item)<-c('movieid',"unknown",as.character(genre$unknown))
colnames(ratings) <- c('userid','movieid','rating')
user <- read.table("/Users/xianliu/ECS189/ml-100k/u.user",sep="|")
colnames(user)<-c("userid","age","gender", "occ","zip")
info<-merge(user,ratings)
info_all <-merge(info, item)

r = Reco()
test <-sample(1:nrow(info_all),5000)
train<-info_all[-test,]
test <- info_all[test,]
train_set = data_memory(user_index = train$userid,
                        item_index = train$movieid,
                        rating =train$rating, index1 = TRUE)
opts=r$tune(train_set, opts = list(dim = 100, niter = 10, nmf = TRUE))
r$train(train_set, opts = c(opts$min, nthread = 1, niter = 10, nmf = TRUE))

test_set = data_memory(test$userid, test$movieid, index1 = TRUE)
pred$rating = r$predict(test_set, out_memory())
error <- mean(abs(pred$rating-test$rating))

trainset<-data.frame(train$userid,train$movieid,train$rating)
rec <- trainReco(trainset, rnk=100, nmf=TRUE)
testset<-data.frame(test$userid, test$movieid)
pre<-predict.RecoS3(rec,testSet =testset )
error2 <- mean(abs(pre-test$rating),na.rm=TRUE)

