# random knn
library('rknn')
library('FNN')
load("/Users/haoyan/Dropbox/STAT542_Shared/testFeature2_modified.RData")
load("/Users/haoyan/Dropbox/STAT542_Shared/trainFeature2_modified.RData")



n_train <- dim(trainFeature)[1]
p_train <- dim(trainFeature)[2]-1

sample_index <- sample(1:n_train,round(0.8*n_train))

train_rknn <- trainFeature[sample_index,]
test_rknn <- trainFeature[-sample_index,]

train.Y <- trainFeature[,1]
train.X <- trainFeature[,2:9]

test.X<- testFeature[,2:9]
train.Y <- factor(train.Y)
model<-knn.cv(train.X,train.Y, k = 1, prob = TRUE)
knn(train.X, test.X, train.Y, k = 1, prob=TRUE)
attributes(.Last.value)

