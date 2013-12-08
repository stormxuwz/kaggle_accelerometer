# random knn
library('rknn')
load("/Users/haoyan/Dropbox/STAT542_Shared/testFeature2_modified.RData")
load("/Users/haoyan/Dropbox/STAT542_Shared/trainFeature2_modified.RData")



n_train <- dim(trainFeature)[1]
p_train <- dim(trainFeature)[2]-1

sample_index <- sample(1:n_train,round(0.8*n_train))

train_rknn <- trainFeature[sample_index,]
test_rknn <- trainFeature[-sample_index,]


