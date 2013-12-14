## Conor O'Sullivan
## Cuttlefish Challenge


cut<-read.csv("training_data.csv")
testcut<-read.csv("test_data.csv")

library('randomForest')

prepareDataScale<-function(dataset) {
  dataset$f_gender<-NULL
  dataset$m_gender<-NULL
  orig_names<-colnames(dataset)
  for (i in 1:20) {
    dataset[,i]<-scale(dataset[,i])
  }
  colnames(dataset)<-orig_names
  for (i in 1:10) {
    dataset<-cbind(dataset, dataset[i]-dataset[i+10]) # gender-wise differences
    colnames(dataset)[ncol(dataset)]<-paste("diffs",i,sep="")
  }
  for (i in 2:20) {
    dataset<-cbind(dataset, dataset[i]-dataset[i-1]) # person-wise differences
    colnames(dataset)[ncol(dataset)]<-paste("extra",i,sep="")
  }
  dataset
}

cut$res<-NA
cut$res[cut$members_became_friends==FALSE]<-0
cut$res[cut$members_became_friends==TRUE]<-1
cut$res<-as.factor(cut$res) # factor column added
cut$members_became_friends<-NULL # remove unneeded column

trainset<-cbind(cut$res, prepareData(cut[,-23]))
colnames(trainset)[1]<-"labels"
finalRF<-randomForest(labels~.,trainset, mtry=6, ntree=1000) # create the random forest
predictions<-predict(finalRF, prepareData(testcut)) # predict the test labels

testcut$members_became_friends<-TRUE
testcut$members_became_friends[predictions==0]<-FALSE # change elements that were marked as 0 by random forest

write.csv(file="test_data_labeled.csv",testcut)
