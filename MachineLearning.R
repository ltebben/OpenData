

setwd("C:/Users/Laura/OpenData")
cancer<-read.csv("cancer.csv", header = TRUE, sep = ',')

malign_subset <- subset(cancer, diagnosis == "M")
apply(malign_subset, 2, min)

require(caret)
nearZeroVar(cancer)
set.seed(1)

inTrain <- createDataPartition(cancer$diagnosis, p=0.7, list=FALSE)
training <- cancer[inTrain,]
testing <- cancer[-inTrain,]
dim(training); dim(testing)

table(testing$diagnosis)

set.seed(1)
require(rpart)
require(rpart.plot)
cart_model<-train(diagnosis ~ ., data=training, method="rpart")

cart_model

pred1<-predict(cart_model, testing)
confusionMatrix(pred1, testing$diagnosis, positive="M")

set.seed(1)
require(randomForest)
rf_model<-train<-train(diagnosis ~ ., data=training, method="rf")

rf_model
require(rattle)
fancyRpartPlot(cart_model$finalModel, sub="")


