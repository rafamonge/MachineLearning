##question  5

library(caret)
library(AppliedPredictiveModeling)
library(dplyr)
set.seed(3433)
data(AlzheimerDisease)
ILPredictors <- predictors %>% select(starts_with("IL"))
adData = data.frame(diagnosis,ILPredictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]



noPCA <- train(training$diagnosis ~ .
               ,method="glm"
               ,data=training)
predictionNoPca <- predict(noPCA, testing)
confusionMatrix(testing$diagnosis, predictionNoPca) ##0.6463  


withPCA <- train(training$diagnosis ~ .
                  ,method="glm"
                 ,preProcess="pca"
                 ,trControl=trainControl(preProcOptions=list(thresh=0.8)),   data=training)
predictionWithPca <- predict(withPCA, testing)
confusionMatrix(predictionWithPca, testing$diagnosis) ##0.7195



