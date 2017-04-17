library(caret)
library(gbm); library(AppliedPredictiveModeling); library(MASS)
set.seed(3433)
data(AlzheimerDisease)
adData <- data.frame(diagnosis, predictors)
#train/test
inTrain <- createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training <- adData[inTrain, ]
testing <- adData[-inTrain, ]
set.seed(62433)
#model building
model_rf <- train(diagnosis ~., method = "rf", data = training)
model_gbm <- train(diagnosis ~., method = "gbm", data = training, verbose = FALSE)
model_lda <- train(diagnosis ~., method = "lda", data = training)
#predictions
pred_rf <- predict(model_rf, testing)
pred_gbm <- predict(model_gbm, testing)
pred_lda <- predict(model_lda, testing)
#checking accuracies
print(confusionMatrix(pred_rf, testing$diagnosis))
print(confusionMatrix(pred_gbm, testing$diagnosis))
print(confusionMatrix(pred_lda, testing$diagnosis))
#stacking the models
comb_fit <- data.frame(pred_rf, pred_gbm, pred_lda, diagnosis = testing$diagnosis)
combModFit <- train(diagnosis ~., method = "rf", comb_fit)
combPred <- predict(combModFit, comb_fit)
#stacked model accuracy
print(confusionMatrix(combPred, testing$diagnosis))