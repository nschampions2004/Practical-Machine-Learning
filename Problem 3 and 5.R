set.seed(3523)
library(AppliedPredictiveModeling); library(e1071); library(caret)
data("concrete")
#test/train
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
##double hashes are question 3
##set.seed(233)
#model building
##model_lasso <- train(CompressiveStrength ~., method = "lasso", data = training)
#prediction
##pred_lasso <- predict(model_lasso, testing)
##plot.enet(model_lasso$finalModel, xvar = "penalty", use.color = T)
set.seed(325)
#model building
model_e <- svm(CompressiveStrength ~., data = training)
pred_e <- predict(model_e, newdata = testing)
resid <- pred_e - testing$CompressiveStrength
print(sqrt(mean(resid^2)))
