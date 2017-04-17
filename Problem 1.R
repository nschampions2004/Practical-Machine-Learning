library(ElemStatLearn)
library(caret); library(ggplot2)
data("vowel.train")
data("vowel.test")
 
vowel.test$y <- factor(vowel.test$y)
vowel.train$y <- factor(vowel.train$y)

set.seed(33833)

model_rf <- train(y ~ ., method = "rf", data = vowel.train)
model_gbm <- train(y  ~., method = "gbm", data = vowel.train)

pred_rf <- predict(model_rf, vowel.test)
pred_gbm <- predict(model_gbm, vowel.test)

qplot(pred_rf, pred_gbm)

comb_fit <- data.frame(pred_rf, pred_gbm, y <- vowel.test$y)
combModFit <- train(y ~., method = "gam", data = comb_fit)
combPred <- predict(combModFit, vowel.test)
