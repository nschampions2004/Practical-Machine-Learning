library(caret); library(ggplot2); library(rattle); library(parallel); library(doParallel)

#reading in the data
training_url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
test_url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
if(!file.exists("test_data.csv")){
  download.file(test_url, destfile = "test_data.csv")}
if(!file.exists("training_data.csv")){
  download.file(training_url, destfile = "training_data.csv")}
training <- read.csv("training_data.csv")
testing <- read.csv("test_data.csv")
#exploratory data analysis
#there are a ton of columns with na's, time to shrink this data set
training <- training[, colSums(is.na(training)) == 0]
testing <- testing[, colSums(is.na(testing)) == 0]
#cleaning the training set
zerovar <- nearZeroVar(training, saveMetrics = FALSE)
training <- training[ , -zerovar]
training <- training[, -c(1,2,3,4,5,6)]
#split the training data into a validation set and training set at the 0.6 level
inTrain <- createDataPartition(training$classe, p = 0.6, list = FALSE)
training <- training[inTrain, ]
checker <- training[-inTrain, ]



#making clusters for parallel processing
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)
#configuring trainControl object
fitControl <- trainControl(method = "cv", number = 5, allowParallel = TRUE)
#model build
model_rf <- train(classe ~ ., data = training, trControl = fitControl)

stopCluster(cluster)
predict_rf_checker <- predict(model_rf, checker)
print(confusionMatrix(predict_rf_checker, checker$classe))
print(predict(model_rf, testing))



#predict_rf <- predict(model_rf, testing)
