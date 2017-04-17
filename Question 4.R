library(lubridate); library(forecast)
url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv"
destfile <- "question4data.csv"
download.file(url, destfile, mode = "wb")
quest4 <- read.csv("question4data.csv")
#train/test
training <- quest4[year(quest4$date) < 2012, ]
testing <- quest4[(year(quest4$date)) > 2011, ]
tstrain <- ts(training$visitsTumblr)
tstest <- ts(testing$visitsTumblr)

#model building
model_bats <- bats(tstrain)
f <- forecast(model_bats, level = 95, h = dim(testing)[1])
print(sum(f$lower < testing$visitsTumblr & testing$visitsTumblr < f$upper) / dim(testing)[1])

