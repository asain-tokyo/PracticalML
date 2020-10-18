# Quiz4

# Q1. 
library(caret)
library(gbm)

library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)
set.seed(33833)

fit1 <- train(y ~ ., data=vowel.train, method='rf')
fit2 <- train(y ~ ., data=vowel.train, method='gbm')

res1 = predict(fit1, newdata=vowel.test)
res2 = predict(fit2, newdata=vowel.test)

matrix1 = confusionMatrix(res1, vowel.test$y)
matrix2 = confusionMatrix(res2, vowel.test$y)

# matrix1 :
# Accuracy : 0.5909          
# 95% CI : (0.5445, 0.6361)
# No Information Rate : 0.0909          
# P-Value [Acc > NIR] : < 2.2e-16 

# matrix2 :
# Accuracy : 0.513           
# 95% CI : (0.4664, 0.5594)
# No Information Rate : 0.0909          
# P-Value [Acc > NIR] : < 2.2e-16 

agreed_sample <- (res1 == res2)
matrix_a <- confusionMatrix(res1[agreed_sample], vowel.test$y[agreed_sample])

# Accuracy : 0.6231          
# 95% CI : (0.5675, 0.6763)
# No Information Rate : 0.1153          
# P-Value [Acc > NIR] : < 2.2e-16

# Q2.

library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)

data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

fit1 <- train(diagnosis ~ ., data=training, method='rf')
fit2 <- train(diagnosis ~ ., data=training, method='gbm')
fit3 <- train(diagnosis ~ ., data=training, method='lda')

res1 <- predict(fit1, newdata=testing)
res2 <- predict(fit2, newdata=testing)
res3 <- predict(fit3, newdata=testing)

matrix1 = confusionMatrix(res1, testing$diagnosis)
matrix2 = confusionMatrix(res2, testing$diagnosis)
matrix3 = confusionMatrix(res3, testing$diagnosis)

matrix1$overall[1]
matrix2$overall[1]
matrix3$overall[1]

stacked <- data.frame(res1, res2, res3, diagnosis = testing$diagnosis)
combinedFit <- train(diagnosis ~ ., method="rf", data=stacked)
combinedRes <- predict(combinedFit, newdata=stacked)
matrix_c <- confusionMatrix(combinedRes, testing$diagnosis)

matrix_c$overall[1]

# 1. rf
# Accuracy 
# 0.7804878 

# 2. gbm
# Accuracy 
# 0.8292683 

# 3. lda
# Accuracy 
# 0.7682927 

# 4. mix
# Accuracy 
# 0.8292683

# Stacked Accuracy: 0.80 is better than random forests and lda and the same as boosting.

# Q3.

set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(233)
library(elasticnet)

lasso<-train(CompressiveStrength ~ ., data= training, method = 'lasso')
plot.enet(lasso$finalModel, xvar='penalty', use.color=TRUE)

# Ans. Cement

# Q4. 

library(lubridate) # For year() function below

dat = read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)

library(forecast)

fit <- bats(tstrain)
fc <- forecast(fit, h=nrow(testing), level=95 )
fc <- as.data.frame(fc)
sum((testing$visitsTumblr > fc$`Lo 95`) & (testing$visitsTumblr < fc$`Hi 95`))/nrow(testing)
# [1] 0.9617021

# Q5.

set.seed(3523)

library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(325)
library(e1071)
fit <- svm(CompressiveStrength ~ ., data=training)
res <- predict(fit, testing)
accuracy(res, testing$CompressiveStrength)

#               ME     RMSE      MAE       MPE     MAPE
#Test set 0.1682863 6.715009 5.120835 -7.102348 19.27739


