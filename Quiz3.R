# Week 3 Quiz 3

# Q1.

library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)

# $ Case                         : Factor w/ 2 levels "Test","Train": 1 2 2 2 1 1 1 1 1 1 ...

train_ds <- subset(segmentationOriginal, Case == 'Train')
test_ds <- subset(segmentationOriginal, Case == 'Test')

set.seed(125)
fit <- train(Class ~ ., data=train_ds, method='rpart')

library(rattle)
fancyRpartPlot(fit$finalModel)

# a. TotalIntench2 = 23,000; FiberWidthCh1 = 10; PerimStatusCh1=2 -> PS
# b. TotalIntench2 = 50,000; FiberWidthCh1 = 10;VarIntenCh4 = 100 -> WS
# c. TotalIntench2 = 57,000; FiberWidthCh1 = 8;VarIntenCh4 = 100 -> PS
# d. FiberWidthCh1 = 8;VarIntenCh4 = 100; PerimStatusCh1=2 -> Not possible to predict

# Q2.

# If K is small in a K-fold cross validation is the bias in the estimate of out-of-sample (test set) accuracy smaller or bigger?
# If K is small is the variance in the estimate of out-of-sample (test set) accuracy smaller or bigger. 
# Is K large or small in leave one out cross validation?

# bigger,
# smaller,
# leave one out cross validation = k-fond (sample size == K)

# Q3.

library(pgmm)
data(olive)
olive = olive[,-1]


fit <- train(Area ~ ., data=olive, method='rpart')
fancyRpartPlot(fit$finalModel)
newdata = as.data.frame(t(colMeans(olive)))
predict(fit, newdata=newdata)

# 1 
# 2.783282 

# 2.783. It is strange because Area should be a qualitative variable - 
# but tree is reporting the average value of Area as a numeric variable in the leaf predicted for newdata

# Q4.

library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

# "sbp", "tobacco", "ldl", "adiposity", "famhist", "typea", "obesity", "alcohol", "age", "chd"

set.seed(13234)

# trainSA$chd <- as.factor(trainSA$chd) 
# testSA$chd <- as.factor(testSA$chd) 
fit <- train(chd ~ age+alcohol+obesity+tobacco+typea+ldl, data=trainSA, method="glm", family="binomial")

train_prediction <- predict(fit, newdata=trainSA)
train_values <- trainSA$chd
test_prediction <- predict(fit, newdata=testSA)
test_values <- testSA$chd

missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

print(missClass(train_values, train_prediction))
print(missClass(test_values, test_prediction))

# [1] 0.2727273
# [1] 0.3116883

# Q5.

library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)

library(randomForest)
set.seed(33833)

res <- randomForest(y ~ ., data=vowel.train)
varImpPlot(res)

# X1, X2, X5, X6, X8, X4, X3, X9, X7, X10
# X2, X1, X5, X6, X8, X4, X9, X3, X10, X7





