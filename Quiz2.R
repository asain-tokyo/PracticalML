# Quiz 2

# Q1.

library(AppliedPredictiveModeling)
data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)
testIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[-testIndex,]
testing = adData[testIndex,]

# Q2.

library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

library(Hmisc)

index <- seq_along(1:nrow(training))

# plot(training$CompressiveStrength)
g <- ggplot(data = training, aes(x = index, y = CompressiveStrength))
g <- g + geom_point()
g

# Outcome : CompressiveStrength
# Variables : Cement, BlastFurnaceSlag, FlyAsh, Water, Superplasticizer, CoarseAggregate, FineAggregate, Age

# FlyAsh
Cut <- cut2(training$FlyAsh, g=5)
g <- ggplot(data = training, aes(x = index, y = CompressiveStrength, color=Cut))
g <- g + geom_point()
g

# Age
Cut <- cut2(training$Age, g=5)
g <- ggplot(data = training, aes(x = index, y = CompressiveStrength, color=Cut))
g <- g + geom_point()
g


# Cement
Cut <- cut2(training$Cement, g=5)
g <- ggplot(data = training, aes(x = index, y = CompressiveStrength, color=Cut))
g <- g + geom_point()
g


# Q3

library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

hist(mixtures$Superplasticizer)

# There are a large number of values that are the same and even if you took the log(SuperPlasticizer + 1) 
# they would still all be identical so the distribution would not be symmetric.


# Q4

library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

cols <- colnames(training)
col_num <- grep('^IL', cols)
training_sub <- training[, c(col_num)]

preProc <- preProcess(training_sub, method="pca", thresh=0.9)
preProc$numComp

# [1] 9

rpca <- prcomp(x=training_sub,scale=T)
summary(rpca)$importance

#                       PC1      PC2      PC3       PC4       PC5       PC6       PC7       PC8       PC9      PC10      PC11
# Standard deviation     2.059075 1.191346 1.055955 0.9879822 0.8989122 0.8623982 0.8189374 0.7554583 0.7279246 0.6392616 0.5513034
# Proportion of Variance 0.353320 0.118280 0.092920 0.0813400 0.0673400 0.0619800 0.0558900 0.0475600 0.0441600 0.0340500 0.0253300
# Cumulative Proportion  0.353320 0.471590 0.564510 0.6458500 0.7131900 0.7751700 0.8310600 0.8786200 0.9227700 0.9568300 0.9821500


# Q5

RNGversion("3.0.0")

library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

cols <- colnames(training)
col_num <- grep('^IL', cols)

training_sub <- training[, c(col_num)]
diagnosis <- training[,c(1)]
training_diag <- cbind(diagnosis, training_sub)

testing_sub <- testing[, c(col_num)]
diagnosis <- testing[,c(1)]
testing_diag <- cbind(diagnosis, testing_sub)

library(e1071)
model_n <- train(diagnosis ~ ., data=training_diag, method='glm')
res_n <- confusionMatrix(testing_diag$diagnosis, predict(model_n, testing_sub))

preProc_pca <-  preProcess(training_sub, method="pca", thresh=0.8)
trainPC <- predict(preProc_pca, training_sub)
# model_pca <- train(training_diag$diagnosis ~ ., data=trainPC,  method="glm")
model_pca <- train(x = trainPC, y = training_diag$diagnosis, method="glm")
testPC <- predict(preProc_pca, testing_sub)
res_pca <- confusionMatrix(testing_diag$diagnosis, predict(model_pca, newdata=testPC))
res_pca
