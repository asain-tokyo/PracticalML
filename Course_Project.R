# Course Project

train_data = read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv")
test_data = read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")

dim(train_data)
# [1] 19622   160
train_cols <- colnames(train_data)

dim(test_data)
# [1]  20 160
test_cols <- colnames(test_data)

# One difference in a column 160.  "classe" for train data, and "problem_id" for test data

summary(train_data$classe)
#   A    B    C    D    E 
# 5580 3797 3422 3216 3607 

# exactly according to the specification (Class A)
# throwing the elbows to the front (Class B)
# lifting the dumbbell only halfway (Class C)
# lowering the dumbbell only halfway (Class D)
# throwing the hips to the front (Class E). 

# Unilateral Dumbbell Biceps Curl 

BC_table <- as.data.frame(matrix(ncol=3, nrow=5))
colnames(BC_table) <- c("class","fashion","decision")
BC_table$class <- c('A','B','C','D','E')
BC_table$fashion[1] <- "Exactly according to the specification"
BC_table$fashion[2] <- "Throwing the elbows to the front"
BC_table$fashion[3] <- "Lifting the dumbbell only halfway"
BC_table$fashion[4] <- "Lowering the dumbbell only halfway"
BC_table$fashion[5] <- "Throwing the hips to the front"
BC_table$decision <- "mistake"
BC_table$decision[1] <- "success"
BC_table

picks <- list()
# remove "blank" and "NA" from train/test data
for (i in 1:ncol(train_data)) {
    col_test <- train_data[,c(i)]
    if (sum(is.na(col_test)) == 0) {
        if (sum(col_test == "") == 0) {
            # no blank/NA - to be picked up
            picks <- c(picks, i)
        }
    }
}
train_norm <- train_data[, unlist(picks)]
test_norm <- test_data[, unlist(picks)]

# We do not need "X", "user_name", "raw_timestamp_part_1","raw_timestamp_part_2", "cvtd_timestamp", "new_window", and "num_window" 

train_norm <- train_norm[,8:60]
test_norm <- test_norm[,8:60]

#### Preparing data
# 80% for train and validation, 20% for test

set.seed(154)
train_list <- createDataPartition(train_norm$classe, p=0.8, list=FALSE)
train_cv <- train_norm[train_list,]
test_cv <- train_norm[-train_list,]

# K-fold cross validation (to minimize overfitting)
train_control <- trainControl(method='repeatedcv', number=10, repeats=10)

# Models (normalize data by preProcess)
# model_glm <- train(classe ~ ., data=train_cv, method='glm', preProcess=c("center","scale"), trControl=train_control)
model_gbm <- train(classe ~ ., data=train_cv, method='gbm', preProcess=c("center","scale"), trControl=train_control)
model_lda <- train(classe ~ ., data=train_cv, method='lda', preProcess=c("center","scale"), trControl=train_control)
model_qda <- train(classe ~ ., data=train_cv, method='qda', preProcess=c("center","scale"), trControl=train_control)
model_rf <- train(classe ~ ., data=train_cv, method='rf', preProcess=c("center","scale"), trControl=train_control)
# model_pcr <- train(classe ~ ., data=train_cv, method='pcr', preProcess=c("center","scale"), trControl=train_control)

gbm_accuracy <- confusionMatrix(train_cv$classe, predict(model_gbm, train_cv))$overall[1]
lda_accuracy <- confusionMatrix(train_cv$classe, predict(model_lda, train_cv))$overall[1]
qda_accuracy <- confusionMatrix(train_cv$classe, predict(model_qda, train_cv))$overall[1]
rf_accuracy <- confusionMatrix(train_cv$classe, predict(model_rf, train_cv))$overall[1]

#> gbm_accuracy
# Accuracy 
# 0.9737563 
# > lda_accuracy
# Accuracy 
# 0.7053952 
# > qda_accuracy
# Accuracy 
# 0.8968087 
# > rf_accuracy
# Accuracy 
# 1 

confusionMatrix(test_cv$classe, predict(model_gbm, test_cv))$overall[1]
confusionMatrix(test_cv$classe, predict(model_rf, test_cv))$overall[1]

predict(model_gbm, test_norm)
# B A B A A E D B A A B C B A E E A B B B

predict(model_rf, test_norm)
# B A B A A E D B A A B C B A E E A B B B

#### PCA ###

library(FactoMineR)
library("factoextra")

train.pca <- PCA(train_norm[,1:52], graph=FALSE)
# fviz_pca_var(train.pca, col.var = "Black")

fviz_pca_var(train.pca, labelsize = 3, repel = TRUE) +
    theme(text = element_text(size = 7.5),
          axis.title = element_text(size = 7.5),
          axis.text = element_text(size = 7.5))

fviz_eig(train.pca, labelsize = 3, repel = TRUE) +
    theme(text = element_text(size = 7.5),
          axis.title = element_text(size = 7.5),
          axis.text = element_text(size = 7.5))

train.pca <- PCA(train_norm[,14:52], graph=FALSE)




######
library(caret)
library(ggplot2)

g <- ggplot(data=train_norm, aes(x=total_accel_dumbbell, y=total_accel_arm, color=classe))
g <- g + geom_point()
g

g <- ggplot(data=train_norm, aes(x=total_accel_dumbbell, y=total_accel_belt, color=classe))
g <- g + geom_point()
g

g <- ggplot(data=train_norm, aes(x=total_accel_dumbbell, y=total_accel_forearm, color=classe))
g <- g + geom_point()
g
    
