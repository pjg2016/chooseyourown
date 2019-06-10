library(tidyverse)
library(caret)
library(gridExtra)
library(ggplot2)
library(randomForest)
library(rpart)
library(dplyr)
library(corrplot)

#Data Collection
dl <- tempfile()
download.file("https://github.com/pjg2016/chooseyourown/files/3267565/CYO.zip", dl)
dataset <- read.csv(unzip(dl, "3C_weka.csv"))
dataset_two <- read.csv(unzip(dl, "2C_weka.csv"))
dataset$class<-as.factor(dataset$class)
rm(dl)

#Data Exploration
ggplot(dataset,aes(x=class,fill=class))+geom_bar(stat = 'count')+labs(x = 'Count of Hernia, Spondylothisthesis or Normal')
summary(dataset)
corr1<-cor(dataset[1:6], dataset[1:6])
corr1
corrplot(corr1, type = "upper", order = "hclust")

#Random Forest to identify variables with most influence
set.seed(1)
rf_spine<-randomForest(class~.,data = dataset)
RF_importance <- importance(rf_spine)
RF_dataframe <- data.frame((RF_importance), MSE = RF_importance[,1])

RF_dataframe

#Tree visualization ~ supporting Random Forest dataframe
fit<- rpart(dataset$class~., data = dataset)
plot(fit, margin = 0.1)
text(fit, cex = 0.75)


#Exploratory analysis, dataset two
dataset_two$class<-as.factor(dataset_two$class)
ggplot(dataset_two,aes(x=class,fill=class))+geom_bar(stat = 'count')+labs(x = 'Normal vs. Abnormal?')

#Random Forest to identify variables with most influence, dataset two
set.seed(1)
rf_spine_two<-randomForest(class~.,data = dataset_two)
RF_importance_two <- importance(rf_spine_two)
RF_dataframe_two <- data.frame((RF_importance_two), MSE = RF_importance_two[,1])

RF_dataframe_two

#Tree visualization ~ supporting Random Forest dataframe
fit_two<- rpart(dataset_two$class~., data = dataset_two)
plot(fit_two, margin = 0.1)
text(fit_two, cex = 0.75)

#create training and test sets
stats_index <- createDataPartition(dataset_two$class, p=0.80, list=FALSE)
 #select 20% of the data for testing the models
test <- dataset_two[-stats_index,]
 #use the remaining 80% of data for training the models
datasetTR <- dataset_two[stats_index,]
#summary of training dataset
summary(datasetTR)

#knn 
set.seed(1)
knnfit <- train(class ~ ., method = "knn",
             tuneGrid = data.frame(k = seq(1, 15, 2)),
             data = datasetTR)
ggplot(knnfit)
knnfit
confusionMatrix(predict(knnfit, test), test$class)$overall["Accuracy"]

#classification tree
train_rpart <-train(class ~ ., method = "rpart",
tuneGrid = data.frame(cp = seq(0,0.1, len = 25)),
data = datasetTR)
plot(train_rpart)
train_rpart
confusionMatrix(predict(train_rpart, test), test$class)$overall["Accuracy"]







