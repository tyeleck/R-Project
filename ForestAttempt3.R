library(randomForest)
library(datasets)
library(caret)
library(tidyverse)
library(tm)
library(rpart)
library(rpart.plot)
library(rattle)
library(partykit)
library(reprtree)
library(party)
library(randomForestExplainer)
install.packages("reprtree")
train <- read.csv("D:/GitHub/R-Project/Datasets/Tweets_with_Sarcasm_and_Irony/train.csv")
# https://cran.rstudio.com/web/packages/randomForestExplainer/vignettes/randomForestExplainer.html
#class and tweets
#-----------------------------------------------------------------------------------------------------------------------------
#Preprocessing train

corpus <- Corpus(VectorSource(train$tweets))

corpus <- tm_map(corpus,PlainTextDocument)
corpus <- tm_map(corpus,tolower)
corpus <- tm_map(corpus,removePunctuation)
corpus <- tm_map(corpus,removeWords,stopwords("english"))
corpus <- tm_map(corpus,stemDocument)

freq <- DocumentTermMatrix(corpus)
sparse <- removeSparseTerms(freq,.995)
tSparse <- as.data.frame(as.matrix(sparse))
colnames(tSparse) = make.names(colnames(tSparse))
tSparse$class = train$class

#Split the data
trainSet <- create_train_test(tSparse,.8,train = T)
testSet <- create_train_test(tSparse,.8,train = F)

#RandomForest
trainSet$class = factor(trainSet$class)
testSet$class = factor(testSet$class)
trainSet$class <- na.omit(trainSet$class)
set.seed(120)
classifier_RF <- randomForest(class~., trainSet, proximity=FALSE, ntree=150)
print(classifier_RF)
printRandomForests(classifier_RF, models=NULL, include.class=NULL, format="")

#GUI model work in progress Dont let Eric use it

x <- ctree(class ~ ., data=trainSet, controls=cforest_control(mtry=2, mincriterion=0))
plot(x, type="simple")
forestModel <- ctree(classifier_RF)

explainer <- explain_rf(classifier_RF, data = trainSet)

# calculates the interactions_frame for default settings so may give different
# results than the function below depending on our settings
# and takes more time
#minimal depth means the number of splits each tree must have to avoid overfitting
# it is a hyperparameter in a random forest that can be turned to increase accuracy and performance
min_depth_frame <- min_depth_distribution(classifier_RF)
head(min_depth_frame, n = 10)
plot_min_depth_distribution(min_depth_frame)
plot_min_depth_interactions(classifier_RF) # takes a long time to run

#prediction and confusion matrix for train set
p1 <- predict(classifier_RF, trainSet)
confusionMatrix(p1, trainSet$class)

#prediction and confusion matrix with test data
p2 <- predict(classifier_RF, testSet)
confusionMatrix(p2, testSet$class)
plot(classifier_RF)



create_train_test <- function(data, size = 0.8, train = TRUE) {
  
  #Shuffle Data
  data <- data[sample(1:nrow(data)), ]
  
  
  n_row = nrow(data)
  total_row = size * n_row
  train_sample <- 1: total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}

