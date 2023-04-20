library('tidyverse')
library('caret')
library('tm')
library('pacman')
library('e1071')

#CSC Project
pacman::p_load(datasets,pacman, dplyr, GGally, ggplot2, ggthemes, ggvis,
               httr, lubridate, plotly, rio, rmarkdown, shiny,
               stringr, tidyverse, lessR, aplpack, readr, tm, SnowballC, rpart.plot)
test <- read.csv("Datasets/Tweets_with_Sarcasm_and_Irony/test.csv")
train <- read.csv("Datasets/Tweets_with_Sarcasm_and_Irony/train.csv")
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

#Function
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

#Split the data
trainSet <- create_train_test(tSparse,.8,train = T)
testSet <- create_train_test(tSparse,.8,train = F)
trainSet$class = factor(trainSet$class)
testSet$class = factor(testSet$class)
trainSet$class <- ifelse(trainSet$class == "sarcasm", "sarcasm", "not sarcasm")
testSet$class <- ifelse(testSet$class == "sarcasm", "sarcasm", "not sarcasm")
vars_to_remove <- nearZeroVar(trainSet)
trainSet <- trainSet[, -vars_to_remove]
trainSet <- head(trainSet, 1000)
view(trainSet)

set.seed(130)
model <- train(
  class~., data = trainSet, method = "svmRadial",
  trControl = trainControl("cv", number = 10),
  preProcess = c("center", "scale"),
  tuneLength = 4
)

model$bestTune

predicted.classes <- model %>% predict(testSet)
mean(predicted.classes == testSet$class)
