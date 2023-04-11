library('tm')
library('pacman')

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

#Split the data
trainSet <- create_train_test(tSparse,.8,train = T)
testSet <- create_train_test(tSparse,.8,train = F)

#Baseline 0.2608835  0.2566578  0.2284174  0.2540414
prop.table(table(tSparse$class))
#Make the Model
fit <- rpart(class~., data = trainSet, method = "class")

#Test model
rpart.plot(fit)
prediction <- predict(fit, testSet, type = "class")
prediction

table_mat <- table(testSet$class, prediction)
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test



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



