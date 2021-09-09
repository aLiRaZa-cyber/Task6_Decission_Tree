library(datasets)
data("iris")
View(iris)

head(iris)
tail(iris)

#to shuffle the dataset to for training and testing the dataset

shuffle_iris <- sample(1 : nrow(iris))
head(shuffle_iris)

iris_data <- iris[shuffle_iris,]
head(iris_data)

View(iris_data)

create_train_test <- function(data, size = 0.8, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample <- 1: total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}

iris_train <- create_train_test(iris_data , size = 0.8, train = TRUE)
iris_test <- create_train_test(iris_data , size = 0.8, train = FALSE)

dim(iris_data) #original data
dim(iris_train) # iris train
dim(iris_test) # iris test

prop.table(table(iris_train$Species))

prop.table(table(iris_test$Species))
#about 33 percen


library(rpart.plot)

fit <- rpart(iris_train$Species~. , data = iris_train , method = "class")
rpart.plot(fit,extra = 106)

predict_unseen <- predict(fit, iris_test, type = "class")
table_mat <- table(iris_test$Species ,predict_unseen)

accuracy_test <- sum(diag(table_mat)) / sum(table_mat)
print(paste("Accuracy for Test is ", accuracy_test))
