# (2.d) A Generic CV Function

error_method <- function(y_true, y_pred) {
  error <- mean(y_true != y_pred)
  return(error)
}

require(caret)
cv_generic <- function(classifier, x_train, y_train, k, loss_func=error_method) {
  loss_lst <- c()
  flds <- createFolds(y_train, k = k, list = TRUE, returnTrain = FALSE)
  
  for (i in 1:k) {
    X_train <- x_train[-flds[[i]],]
    Y_train <- y_train[-flds[[i]]]
    X_valid <- x_train[flds[[i]],]
    Y_test <- y_train[flds[[i]]]
    
    model <- classifier(X_train, Y_train, X_valid)
    loss_lst <- c(loss_lst, loss_func(Y_test, model$pred))
  }
  return(loss_lst)
}

