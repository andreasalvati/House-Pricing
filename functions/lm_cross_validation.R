lm.model_cross_validation <- function(training_dataset, validation_dataset) {
  # Create a training control configuration that applies a 5-fold cross validation
  train_control_config <- trainControl(method = "repeatedcv", 
                                       number = 5, 
                                       repeats = 1,
                                       returnResamp = "all")
  
  # Fit a glm model to the input training data
  this.model <- train(price ~ ., 
                      data = training_dataset, 
                      method = "glm", 
                      metric = "MAPE",
                      preProc = c("center", "scale"),
                      trControl=train_control_config)
  
  # Prediction
  this.model.pred <- predict(this.model, validation_dataset)
  this.model.pred[is.na(this.model.pred)] <- 0 # To avoid null predictions
  
  # RMSE of the model
  thismodel.mape <- mape(this.model.pred, validation_dataset$price)
  
  # Error in terms of the mean deviation between the predicted value and the price of the houses
  thismodel.price_error <- mean(abs((exp(this.model.pred) -1) - (exp(validation_dataset$price) -1)))
  
  return(as.numeric(thismodel.mape))
}