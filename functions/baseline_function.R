lm.model <- function(training_dataset, validation_dataset, title) {
  # Create a training control configuration that applies a 5-fold cross validation
  train_control_config <- trainControl(method = "repeatedcv", 
                                       number = 5, 
                                       repeats = 1,
                                       returnResamp = "all")
  
  # Fit a glm model to the input training data
  this.model <- train(SalePrice ~ ., 
                      data = training_dataset, 
                      method = "glm", 
                      metric = "RMSE",
                      preProc = c("center", "scale"),
                      trControl=train_control_config)
  
  # Prediction
  this.model.pred <- predict(this.model, validation_dataset)
  this.model.pred[is.na(this.model.pred)] <- 0 # To avoid null predictions
  
  # RMSE of the model
  thismodel.rmse <- sqrt(mean((this.model.pred - validation_dataset$SalePrice)^2))
  
  # Error in terms of the mean deviation between the predicted value and the price of the houses
  thismodel.price_error <- mean(abs((exp(this.model.pred) -1) - (exp(validation_dataset$SalePrice) -1)))
  
  # Plot the predicted values against the actual prices of the houses
  my_data <- as.data.frame(cbind(predicted=(exp(this.model.pred) -1), observed=(exp(validation_dataset$SalePrice) -1)))
  ggplot(my_data, aes(predicted, observed)) +
    geom_point() + geom_smooth(method = "lm") +
    labs(x="Predicted") +
    ggtitle(ggtitle(paste(title, 'RMSE: ', format(round(thismodel.rmse, 4), nsmall=4), ' --> Price ERROR:', format(round(thismodel.price_error, 0), nsmall=0), 
                          ' €', sep=''))) +  
    scale_x_continuous(labels = scales::comma) + 
    scale_y_continuous(labels = scales::comma)
}


