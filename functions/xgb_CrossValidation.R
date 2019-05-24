xgb.model_cross_validation <- function(training_dataset, validation_dataset, label, target) {
  xgb_<-xgboost(booster='gbtree',
                 data= training_dataset,
                 label= label,
                 nrounds = 100,
                 objective='reg:linear')
  
  # Prediction
  this.model.pred <- predict(xgb_, newdata = validation_dataset, type='response')
  
  # RMSE of the model
  thismodel.mape <- mape(target, this.model.pred)
  
  return(as.numeric(thismodel.mape))
}