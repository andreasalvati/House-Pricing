GridSerarch_xgb <- function(data, label,subsample=c(0.9, 1), colsample_bytree=c(0.5, 0.6), 
                            max_depth = c(8, 9), 
                            min_child = seq(1), 
                            eta = c(0.1, 0.2), 
                            gamma = c(1)){
  
  searchGridSubCol <- expand.grid(subsample = c(0.9, 1), 
                                  colsample_bytree = c(0.5, 0.6),
                                  max_depth = c(8, 9),
                                  min_child = seq(1), 
                                  eta = c(0.1, 0.2),
                                  gamma = c(1)
  )
  
  ntrees <- 100
  
  system.time(
    rmseErrorsHyperparameters <- apply(searchGridSubCol, 1, function(parameterList){
      
      #Extract Parameters to test
      currentSubsampleRate <- parameterList[["subsample"]]
      currentColsampleRate <- parameterList[["colsample_bytree"]]
      currentDepth <- parameterList[["max_depth"]]
      currentEta <- parameterList[["eta"]]
      currentMinChild <- parameterList[["min_child"]]
      currentgamma <- parameterList[["gamma"]]
      xgboostModelCV <- xgb.cv(data =  xgb_ig_training, 
                               label = training$price, 
                               nrounds = ntrees, 
                               nfold = 5, 
                               showsd = TRUE, 
                               metrics = "rmse", 
                               verbose = TRUE, 
                               "eval_metric" = "rmse",
                               "objective" = "reg:linear", 
                               "max.depth" = currentDepth,
                               "eta" = currentEta, 
                               "gamma" = currentgamma,
                               "subsample" = currentSubsampleRate, 
                               "colsample_bytree" = currentColsampleRate, 
                               print_every_n = 10, 
                               "min_child_weight" = currentMinChild, 
                               booster = "gbtree",
                               early_stopping_rounds = 10)
      
      xvalidationScores <- as.data.frame(xgboostModelCV$evaluation_log)
      rmse <- tail(xvalidationScores$test_rmse_mean, 1)
      trmse <- tail(xvalidationScores$train_rmse_mean,1)
      output <- return(c(rmse, trmse, currentSubsampleRate, currentColsampleRate, currentDepth, currentEta, currentMinChild))}))
  
  # finding the best combination of hyperparameters
  output <- as.data.table(t(rmseErrorsHyperparameters))
  varnames <- c("TestRMSE", "TrainRMSE", "SubSampRate", "ColSampRate", "Depth", "eta", "gamma")
  names(output) <- varnames
  optmimized_param <- output[TestRMSE == min(TestRMSE),3:7]
  
  return(optmimized_param)
}