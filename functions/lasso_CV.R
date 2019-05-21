lasso_CV <- function(training, validation, min_int, max_int, steps){
  
  # cross validation
  lasso_mape <- c()
  lasso_index <- c()
  
  for (i in seq(min_int,max_int,steps)){
    
    lambdas <- 10^seq(-3, 0, by = .05)
    
    set.seed(i)
    train_control_config <- trainControl(method = "repeatedcv", 
                                         number = 5, 
                                         repeats = 1,
                                         returnResamp = "all")
    
    lasso.mod_cv <- train(price ~ ., data = training, 
                          method = "glmnet", 
                          metric = "MAPE",
                          trControl=train_control_config,
                          tuneGrid = expand.grid(alpha = 1, lambda = lambdas))
    
    lasso.mod.pred <- predict(lasso.mod_cv, validation)
    lasso.mod.pred[is.na(lasso.mod.pred)] <- 0
    
    my_data <- as.data.frame(cbind(predicted=(exp(lasso.mod.pred) -1), observed=(exp(validation$price) -1)))
    lasso.mod.mape <- mape(lasso.mod.pred, validation$price)
    
    lasso_mape <- c(lasso_mape, lasso.mod.mape)
    lasso_index <- c(lasso_index, i)
  }
  
  lasso_cross_validation <- data.table(score=lasso_mape, indexes=lasso_index)
  # optimal seed
  lasso_seed <- lasso_cross_validation[score == min(score),][1,indexes]
  
  return(lasso_seed)
}