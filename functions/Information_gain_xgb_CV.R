
information_gain_xgb_CV <- function(training, validation, label, target, int_min, int_max, steps){
  
  ig_mapes <- c()
  ig_index <- c()
  
  for (i in seq(int_min, int_max, steps)){
    weights<- data.frame(information.gain(price ~ ., training))
    weights$feature <- rownames(weights)
    weights[order(weights$attr_importance, decreasing = TRUE),]
    information_gain_features <- weights$feature[weights$attr_importance > i]
    
    xgb_ig_training <- training[,c(information_gain_features,"price"), with=FALSE]
    xgb_ig_training <- as.matrix(xgb_ig_training[, !'price', with=F])
    
    xgb_ig_validation <- validation[,c(information_gain_features,"price"), with=FALSE]
    xgb_ig_validation <- as.matrix(xgb_ig_validation[, !'price', with=F])
    
    mapes <- xgb.model_cross_validation(xgb_ig_training, xgb_ig_validation, label, target)
    
    ig_mapes <- c(ig_mapes, mapes)
    ig_index <- c(ig_index,i)
  }
  
  ig_cross_validation <- data.table(score=ig_mapes,indexes=ig_index)
  ig_cross_validation_threshold <- ig_cross_validation[score == min(score),][1,indexes]
  return(ig_cross_validation_threshold)
}
