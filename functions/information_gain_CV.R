
information_gain_CV <- function(training, validation, test, int_min, int_max, steps){
  
  weights<- data.frame(information.gain(price ~ ., training))
  weights$feature <- rownames(weights)
  weights[order(weights$attr_importance, decreasing = TRUE),]
  
  ig_mapes <- c()
  ig_index <- c()
  
  for (i in seq(int_min, int_max, steps)){
    information_gain_features <- weights$feature[weights$attr_importance > i]
    weights <- data.table(information.gain(price ~ ., data = training[,c(information_gain_features,"price"), with=FALSE]))
    weights$feature <- rownames(weights)
    weights[order(weights$attr_importance, decreasing = TRUE),]
    
    ig_training <- training[,c(information_gain_features,"price"), with=FALSE]
    ig_test <- test[,c(information_gain_features,"price"), with=FALSE]
    ig_validation <- validation[,c(information_gain_features,"price"), with=FALSE]
    
    mapes <- lm.model_cross_validation(ig_training, ig_validation)
    
    ig_mapes <- c(ig_mapes, mapes)
    ig_index <- c(ig_index,i)
  }
  
  ig_cross_validation <- data.table(score=ig_mapes,indexes=ig_index)
  ig_cross_validation_threshold <- ig_cross_validation[score == min(score),][1,indexes]
  return(ig_cross_validation_threshold)
}

