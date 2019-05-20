mae<-function(real, predicted){
  return(mean(abs(real^2-predicted^2)))
}

mape<-function(real,predicted){
  return(mean(abs(((exp(real) -1) - (exp(predicted) -1))/(exp(real)-1))))
}

rmse<-function(real,predicted){
  return(sqrt(mean((real^2-predicted^2)^2)))
}
