mae<-function(real, predicted){
  return(mean(abs(real-predicted)))
}

#mape<-function(real,predicted){
  #return(mean(abs((real-predicted)/real)*100))
#}

rmse<-function(real,predicted){
  return(sqrt(mean((real-predicted)^2)))
}

mape<-function(real,predicted){
  return(mean(abs(((exp(real)-1)-(exp(predicted)-1))/(exp(real)-1))*100))
}