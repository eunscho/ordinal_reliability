twopl<-function(th,a,b,D=1.7){
  z<-D*a*(th-b)
  P<-1/(1+exp(-z))
  QP<-cbind(1-P,P)
  return(QP)
}