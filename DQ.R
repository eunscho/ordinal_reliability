DQ <-function(a,b,nq=101,theta=c(-6,6),D=1.7){ 
  
  n<-length(a) # number of items
  
  # set up quadrature grid and normalized weights
  th<-seq(theta[1],theta[2],length.out=nq)
  w<-dnorm(th)
  w<-w/sum(w)
  
  sig2e<-0  # error variance
  Pbar<-rep(0,nq)
  sig2elist<-vector("numeric")
  truelist<-vector("numeric")
  for(j in 1:n){
    
    # compute traceline
    QP<-twopl(th,a[j],b[j],D)
    
    # Product
    QxP<-QP[,1]*QP[,2]
    
    # integrate for error variance
    sig2ej<-sum(QxP*w) # Eq 4
    sig2e<-sig2e+sig2ej # Eq 5
    
    sig2elist<-c(sig2elist,sig2ej) # save sig2ej for later use
    pii<-sum(QP[,2]*w) # use quadrature to obtain marginal probability of correct answer
    truevarj<-pii*(1-pii)-sig2ej # Eq 11
    truelist<-c(truelist,truevarj)
    
    # save P to accumulate what the mean is at each point of theta
    Pbar<-Pbar+QP[,2]
    
  }
  
  Pbar<-Pbar/n
  
  # Integrate to obtain true score variance
  sig2tau<-sum((n*Pbar)^2 *w) - (sum(n*Pbar*w))^2 # Eq 6
  
  rel<-sig2tau/(sig2tau+sig2e) # Eq 16
  
  # attr(rel,"sig2tau")<-sig2tau
  # attr(rel,"sig2eval")<-sig2e
  # attr(rel,"sig2e")<-sig2elist # error variance per item
  # attr(rel,"truevar")<-truelist # true score variance per item
  # attr(rel,"varY")<-sig2elist+truelist # total variance per item
  
  return(rel)
  
}