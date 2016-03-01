source("utility.R")

SBR<-function(y,Phi,iter){
  #Sparse Bayesian Regression
  sigma2=0.5; 
  n=ncol(Phi);
  PhiTPhi=t(Phi)%*%Phi;
  A=matrix(1,nrow=n,ncol=1); 
  
  for (i in 1:iter){
    w=weightEst(y,Phi,sigma2,A);
    A=alphaEst(w$Sigma,w$mu);
    sigma2=Esigma(y,Phi,w$mu,w$Sigma,PhiTPhi,n);
  }
  list(mu,Sigma,A)
}


Esigma<-function(y,Phi,mu,Sigma,Phi2,n){
  delta=1e-9;
  a=delta+n/2;
  PhiTPhiSigma=Phi2*Sigma;
  b=0.5*(norm2(y-Phi%*%mu)+sum(PhiTPhiSigma)+2*delta);
  
  sigma2=b/a;
}

weightEst<-function(y,Phi,sigma2,A){
  m=nrow(Phi);
  Ainv=diag(1/A);
  # Amatrix=inv(sigma2*eye(m)+Phi*Ainv*Phi'); %imagesc(log(abs(Amatrix)));
  browser()
  Sigma=Ainv-Ainv%*%t(Phi)%*%
        solve((sigma2*eye(m)+Phi%*%Ainv%*%t(Phi)),
              Phi%*%Ainv);
  mu=Sigma%*%(t(Phi)%*%y)/sigma2;
  list(Sigma, mu)
}

alphaEst<-function(Sigma,mu){
  delta=1e-9;
  A=(delta+1)/(diag(Sigma)+mu**2+delta);
}
