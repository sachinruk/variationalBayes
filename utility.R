diagSum<-function(m,d){
  jump = nrow(m)+1
  m[seq(1,length(m),jump)] = m[seq(1,length(m),jump)] + d
}

diagMult<-function(A,d,side){
  #returns the result of right/left multiplying by a diagonal
  #inputs:
  #A: matrix to be multiplied
  #d: vector of diagonal elements
  #side: 'l' or 'r'
  if (side=="l") #left multiply the DIAGONAL
    res=sweep(A,1,d,"*")
  else #right multiply the DIAGONAL
    res=sweep(A,2,d,"*")
}

norm2<-function(x){
  sum(x**2)
}