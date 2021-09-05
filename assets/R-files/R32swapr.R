#####################################################################
# R Codes 3.2
#
# Computation of swap rate assuming constant notional principal
#
#####################################################################

rs = function(ys){
  n = length(ys)
  xx = seq(1,n,1)
  xxU=xx[2:n]
  xxL=xx[1:n-1]
  yf=ys
  yf[2:n]=(((1+ys[2:n])^xxU/(1+ys[1:n-1])^xxL)^(1/(xxU-xxL))-1)
  rs=sum(yf/(1+ys)^(xx))/sum(1/(1+ys)^(xx))
  options(digits = 5)
  cat("Current spot rates up to", n, "years: ", ys,"\n")
  cat("Forward rates are: ", yf,"\n")
  cat("Swap rate is: ", rs,"\n")
  
}

