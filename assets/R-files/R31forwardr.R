#############################################################
# R Codes 3.1
#
# Computation of forward rates given spot rates, Eq (3.8)
#############################################################
fts=function(spot) {
    n=length(spot)
    ns=n-1
    ws=matrix(nrow=ns+1,ncol=ns)
    for (i in 1:ns) {
      for (j in 1:(n-i))
      {ws[i,j]=((1+spot[i+j])^(i+j)/(1+spot[i])^i)^(1/j)-1} }
    ws=cbind(spot,ws)
    rownames(ws)=seq(1,n,1)
    colnames(ws)=c("spot",seq(1,ns,1))
    return(ws) 
}