############################################################
# R Codes 8.2
# Compute Macaulay and modified duration, and Macaulay and
# modified convexity of a bond
#
# smt = settlement date, character variable (YYYY-MM-DD)
# mty = maturity date, character variable (YYYY-MM-DD)
# crt = coupon rate per year
# yld = yield rate per year
# rdv = redemption value per 100 face value
# frq = frequency of coupon payments per year
#
############################################################
source("R81pddc.R")

BPDDC=function(smt,mty,crt,yld,rdv,frq) {
  c=crt/frq
  y=(1+yld/frq)^frq-1
  dmty=as.Date(mty)
  dsmt=as.Date(smt)
  ic=floor(as.numeric(dmty-dsmt)/(365/frq))+1
  nm=paste(as.character(-12/frq),"month")
  indx=seq(dmty,length.out=ic+1,by=nm)
  indx=sort(indx,F)
  x=as.numeric(dsmt)
  vec=as.numeric(indx)
  i=findInterval(x,vec)
  re=as.numeric(dsmt-indx[i])/365
  ncf=ic-i+1
  time=c(1:ncf)/frq-re
  cf=rep(100*c,ncf)
  cf[ncf]=cf[ncf]+rdv
  summary=PDDC(cf,time,y)
  rownames(summary)[1]="Bond invoice price"
  return(summary) }

