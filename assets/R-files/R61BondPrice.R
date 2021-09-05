####################################################################
# R Codes 6.1 
# Compute price of a bond assuming flat term structure.
#
# smt = settlement date, character variable (YYYY-MM-DD)
# mty = maturity date, character variable (YYYY-MM-DD)
# crt = coupon rate per year
# yld = annual yield rate
# rdv = redemption value per 100 face value
# frq = frequency of coupon payments per year
####################################################################

bondprice=function(smt,mty,crt,yld,rdv,frq) {
  c=crt/frq
  y=yld/frq
  dmty=as.Date(mty)
  dsmt=as.Date(smt)
  ic=floor(as.numeric(dmty-dsmt)/(365/frq))+1
  nm=paste(as.character(-12/frq),"month")
  indx=seq(dmty,length.out=ic+1,by=nm)
  indx=sort(indx,F)
  x=as.numeric(dsmt)
  vec=as.numeric(indx)
  i=findInterval(x,vec)
  ic=length(indx)-i
  if (smt==indx[i]) {
    pp=100*c*(1-(1+y)^(-ic))/y+rdv/((1+y)^(ic))
    return(list("ListedPrice"=pp,"InvoicePrice"=pp))
  } else {
    t=as.numeric(dsmt-indx[i])/as.numeric(indx[i+1]-indx[i])
    dp=(100*c*(1-(1+y)^(-ic))/y+rdv/((1+y)^(ic)))*(1+y)^t
    cp=dp-100*c*t
    return(list("ListedPrice"=cp,"InvoicePrice"=dp)) } }
