##############################################################
# R Codes 7.1 (bondYTM.R)
# Computation of yield to maturity of a bond
#
# smt = settlement date, character variable (YYYY-MM-DD)
# mty = maturity date, character variable (YYYY-MM-DD)
# crt = coupon rate per year 
# pr = invoice price of the bond
# rdv = redemption value per 100 face value
# frq = frequency of coupon payments per year
##############################################################

source("R61BondPrice.R") 
# the source file must be in the same working directory 
# as the current file

ytm=function(smt,mty,crt,pr,rdv,frq) {
  YTM=uniroot(function(x) 
    as.numeric(bondprice(smt,mty,crt,x,rdv,frq)$InvoicePrice)-pr,
    c(0.001,0.4))
  return(YTM$root) }
