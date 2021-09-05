###############################################################
# R Codes 10.1 (mvlognrate.R)
# Computation of the mean and variance of various accumulation
# and annuity functions assuming lognormal interest rate model
#
# n = number of payments (larger than 1)
# (mu, sigma2) = parameters of lognormal distribution
###############################################################

options(digits=5)
source("R21AnnFunc.R")

# The source file which contains R codes 2.1 in Chapter 2
# must be in the same working directory as the current file

EVasf=function(n,mu,sig2) {
  rs=exp(mu+0.5*sig2)-1
  ra=exp(mu-0.5*sig2)-1
  js=2*rs+rs*rs+exp(2*mu+sig2)*(exp(sig2)-1)
  ja=exp(2*(mu-sig2))-1
  Esdn=ani(n,rs)*(1+rs)^(n+1)
  Vsdnf=function(js,rs,n) {
    snr=ani(n,rs)*(1+rs)^(n+1)
    ans=(js+rs+2)/(js-rs)*ani(n,js)*(1+js)^(n+1)-
      (2*js+2)/(js-rs)*snr-(snr)^2
    return(ans) }
  Vsdn=Vsdnf(js,rs,n)
  Esn=1+ani(n-1,rs)*(1+rs)^n
  Vsn=Vsdnf(js,rs,n-1)
  Eanf=function(ra,n) return(ani(n,ra))
  Ean=Eanf(ra,n)
  Vanf=function(ja,ra,n) {
    ans=(ja+ra+2)/(ra-ja)*ani(n,ja)-
      (2*ra+2)/(ra-ja)*ani(n,ra)-(ani(n,ra))^2
    return(ans) }
  Van=Vanf(ja,ra,n)
  Eadn=1+Eanf(ra,n-1)
  Vadn=Vanf(ja,ra,n-1)
  summary=c(n,mu,sig2,Ean,Van,Eadn,Vadn,Esn,Vsn,Esdn,Vsdn)
  summary=as.matrix(summary)
  colnames(summary)=""
  rownames(summary)=(c("n","mu","sigma^2 ","Ean","Van",
                       "Eadn","Vadn ","Esn","Vsn","Esdn","Vsdn"))
  return(list("summary"=summary))
  }

  