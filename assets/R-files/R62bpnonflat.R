#############################################################
# R Codes 6.2
# Bond price given a nonflat yield curve
#
# ytm = years to maturity
# crt = coupon rate per year
# yc = yield curve, rates per year
# rdv = redemption value per 100 face value
# frq = frequency of coupon payments per year
#############################################################

bpyc=function(ytm,crt,yc,rdv,frq) {
  ycfrq=yc/frq
  cp=100*crt/frq
  nve=ytm*frq
  nv=seq(from=1,to=nve,by=1)
  ycf=ycfrq[1:nve]
  bp=sum(cp/(1+ycf)^(nv))+rdv/(1+ycf[nve])^(nve)
  cat("Bond price =",bp) }

