######################################################################
# R Codes 4.1
# Computation of Internal Rate of Return (IRR) per year
#
# cf = cash flows
# time = timing of cash flows (in years)
# pv = present value of cash flows
# default interval of IRR, L = 0 and U = 1, which can be changed
######################################################################

irr=function(cf,time,pv,L=0,U=1) {
  IRR=uniroot(function(x) sum(cf*((1+x)^(-time)))-pv,
              lower=L,upper=U)$root
  return(IRR) }


