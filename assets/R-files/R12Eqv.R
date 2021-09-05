###################################################################
# R Codes 1.2
# Equation of Value (1.36), cash flows x start at 0 and end at n
#
# PV(x,i) = present value of x at effective rate i
# FV(x,i) = future value of x at effective rate i
# LP(pv,n,i) = n level payments x with present value equating
# pv at effective rate i
# EI(pv,x) = effective rate for present value of x to equate pv
###################################################################
PV=function(x,i)
{n=(1:length(x)); sum(x*((1+i)^(-(n-1))))}
FV=function(x,i)
{n=length(x)-(1:length(x)); sum(x*((1+i)^(n)))}
LP=function(pv,n,i)
{a=(1-(1+i)^(-n))/i*(1+i); pv/a}
EI=function(pv,x)
{uniroot(function(i) PV(x,i)-pv,lower=0,upper=1)$root}

