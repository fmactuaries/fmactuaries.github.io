##################################################################
# R Codes 4.3
# Computation of Dollar Weighted Rate of Return (DWWR) per year
#
# time = timing of cash flows (in months)
# dw = deposits (+ve) and withdrawals (-ve)
# b0 = fund balance at beginning
# b1 = fund balance at end
##################################################################

DWRR=function(time,dw,b0,b1) {
  dwrr=(b1-b0-sum(dw))/(b0+sum((1-time/12)*dw))
  return(dwrr) }

