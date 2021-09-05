######################################################################
# R Codes 4.2
# Computation of Time Weighted Rate of Return (TWRR) per year
#
# time = timing of cash flow (in months)
# balance = fund value prior to cash flows (including two end values)
# dw = deposits (+ve) and withdrawals (-ve)
######################################################################

TWRR=function(time,balance,dw) {
  schedule=cbind(time,balance,dw)
  twrr_vector=vector(length=nrow(schedule)-1)
  for (i in 1:NROW(twrr_vector)) {
    twrr_vector[i]=schedule[i+1,2]/(schedule[i,2]+schedule[i,3]) }
  twrr=prod(twrr_vector)^(12/max(time))-1
  return(twrr) }


