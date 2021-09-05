############################################################
# R Codes 8.1
# Compute Macaulay and modified duration, and Macaulay and
# modified convexity for a general stream of cash flows
#
# cf = cash flows
# time = time of cash flows in years
# interest = effective interest rate per year
#
############################################################

PDDC=function(cf,time,interest) {
  dis=1/(1+interest)^time
  pv=cf%*%dis
  dur_mac=((cf*time)%*%dis)/pv
  dur_mod=dur_mac/(1+interest)
  con_mac=((cf*time^2)%*%dis)/pv
  con_mod=((cf*time*(time+1))%*%dis)/pv
  summary=rbind(pv,dur_mac,dur_mod,con_mac,con_mod)
  rownames(summary)=c("Present value","Macaulay duration",
                      "Modified duration","Macaulay convexity",
                      "Modified convexity ")
  colnames(summary)=""
  return(summary) }