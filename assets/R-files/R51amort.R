#####################################################################
# R Codes 5.1
# Construction of amortization table
#
# loan = loan amount
# year = term of loan in years
# freq = frequency of payments per year
# yrate = nominal loan rate per year
# payment = level loan repayment
######################################################################

amort=function(loan,year,freq,yrate,payment) {
  j=yrate/freq
  payno=year*freq
  summary_matrix=rbind(loan,year,freq,payno,yrate*100,payment)
  colnames(summary_matrix)=c("")
  rownames(summary_matrix)=c("Loan amount","Term (Years)",
                             "Payments per year","Payment number","Nominal interest rate (%)",
                             "Level payment")
  summary_matrix=round(summary_matrix,digits=2)
  amort_matrix=matrix(nrow=payno+1,ncol=5)
  rownames(amort_matrix)=rep("",times=nrow(amort_matrix))
  colnames(amort_matrix)=c("Year"," Pay no"," Interest",
                           " Principal"," Balance")
  amort_matrix[1,]=c(0,0,0,0,loan)
  for (i in 1:payno) {
    if (as.integer(i/freq)==i/freq) {amort_matrix[i+1,1]=i/freq}
    else
      amort_matrix[i+1,1]=as.integer(i/frequency)+1
    amort_matrix[i+1,2]=i
    amort_matrix[i+1,3]=amort_matrix[i,5]*j
    amort_matrix[i+1,4]=payment-amort_matrix[i+1,3]
    amort_matrix[i+1,5]=amort_matrix[i,5]-amort_matrix[i+1,4] }
  amort_matrix[,3:5]=round(amort_matrix[,3:5],digits=2)
  return(list("summary"=summary_matrix,"schedule"=amort_matrix)) }