####################################################################
# R Codes 5.2
#
# loan = loan amount
# year = term of loan in years
# freq = frequency of payments per year
# Lrate = nominal loan rate per year
# SFrate = nominal SF rate per year
####################################################################

SF=function(loan,year,freq,Lrate,SFrate) {
  Li=Lrate/freq
  SFi=SFrate/freq
  pn=year*freq
  Lf=(1-(1+Li)^(-pn))/Li
  SFf=((1+SFi)^(pn)-1)/SFi
  IP=loan*Li
  SFP=loan/SFf
  TP=IP+SFP
  period=year*freq
  summary_mat=rbind(loan,year,freq,Lrate*100,SFrate*100)
  rownames(summary_mat)=c("Loan amount","Term (Years)","Payments per year",
                          "Nominal loan interest rate (%)","Nominal SF interest rate (%)")
  colnames(summary_mat)=""
  summary_mat=round(summary_mat,digits=2)
  SF_mat=matrix(nrow=period+1,ncol=7)
  colnames(SF_mat)=c("Year","Pay no","Total pmt","Loan int",
                     "SF pmt","SF int","SF Bal")
  rownames(SF_mat)=rep("",nrow(SF_mat))
  SF_mat[1,]=rep(0,times=7)
  for (i in 1:period) {
    if (as.integer(i/freq)==i/freq) {SF_mat[i+1,1]=i/freq}
    else
      SF_mat[i+1,1]=as.integer(i/freq)+1
    SF_mat[i+1,2]=i
    SF_mat[i+1,3]=TP
    SF_mat[i+1,4]=IP
    SF_mat[i+1,5]=SFP
    SF_mat[i+1,6]=SF_mat[i,7]*SFi
    SF_mat[i+1,7]=SF_mat[i,7]+SF_mat[i+1,5]+SF_mat[i+1,6] }
  SF_mat=SF_mat[-c(1),]
  SF_mat[,3:7]=round(SF_mat[,3:7],digits=2)
  return(list("summary"=summary_mat,"schedule"=SF_mat)) }