####################################################################
# R Codes 2.1
# Functions to compute present values of annuity-immediates
#
# ani(n,i) # Eq (2.1)
# anik(n,i,k) # Eq (2.20)
# anim(n,i,m) # Eq (2.22)
# abni(n,i) # Eq (2.30)
# aniPD(P,D,n,i) # Eq (2.35)
# Iani(n,i) # Eq (2.36)
# Dani(n,i) # Eq (2.40)
# Iabni(n,i) # Eq (2.43)
# Ibabni(n,i) # Eq (2.44)
####################################################################

ani=function(n,i) (1-(1+i)^(-n))/i

anik=function(n,i,k) (1-(1+i)^(-n))/((1+i)^(k)-1)

anim=function(n,i,m) {r=m*((1+i)^(1/m)-1); (1-(1+i)^(-n))/r}

abni=function(n,i) (1-(1+i)^(-n))/log(1+i)

aniPD=function(P,D,n,i) P*ani(n,i)+D*((ani(n,i)-n*(1+i)^(-n))/i)

Iani=function(n,i) ((1+i)*ani(n,i)-n*(1+i)^(-n))/i

Dani=function(n,i) (n-ani(n,i))/i

Iabni=function(n,i) ((1+i)^(ani(n,i))-n*(1+i)^(-n))/(log(1+i))

Ibabni=function(n,i) (abni(n,i)-n*(1+i)^(-n))/(log(1+i))

