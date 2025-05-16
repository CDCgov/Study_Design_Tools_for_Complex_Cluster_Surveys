


#
# Estimation
#

# ESS Effective Sample Size eq. B2-1 for ESS p.129
ESS = function(p,d,alpha=0.05){
  if(p<0|p>1)stop("p must be a number from 0 to 1")
  if(d<=0|d>=0.3)stop("d must be a number between 0 and 0.3")
  if(alpha<=0|alpha>=1)stop("alpha must be a number between 0 and 1")
  # Calculate k according to Table K on p.129
  k=1
  if(p<d|p>1-d)k=8*d*(1-2*d)
  if(p>=d&p<0.3)k=4*(p+d)*(1-p-d)
  if(p>0.7&p<=1-d)k=4*(p-d)*(1-p+d)
  z=qnorm(1-alpha/2)
  ceiling(k*z^2/4/d^2 + 1/d - 2*z^2 + (z+2)/k)
}


# Solve for d for given n, p, m, icc, cv, r, alpha
halfWidthCI = function(n,p,m,icc,cv,r,alpha=0.05){
  deff = DE(m,icc,cv)
  inf = INF(r)
  ess = n/deff/inf
  z = qnorm(1-alpha/2)
  
  f = function(d,ess,p,z){
    k=1
    if(p<d|p>1-d)k=8*d*(1-2*d)
    if(p>=d&p<0.3)k=4*(p+d)*(1-p-d)
    if(p>0.7&p<=1-d)k=4*(p-d)*(1-p+d)
    (  ess - (k*z^2/4/d^2 + 1/d - 2*z^2 + (z+2)/k)  )^2
  }
  optimize(f,c(0,1),ess=ess,p=p,z=z)$minimum
}












#
# Classification
#

# ESS Effective Sample Size for classification eq. B2-3 p.131
ESScl = function(P0, delta, alpha, beta, direction){
  if(P0<0|P0>1)stop("P0 must be a number from 0 to 1")
  if(alpha<=0|alpha>=1|beta<=0|beta>=1)stop("alpha & beta must be a number between 0 and 1")
  dir = tolower(direction)
  if(!dir %in% c('above','below'))stop("direction must be either 'above' or 'below' (case-insensitive)")
  if(dir=='above' & P0+delta>1)stop("P0+delta>1")
  if(dir=='below' & P0-delta<0)stop("P0-delta<0")
  if (dir=='above') {P1=P0+delta} else {P1=P0-delta}
  z1=qnorm(1-alpha)
  z2=qnorm(1-beta)  
  n_0 = ((z1*sqrt(P0*(1-P0))+z2*sqrt(P1*(1-P1)))/(P1-P0))**2
  ceiling((n_0/4)*(1+sqrt(1+2/(n_0*abs(delta))))**2)
}


# Solve for delta for given n, P0, alpha, beta, direction, m, icc, cv, r
getDelta = function(n, P0, alpha, beta, direction, m, icc, cv, r){
  dir = tolower(direction)
  deff = DE(m,icc,cv)
  inf = INF(r)
  ess = n/deff/inf
  z1=qnorm(1-alpha)
  z2=qnorm(1-beta)  
  
  f = function(delta,ess,P0,z1,z2){
    if (dir=='above') {P1=P0+delta} else {P1=P0-delta}
    n_0 = ((z1*sqrt(P0*(1-P0))+z2*sqrt(P1*(1-P1)))/(P1-P0))**2
    x = (n_0/4)*(1+sqrt(1+2/(n_0*abs(delta))))**2
    (ess-x)**2
  }
  optimize(f,c(0,1),ess=ess,P0=P0,z1=z1,z2=z2)$minimum
}












#
# General
#


# DEFF Design Effect p.110 equation for DEFF
DE = function(m,icc=1/6,cv=0.50){
 (1+(m-1)*icc)*(1+cv^2)
}


# Inflation factor due to non-response; p.112
# Note: r %in% (0,1)
INF = function(r)1/(1-r)




