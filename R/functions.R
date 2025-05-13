
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

# DEFF Design Effect p.110 equation for DEFF
DE = function(m,icc=1/6,cv=0.50){
 (1+(m-1)*icc)*(1+cv^2)
}


# Inflation factor due to non-response; p.112
# Note: r %in% (0,1)
INF = function(r)1/(1-r)


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
