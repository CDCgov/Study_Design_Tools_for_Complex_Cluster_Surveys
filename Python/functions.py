
# ESS Effective Sample Size eq. B2-1 for ESS p.129
def ESS(p,d,alpha=0.05):
  if p<0 or p>1: exit("p must be a number from 0 to 1")
  if d<=0 or d>=0.3: exit("d must bea number between 0 and 0.3")
  if alpha<=0 or alpha>=1: exit("alpha must be a number between 0 and 1")
  k = 1  
  if p < d or p > 1-d: k = 8*d*(1-2*d)
  if p >= d and p < 0.3: k = 4*(p+d)*(1-p-d)
  if p > 0.7 and p <= 1-d: k = 4*(p-d)*(1-p+d)
  z = norm.ppf(1-alpha/2)
  return ceil(
     k*z**2/4/d**2 + 1/d - 2*z**2 + (z+2)/k
  )
    

# DEFF Design Effect p.110 equation for DEFF
def DE(m,icc=1/6,cv=0.50):
    return (1+(m-1)*icc)*(1+cv**2)


# Inflation factor due to non-response; p.112
# Note: r %in% (0,1)
def INF(r):
    return 1/(1-r)


# Solve for d for given n, p, m, icc, cv, r, alpha
def halfWidthCI(n,p,m,icc,cv,r,alpha=0.05):
    deff = DE(m,icc,cv)
    inf = INF(r)
    ess = n/deff/inf
    z = norm.ppf(1-alpha/2)
    def f(d,ess,p,z):
       k = 1  
       if p < d or p > 1-d: k = 8*d*(1-2*d)
       if p >= d and p < 0.3: k = 4*(p+d)*(1-p-d)
       if p > 0.7 and p <= 1-d: k = 4*(p-d)*(1-p+d)
       return (ess - (k*z**2/4/d**2 + 1/d - 2*z**2 + (z+2)/k))**2
    val = minimize_scalar(f,args=(ess,p,z),bounds=(0,1),method='bounded').x
    val = val.item()
    return val