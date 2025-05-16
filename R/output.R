

#
# Estimation
#

# Each input can be a single number or a list of numbers.
# Note that a data.frame is output with number of rows equal to
#   the product of the lengths of the input lists ... so it can get
#   large quickly.

# Make a table of sample sizes.
# Output columns:
#  ess(p,d,alpha) Effective Sample Sizes
#  deff(m,icc,cv) Design Effect 
#  inf(f) Inflation Factor
#  n(ess,deff,inf) Sample Size
#  nc(n,m) Number of Clusters
nOutTab = function(d,p,m,icc=1/6,cv=0.50,r,alpha=0.05){
  val=expand.grid(p,d,alpha,m,icc,cv,r)
  colnames(val) = c("p","d","alpha","m","icc","cv","r")
  val$ess=apply(val[,c("d","p")],1,function(x)ESS(x[2],x[1]))
  val$deff=DE(val$m,icc=val$icc,cv=val$cv)
  val$inf=INF(val$r)
  val$n = ceiling(val$ess * val$deff * val$inf)
  val$nc = ceiling(val$n / val$m)
  colnames(val) = c(
    "p","d","alpha","m","icc","cv","r",
    "ess(p,d,alpha)","deff(m,icc,cv)","inf(r)","n(ess,deff,inf)",
    "nc(n,m)"
  )
  val
}





# Find half-width of CI given n (and everything else)
#
dOutput = function(n,p,m,icc=1/6,cv=0.50,r,alpha=0.05){
  val=expand.grid(n,p,m,icc,cv,r,alpha)
  colnames(val) = c("n","p","m","icc","cv","r","alpha")
  val$d = apply(val,1,function(x)
      halfWidthCI(x[1],x[2],x[3],x[4],x[5],x[6],x[7])
    )
  val
} 






#
# Classification
#

nclOutTab = function(P0,delta,alpha,beta,direction,m,icc,cv,r){
  dir = tolower(direction)
  # Only do either above or below but not both in the same table
  if(!dir %in% c('above','below'))stop("direction must be either 'above' or 'below' (case-insensitive). This function only allows for creating an output table for one or the other 'above' or 'below' but not both.")
  val=expand.grid(P0,delta,alpha,beta,m,icc,cv,r)
  colnames(val) = c("P0","delta","alpha","beta","m","icc","cv","r")
  val$ess=apply(val[,c("P0","delta","alpha","beta")],1,function(x)ESScl(x[1],x[2],x[3],x[4],dir))
  val$deff=DE(val$m,icc=val$icc,cv=val$cv)
  val$inf=INF(val$r)
  val$n = ceiling(val$ess * val$deff * val$inf)
  val$nc = ceiling(val$n / val$m)
  colnames(val) = c(
    "P0","delta","alpha","beta","m","icc","cv","r",
    "ess(P0,delta,alpha,beta,direction)","deff(m,icc,cv)","inf(r)",
    "n(ess,deff,inf)","nc(n,m)"
  )
  list(Table=val, direction=dir)
}