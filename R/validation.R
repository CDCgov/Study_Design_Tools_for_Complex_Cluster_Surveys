

#
# Estimation
#

dLst = seq(0.03,0.10,by=0.01)
pLst = c(seq(0.05,0.25,by=0.05),0.50,seq(0.75,0.95,by=0.05))
nLst = seq(100,1000,by=100)

m = 15
icc = 1/6
cv = 0.50
r = 0.15

# Make Table B-1 p.118 to validate ESS
Tab_B_1 = t(outer(pLst,dLst,Vectorize(ESS)))
dimnames(Tab_B_1) = list(dLst,pLst)
Tab_B_1


# Calculate d as a function of n & p
dTab = outer(nLst,pLst,Vectorize(halfWidthCI),m,icc,cv,r)
dimnames(dTab) = list(nLst,pLst)
round(dTab,2)

# Now use these calculated d-values to see if they give the right n
ndTab = dTab
for(i in 1:nrow(dTab)){
for(j in 1:ncol(dTab)){
  ndTab[i,j] = ESS(pLst[j],dTab[i,j]) * DE(m,icc,cv)*INF(r)
}}
round(ndTab,1)
round(ndTab,1)-nLst



#
# Classification
#

pLst=seq(0.5, 0.95, by=0.05)
dLst = c(1,5,10,15)/100
aLst = c(0.10,0.05)
bLst = c(0.20,0.10)

m = 15
icc = 1/6
cv = 0.50
r = 0.15

# Make Tables B-2 p.123 & B-3 p. 125 to validate ESScl
# NOTE: In Table B-3 we omit rows where P0+delta>1 because that does not make sense
val = expand.grid(pLst,dLst)
colnames(val) = c("P0","delta")
val$a10b20 = apply(val[,1:2],1,function(x)ESScl(x[1],x[2],0.10,0.2,'below'))
val$a05b20 = apply(val[,1:2],1,function(x)ESScl(x[1],x[2],0.05,0.2,'below'))
val$a10b10 = apply(val[,1:2],1,function(x)ESScl(x[1],x[2],0.10,0.1,'below'))
val$a05b10 = apply(val[,1:2],1,function(x)ESScl(x[1],x[2],0.05,0.1,'below'))
Tab_B_2 = val
Tab_B_2

val = expand.grid(pLst,dLst)
val = val[apply(val,1,sum)<1,]
colnames(val) = c("P0","delta")
val$a10b20 = apply(val[,1:2],1,function(x)ESScl(x[1],x[2],0.10,0.2,'above'))
val$a05b20 = apply(val[,1:2],1,function(x)ESScl(x[1],x[2],0.05,0.2,'above'))
val$a10b10 = apply(val[,1:2],1,function(x)ESScl(x[1],x[2],0.10,0.1,'above'))
val$a05b10 = apply(val[,1:2],1,function(x)ESScl(x[1],x[2],0.05,0.1,'above'))
Tab_B_3 = val
Tab_B_3

# Calculate delta as a function of n, P0, alpha, beta
val = expand.grid(pLst,dLst,aLst,bLst)
colnames(val) = c("P0","delta","alpha","beta")
val$ess = apply(val,1,function(x)ESScl(x[1],x[2],x[3],x[4],'below'))
val$n = val$ess * DE(m,icc,r) * INF(r)
val$deltaGot = apply(val,1,function(x)getDelta(x[6],x[1],x[3],x[4],'below',m,icc,cv,r))
deltaTab = val
deltaTab