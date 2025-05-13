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
