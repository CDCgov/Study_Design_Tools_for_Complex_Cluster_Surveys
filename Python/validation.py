dLst = np.arange(0.03, 0.10, 0.01) #(8,)
pLst = np.array([0.05,0.1,0.15,0.2,0.25,0.5,0.75,0.8,0.85,0.9,0.95]) #(11,)
nLst = np.arange(100, 1001, 100) #(10,)

m = 15
icc = 1/6
cv = 0.50
r = 0.15

# Make Table B-1 p.118 to validate ESS
Tab_B_1 = np.vectorize(ESS)(vR(pLst),vC(dLst))

# Calculate d as a function of n & p
dTab = np.vectorize(halfWidthCI)(vC(nLst),vR(pLst),m,icc,cv,r)

# Now use these calculated d-values to see if they give the right n
ndTab = np.vectorize(ESS)(vR(pLst),dTab) *  DE(m,icc,cv) * INF(r)
