#
# Python code
# Implements functionalities for study design from:
#   Vaccination Coverage Cluster Surveys: Reference Manual. 
#   Geneva: World Health Organization; 2018 (WHO/IVB/18.09). 
#   Licence: CC BY-NC-SA 3.0 IGO.
#   Ordering code: WHO/IVB/18.09
#
# Page numbers refer to this document
#
# Terms and Parameters used:
#   CI = Confidence Interval
#     halfWidthCI(n,p,m,icc,cv,r,alpha)
#       finds the half-width CI (d) for a given sample size and design
#     n = sample size
#     all other terms defined below
#   ESS = Effective Sample Size; p.129
#     ESS(p,d,alpha)
#     p = expected coverage proportion
#     d = desired half-width of the CI
#     alpha = the probability value used to define the precision for 
#             estimated CIs; usually 0.05 for 95% CI
#   DEFF = Design Effect; p.110
#     DE(m,icc,cv)
#     m = target number of respondents per cluster; p.109
#     icc = intracluster correlation coefficient; p.109
#     cv = coefficient of variation of sample weights
#   INF = Inflation factor to account for non-response; p.112, 128
#     INF(r)
#     r = anticipated non-response rate, from 0 to 1
#
# Outline:
#   * Computation functions
#   * Validation of computation functions
#   * Output functions
#   * Example usage
#


import numpy as np
from scipy.stats import norm
from math import ceil
#from sys import exit
import sys
from scipy.optimize import minimize_scalar
import itertools
import pandas as pd
import matplotlib.pyplot as plt

x = sys.path[0]+"\\utility.py"
exec(open(x).read())




#
# Computation functions
#
# ESS(p,d,alpha=0.05)
# DE(m,icc=1/6,cv=0.05)
# INF(r)
# halfWidthCI(n,p,m,icc,cv,r,alpha=0.05)

myImport("\\functions.py")






#
# Validation of computation functions
#

myImport("\\validation.py")


print('\nRecreate Table B-1 p.118 to validate ESS')
print('(d,p)')
print(Tab_B_1)


print('\nd calculated as a function of n & p')
print('(n,p)')
print(np.vectorize(round)(dTab,2))

print('\nDo the calculated d give back the correct n?')
print('(n,p)')
print(np.vectorize(round)(ndTab,1))

print("\nClose ... what's the difference?")
print(np.vectorize(round)(ndTab,1)-vC(nLst))






#
# Output functions
#
# nOutTab(p,d,m,icc,cv,r,alpha)
# dOutTab(n,p,m,icc,cv,r,alpha)

myImport("\\output.py")






#
# Example usage
#

# Some example inputs

n     = [300,900]
d     = [0.05,0.10]
p     = [0.10,0.25]
m     = [5,15]
icc   = [1/22,1/6]
cv    = [0.50]
nrr   = [15/100]
alpha = [0.05]

print('\nCompute sample size required for various inputs')
x = nOutTab(d,p,m,icc,cv,nrr,alpha)
print(x)

print("\nCompute study's half-width CI for various sample sizes & other inputs")
x = dOutput(n,p,m,icc,cv,nrr,alpha)
print(x)



# Some example inputs
n =  np.arange(100, 1001, 100) #(10,)
p = np.array([0.05,0.25])
m = 15
icc = 1/6
cv = 0.50
nrr = 15/100
alpha = 0.05

x = expand_grid(n,p)
x.columns = ('n','p')
x['d'] = x.apply(lambda x: halfWidthCI(x['n'],x['p'],m,icc,cv,nrr,alpha),axis=1)
#x.set_index('n',inplace=True)
#x.groupby('p')['d']
xp = x.pivot(index="n", columns="p", values="d")
xp.plot(legend=True)
plt.show()

# See how well d was computed (or not)
x['nBack'] = x.apply(lambda x: ESS(x['p'],x['d'])* DE(m,icc,cv)*INF(nrr),axis=1)
print(x)

