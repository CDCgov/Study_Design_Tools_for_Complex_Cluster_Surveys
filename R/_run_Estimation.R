## rm(list=ls());gc()

#
# R code
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






#
# Setup
#

# set working directory as git repo root
setwd("C:/Users/JNN6/OneDrive - CDC/GitHub/cluster_survey_sample_size")






#
# Computation functions
#
# ESS(p,d,alpha=0.05)
# DE(m,icc=1/6,cv=0.05)
# INF(r)
# halfWidthCI(n,p,m,icc,cv,r,alpha=0.05)

source("R/functions.R")






#
# Validation of computation functions
#

source("R/validation.R")

# Recreate Table B-1 p.118 to validate ESS
# (d,p)
Tab_B_1


# d calculated as a function of n & p
# (n,p)
round(dTab,2)

# Do the calculated d give back the correct n?
# (n,p)
round(ndTab,1)

# Close ... what's the difference?
round(ndTab,1)-nLst






#
# Output functions
#
# nOutTab(p,d,m,icc,cv,r,alpha)
# dOutTab(n,p,m,icc,cv,r,alpha)

source("R/output.R")






#
# Example usage
#

# Some example inputs
n = c(300,900)
d = c(5,10)/100
p = c(0.10,0.25)
m = c(5,15)
icc = c(1/22,1/6)
cv = c(0.50)
r = c(15)/100
alpha = c(0.05)

# Compute sample size required for various inputs
nOutTab(d,p,m,icc,cv,r,alpha)

# Compute study's half-width CI for various sample sizes & other inputs
dOutput(n,p,m,icc,cv,r,alpha)



# Some example inputs
n = seq(100,1000,by=100)
p = c(0.05,0.25)
m = 15
icc = 1/6
cv = 0.50
r = 15/100
alpha = 0.05

# Examine how d changes for varying n
dPlot = outer(n,p,Vectorize(halfWidthCI),m,icc,cv,r)
plot(n,dPlot[,1],type='l')
lines(n,dPlot[,2],col='red')

# See how well d was computed (or not)
sapply(dPlot,function(x)ESS(0.05,x)* DE(m,icc,cv)*INF(r))


