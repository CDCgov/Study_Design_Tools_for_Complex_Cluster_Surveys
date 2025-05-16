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
#   Delta = [ ... will include code to calculate this for given n ...]
#
#   ESScl = Effective Sample Size; p.131
#     ESScl(P0, delta, alpha, beta, direction)
#     P0 = the programmatic threshold
#     delta = a coverage percent defining a distance from P0
#     alpha = Type I error
#     beta = Type II error
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
# ESScl(P0, delta, alpha, beta, direction)
# DE(m,icc=1/6,cv=0.05)
# INF(r)
# halfWidthCI(n,p,m,icc,cv,r,alpha=0.05)

source("R/functions.R")






#
# Validation of computation functions
#

source("R/validation.R")

# Recreate Tables B-2 p.123 & B-3 p. 125 to validate ESScl
# (d,p)
Tab_B_2
Tab_B_3

# delta calculated as a function of n, P0, alpha, beta, direction
deltaTab








#
# Output functions
#
# nclOutTab(P0,delta,alpha,beta,direction,m,icc,cv,r)

source("R/output.R")






#
# Example usage
#

# Some example inputs
p = c(.7,.8)
d = c(.01,.05)
a = c(.1,.05)
b = c(.2,.1)
m = c(5,15)
icc = c(1/22,1/6)
cv = c(0.50)
r = c(15)/100

# Compute sample size required for various inputs
exTab = nclOutTab(p,d,a,b,'below',m,icc,cv,r)[[1]]

# Compute delta for given n and everything else
exTab$deltaGot = apply(exTab,1,function(x)
  getDelta(x[12],x[1],x[3],x[4],'below',x[5],x[6],x[7],x[8])
)
exTab









# n = c(7000,500)
# P0 = c(.7,.8)
# alpha = c(.1)
# beta = c(.2)
# direction = 'below'
# m = c(5)
# icc = c(1/22)
# cv = c(0.50)
# r = c(15)/100
# #n = c(9363,7418,411,328,13165,10115,554,438,
# #        14024,10804,595,476,18230,14023,766,609)
# 
# 
# 
# # Find delta given n (and everything else)
# #
# deltaOutput = function(n, P0, alpha, beta, direction, m, icc, cv, r){
#   val=expand.grid(n,P0,alpha,beta,m,icc,cv,r)
#   colnames(val) = c("n","P0","alpha","beta","m","icc","cv","r")
#   val$delta = apply(val,1,function(x)
#       getDelta(x[1],x[2],x[3],x[4],direction,x[5],x[6],x[7],x[8])
#     )
#   val
# } 
# 
# 
# 
# deltaOutput(n,P0,alpha,beta,'below',m,icc,cv,r)
#val$direction = as.character(val$direction)
#val = val[!(val$direction=='below' & val$P0-val$delta<0),]
#val = val[!(val$direction=='above' & val$P0+val$delta>1),]
