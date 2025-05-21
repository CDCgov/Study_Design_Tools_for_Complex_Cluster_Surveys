  
################################################################################
#                         Part 1(Table B-4):
# Testing for differences in vaccination coverage between places or subgroups, 
# or between two future surveys
# 
# Null hypothesis(Ho): Coverages is the same in the two places/groups  
# Alternative Hypothesis(Ha): Coverage differs in the two places/groups 
################################################################################
# Parameters for function ESS_2Grp_2sided 
# P1: the estimated coverage level from one of the two surveys
# Delta: the difference above P1 form which the survey should be well
#        powered to reject the null hypothesis                                 
# Alpha: type 1 error, reject null when null is true, claim there is difference 
#        when in fact there isn't, default=0.05
# Beta:  type 2 error, fail to reject null when null is false, claim there is 
#        no differece in coverage when in fact there is, default=0.20
# SS_ratio: sample size ratio between two groups
#          (i.e., how many times SS in group2 over group 1),default=1.0
################################################################################

ESS_2Grp_2sided<-function(P1, Delta, Alpha=0.05, Beta=0.20,SS_ratio=1.0){
#Exception handling:check the input parameters, print the error message and exit the function
  if((P1<0|P1>1 )|(Delta<0|Delta>1)|(Alpha<0|Alpha>1)|(Beta<0|Beta>1)|SS_ratio<0)
  stop("P1, Delta, Alpha, Beta must be between 0 and 1, SS Ratio must be greater than 0")  

# Equation B3-1, n1 without continuity correction
  P2=P1+Delta
  P_hat=(P1+SS_ratio*(P1+Delta))/(SS_ratio+1.0)
  n1_no_cc_numer=(qnorm(1-Alpha/2.0)*sqrt((SS_ratio+1)*P_hat*(1-P_hat))+
                        qnorm(1-Beta)*sqrt(SS_ratio*P1*(1-P1)+P2*(1-P2)))**2
  n1_no_cc_denom=SS_ratio*((P2-P1)**2)
  n1_no_cc=n1_no_cc_numer/n1_no_cc_denom

#Equation B3-2
  n2_no_cc=SS_ratio*n1_no_cc

#Equation B3-3
n1_with_cc=(n1_no_cc/4.0)*(1.0+sqrt(1.0+(2*(SS_ratio+1))/(n1_no_cc*SS_ratio*abs(P2-P1))))**2
n2_with_cc=SS_ratio*n1_with_cc  

# Print out the results
print("###################################################")
print("Input parameters")
print(paste("P1=", P1,",", "Delta=", Delta,",", "P2=P1+Delta=", P2,",",
          "Alpha=", Alpha,",", "Beta=", Beta))

ESS1=ceiling(n1_with_cc)
ESS2=ceiling(n2_with_cc)
print("###################################################")
print("ESS for group 1(Table B-4):")
print(ESS1)
print("Sample Size ratio(i.e.,how many times ESS in group 2 vs group 1):")
print(SS_ratio)
print("ESS for group 2:")
print(ESS2)
print("###################################################")
#Return ESS1 and ESS2 in the list, and can be retrieved for the other calculations 
return(list(ESS1,ESS2))
}

ESS_2Grp_2sided(0.5,0.01,0.10,0.2,1)


#############################################################################
# Validate Part I: Reproduce table B-4
############################################################################

Validate_ESS_2Grp_2sided<-function(P1, Delta, Alpha=0.05, Beta=0.20,SS_ratio=1.0){
  #Exception handling:check the input parameters, print the error message and exit the function
  if((P1<0|P1>1 )|(Delta<0|Delta>1)|(Alpha<0|Alpha>1)|(Beta<0|Beta>1)|SS_ratio<0)
    stop("P1, Delta, Alpha, Beta must be between 0 and 1, SS Ratio must be greater than 0")  
  
  # Equation B3-1, n1 without continuity correction
  P2=P1+Delta
  P_hat=(P1+SS_ratio*(P1+Delta))/(SS_ratio+1.0)
  n1_no_cc_numer=(qnorm(1-Alpha/2.0)*sqrt((SS_ratio+1)*P_hat*(1-P_hat))+
                    qnorm(1-Beta)*sqrt(SS_ratio*P1*(1-P1)+P2*(1-P2)))**2
  n1_no_cc_denom=SS_ratio*((P2-P1)**2)
  n1_no_cc=n1_no_cc_numer/n1_no_cc_denom
  
  # Equation B3-2
  n2_no_cc=SS_ratio*n1_no_cc
  
  #Equation B3-3
  n1_with_cc=(n1_no_cc/4.0)*(1.0+sqrt(1.0+(2*(SS_ratio+1))/(n1_no_cc*SS_ratio*abs(P2-P1))))**2
  n2_with_cc=SS_ratio*n1_with_cc  
  
  ESS1=ceiling(n1_with_cc)
  ESS2=ceiling(n2_with_cc)
  
  return(ESS1)
}

P1<-c(seq(0.5,0.95, 0.05),seq(0.50,0.95,0.05),seq(0.50,0.90,0.05),seq(0.50,0.85,0.05))
Delta<-c(rep(0.01,length(seq(0.5,0.95, 0.05))),rep(0.05,length(seq(0.5,0.95, 0.05))),
         rep(0.10,length(seq(0.5,0.90, 0.05))),rep(0.15,length(seq(0.5,0.85, 0.05))))
Alpha<-c(0.10,0.05,0.10,0.05)
Beta<-c(0.2,0.2,0.10,0.10)
Table_B_4_C<-matrix(rep(0,37*4),nrow=37)
for(i in 1:37){
   for(j in 1:4){    
   Table_B_4_C[i,j]<-Validate_ESS_2Grp_2sided(P1[i], Delta[i], Alpha[j], Beta[j],SS_ratio=1.0)}
}

Table_B_4<-cbind(P1,Delta,Table_B_4_C)
colnames(Table_B_4)<-c("P1","Delta","Alpha=10%,Power=80%","Alpha=5%,Power=80%",
                       "Alpha=10%,Power=90%","Alpha=5%,Power=90%")
Table_B_4


################################################################################
#                         Part 2
# Testing for an increase in coverage over time, when the earlier survey was 
# conducted in the past
#  
# Null hypothesis(Ho): p2<=p1, coverage for the planned survey is less or equal
#                      than the previously conducted survey  
# Alternative Hypothesis: p2>p1, coverage for the planned survey is greater than
#                      the previous survey  
################################################################################
# Parameters for function ESS_1Grp_1sided 
# P1: coverage for the previously conducted survey
# P2: coverage for the planned survey
#        powered to reject the null hypothesis                                 
# Alpha: type 1 error, reject null when null is true, claim there is coverage 
#        increase when in fact there isn't, default=0.05
# Beta:  type 2 error, fail to reject null when null is false, claim there is 
#        no coverage increase when in fact there is, default=0.20
# ESS1: Effective sample size from early conducted survey
################################################################################

ESS_1Grp_1sided<-function(P1, P2, Alpha=0.05, Beta=0.20,ESS1){
  #Exception handling:check the input parameters, print the error message and exit the function
  if((P1<0|P1>1 )|(P2<0|P2>1)|(Alpha<0|Alpha>1)|(Beta<0|Beta>1)|P2<P1)
    stop("P1,P2, Delta, Alpha, Beta must be between 0 and 1, P1 must be less than P2")  
  
  # Equation B3-7, n without continuity correction
  P_hat=(P1+P2)/2
  n_no_cc_numer=(qnorm(1-Alpha)*sqrt(2*P_hat*(1-P_hat))+
                    qnorm(1-Beta)*sqrt(P1*(1-P1)+P2*(1-P2)))**2
  n_no_cc_denom=(P2-P1)**2
  n_no_cc=n_no_cc_numer/n_no_cc_denom
  
  
  # Equation B3-8
  
  n_with_cc=(n_no_cc/4.0)*(1.0+sqrt(1.0+4/(n_no_cc*abs(P2-P1))))**2
  
  # Equation B3-9
  SS_ratio=n_with_cc/(2*ESS1-n_with_cc)
  print ("Sample Size Ratio=")
  print(SS_ratio)
  if(SS_ratio<0) stop("No positive sample size ratio exists and the study as planned should be abandoned")
  
  # Equation B3-10
  if(SS_ratio>0) n2_with_cc=SS_ratio*ESS1
  
  # Print out the results
  print("###################################################")
  print("Input parameters")
  print(paste("P1=", P1,",", "p2=", P2,",","Alpha=", Alpha,",", "Beta=", 
              Beta,",", "ESS1=", ESS1))
  
  ESS2=ceiling(n2_with_cc)
  
  print("###################################################")
  print("ESS for group 1:")
  print(ESS1)
  print("Sample Size ratio(i.e.,how many times ESS in group 2 vs known sample size in group 1):")
  print(SS_ratio)
  print("ESS for group 2:")
  print(ESS2)
  print("###################################################")
  #Return ESS2  
  return(ESS2)
}

ESS_1Grp_1sided(0.7,0.8,0.05,0.2,174)

