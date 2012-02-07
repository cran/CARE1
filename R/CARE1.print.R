#' quick analysis of epidemiological data.
#' @param z the vector of capture histories or ascertainment records.
#' @return output
#' @author T.C. Hsieh
#' @export
#' @examples data(HAV)
#' CARE1.print(HAV)

CARE1.print <-	function(z){
	z=as.vector(unlist(z))
	
	if(length(z)==7){
		out1=matrix(0,1,3,dimnames =list(c("  "),c("     n1","       n2","       n3")))
		out2=matrix(0,3,5,dimnames=list(c("       pair(1,2)","       pair(1,3)","       pair(2,3)"),
						c("     Petersen","     Chapman","     se","    cil","    ciu")))
		out3=matrix(0,3,7,dimnames =list(c("       Nhat-0","       Nhat","       Nhat-1"),
						c("       M","     D","     C^","     est", "     se","     cil","     ciu")))
		
		out4=matrix(0,3,7,dimnames=list(c("       Nhat-0","       Nhat","       Nhat-1"),
						c("       u1","     u2","     u3","     r12","     r13","     r23","     r123")))
	}
	
	else if(length(z)==15){
		out1=matrix(0,1,4,dimnames =list(c("  "),c("     n1","       n2","       n3","       n4")))
		out2=matrix(0,6,5,dimnames=list(c("       pair(1,2)","       pair(1,3)","       pair(1,4)","       pair(2,3)","       pair(2,4)","       pair(3,4)"),
						c(" Petersen","   Chapman","   se","  cil","  ciu")))
		out3=matrix(0,3,7,dimnames =list(c("       Nhat-0","       Nhat","       Nhat-1"),
						c("     M","     D","     C^","     est", "     se","    cil","    ciu")))
		out4=matrix(0,3,10,dimnames =list(c("       Nhat-0","       Nhat","       Nhat-1"),
						c("     u1","     u2","     u3","     u4","     r12","     r13","     r14","     r23","     r24","     r34")))
	}
	
	else if(length(z)==31){
		out1=matrix(0,1,5,dimnames =list(c("  "),c("     n1","       n2","       n3","       n4","       n5")))
		out2=matrix(0,10,5,dimnames=list(c("       pair(1,2)","       pair(1,3)","       pair(1,4)","       pair(1,5)","       pair(2,3)","       pair(2,4)","       pair(2,5)","       pair(3,4)","       pair(3,5)","       pair(4,5)"),
						c("     Petersen","     Chapman","     se","    cil","    ciu")))
		out3=matrix(0,3,7,dimnames =list(c("       Nhat-0","       Nhat","       Nhat-1"),
						c("     M","     D","     C^","     est", "     se","    cil","    ciu")))
		out4=matrix(0,3,15,dimnames =list(c("       Nhat-0","       Nhat","       Nhat-1"),
						c("      u1","     u2","     u3","     u4","     u5","     r12","    r13","    r14","    r15","    r23","   r24","    r25","    r34","    r35","    r45")))
	}
	else  "Invalid argument error! Please input valid ascertainment records."

	out1[1,]=estN.n(z)
	out2[,1:5]=round(estN.pair(z))
	out3[1,]=cbind(estN.stat(z),round(estN(z,method="Indep",se=TRUE,nboot=200)))
	out3[2,]=cbind(estN.stat(z),round(estN(z,method="HSC",se=TRUE,nboot=200)))
	out3[3,]=cbind(estN.stat(z),round(estN(z,method="LSC",se=TRUE,nboot=200)))
	out4[1,]=round(estN.para(z,estN(z,method="Indep")),2)
	out4[2,]=round(estN.para(z,estN(z,method="HSC")),2)
	out4[3,]=round(estN.para(z,estN(z,method="LSC")),2)
	out=list(out1,out2,out3,out4)
	
	
	cat("Number of identified cases in each list: \n")
	print(out[[1]])
	cat("\n")
	
	cat("(1)ESTIMATES BASED ON ANY PAIR OF SAMPLES: \n")
	print(out[[2]]) 
	cat("\n")
	cat("
    Note1: Refer to Seber(1982,pages 59 and 60) for Petersen estimator
           and Chapman estimators as well as s.e formula.
    Note2: A log-transformation is used is used to obtain the confidence
           interval so that the lower limit is always greater than the
           number of ascertained. Refer to Chao(1987,Biometrics,43,783-791)
           for the construction of the confidence interval.\n")
	
	cat("(2)SAMPLE COVERAGE APPROACH: \n")
	print(out[[3]])
	cat("\n")
	if(lee1(z)<=0)
		cat("
    (0)Warning : The estimated sample coverage(overlapping information)is too 
                 low so that Nhat is unstable. Recommend the use of Nhat-0 or Nhat-1. \n")
	cat("   parameter estimates: \n")
	print(out[[4]]) 
	cat("\n")
	cat("
	DEFINITIONS for the sample coverage approach:
    M: number of individuals ascertained in at least one list.
    D: the average of the number of invididuals listed in the combination 
       of any two lists omitting the other one.
    C^: sample coverage estimate, see Equation (14) of Chao and Tsay(1998).
        est: population size estimate.
	se: estimated standard error of the population size estimation based on
        bootstrap replications. 
    cil: 95% confidence interval lower limit(using a log-transformation).   
    ciu: 95% confidence interval upper limit(using a log-transformation).
    Nhat: Population size estimate for sufficiently high sample coverage cases, 
          see Equation (20) of Chao and Tsay (1998).
    Nhat-1: One-step population size estimate for low sample coverage cases;
            see Equation (2.21) of Chao et al. (1996). This estimator is suggested
            for use when the estimated se of Nhat is relatively large.
    u1,u2,u3: estimated mean probabilities depending on the estimate of N.
    r12,r13,r23,r123 etc.: estimated coefficient of covariation(CCV) depending on the 
    				       estimate of N. 
					\n ") 
}

