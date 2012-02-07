#' Estimating population size via sample coverage
#' @param z the vector of capture histories or ascertainment records.
#' @param method the method employed to estimate population size.
#' method="Indep": population size estimate for independent samples;\cr
#' method="HSC": Population size estimate for sufficiently high sample coverage cases;\cr
#' method="LSC": One-step population size estimate for low sample coverage cases.
#' @param se should calculus bootstrap standard error?
#' @param nboot the number of bootstrap resampling times.
#' @return population size estimator
#' @author T.C. Hsieh
#' @note se: estimated standard error of the population size estimation based on
#' 200 bootstrap replications.  Note this s.e. might vary with trials. \cr
#' cil: 95\% confidence interval lower limit (using a log-transformation). \cr
#' ciu: 95\% confidence interval upper limit (using a log-transformation). \cr
#' Nhat-0(Indep): population size estimate for independent samples; see Equation (15) of Chao and Tsay (1998). \cr
#' Nhat: Population size estimate for sufficiently high sample coverage cases; see Equation (20) of Chao and Tsay (1998). \cr
#' Nhat-1: One-step population size estimate for low sample coverage cases; see Equation (2.21) of Chao et al. (1996). 
#' This estimator is suggested for use when the estimated s.e. of Nhat is relatively large.
#' @references Chao A, Tsay P, Lin SH, Shau WY, Chao DY. 2001. The applications of capture recapture models to epidemiological data. Statistics in Medicine 20(20): 3123-3157.
#' @examples data(HAV)
#' estN(HAV,method="LSC",se=TRUE,nboot=200)
#' @export

estN=function(z,method="Indep",se=FALSE,nboot=200){
	METHODS = c("Indep","HSC","LSC")
	method  = match.arg(method, METHODS)	
	z=as.vector(unlist(z))
	FUN=function(z){
		if(method == "Indep")
			nhat = estN.Indep(z)
		if(method == "HSC")
			nhat = estN.HSC(z)		
		if(method == "LSC")
			nhat = estN.LSC(z)
		return(c("est"=nhat))
	}
	nhat=FUN(z)
	
	if(se){
		z_bs = estN.bootstrap(z,round(nhat),nboot)
		se = sd(apply(z_bs,2,FUN))
		M=sum(z)
		c=exp(1.96*(log(1+se^2/(nhat-M)^2))^(0.5))
		cil=M+(nhat-M)/c
		ciu=M+(nhat-M)*c
		out=cbind("est"=nhat,"se"=se,"cil"=cil,"ciu"=ciu)
		return(out)
	}
	else
		return(cbind(nhat))
}
