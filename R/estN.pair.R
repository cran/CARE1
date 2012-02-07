#' Estimating population size based on any pair of samples
#' @param z the vector of capture histories or ascertainment records.
#' @return estimates based on any pair of samples
#' @author T.C. Hsieh
#' @note Note 1: Refer to Seber (1982, pages 59 and 60) for the Petersen 
#' estimator and the Chapman estimator as well as s.e. formula.\cr
#' Note 2: A log-transformation is used to obtain the confidence 
#' interval so that the lower limit is always greater than
#' the number of ascertained. Refer to Chao (1987, Biometrics,
#' 43, 783-791) for the construction of the confidence interval.
#' @references Chao A, Tsay P, Lin SH, Shau WY, Chao DY. 2001. The applications of capture recapture models to epidemiological data. Statistics in Medicine 20(20): 3123-3157.
#' @examples data(HAV)
#' estN.pair(HAV)
#' @export

estN.pair=function(z){
	z=as.vector(unlist(z))
	if(length(z) == 7)
		est = estN.pair3(z)
	else if(length(z) == 15)
		est = estN.pair4(z)		
	else if(length(z) == 31)
		est = estN.pair5(z)
	else "Invalid argument error! Please input valid ascertainment records."
	return(est)
}
