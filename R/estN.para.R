#' estimated mean probabilities depending on the estimate of N, and estimated coefficient of covariation (CCV) depending on the estimate of N.
#' @title Parameter  estimates
#' @param z the vector of capture histories or ascertainment records.
#' @param nhat the estimation of population size.
#' @return Parameter  estimates
#' @author T.C. Hsieh
#' @note u:estimated mean probabilities depending on the estimate of N.\cr
#' r:estimated coefficient of covariation (CCV) depending on the estimate of N.
#' @references Chao A, Tsay P, Lin SH, Shau WY, Chao DY. 2001. The applications of capture recapture models to epidemiological data. Statistics in Medicine 20(20): 3123-3157.
#' @examples data(HAV)
#' estN.para(HAV)
#' @export

estN.para=function(z,nhat){
	z=as.vector(unlist(z))
	nhat=unlist(nhat)
	if(length(z) == 7)
		est = estN.para3(z,nhat)
	else if(length(z) == 15)
		est = estN.para4(z,nhat)		
	else if(length(z) == 31)
		est = estN.para5(z,nhat)
	else "Invalid argument error! Please input valid ascertainment records."
	return(est)
}

