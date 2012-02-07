\name{estN}
\alias{estN}
\title{Estimating population size via sample coverage}
\usage{estN(z, method="Indep", se=FALSE, nboot=200)}
\description{Population size estimation based on sample coverage approach for closed capture-recapture models.}
\arguments{\item{z}{the vector of capture histories or ascertainment records.}
\item{method}{the method employed to estimate population size.
method="Indep": population size estimate for independent samples;\cr
method="HSC": population size estimate for sufficiently high sample coverage cases;\cr
method="LSC": one-step population size estimate for low sample coverage cases.}
\item{se}{should calculus bootstrap standard error?}
\item{nboot}{the number of bootstrap resampling times.}}
\value{population size estimator}
\note{
	\itemize{
		\item{ \code{se}} {estimated standard error of the population size estimation based on bootstrap replications, which might vary with trials.}
		\item{ \code{cil}} {confidence interval lower limit (using a log-transformation).}
		\item{ \code{ciu}} {95\% confidence interval upper limit (using a log-transformation).}
		\item{ \code{Nhat-0(Indep)}} {population size estimate for independent samples; see Equation (9) of Chao et al. (2001).}
		\item {\code{Nhat}} {Population size estimate for sufficiently high sample coverage cases; see Equation (12) of Chao et al. (2001).}
		\item {\code{Nhat-1}} {One-step population size estimate for low sample coverage cases; see Equation (13) of Chao et al. (2001). 
					  This estimator is suggested for use when the estimated s.e. of Nhat is relatively large.}}
	}
\references{
Chao, A., and Tsay, P. K. (1998). A sample coverage approach to multiple-system estimation with application to census undercount. Journal of the American Statistical Association, 93: 283-293. \cr
Chao, A., Tsay, P. K., Lin, S. H., Shau, W. Y., and Chao, D. Y. (2001). The applications of capture-recapture models to epidemiological data. Statistics in Medicine, 20: 3123-3157. }
\examples{data(HAV)
estN(HAV,method="LSC",se=TRUE,nboot=200)}
