\name{estN.para}
\alias{estN.para}
\title{Parameter estimates}
\usage{estN.para(z,nhat)}
\description{Report the estimated mean probabilities depending on the estimate of N, and estimated coefficient of covariation (CCV) depending on the estimate of N.}
\value{Parameter  estimates}
\note{
	\itemize{
		\item{ \code{u}} {estimated mean probabilities depending on the estimate of N.}
		\item{ \code{r}} {estimated coefficient of covariation (CCV) depending on the estimate of N.}}
	}
\references{Chao, A., Tsay, P. K., Lin, S. H., Shau, W. Y., and Chao, D. Y. (2001). The applications of capture-recapture models to epidemiological data. Statistics in Medicine, 20: 3123-3157.}
\arguments{
\item{z}{the vector of capture histories or ascertainment records.}
\item{nhat}{the estimation of population size.}
}
\examples{data(HAV)
nhat <- estN(HAV,method="LSC",se=FALSE)
estN.para(HAV, nhat)}
