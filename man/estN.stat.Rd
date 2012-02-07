\name{estN.stat}
\alias{estN.stat}
\title{Statistics of sample coverage approach}
\usage{estN.stat(z)}
\description{Report some statistics of sample coverage approach.}
\value{
	\itemize{
		\item{ \code{M}} {number of individuals ascertained in at least one list.}
		\item{ \code{D}} {the average of the number of individuals listed in the combination of two lists omitting the third.}
		\item{ \code{C^}} {sample coverage estimate, see Equation (14) of Chao and Tsay (1998).}
		}
	}
\arguments{\item{z}{the vector of capture histories or ascertainment records.}}
\references{
Chao, A., and Tsay, P. K. (1998). A sample coverage approach to multiple-system estimation with application to census undercount. Journal of the American Statistical Association, 93: 283-293.}
\examples{data(HAV)
estN.stat(HAV)}
