\name{Congenital}
\alias{Congenital}
\docType{data}
\title{Congenital anomaly data}
\description{
Infants born data with a specific congenital anomaly in Massachusetts
}
\usage{data(Congenital)}
\format{A five lists data with 31 variables.}
\details{
An epidemiological data with five distinct types of sources, which contains: obstetric records (183 cases); 
other hospital records (215 cases); list maintained by state Department of Public Health (36 cases); 
list maintained by state Department of Mental Health (263cases), and records by special schools (252 cases). 
The total number of cases identified was 537.
}

\references{
Fienberg, S. E. (1972). The multiple recapture census for closed populations and incomplete 2k contingency tables. Biometrika, 59: 591-603. 
Wittes, J. T., Colton, T., and Sidel, V. W. (1974). Capture-recapture methods for assessing the completeness of case ascertainment when using multiple information sources. Journal of Chronic Diseases, 27: 25. 
}
\examples{
data(Congenital)
CARE1.print(Congenital)
}
\keyword{datasets}
