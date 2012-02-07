\name{CARE1.print}
\alias{CARE1.print}
\title{A quick analysis function for capture-recapture data}
\usage{CARE1.print(z)}
\description{A quick analysis function for of capture-recapture data which integrating all output in the same time.}
\value{integrated output}
\arguments{\item{z}{the vector of capture histories or ascertainment records.}}
\references{
Chao, A. (1987). Estimating the population size for capture-recapture data with unequal catchability. Biometrics, 43: 783-791. \cr
Chao, A., Tsay, P. K., Lin, S. H., Shau, W. Y., and Chao, D. Y. (2001). The applications of capture-recapture models to epidemiological data. Statistics in Medicine, 20: 3123-3157. \cr
Seber, G. A. F. (1982). The estimation of animal abundance (2 ed.): Griffin, London.}
\examples{data(HAV)
CARE1.print(HAV)}
