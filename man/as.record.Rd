\name{as.record}
\alias{as.record}
\title{Transform observed data to ascertainment records}
\usage{as.record(x)}
\description{Transform observed data to ascertainment records.}
\value{the vector of capture histories or ascertainment records.}
\arguments{\item{x}{the matrix of the observed capture histories}}
\details{x has one row per unit captured in the experiment. Each row is an observed capture history. 
It must contain only zeros and ones; the number one indicates a capture. 
In this case, the number of columns in the table represents the number of capture occasions in the experiment (noted t). 
Here is a fictive example of a data set of this type for t=3: \cr
	\tabular{rlll}{
	[1,]  \tab 0  \tab  1  \tab  0 \cr
	[2,]  \tab 1  \tab  0  \tab  1 \cr
	[3,]  \tab 0  \tab  1  \tab  0 \cr
	[4,]  \tab 0  \tab  1  \tab  1 \cr
	[5,]  \tab 1  \tab  0  \tab  1 \cr
	[6,]  \tab 1  \tab  0  \tab  1 \cr
	[7,]  \tab 0  \tab  0  \tab  1 \cr
	[8,]  \tab 0  \tab  1  \tab  0 \cr
	[9,]  \tab 0  \tab  1  \tab  0 \cr
	[10,] \tab 1  \tab  1  \tab  0 \cr
	}
}
\examples{
x=matrix(sample(0:1,300,TRUE),ncol=3)
as.record(x)}
