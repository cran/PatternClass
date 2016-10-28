\name{WhittleData}
\alias{WhittleData}
\docType{data}
\title{
Whittle correction matrix used by this package.
}
\description{
Specific location for storing the Whittle correction matrix to protect it from being overwritten accidentally.
}
\usage{data(WhittleData)}
\format{
  The format is:
<environment: 0x0000000005686de0> 
}
\details{
\code{DIFF50}{
This is a lookup table that has been generated in advance and provided with the package.  It provides the bias correction values for estimating spatial autocorrelation from a categorical map using the Whittle method. The lookup table is a matrix of 10 rows (configuration) and 9 columns (composition).  Each bias correction value is based on the mean of 50 replicates. This matrix can be rebuilt using more replicates and for data sets with extents different from 64x64 cells by calling the \code{build.lut} function. Warning: rebuilding this lookup tables takes a lot of time.
}
}
\source{
The data are all simulated using functions available in the \code{PatternClass} package.
}
\references{
Remmel, T.K. and M.-J. Fortin. 2013. Categorical class map patterns: characterization and comparison. Landscape Ecology. DOI: 10.1007/s/10980-013-9905-x.

Remmel, T.K. and M.-J. Fortin. What constitutes a significant difference in landscape pattern? (using R). 2016. In Gergel, S.E. and M.G. Turner. Learning landscape ecology: concepts and techniques for a sustainable world (2nd ed.). New York: Springer. 
}
\examples{
data(WhittleData)
str(WhittleData)
}
\keyword{datasets}
