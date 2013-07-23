\name{CARsimu}
\alias{CARsimu}
\title{
This function simulates an image using a FFT implementation of a Conditional Autoregressive (CAR) model.
}
\description{
A CAR landscape simulator for isotropic conditions.
}
\usage{
CARsimu(LEVEL = 6, rho = 0.2499, row2 = 0, col2 = 0, rc1 = 0, cr1 = 0, 
maindi = 1, rajz = TRUE)
}
\arguments{
  \item{LEVEL}{
This is the power (n) with base 2 that defines the image size: 2^n x 2^n.
}
  \item{rho}{
This is the spatial autocorrelation parameter.
}
  \item{row2}{
For later implementation - 2nd order neighbour spatial autocorrelation parameter (rows).
}
  \item{col2}{
For later implementarion - 2nd order neighbour spatial autocorrelation parameter (columns).
}
  \item{rc1}{
For later implementation - diagonal spatial autocorreltion parameter.
}
  \item{cr1}{
For later implementation - diagonal spatial autocorrelation parameter.
}
  \item{maindi}{
For later implementation.
}
  \item{rajz}{
When rajz = TRUE, the simulated map will be drawn.
}
}
\details{
This function can actually be parameterized in many ways, however, for isotropic landscapes, only the rho parameter is to be used.  Additional parameterizations, although possible, are not permitted with the use of the other functions provided with the \code{PatternClass} package
}
\value{
The output is a simulated map on a grid (provided as a matrix).
}
\references{
Remmel, T.K. and F. Csillag. 2003. When are two landscape pattern indices significantly different? Journal of Geographical Systems 5(4):331-351.

Remmel, T.K. and M.-J. Fortin. 2013. Categorical class map patterns: characterization and comparison. Landscape Ecology. DOI: 10.1007/s/10980-013-9905-x.

Remmel, T.K. and M.-J. Fortin. What constitutes a significant difference in landscape pattern? (using R) (Pending 2014). In Gergel, S.E. and M.G. Turner. Learning landscape ecology: concepts and techniques for a sustainable world (2nd ed.). New York: Springer. 
}
\author{
Ferenc (Ferko) Csillag and Sandor Kabos; modified by Tarmo K. Remmel
}
\note{
All simulated landscapes are stationary and isotropic, thus extent cannot be larger than what would be expected to generally adhere to these constraints.  The parameterization requires that the sum of all spatial autocorrelation parameters sum to < 0.5.  This is because the simulator is isotropic.  A perfect 1.0 spatial autocorrelation parameter can be approximated by 0.499999 (infinitely repeating), due to computational limits.  Outputs of this function can be saved to objects and used as continuous simulated landscape maps.  To obtain categorical maps, these continuous maps can be sliced based on their distributions, as necessary.
}

\seealso{
See Also as \code{\link{singlemap}}
}
\examples{
CARsimu(LEVEL = 6, rho = 0.2499, row2 = 0, col2 = 0, rc1 = 0, cr1 = 0, 
maindi = 1, rajz = TRUE)
}
\keyword{ hplot }
