\name{build.lut}
\alias{build.lut}
\title{
Bias correction lookup table (LUT) builder based on simulations.
}
\description{
This function needs to be run only once, at the beginning of analysis, to produce a lookup table of spatial autocorrelation (rho) bias correction factors.  Since this process takes a long time, due to the need for simulating numerous replicates (generally 50 or more) landscapes at each pair-wise combination of composition and configuration parameterization, we provide a lookup table with the package that is used by the \code{singlemap} function internally.  Currently, the implementation is limited to 64x64 pixel maps; however, future developments will expand on this limitation and thus the build.lut function is provided for those that may wish to fast-track the construction of scaled lookup tables for analyses of landscapes of different size.  Bias corrections are achieved by comparing Whittle estimates of spatial autocorrelation on binary versus continuous surfaces with identical parameterizations.
}
\usage{
build.lut(LEVEL = 6, REPSIM = 5, RAJZ = FALSE, CIM = "", ENV="data")
}
\arguments{
  \item{LEVEL}{
A power (n) of base 2 that controls the size of the simulated maps used to process the bias correction surface: 2^n x 2^n cells.
}
  \item{REPSIM}{
The number of simulation replicates at each combination of composition and configuration.
}
  \item{RAJZ}{
A Boolean flag that when TRUE will draw the result.
}
  \item{CIM}{
An option that permits the naming of image objects.  This is provided for potential future expansion.
}
  \item{ENV}{
The name of the environment where temporary objects are stored. The default is data.
}

}
\details{
This does not need to be run if you have downloaded and installed the \code{PatternClass} package and are using it to analyze 64x64 pixel image subsets as proposed.  The \code{singlemap} function will call on the provided internal lookup table and thus save on substantial processing time.  The current implementation provides a lookup table with bias correction values based on the mean of 50 replicate simulations for each pair-wise combination of composition and configuration parameterization.  Thus, the 9 levels of composition, 11 levels of spatial autocorrelation, and 50 replicates, means that 9*11*50 = 4950 landscapes are simulated and used to produce the bias correction lookup table that is provided with the package.
}
\value{
The result is a lookuptable (LUT) that is a 9x11 matrix of values used by singlemap().
}
\references{
Remmel, T.K. and M.-J. Fortin. 2013. Categorical class map patterns: characterization and comparison. Landscape Ecology. DOI: 10.1007/s/10980-013-9905-x.

Remmel, T.K. and M.-J. Fortin. What constitutes a significant difference in landscape pattern? (using R). 2016. In Gergel, S.E. and M.G. Turner. Learning landscape ecology: concepts and techniques for a sustainable world (2nd ed.). New York: Springer. 
}
\author{
Tarmo K. Remmel
}
\note{
The current implementation is for 64x64 image subsets.  Future developments will expand this extent limitation.  However, our work has shown that this extent provides a reasonable compromise between statistical power and computing efficiency.
}
\seealso{
See Also \code{\link{singlemap}}
}
\examples{
\donttest{build.lut(LEVEL = 6, REPSIM = 5, RAJZ = FALSE, CIM = "", ENV="data")}
}
\keyword{ distribution }
